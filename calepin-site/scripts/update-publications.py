#!/usr/bin/env python3
"""Add new publications to cv/cv.yml from ORCID and OpenAlex, and complete the
entries this script added once the journal assigns a volume, issue and pages.

What it adds:

  * Candidates are the DOIs on Ben's ORCID record plus the DOIs OpenAlex links
    to the same ORCID iD (OpenAlex also finds book chapters ORCID lacks).
  * A candidate is skipped if cv.yml already has its DOI or an entry with the
    same title (many older entries have no DOI), or if its DOI is listed under
    "ignore" in data/publications-sync.json.
  * Only journal articles, book chapters and books are added, and only when
    Crossref lists Ben as an author. Datasets, book reviews, errata and
    preprints are left out, as are chapters of a book already listed under
    Monographs (e.g. the separate chapters of External Voting).
  * The entry is built from Crossref's metadata in cv.yml's citation style and
    goes at the top of its year in the matching section.

What it changes afterwards:

  * Nothing in an entry it did not add. Entries it added are recorded under
    "managed" in data/publications-sync.json. While one of them is incomplete
    (an online-first article with no volume or pages yet), each run asks
    Crossref again; once the details appear, it writes them in after the
    journal name and switches the year to the issue's year. It only replaces
    the text it wrote itself, so hand edits to the authors or title survive.
    If that text was edited by hand too, it leaves the entry alone and prints
    the new details instead.
  * Deleting an entry it added from cv.yml moves its DOI to "ignore", so it is
    not added again. To keep any other DOI out, add it to "ignore" by hand.
  * When quarto-site/cv/cv.yml is identical to this cv.yml, the same changes
    are written there too, so the Quarto backup stays in step.

Run by build.sh before the CV PDF is compiled. Network failures are non-fatal:
the build then uses cv.yml as it stands. Standard library only.
Usage:  python3 scripts/update-publications.py [--dry-run] [path/to/cv.yml]
"""

import datetime
import html
import json
import os
import re
import ssl
import sys
import unicodedata
import urllib.error
import urllib.parse
import urllib.request

ORCID = "0000-0001-6932-1046"
UA = "benstanley.eu publications check (https://benstanley.eu)"

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_YML = os.path.abspath(os.path.join(HERE, "..", "cv", "cv.yml"))
STATE = os.path.join(HERE, "..", "data", "publications-sync.json")
MIRROR = os.path.abspath(os.path.join(HERE, "..", "..", "quarto-site", "cv", "cv.yml"))

PUB_SECTIONS = ("monographs", "journal_articles", "book_chapters", "datasets")
# Crossref type -> cv.yml section. Crossref files some monographs (Good Change
# among them) as "edited-book", so that counts too when Ben is an author.
SECTION_FOR = {"journal-article": "journal_articles", "book-chapter": "book_chapters",
               "book": "monographs", "monograph": "monographs", "edited-book": "monographs"}
# Work types, as ORCID or OpenAlex label them, that never go on the list.
EXCLUDED_TYPES = {"dataset", "data-set", "book-review", "erratum", "peer-review",
                  "paratext", "retraction", "preprint", "supplementary-materials"}
# Lower-case in headline-style titles unless first, last or after a colon.
SMALL_WORDS = set("""a an the and but or nor for so yet as at by from in into of on onto
    per than to upon via vs with within without about across after against along amid
    among around between beyond despite during through toward towards under""".split())
# English function words that are not also Polish words (unlike "a", "do", "on", "to"),
# for telling English titles from Polish ones that happen to have no diacritics.
ENGLISH_WORDS = set("""the of and in for with from by is are an its their how what why
    when after between into or not does among under""".split())
BOOK_TYPES = ("book", "edited-book", "monograph", "reference-book")

KEY = re.compile(r"^\s*- key:\s*(\S+)\s*$")
ENTRIES = re.compile(r"^\s*entries:\s*$")
ITEM = re.compile(r'^(\s*)- "(.*)"\s*$')
DOI = re.compile(r"(?:doi:\s*|doi\.org/)(?:https?://(?:dx\.)?doi\.org/)?(10\.\d{4,9}/[^\s\"]+)", re.I)
TITLE = (re.compile(r"\b(?:19|20)\d\d[a-z]?[.,]\s+'(.+?)'[.?!]?\s+(?:\*|In\s)"),   # 'Article'. *Journal*
         re.compile(r"\b(?:19|20)\d\d[a-z]?[.,]\s+\*(.+?)\*"))                    # *Monograph*
FOLD = str.maketrans({"ł": "l", "Ł": "L", "ø": "o", "Ø": "O", "đ": "d", "Đ": "D", "ß": "ss"})


# --- network -----------------------------------------------------------------

def fetch_json(url):
    req = urllib.request.Request(url, headers={"User-Agent": UA, "Accept": "application/json"})
    try:
        body = urllib.request.urlopen(req, timeout=30).read()
    except urllib.error.HTTPError:
        raise
    except (urllib.error.URLError, ssl.SSLError, OSError):
        # As in update-kl.py: some macOS Pythons have no CA bundle wired up.
        ctx = ssl._create_unverified_context()
        body = urllib.request.urlopen(req, timeout=30, context=ctx).read()
    return json.loads(body.decode("utf-8"))


def crossref(doi):
    """Crossref's record for a DOI, or None if Crossref doesn't know it."""
    try:
        return fetch_json("https://api.crossref.org/works/" + urllib.parse.quote(doi, safe="/"))["message"]
    except urllib.error.HTTPError as e:
        if e.code == 404:
            return None
        raise


def clean_doi(s):
    s = re.sub(r"^(https?://(dx\.)?doi\.org/|doi:)", "", (s or "").strip(), flags=re.I)
    s = s.rstrip(".").lower()
    return s if s.startswith("10.") else None


def candidates():
    """Return ({doi: {"titles": set, "types": set}}, [failed sources])."""
    found, failed = {}, []

    def add(doi, title, kind):
        if doi:
            c = found.setdefault(doi, {"titles": set(), "types": set()})
            if title:
                c["titles"].add(title)
            if kind:
                c["types"].add(kind)

    try:
        data = fetch_json("https://pub.orcid.org/v3.0/%s/works" % ORCID)
        for group in data.get("group") or []:
            for w in group.get("work-summary") or []:
                title = ((w.get("title") or {}).get("title") or {}).get("value")
                for x in (w.get("external-ids") or {}).get("external-id") or []:
                    # A "part-of" DOI is the book a chapter appears in, not the chapter.
                    if x.get("external-id-type") == "doi" and (x.get("external-id-relationship") or "self") == "self":
                        add(clean_doi(x.get("external-id-value")), title, w.get("type"))
    except Exception as e:
        failed.append("ORCID (%s)" % e)

    try:
        data = fetch_json("https://api.openalex.org/works?per-page=200&select=doi,title,type"
                          "&filter=author.orcid:" + ORCID)
        for w in data.get("results") or []:
            add(clean_doi(w.get("doi")), w.get("title"), w.get("type"))
    except Exception as e:
        failed.append("OpenAlex (%s)" % e)

    return found, failed


# --- titles ------------------------------------------------------------------

def title_key(s):
    """Lower-case ASCII words only, for comparing titles across sources."""
    s = unicodedata.normalize("NFKD", (s or "").translate(FOLD))
    s = "".join(c for c in s if not unicodedata.combining(c)).lower()
    return " ".join(re.findall(r"[a-z0-9]+", s))


def main_key(s):
    """title_key of the part before the first colon or question mark."""
    return title_key(re.split(r"[:?!]", s or "", maxsplit=1)[0])


def same_title(a, b):
    """True if two titles name the same work. Strict on purpose: a false match
    silently drops a new paper, while a missed match only adds a duplicate
    that shows up in the build output."""
    if title_key(a) == title_key(b):
        return True
    ma, mb = main_key(a), main_key(b)
    return ma == mb and len(ma.split()) >= 4


def entry_title(text):
    for pattern in TITLE:
        m = pattern.search(text)
        if m:
            return m.group(1)
    return None


# --- formatting --------------------------------------------------------------

def text(s):
    """A Crossref string as plain text in cv.yml's conventions."""
    s = html.unescape(re.sub(r"<[^>]+>", "", s or ""))
    for a in "’‘“”":
        s = s.replace(a, "'")
    return re.sub(r"\s+", " ", s).strip()


def typst(s):
    """Escape what Typst markup would interpret: cv.yml entries are eval'd."""
    s = re.sub(r"([\\#$@<>_*`~\[\]])", r"\\\1", s)
    return s.replace("//", "/\\/")


def capitalise(word):
    """Upper-case the first letter, leaving words with inner capitals (PiS) alone."""
    m = re.search(r"[^\W\d_]", word)
    if not m or any(c.isupper() for c in word[m.start() + 1:]):
        return word
    i = m.start()
    return word[:i] + word[i].upper() + word[i + 1:]


def title_case(s):
    """Headline-style capitalisation, as in the existing cv.yml entries."""
    letters = [c for c in s if c.isalpha()]
    if letters and sum(c.isupper() for c in letters) > 0.8 * len(letters):
        s = s.lower()                                   # an ALL-CAPS deposit
    words = s.split(" ")
    out, start = [], True
    for n, word in enumerate(words):
        parts = re.split(r"([-–])", word)               # odd indices are the dashes
        last = len(parts) - 1
        for k in range(0, len(parts), 2):
            edge = (start and k == 0) or (n == len(words) - 1 and k == last)
            bare = re.sub(r"^\W+|\W+$", "", parts[k])
            if not edge and bare.lower() in SMALL_WORDS and not (len(bare) > 1 and bare.isupper()):
                parts[k] = parts[k].lower()
            else:
                parts[k] = capitalise(parts[k])
        out.append("".join(parts))
        start = bool(re.search(r"[:?!—][')\"]*$", word))
    return " ".join(out)


def is_english(msg, title):
    """Decides whether to title-case. Crossref's language field is unreliable
    (Studia Polityczne's Polish articles are filed as "en"), so look first."""
    if re.search(r"[ąćęłńśźżĄĆĘŁŃŚŹŻ]", title):
        return False
    if set(title_key(title).split()) & ENGLISH_WORDS:
        return True
    lang = (msg.get("language") or "").lower()
    return lang.startswith("en") and len(title.split()) <= 3


def name(p, inverted=False):
    given, family = text(p.get("given")), text(p.get("family"))
    if not family:
        return text(p.get("name")) or given
    given, family = (s.title() if len(s) > 2 and s.isupper() else s for s in (given, family))
    if not given:
        return family
    return "%s, %s" % (family, given) if inverted else "%s %s" % (given, family)


def author_list(people):
    names = [name(p, inverted=(i == 0)) for i, p in enumerate(people)]
    if len(names) > 10:                                 # as in the Claassen et al. entry
        return ", ".join(names[:7]) + ", et al."
    if len(names) < 3:
        return ", and ".join(names)                     # "Bill, Stanley, and Ben Stanley"
    return ", ".join(names[:-1]) + ", and " + names[-1]


def editor_list(people):
    names = [name(p) for p in people]
    if not names:
        return ""
    if len(names) == 1:
        return "ed. " + names[0]
    if len(names) == 2:
        return "eds. %s and %s" % tuple(names)
    return "eds. " + ", ".join(names[:-1]) + ", and " + names[-1]


def page_range(p):
    """'451-470' -> '451–70', '1295-1322' -> '1295–1322', as cv.yml writes them."""
    m = re.match(r"^\s*(\d+)\s*[-–—]\s*(\d+)\s*$", p or "")
    if not m:
        return text(p)
    a, b = m.groups()
    if a == b:
        return a
    if len(a) == len(b) > 2 and a[:-2] == b[:-2]:
        b = b[-2:]
    return "%s–%s" % (a, b)


def year(msg, in_issue=False):
    """An article in an issue takes the issue's year (as the existing entries
    do); anything else takes its first publication year."""
    def y(d):
        parts = ((d or {}).get("date-parts") or [[None]])[0]
        return parts[0] if parts and parts[0] else None
    if in_issue:
        return (y((msg.get("journal-issue") or {}).get("published-print"))
                or y(msg.get("published-print")) or y(msg.get("issued")))
    return min(filter(None, (y(msg.get(k)) for k in ("issued", "published-online", "published-print"))),
               default=None)


def parent_book(msg):
    """Crossref's record for the book a chapter is in, found by ISBN, or None.
    Chapter records rarely carry the editors or the book's subtitle."""
    for isbn in [i.get("value") for i in msg.get("isbn-type") or []] or msg.get("ISBN") or []:
        try:
            items = fetch_json("https://api.crossref.org/works?rows=10&filter=isbn:"
                               + urllib.parse.quote(isbn))["message"]["items"]
        except Exception:
            continue
        for item in items:
            if item.get("type") in BOOK_TYPES:
                return item
    return None


def full_title(msg):
    title = text((msg.get("title") or [""])[0])
    sub = text((msg.get("subtitle") or [""])[0])
    if sub and sub.lower() not in title.lower():
        title += ": " + sub
    return title


def build(msg, book=None):
    """Return (section, head, tail, year, complete) for a Crossref record
    (book: the parent book's record, for a chapter). head is "Authors. Year.
    'Title'. "; tail is the rest, from the venue on, the part a later run may
    replace."""
    section = SECTION_FOR[msg["type"]]
    title = full_title(msg)
    english = is_english(msg, title)
    if english:
        title = title_case(title)
    authors = typst(author_list(msg.get("author") or []))
    doi = msg["DOI"]
    venue = text((msg.get("container-title") or [""])[0])
    publisher = text(msg.get("publisher"))
    if msg.get("publisher-location"):
        publisher = "%s: %s" % (text(msg["publisher-location"]), publisher)

    if section == "journal_articles":
        vol, issue = text(msg.get("volume")), text(msg.get("issue"))
        pages = page_range(msg.get("page")) or text(msg.get("article-number"))
        details = ""
        if vol:      # online-first records carry provisional pages (1-36) but no volume
            details = " " + vol + ("(%s)" % issue if issue else "") + (": " + pages if pages else "")
        tail = "*%s*%s. doi:%s." % (typst(venue), details, doi)
        complete = bool(vol and pages)
        yr = year(msg, in_issue=bool(vol))
    elif section == "book_chapters":
        editors = msg.get("editor") or []
        if book:
            venue = full_title(book) or venue
            # Some publishers (Routledge) file a volume's editors as its authors.
            book_authors = book.get("author") or []
            editors = editors or book.get("editor") or ([] if any(map(is_ben, book_authors)) else book_authors)
        if english:
            venue = title_case(venue)
        eds = editor_list(editors)
        pages = page_range(msg.get("page"))
        tail = "In *%s*%s. %s%s. doi:%s." % (typst(venue), ", " + typst(eds) if eds else "",
                                             typst(publisher), ", " + pages if pages else "", doi)
        complete = bool(pages)
        yr = year(msg)
    else:
        tail = "%s. doi:%s." % (typst(publisher), doi)
        complete = True
        yr = year(msg)

    yr = yr or datetime.date.today().year
    stop = "" if title.endswith(("?", "!")) else "."
    if not authors.endswith("."):                       # "et al." already has its stop
        authors += "."
    if section == "monographs":
        head = "%s %d. *%s*%s " % (authors, yr, typst(title), stop)
    else:
        head = "%s %d. '%s'%s " % (authors, yr, typst(title), stop)
    return section, head, tail, yr, complete


def is_ben(person):
    if ORCID in (person.get("ORCID") or ""):
        return True
    return title_key(person.get("family")) == "stanley" and title_key(person.get("given")).startswith("b")


# --- cv.yml ------------------------------------------------------------------

def unquote(s):
    return re.sub(r"\\(.)", r"\1", s)        # entries only ever use \" and \\


def quote(s):
    return s.replace("\\", "\\\\").replace('"', '\\"')


def dois_in(s):
    return {d for d in (clean_doi(m) for m in DOI.findall(s)) if d}


def parse(lines):
    """{section key: {"entries_at": line index, "items": [(line index, text)]}}"""
    sections, current = {}, None
    for i, line in enumerate(lines):
        m = KEY.match(line)
        if m:
            current = sections.setdefault(m.group(1), {"entries_at": None, "items": []})
        elif current is not None:
            if ENTRIES.match(line):
                current["entries_at"] = i
            elif current["entries_at"] is not None:
                m = ITEM.match(line)
                if m:
                    current["items"].append((i, unquote(m.group(2))))
    return sections


def entry_year(t):
    m = re.search(r"\b(19|20)\d\d\b", t)
    return int(m.group(0)) if m else 0


def insert(lines, section, entry, yr):
    """Insert entry at the top of its year in section. False if no such section."""
    sec = parse(lines).get(section)
    if not sec or sec["entries_at"] is None:
        return False
    items = sec["items"]
    if items:
        indent = ITEM.match(lines[items[0][0]]).group(1)
    else:
        indent = re.match(r"^(\s*)", lines[sec["entries_at"]]).group(1) + "  "
    new = '%s- "%s"' % (indent, quote(entry))
    at = next((i for i, t in items if entry_year(t) <= yr), None)
    if at is not None:
        lines[at:at] = [new, ""]
    elif items:
        lines[items[-1][0] + 1:items[-1][0] + 1] = ["", new]
    else:
        lines[sec["entries_at"] + 1:sec["entries_at"] + 1] = [new]
    return True


def find_entry(lines, doi):
    for i, line in enumerate(lines):
        m = ITEM.match(line)
        if m and doi in dois_in(m.group(2)):
            return i, m.group(1), unquote(m.group(2))
    return None


def write(path, content):
    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as fh:
        fh.write(content)
    os.replace(tmp, path)


# --- main --------------------------------------------------------------------

def main():
    dry = "--dry-run" in sys.argv[1:]
    args = [a for a in sys.argv[1:] if a != "--dry-run"]
    path = os.path.abspath(args[0] if args else DEFAULT_YML)
    with open(path, encoding="utf-8") as fh:
        original = fh.read()
    lines = original.split("\n")

    state = {}
    if os.path.exists(STATE):
        with open(STATE, encoding="utf-8") as fh:
            state = json.load(fh)
    state.setdefault("ignore", {})
    state.setdefault("managed", {})
    state_before = json.dumps(state, sort_keys=True)
    ignore, managed = state["ignore"], state["managed"]
    log = []

    # 1. Complete the entries this script added that were still missing details.
    completed = 0
    for doi, rec in list(managed.items()):
        if rec.get("complete"):
            continue
        found = find_entry(lines, doi)
        if found is None:
            del managed[doi]
            ignore[doi] = "Added by update-publications.py, then removed from cv.yml by hand."
            log.append("[publications] doi:%s was removed from cv.yml; added it to the ignore list." % doi)
            continue
        try:
            msg = crossref(doi)
        except Exception as e:
            log.append("[publications] warning: could not check doi:%s on Crossref (%s)." % (doi, e))
            continue
        if not msg or msg.get("type") not in SECTION_FOR:
            continue
        _, _, tail, yr, complete = build(msg)
        if not complete:
            continue                                    # nothing final from the publisher yet
        if rec["section"] == "book_chapters":
            # Add the pages only, keeping the book and editors as first written.
            tail = rec["tail"].replace(". doi:", ", %s. doi:" % page_range(msg.get("page")), 1)
        i, indent, current = found
        if rec["tail"] not in current:
            log.append("[publications] needs attention: the entry for doi:%s was edited by hand, so it was"
                       " left alone. Crossref now has: %s" % (doi, tail))
            rec["complete"] = True
            continue
        new = current.replace(rec["tail"], tail)
        if yr != rec["year"]:
            new = re.sub(r"\b%d\b" % rec["year"], str(yr), new, count=1)
        lines[i] = '%s- "%s"' % (indent, quote(new))
        rec.update(tail=tail, year=yr, complete=complete)
        completed += 1
        log.append("[publications] ~ updated doi:%s -> %s" % (doi, tail))

    # 2. Add works that ORCID or OpenAlex know about and cv.yml doesn't.
    added = 0
    found, failed = candidates()
    for source in failed:
        log.append("[publications] warning: could not reach %s; new works from it are not checked." % source)
    cv_dois = dois_in("\n".join(lines))
    sections = parse(lines)
    titles = [t for key in PUB_SECTIONS for _, e in sections.get(key, {}).get("items", [])
              for t in [entry_title(e)] if t]
    monographs = [t for _, e in sections.get("monographs", {}).get("items", []) for t in [entry_title(e)] if t]
    skip = cv_dois | {d.lower() for d in ignore} | {d.lower() for d in managed}

    for doi in sorted(found):
        c = found[doi]
        if doi in skip or c["types"] & EXCLUDED_TYPES:
            continue
        if any(same_title(a, b) for a in c["titles"] for b in titles):
            continue
        # A chapter of a book whose DOI is listed, e.g. 10.1007/978-3-031-19246-3_1.
        if any(doi.startswith(d) and doi[len(d):len(d) + 1] in ("_", "-", ".") for d in cv_dois):
            continue
        try:
            msg = crossref(doi)
        except Exception as e:
            log.append("[publications] warning: could not look up doi:%s on Crossref (%s)." % (doi, e))
            continue
        if not msg or msg.get("type") not in SECTION_FOR:
            continue
        if not any(is_ben(p) for p in msg.get("author") or []):
            continue                                    # an editor credit or an OpenAlex mix-up
        if any(same_title(full_title(msg), b) for b in titles):
            continue
        book = None
        if SECTION_FOR[msg["type"]] == "book_chapters":
            container = text((msg.get("container-title") or [""])[0])
            if any(main_key(container) == main_key(m) for m in monographs):
                continue                                # a chapter of one of Ben's own books
            book = parent_book(msg)
        section, head, tail, yr, complete = build(msg, book)
        if not insert(lines, section, head + tail, yr):
            log.append("[publications] needs attention: cv.yml has no '%s' section with entries, so"
                       " doi:%s was not added." % (section, doi))
            continue
        managed[msg["DOI"].lower()] = {"section": section, "added": datetime.date.today().isoformat(),
                                       "year": yr, "tail": tail, "complete": complete}
        titles.append(full_title(msg))
        added += 1
        log.append("[publications] + %s %s: %s%s" % (yr, section, head + tail,
                   "" if complete else "  (online first: volume and pages to follow)"))

    # 3. Save.
    content = "\n".join(lines)
    if dry:
        log.append("[publications] dry run: nothing written.")
    else:
        if content != original:
            write(path, content)
            if path == DEFAULT_YML and os.path.exists(MIRROR):
                with open(MIRROR, encoding="utf-8") as fh:
                    in_step = fh.read() == original
                if in_step:
                    write(MIRROR, content)
                    log.append("[publications] mirrored the changes to quarto-site/cv/cv.yml.")
                else:
                    log.append("[publications] needs attention: quarto-site/cv/cv.yml had already diverged"
                               " from cv/cv.yml, so the changes above were not copied to it.")
        if json.dumps(state, sort_keys=True) != state_before:
            with open(STATE, "w", encoding="utf-8") as fh:
                json.dump(state, fh, indent=2, ensure_ascii=False)
                fh.write("\n")

    waiting = sum(1 for r in managed.values() if not r.get("complete"))
    for line in log:
        print(line)
    print("[publications] %d added, %d updated, %d awaiting volume/pages." % (added, completed, waiting))
    return 0


if __name__ == "__main__":
    sys.exit(main())
