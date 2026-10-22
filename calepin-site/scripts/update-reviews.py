#!/usr/bin/env python3
"""Keep the peer reviewing entry of cv/cv.yml in step with the peer reviews on
Ben's ORCID record, which Web of Science Researcher Profiles (formerly
Publons) and some journals send there.

What it writes:

  * The entry of the "reviewing" section that starts "Peer reviewer for": the
    journals Ben has reviewed for, each with its number of reviews, most
    first, then the total and the years they span: "Peer reviewer for
    _East European Politics_ (13 reviews), ... and _Political Studies_ (1):
    60 verified reviews, 2013–2026."
  * Reviews are counted as ORCID groups them, so one review that two sources
    send under the same identifier counts once. One that Web of Science and
    the journal both send, each under its own identifier, counts twice: ORCID
    gives only the year, so the two can't be told apart. Records of editorial
    roles are not counted.
  * Journals are identified by the ISSN ORCID files each review under and
    named from OpenAlex, or from Crossref for an ISSN OpenAlex doesn't know.

What it keeps:

  * A journal's name as already spelt in the entry, if it matches one of the
    journal's names in OpenAlex, ignoring case, punctuation, "&" for "and" and
    a leading "The". So "Europe-Asia Studies" keeps its hyphen and "East
    European Politics and Societies" its former title. To rename a journal,
    edit its name in cv.yml.
  * Journals in the entry with no reviews on ORCID, listed after the total as
    "Also peer reviewer for ...". Add or remove those by hand; one that later
    turns up on ORCID moves into the counted list.
  * The section's other entries, untouched.
  * When quarto-site/cv/cv.yml is identical to cv.yml, the same change is
    written there too, so the Quarto backup stays in step.

Run by build.sh after update-datasets.py, before the CV PDF is compiled.
Network failures are non-fatal: cv.yml is then left as it is. Standard library
only. Usage:  python3 scripts/update-reviews.py [--dry-run]
"""

import importlib.util
import os
import re
import sys
import unicodedata
import urllib.error

HERE = os.path.dirname(os.path.abspath(__file__))

# Network access, Typst escaping and the cv.yml reading and writing come from
# update-publications.py, as in update-datasets.py. No .pyc: scripts/__pycache__
# is not git-ignored.
sys.dont_write_bytecode = True
_spec = importlib.util.spec_from_file_location("update_publications", os.path.join(HERE, "update-publications.py"))
pubs = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(pubs)

PEER_REVIEWS = "https://pub.orcid.org/v3.0/%s/peer-reviews" % pubs.ORCID
OPENALEX = ("https://api.openalex.org/sources?filter=issn:%s"
            "&select=id,display_name,issn,alternate_titles&per_page=100")
CROSSREF = "https://api.crossref.org/journals/%s"
CHUNK = 50            # ISSNs per OpenAlex request; it takes up to 100 OR'd values
SECTION = "reviewing"
LEAD = "Peer reviewer for "
ITALIC = re.compile(r"(?<!\\)_(.+?)(?<!\\)_")
PLAIN = re.compile(r"^(\s*)- ([^\"'\s].*?)\s*$")    # an unquoted entry, as written by hand


def fold(name):
    """A journal name reduced for matching: no case, accents, punctuation or leading "The"."""
    s = unicodedata.normalize("NFKD", pubs.unquote(name)).encode("ascii", "ignore").decode().lower()
    s = re.sub(r"^the\s+", "", s.replace("&", " and "))
    return re.sub(r"[^a-z0-9]+", "", s)


def year_of(summary):
    value = ((summary.get("completion-date") or {}).get("year") or {}).get("value")
    return int(value) if value else None


def reviews():
    """({ISSN: [year of each review, or None]}, {group IDs that are not ISSNs})."""
    found, other = {}, set()
    for group in pubs.fetch_json(PEER_REVIEWS).get("group") or []:
        # One "peer-review-group" per review; its summaries are the sources' versions of it.
        for review in group.get("peer-review-group") or []:
            versions = [s for s in review.get("peer-review-summary") or []
                        if (s.get("reviewer-role") or "reviewer") == "reviewer"]
            if not versions:
                continue
            gid = versions[0].get("review-group-id") or ""
            kind, _, issn = gid.partition(":")
            if kind != "issn" or not issn:
                other.add(gid)
                continue
            years = [y for y in map(year_of, versions) if y]
            found.setdefault(issn.upper(), []).append(min(years) if years else None)
    return found, other


def journals(issns):
    """{ISSN: (journal key, name, {folded names})}. A journal filed under both its
    print and its online ISSN gets the same key, so its reviews are added up."""
    out, issns = {}, sorted(issns)
    for k in range(0, len(issns), CHUNK):
        for src in pubs.fetch_json(OPENALEX % "|".join(issns[k:k + CHUNK])).get("results") or []:
            known = {fold(t) for t in [src["display_name"]] + (src.get("alternate_titles") or [])}
            for issn in src.get("issn") or []:
                if issn.upper() in issns:
                    out[issn.upper()] = (src["id"], src["display_name"], known)
    for issn in sorted(set(issns) - set(out)):
        try:
            title = pubs.fetch_json(CROSSREF % issn)["message"].get("title")
        except urllib.error.HTTPError as e:
            if e.code == 404:
                continue
            raise
        if title:
            out[issn] = ("crossref:" + issn, title, {fold(title)})
    return out


def locate(lines, entries_at):
    """(line index, indent, text) of the entry starting "Peer reviewer for", or None."""
    depth = len(lines[entries_at]) - len(lines[entries_at].lstrip())
    for i in range(entries_at + 1, len(lines)):
        line = lines[i]
        if line.strip() and len(line) - len(line.lstrip()) <= depth:
            break
        m = pubs.ITEM.match(line)
        if m:
            text = pubs.unquote(m.group(2))
        else:
            m = PLAIN.match(line)
            text = m.group(2) if m else ""
        if text.startswith(LEAD):
            return i, m.group(1), text
    return None


def series(items):
    return items[0] if len(items) == 1 else ", ".join(items[:-1]) + " and " + items[-1]


def entry(counted, extra):
    rows = sorted(counted.values(), key=lambda j: (-len(j["years"]), fold(j["name"])))
    items = []
    for j in rows:
        n = len(j["years"])
        unit = "" if items else (" review" if n == 1 else " reviews")
        items.append("_%s_ (%d%s)" % (j["name"], n, unit))
    total = sum(len(j["years"]) for j in rows)
    years = [y for j in rows for y in j["years"] if y]
    span = "" if not years else ", %d" % min(years) if min(years) == max(years) \
        else ", %d–%d" % (min(years), max(years))
    text = "%s%s: %d verified review%s%s." % (LEAD, series(items), total, "" if total == 1 else "s", span)
    if extra:
        text += " Also peer reviewer for %s." % series(["_%s_" % n for n in extra])
    return text


def main():
    dry = "--dry-run" in sys.argv[1:]
    path = pubs.DEFAULT_YML
    with open(path, encoding="utf-8") as fh:
        original = fh.read()
    lines = original.split("\n")

    try:
        found, other = reviews()
        named = journals(set(found)) if found else {}
    except Exception as e:
        print("[reviews] warning: could not read ORCID, OpenAlex or Crossref (%s); cv.yml unchanged." % e)
        return 0
    if not found:
        print("[reviews] warning: ORCID lists no peer reviews; cv.yml unchanged.")
        return 0
    for gid in sorted(other):
        print("[reviews] needs attention: ORCID files a review under '%s', not a journal ISSN;"
              " it is left out." % gid)
    for issn in sorted(set(found) - set(named)):
        print("[reviews] needs attention: neither OpenAlex nor Crossref knows ISSN %s;"
              " its %d review(s) are left out." % (issn, len(found[issn])))
    if not named:
        print("[reviews] needs attention: no journal could be named; cv.yml unchanged.")
        return 0

    sec = pubs.parse(lines).get(SECTION)
    if not sec or sec["entries_at"] is None:
        print("[reviews] needs attention: cv.yml has no '%s' section with entries." % SECTION)
        return 0
    at = locate(lines, sec["entries_at"])
    if at:
        after = lines[at[0] + 1] if at[0] + 1 < len(lines) else ""
        if after.strip() and len(after) - len(after.lstrip()) > len(at[1]):
            print("[reviews] needs attention: the 'Peer reviewer for' entry runs onto a second"
                  " line of cv.yml; cv.yml unchanged.")
            return 0

    counted = {}
    for issn, years in found.items():
        if issn in named:
            key, name, known = named[issn]
            j = counted.setdefault(key, {"name": pubs.typst(pubs.text(name)), "known": set(),
                                         "years": [], "kept": False})
            j["known"] |= known
            j["years"] += years

    # Names already in the entry: a journal's spelling there wins over OpenAlex's,
    # and journals ORCID doesn't have stay as "Also peer reviewer for".
    extra = []
    for n in ITALIC.findall(at[2]) if at else []:
        j = next((j for j in counted.values() if fold(n) in j["known"]), None)
        if j:
            if not j["kept"]:
                j["name"], j["kept"] = n, True
        elif fold(n) not in {fold(e) for e in extra}:
            extra.append(n)

    new = entry(counted, extra)
    if at:
        lines[at[0]] = '%s- "%s"' % (at[1], pubs.quote(new))
    else:
        indent = re.match(r"^(\s*)", lines[sec["entries_at"]]).group(1) + "  "
        lines[sec["entries_at"] + 1:sec["entries_at"] + 1] = ['%s- "%s"' % (indent, pubs.quote(new))]
    content = "\n".join(lines)

    if not at:
        print("[reviews] + %s" % new)
    elif at[2] != new:
        print("[reviews] ~ %s\n            (was: %s)" % (new, at[2]))
    if dry:
        print("[reviews] dry run: nothing written.")
    elif content != original:
        pubs.write(path, content)
        if os.path.exists(pubs.MIRROR):
            with open(pubs.MIRROR, encoding="utf-8") as fh:
                in_step = fh.read() == original
            if in_step:
                pubs.write(pubs.MIRROR, content)
                print("[reviews] mirrored the change to quarto-site/cv/cv.yml.")
            else:
                print("[reviews] needs attention: quarto-site/cv/cv.yml had already diverged"
                      " from cv/cv.yml, so the change above was not copied to it.")
    total = sum(len(j["years"]) for j in counted.values())
    print("[reviews] %d review(s) for %d journal(s) on ORCID%s%s."
          % (total, len(counted),
             ", and %d more journal(s) listed by hand" % len(extra) if extra else "",
             "" if content != original or dry else "; no changes"))
    return 0


if __name__ == "__main__":
    sys.exit(main())
