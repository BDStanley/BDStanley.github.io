#!/usr/bin/env python3
"""Refresh data/media-reach.json, the news articles behind the Media reach page
(pages/media-reach.typ): articles that quote or interview Ben, take up his
ideas, or report on his research; where they appeared, in which languages,
and when.

How articles are found:

  * Google News search, through its RSS feed (no key needed), with the
    queries in QUERIES. They cover Ben's name with a word tying it to his
    field, in English, Polish (with its case forms), German, French, Spanish,
    Italian, Portuguese, Dutch, Czech, Slovak, Hungarian, Ukrainian, Russian,
    Norwegian, Swedish and Estonian; the key ideas' phrases in those languages; and his joint work
    with Stanley Bill and Pooling the Poles. Each query runs in the Google
    News edition of its language. A normal build asks only for articles of
    the last 13 months. `--backfill` asks again with no dates, then year by
    year from 2008 for the queries with enough results to need it (Google
    returns only its top matches).
  * Google Alerts: the RSS feeds listed under "alert_feeds" in
    data/media-sync.json, read on every build. `--alerts` prints a link
    that opens Google Alerts with each query filled in.

What is kept:

  * Each article's address is resolved from Google's link and its page is
    read, to check that it names Ben (in any of the forms in BEN), uses the
    idea's phrase, or names his work, as its query requires; and to find its
    author and language. An article whose page can't be read, or comes back
    as a short paywall stub (under STUB characters) without his name, is kept
    and marked unchecked. One whose page fails the check is dropped and
    remembered under "dropped", so it isn't fetched again.
  * Articles Ben wrote are dropped too: by their author, OWN_TITLE, OWN_URL,
    or because the Media page lists them (data/kultura-liberalna.yml and the
    links in pages/media.typ). They are not mentions.
  * Pages on academic publishers' and repositories' sites (NOT_MEDIA) are
    left out: they are academic citations, which the other pages count.
  * Addresses listed under "ignore" in data/media-sync.json are dropped. Use
    this for false matches the check misses (another Ben Stanley, say).
  * Articles once found stay, even after Google News stops returning them.
    The file is an archive: everything is stored under "records".
  * An article matching several queries is filed under one category: an idea
    first, then research, then interviews and commentary.

Countries are the outlets' (OUTLET_COUNTRIES, then the web address's country
code). Outlets with none of their own (.eu addresses and international ones)
are listed apart and not mapped. Languages are the pages' own, else the
edition's.

The page only lays out what is here: every count, colour, bar length, hover
text and list entry is worked out in this script, so the Calepin page and its
Quarto twin (quarto-site/R/media-helpers.R) show the same thing. The file is
copied to quarto-site/data/.

It also writes data/media-mentions.csv: the same articles as a spreadsheet
(date, title, outlet, country or region, language, author, category, key idea,
whether the page was checked, link). data/ is not published with the site.

Run by build.sh. Network failures are non-fatal: the build then uses the
committed file. Standard library only.
`--offline` counts the stored articles again without asking Google, after
editing data/media-sync.json for instance. `--resolve` doesn't search either,
but resolves and reads the articles still waiting (Google refuses to resolve
many links in a row, so a backfill may leave some for later).

Usage:  python3 scripts/update-media-reach.py [--backfill | --resolve | --offline] [--dry-run] [--alerts]
"""

import csv
import datetime
import html
import json
import math
import os
import re
import ssl
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from collections import Counter, defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.abspath(os.path.join(HERE, "..", "data", "media-reach.json"))
CSV = os.path.abspath(os.path.join(HERE, "..", "data", "media-mentions.csv"))
SYNC = os.path.abspath(os.path.join(HERE, "..", "data", "media-sync.json"))
MIRROR = os.path.abspath(os.path.join(HERE, "..", "..", "quarto-site", "data", "media-reach.json"))
MAP = os.path.abspath(os.path.join(HERE, "..", "data", "world-map.json"))

UA = ("Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) "
      "Chrome/126.0 Safari/537.36")
FIRST_YEAR = 2008
PAUSE = 1.2            # seconds between requests to Google when searching
RESOLVE_PAUSE = 4      # and when resolving its links to articles
WINDOW_FROM = 8        # results at which --backfill also asks year by year
MAX_RESOLVE = {"build": 60, "backfill": 600}   # articles resolved and read per run
STUB = 5000            # characters of text below which a page is a paywall stub, not the article

# Google News editions: (hl, gl, ceid) and the language they stand for.
ED = {
    "en-GB": ("en-GB", "GB", "GB:en", "English"), "en-US": ("en-US", "US", "US:en", "English"),
    "pl": ("pl", "PL", "PL:pl", "Polish"), "de": ("de", "DE", "DE:de", "German"),
    "fr": ("fr", "FR", "FR:fr", "French"), "es": ("es", "ES", "ES:es", "Spanish"),
    "it": ("it", "IT", "IT:it", "Italian"), "pt": ("pt-BR", "BR", "BR:pt-419", "Portuguese"),
    "nl": ("nl", "NL", "NL:nl", "Dutch"), "cs": ("cs", "CZ", "CZ:cs", "Czech"),
    "sk": ("sk", "SK", "SK:sk", "Slovak"), "hu": ("hu", "HU", "HU:hu", "Hungarian"),
    "uk": ("uk", "UA", "UA:uk", "Ukrainian"), "ru": ("ru", "RU", "RU:ru", "Russian"),
    "no": ("no", "NO", "NO:no", "Norwegian"), "sv": ("sv", "SE", "SE:sv", "Swedish"),
    "et": ("et", "EE", "EE:et", "Estonian"), "pt-PT": ("pt-PT", "PT", "PT:pt-150", "Portuguese"),
}
EN = ["en-GB", "en-US"]

# Ben in the forms the press uses: Polish case endings, his full name, Cyrillic.
BEN = re.compile(r"\bBen(?:a|em|owi|ie)?\s+Stanley(?:a|em|owi|u)?\b|\bBenjamin(?:a|em)?\s+(?:David\s+)?Stanley"
                 r"|Бен\w*\s+Ст[еэ]нл[иіе]\w*")
BILL = re.compile(r"\bStanley(?:a|em)?\s+Bill(?:a|em|owi)?\b")
STANLEY = re.compile(r"\bStanley")

# The key ideas, in the order the page lists them.
KEY_IDEAS = [("thin", "The thin ideology of populism"),
             ("trilemma", "The post-illiberal trilemma"),
             ("forging", "Forging, bending and breaking")]
THIN = re.compile(r"thin[- ]?(?:centred|centered)?[- ]?ideolog|cienk\w*\s+ideolog|ideolog\w*\s+o\s+cienkim"
                  r"|dünne\w*\s+ideolog|idéologie\s+mince|ideolog[íi]a\s+(?:delgada|sottile)", re.I)
TRILEMMA = re.compile(r"post-?\s?il+iberal\w*\s+tril+em|tril+em\w*\s+post-?\s?il+iberal|posztilliberális\s+trilemma"
                      r"|tryl+emat\w*\s+post|post-?\s?il+iberaln\w*\s+tryl+emat", re.I)
FORGING = re.compile(r"forging,?\s+bending,?\s+(?:and\s+)?breaking", re.I)
PLAYBOOK = re.compile(r"illiberal\s+playbook", re.I)
# Ben's own columns ("STANLEY: ..." in Kultura Liberalna, "Stanley dla Polityki"),
# besides those whose author is Ben. They are not mentions; the Media page lists them.
OWN_TITLE = re.compile(r"^STANLEY:|^Stanley dla\b")
OWN_URL = re.compile(r"kulturaliberalna\.pl/.*/stanley-")
KL = os.path.abspath(os.path.join(HERE, "..", "data", "kultura-liberalna.yml"))
MEDIA_PAGE = os.path.abspath(os.path.join(HERE, "..", "pages", "media.typ"))


def own_urls():
    """Addresses of Ben's columns, from the Media page's sources."""
    urls = set()
    for path, pattern in ((KL, r'url:\s*"([^"]+)"'), (MEDIA_PAGE, r'link\("(https?://[^"]+)"')):
        if os.path.exists(path):
            urls |= {norm_url(u) for u in re.findall(pattern, open(path, encoding="utf-8").read())}
    return urls

# Each query: its category, the idea it tracks (for ideas), the query as typed
# into Google News or Google Alerts, the editions it runs in, and what a page
# must contain to count: a list of patterns, all of which must match.
QUERIES = [
    # Interviews and commentary: Ben named, with a word tying him to his field.
    ("commentary", None, '"Ben Stanley" (Poland OR Polish OR populism OR populist OR SWPS OR "political scientist") -football -footballer -Harambee', EN, [BEN]),
    ("commentary", None, '"Ben Stanley" (PiS OR "Law and Justice" OR Kaczyński OR Tusk OR Duda OR Nawrocki)', EN, [BEN]),
    ("commentary", None, '"Ben Stanley" (politolog OR SWPS OR PiS OR wybory OR sondaż OR populizm OR Sejm)', ["pl"], [BEN]),
    ("commentary", None, '("Bena Stanleya" OR "Benem Stanleyem" OR "Benowi Stanleyowi") -piłkarz -piłkarza -Harambee', ["pl"], [BEN]),
    ("commentary", None, '"Ben Stanley" (Polen OR Politologe OR Populismus)', ["de"], [BEN]),
    ("commentary", None, '"Ben Stanley" (Pologne OR politologue OR populisme)', ["fr"], [BEN]),
    ("commentary", None, '"Ben Stanley" (Polonia OR politólogo OR populismo)', ["es"], [BEN]),
    ("commentary", None, '"Ben Stanley" (Polonia OR politologo OR populismo)', ["it"], [BEN]),
    ("commentary", None, '"Ben Stanley" (Polônia OR Polónia OR "cientista político")', ["pt", "pt-PT"], [BEN]),
    ("commentary", None, '"Ben Stanley" (Polen OR politicoloog)', ["nl"], [BEN]),
    ("commentary", None, '"Ben Stanley" (Polsko OR politolog)', ["cs"], [BEN]),
    ("commentary", None, '"Ben Stanley" (Poľsko OR politológ)', ["sk"], [BEN]),
    ("commentary", None, '"Ben Stanley" (Lengyelország OR politológus)', ["hu"], [BEN]),
    ("commentary", None, '"Бен Стенлі"', ["uk"], [BEN]),
    ("commentary", None, '"Бен Стэнли" Польша', ["ru"], [BEN]),
    # The key ideas. The thin ideology is Mudde's term too, so a page must also name Stanley.
    ("ideas", "thin", '"thin ideology" Stanley', EN, [THIN, STANLEY]),
    ("ideas", "thin", '"thin-centred ideology" Stanley', ["en-GB"], [THIN, STANLEY]),
    ("ideas", "thin", '"cienka ideologia" OR "cienką ideologią" OR "cienkiej ideologii"', ["pl"], [THIN, STANLEY]),
    ("ideas", "thin", '"dünne Ideologie" Stanley', ["de"], [THIN, STANLEY]),
    ("ideas", "thin", '"idéologie mince" Stanley', ["fr"], [THIN, STANLEY]),
    ("ideas", "thin", '"ideología delgada" Stanley', ["es"], [THIN, STANLEY]),
    ("ideas", "trilemma", '"post-illiberal trilemma"', EN, [TRILEMMA]),
    ("ideas", "trilemma", '"trylemat postiliberalny" OR "postiliberalny trylemat" OR "trylemat post-iliberalny"', ["pl"], [TRILEMMA]),
    ("ideas", "trilemma", '"post-illiberale trilemma"', ["nl"], [TRILEMMA]),
    ("ideas", "trilemma", '"post-illiberale Trilemma" OR "postilliberale Trilemma" OR "postilliberales Trilemma"', ["de"], [TRILEMMA]),
    ("ideas", "trilemma", '"trilemme post-illibéral"', ["fr"], [TRILEMMA]),
    ("ideas", "trilemma", '"trilema post-iliberal" OR "trilema postiliberal"', ["es"], [TRILEMMA]),
    ("ideas", "trilemma", '"trilemma post-illiberale"', ["it"], [TRILEMMA]),
    ("ideas", "trilemma", '"posztilliberális trilemma"', ["hu"], [TRILEMMA]),
    ("ideas", "forging", '"forging, bending and breaking"', EN, [FORGING]),
    ("ideas", "forging", '"illiberal playbook" (Pirro OR Stanley)', EN, [PLAYBOOK, re.compile(r"Pirro|Stanley")]),
    # Research in the news: the joint work with Stanley Bill, and Pooling the Poles.
    ("research", None, '"Stanley Bill" "Ben Stanley"', EN + ["pl"], [BEN, BILL]),
    ("research", None, '"Good Change" "Stanley Bill"', EN, [re.compile(r"Good Change", re.I), BILL]),
    ("research", None, '"Pooling the Poles"', ["en-GB", "pl"], [re.compile(r"Pooling the Poles", re.I)]),
    # Added later; new searches go last, as stored articles refer to searches by position.
    ("commentary", None, '"Ben Stanley" (Polen OR statsviter OR polsk)', ["no"], [BEN]),
    ("commentary", None, '"Ben Stanley" (Polen OR statsvetare OR polsk)', ["sv"], [BEN]),
    ("commentary", None, '"Ben Stanley" (Poola OR politoloog)', ["et"], [BEN]),
]
CATEGORIES = [("commentary", "Interviews and commentary", "#2a78d6"),
              ("ideas", "Ideas", "#eb6834"),
              ("research", "Research in the news", "#1baf7a")]
CAT_LABEL = {k: label for k, label, _ in CATEGORIES}
CAT_COLOUR = {k: c for k, _, c in CATEGORIES}
RANK = {"ideas": 0, "research": 1, "commentary": 2}

# Outlets whose address doesn't say where they are (by domain, ending).
OUTLET_COUNTRIES = {
    "theguardian.com": "GB", "bbc.com": "GB", "ft.com": "GB", "economist.com": "GB", "reuters.com": "GB",
    "newstatesman.com": "GB", "opendemocracy.net": "GB", "spectator.com": "GB", "unherd.com": "GB",
    "theconversation.com": None, "emerging-europe.com": "GB", "lse.ac.uk": "GB", "prospectmagazine.co.uk": "GB",
    "politico.eu": "BE", "euractiv.com": "BE", "euobserver.com": "BE", "euronews.com": "FR", "france24.com": "FR",
    "rfi.fr": "FR", "voxeurop.eu": "FR", "dw.com": "DE", "socialeurope.eu": "DE", "rferl.org": "CZ",
    "commonspace.eu": "NL", "notesfrompoland.com": "PL", "tvpworld.com": "PL", "polandin.com": "PL",
    "visegradinsight.eu": "PL", "balkaninsight.com": "RS", "eurozine.com": "AT", "intellinews.com": "DE",
    "nytimes.com": "US", "washingtonpost.com": "US", "wsj.com": "US", "bloomberg.com": "US", "apnews.com": "US",
    "cnn.com": "US", "npr.org": "US", "foreignpolicy.com": "US", "foreignaffairs.com": "US", "theatlantic.com": "US",
    "newyorker.com": "US", "time.com": "US", "vox.com": "US", "slate.com": "US", "nationalreview.com": "US",
    "thenation.com": "US", "prospect.org": "US", "journalofdemocracy.org": "US", "csmonitor.com": "US",
    "nbcnews.com": "US", "cbsnews.com": "US", "abcnews.go.com": "US", "usnews.com": "US", "axios.com": "US",
    "newsweek.com": "US", "yahoo.com": "US", "msn.com": "US", "businessinsider.com": "US", "huffpost.com": "US",
    "aljazeera.com": "QA", "kyivindependent.com": "UA", "kyivpost.com": "UA", "meduza.io": "LV",
    "themoscowtimes.com": "NL", "jamestown.org": "US", "cepa.org": "US", "brookings.edu": "US",
    "worldpoliticsreview.com": "US", "lemonde.fr": "FR", "elpais.com": "ES", "spiegel.de": "DE",
    "legrandcontinent.eu": "FR", "lostiempos.com": "BO", "jelen.media": "HU", "theunpopulist.net": "US",
    "key.aero": "GB", "worldcrunch.com": "FR", "biztimes.com": "US", "pulsesports.co.ke": "KE",
    "prio.org": "NO", "ecpr.eu": "GB", "lavie-des-idees.fr": "FR", "laviedesidees.fr": "FR",
}
TLD_COUNTRY = {"uk": "GB"}
# Sites Google News indexes that are not media: academic publishers and
# repositories, whose pages are academic citations (counted elsewhere).
NOT_MEDIA = {"cambridge.org", "illiberalism.org", "tandfonline.com", "springer.com", "sagepub.com", "wiley.com",
             "oup.com", "jstor.org", "researchgate.net", "academia.edu", "ssrn.com", "mdpi.com", "frontiersin.org",
             "doi.org", "scholar.google.com", "semanticscholar.org"}
NO_COUNTRY = {"EU": "Pan-European outlets", "INT": "International outlets"}
LANGUAGES = {"pl": "Polish", "en": "English", "de": "German", "fr": "French", "es": "Spanish", "it": "Italian",
             "pt": "Portuguese", "cs": "Czech", "sk": "Slovak", "hu": "Hungarian", "nl": "Dutch", "uk": "Ukrainian",
             "ru": "Russian", "ro": "Romanian", "lt": "Lithuanian", "lv": "Latvian", "et": "Estonian",
             "sv": "Swedish", "da": "Danish", "no": "Norwegian", "nb": "Norwegian", "fi": "Finnish",
             "sr": "Serbian", "hr": "Croatian", "bs": "Bosnian", "bg": "Bulgarian", "sl": "Slovenian",
             "el": "Greek", "tr": "Turkish", "ca": "Catalan", "ja": "Japanese", "zh": "Chinese", "ar": "Arabic",
             "he": "Hebrew", "ka": "Georgian", "be": "Belarusian", "mk": "Macedonian", "sq": "Albanian"}
MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"]   # English, whatever the locale

# Colours: categories take the first three slots of the dataviz reference
# palette, in order, as the Policy reach page's types do; outlets and
# languages are single series in the site's red; countries use the same red
# ramp as the other reach pages' maps.
SERIES = "#8b0000"
RAMP = ["#eda496", "#e38070", "#d55d4b", "#c13d30", "#a72219", "#8b0000"]
NO_ARTICLES = "#e1e0d9"
BINS = [1, 2, 5, 10, 20, 50]
TOP_OUTLETS = 15


# --- network -----------------------------------------------------------------

def fetch(url, data=None, headers=None, timeout=30):
    req = urllib.request.Request(url, data=data, headers=dict({"User-Agent": UA, "Accept-Language": "en,pl;q=0.8"},
                                                              **(headers or {})))
    try:
        resp = urllib.request.urlopen(req, timeout=timeout)
    except urllib.error.HTTPError:
        raise
    except (urllib.error.URLError, ssl.SSLError, OSError):
        # As in update-kl.py: some macOS Pythons have no CA bundle wired up.
        resp = urllib.request.urlopen(req, timeout=timeout, context=ssl._create_unverified_context())
    body = resp.read(3_000_000)
    charset = resp.headers.get_content_charset() or "utf-8"
    return body.decode(charset, "replace"), resp.geturl()


def google(url, data=None, headers=None):
    """A request to Google, paced, waiting out a refusal (429/503) twice."""
    for attempt in range(3):
        time.sleep(PAUSE)
        try:
            return fetch(url, data, headers)[0]
        except urllib.error.HTTPError as e:
            if e.code not in (429, 503) or attempt == 2:
                raise
            time.sleep(60 * (attempt + 1))


def news_search(q, ed, after=None, before=None):
    """[candidate] from Google News search in one edition."""
    hl, gl, ceid, lang = ED[ed]
    if after:
        q = "%s after:%s before:%s" % (q, after, before)
    x = google("https://news.google.com/rss/search?" + urllib.parse.urlencode({"q": q, "hl": hl, "gl": gl, "ceid": ceid}))
    out = []
    for it in re.findall(r"<item>(.*?)</item>", x, re.S):
        link = re.search(r"<link>(.*?)</link>", it, re.S).group(1).strip()
        src = re.search(r'<source url="([^"]*)">(.*?)</source>', it, re.S)
        outlet = html.unescape(src.group(2)) if src else ""
        title = html.unescape(re.search(r"<title>(.*?)</title>", it, re.S).group(1))
        if outlet and title.endswith(" - " + outlet):
            title = title[:-len(outlet) - 3]
        try:
            date = datetime.datetime.strptime(re.search(r"<pubDate>(.*?)</pubDate>", it).group(1)[5:16], "%d %b %Y").date()
        except (AttributeError, ValueError):
            date = None
        out.append({"gid": link.split("/articles/")[-1].split("?")[0], "google_url": link, "url": "",
                    "title": title.strip(), "outlet": outlet.strip(), "outlet_url": src.group(1) if src else "",
                    "date": date.isoformat() if date else "", "edition_language": lang})
    return out


def alert_feed(url):
    """[candidate] from a Google Alerts RSS (Atom) feed."""
    x = fetch(url)[0]
    query = html.unescape((re.search(r"<title>Google Alert - (.*?)</title>", x) or re.search(r"<title>(.*?)</title>", x)).group(1))
    out = []
    for e in re.findall(r"<entry>(.*?)</entry>", x, re.S):
        href = html.unescape(re.search(r'<link href="([^"]+)"', e).group(1))
        real = urllib.parse.parse_qs(urllib.parse.urlparse(href).query).get("url", [href])[0]
        title = re.sub(r"<[^>]+>", "", html.unescape(re.search(r"<title[^>]*>(.*?)</title>", e, re.S).group(1)))
        date = (re.search(r"<published>(.*?)</published>", e) or re.search(r"<updated>(.*?)</updated>", e)).group(1)[:10]
        out.append({"gid": "", "google_url": "", "url": real, "title": html.unescape(title).strip(),
                    "outlet": "", "outlet_url": "", "date": date, "edition_language": "", "alert": query})
    return out


def resolve(gid):
    """The publisher's address behind a Google News article ID, or None."""
    # The /rss/articles/ page carries the same signature as /articles/, and
    # Google rate-limits it separately (and less readily).
    page = google("https://news.google.com/rss/articles/" + gid)
    sig, ts = re.search(r'data-n-a-sg="([^"]+)"', page), re.search(r'data-n-a-ts="([^"]+)"', page)
    if not (sig and ts):
        return None
    inner = ["garturlreq", [["X", "X", ["X", "X"], None, None, 1, 1, "US:en", None, 1, None, None, None, None, None, 0, 1],
                            "X", "X", 1, [1, 1, 1], 1, 1, None, 0, 0, None, 0], gid, int(ts.group(1)), sig.group(1)]
    body = "f.req=" + urllib.parse.quote(json.dumps([[["Fbv4je", json.dumps(inner), None, "generic"]]]))
    r = google("https://news.google.com/_/DotsSplashUi/data/batchexecute", body.encode(),
               {"Content-Type": "application/x-www-form-urlencoded;charset=UTF-8"})
    try:
        return json.loads(json.loads(r.split("\n\n")[1])[:-2][0][2])[1]
    except (IndexError, ValueError, TypeError):
        return None


# --- reading an article --------------------------------------------------------

def page_text(page):
    s = re.sub(r"<(script|style|noscript)[^>]*>.*?</\1>", " ", page, flags=re.S | re.I)
    return html.unescape(re.sub(r"\s+", " ", re.sub(r"<[^>]+>", " ", s)))


def article_text(page):
    """The text of the article itself, for the check: the longest <article>
    (else <main>, else the page), without navigation, sidebars, footers and
    "read also" links, where an outlet's other pieces, Ben's columns or a story
    about him among them, would otherwise make any page "name" him."""
    s = re.sub(r"<(script|style|noscript|nav|aside|footer|header|form)\b[^>]*>.*?</\1>", " ", page, flags=re.S | re.I)
    s = re.sub(r"<(p|div|li|h\d)\b[^>]*>(?:\s|<[^>]+>)*(?:Czytaj (?:też|także|więcej)|Zobacz (?:też|także)|Read (?:also|more)"
               r"|See also|Lesen Sie auch|Lire aussi|Leer también)\b.*?</\1>", " ", s, flags=re.S | re.I)
    for tag in ("article", "main"):
        blocks = re.findall(r"<%s\b[^>]*>(.*?)</%s>" % (tag, tag), s, flags=re.S | re.I)
        if blocks:
            s = max(blocks, key=len)
            break
    return page_text(s)


def author_of(page, outlet):
    names = []
    for block in re.findall(r'<script[^>]+application/ld\+json[^>]*>(.*?)</script>', page, re.S | re.I):
        try:
            data = json.loads(block.strip())
        except ValueError:
            continue
        stack = data if isinstance(data, list) else [data]
        while stack:
            d = stack.pop(0)
            if isinstance(d, dict):
                stack += d.get("@graph", []) if isinstance(d.get("@graph"), list) else []
                a = d.get("author")
                for x in (a if isinstance(a, list) else [a]):
                    n = x.get("name") if isinstance(x, dict) else x
                    if isinstance(n, str) and n.strip():
                        names.append(n.strip())
        if names:
            break
    if not names:
        for pat in (r'<meta[^>]+(?:name|property)="(?:author|article:author|parsely-author|sailthru\.author|dc\.creator|byl)"[^>]+content="([^"]+)"',
                    r'<meta[^>]+content="([^"]+)"[^>]+(?:name|property)="(?:author|article:author|parsely-author|dc\.creator)"'):
            m = re.search(pat, page, re.I)
            if m:
                names = [m.group(1)]
                break
    out = []
    for n in names:
        n = re.sub(r"^\s*(by|autor|author)[:\s]+", "", html.unescape(n), flags=re.I).strip()
        if re.fullmatch(r"[a-z0-9_.-]+", n):
            continue                       # a CMS user name ("florent", "ewa_no"), not a byline
        if n and not n.startswith("http") and n.lower() != (outlet or "").lower() and n not in out and len(n) < 80:
            out.append(n)
    return ", ".join(out[:3])


def language_of(page, fallback):
    m = re.search(r'<html[^>]*\blang=["\']?([a-zA-Z]{2,3})', page)
    return LANGUAGES.get(m.group(1).lower(), fallback) if m else fallback


def host(url):
    h = (urllib.parse.urlparse(url).hostname or "").lower()
    return re.sub(r"^(www\d?|m|amp|mobile)\.", "", h)


def base_domain(h):
    """The registrable domain: wiadomosci.onet.pl -> onet.pl, news.bbc.co.uk -> bbc.co.uk."""
    parts = h.split(".")
    if len(parts) > 2 and parts[-2] in ("co", "com", "org", "net", "gov", "ac", "edu") and len(parts[-1]) == 2:
        return ".".join(parts[-3:])
    return ".".join(parts[-2:])


def norm_url(url):
    p = urllib.parse.urlparse(url)
    q = "&".join(kv for kv in p.query.split("&") if kv and not kv.startswith(("utm_", "fbclid", "gclid", "ocid")))
    return "%s%s%s" % (host(url), p.path.rstrip("/"), ("?" + q) if q else "")


def country_of(domain, names):
    for d, c in OUTLET_COUNTRIES.items():
        if domain == d or domain.endswith("." + d):
            return c or "INT"
    tld = domain.rsplit(".", 1)[-1]
    if tld == "eu":
        return "EU"
    code = TLD_COUNTRY.get(tld, tld.upper())
    return code if code in names else "INT"


# --- gathering -------------------------------------------------------------------

def gather(backfill, feeds, log):
    """[(candidate, query index)] from Google News and the alert feeds."""
    found = []
    this_year = datetime.date.today().year
    try:
        for i, (_, _, q, eds, _) in enumerate(QUERIES):
            for ed in eds:
                if backfill:
                    first = news_search(q, ed)
                    found += [(c, i) for c in first]
                    if len(first) >= WINDOW_FROM:
                        for y in range(FIRST_YEAR, this_year + 1):
                            found += [(c, i) for c in news_search(q, ed, "%d-01-01" % y, "%d-01-01" % (y + 1))]
                else:
                    today = datetime.date.today()
                    found += [(c, i) for c in news_search(q, ed, (today - datetime.timedelta(days=400)).isoformat(),
                                                          (today + datetime.timedelta(days=1)).isoformat())]
    except urllib.error.HTTPError as e:
        # Google has started refusing; keep what came before.
        log.append("[media-reach] Google News refused a search (HTTP %d); using the %d results so far." % (e.code, len(found)))
    for url in feeds:
        try:
            entries = alert_feed(url)
        except (urllib.error.URLError, OSError, ValueError, AttributeError) as e:
            log.append("[media-reach] could not read the alert feed %s (%s)." % (url, type(e).__name__))
            continue
        for c in entries:
            # Filed under the query the alert was set up with, else as commentary.
            i = next((k for k, qq in enumerate(QUERIES) if qq[2] == c["alert"]), 0)
            found.append((c, i))
    return found


def check(rec, page_txt):
    """True if the page has what any query that found the article requires."""
    text = rec["title"] + " " + page_txt
    return any(all(p.search(text) for p in QUERIES[i][4]) for i in rec["queries"])


def file_under(rec):
    best = min(rec["queries"], key=lambda i: RANK[QUERIES[i][0]])
    rec["category"], rec["idea"] = QUERIES[best][0], QUERIES[best][1]


def process(old, sync, backfill, names, log, offline=False, resolve_only=False):
    records = {r["key"]: r for r in old.get("records", [])}
    dropped = dict(old.get("dropped", {}))
    ignore = set(norm_url(u) for u in sync.get("ignore", {}))
    by_gid = {r["gid"]: r for r in records.values() if r.get("gid")}
    own = own_urls()
    found = [] if offline or resolve_only else gather(backfill, sync.get("alert_feeds", []), log)
    if not (offline or resolve_only):
        log.append("[media-reach] %d results from Google." % len(found))

    for c, i in found:
        key = norm_url(c["url"]) if c["url"] else "gnews:" + c["gid"]
        if key in dropped or (c["gid"] and "gnews:" + c["gid"] in dropped):
            continue
        rec = records.get(key) or (by_gid.get(c["gid"]) if c["gid"] else None)
        if rec is None:
            rec = dict(c, key=key, queries=[], checked=None, author="", language=c["edition_language"])
            records[key] = rec
            if c["gid"]:
                by_gid[c["gid"]] = rec
        if i not in rec["queries"]:
            rec["queries"].append(i)

    # Resolve and read what hasn't been read yet, up to the run's limit.
    todo = [] if offline else [r for r in records.values() if r.get("checked") is None]
    limit = MAX_RESOLVE["backfill" if backfill or resolve_only else "build"]
    global PAUSE
    PAUSE = RESOLVE_PAUSE           # Google refuses fast link-resolving sooner than searching
    for rec in todo[:limit]:
        try:
            if not rec["url"] and rec.get("gid"):
                rec["url"] = resolve(rec["gid"]) or ""
            if not rec["url"]:
                rec["checked"] = False
                continue
            page, final = fetch(rec["url"], timeout=20)
            rec["url"] = final or rec["url"]
            if not check(rec, article_text(page)):
                if len(page_text(page)) < STUB:
                    # A paywall or script-only page: the article isn't there to check.
                    rec["checked"] = False
                    continue
                dropped[rec["key"]] = "page does not have what the query requires: " + rec["title"][:80]
                records.pop(rec["key"], None)
                continue
            rec["checked"] = True
            rec["author"] = author_of(page, rec["outlet"])
            rec["language"] = language_of(page, rec["language"] or "")
            if (BEN.search(rec["author"]) or OWN_TITLE.search(rec["title"]) or OWN_URL.search(rec["url"])
                    or norm_url(rec["url"]) in own):
                dropped[rec["key"]] = "written by Ben (his columns are on the Media page): " + rec["title"][:80]
                records.pop(rec["key"], None)
        except urllib.error.HTTPError as e:
            if not rec["url"] and e.code in (429, 503):
                log.append("[media-reach] Google refused to resolve links (HTTP %d); the rest wait for a later run." % e.code)
                break
            rec["checked"] = False if rec["url"] else None
            log.append("[media-reach] could not read %s (HTTP %d)." % (rec["url"] or rec["title"][:60], e.code))
        except (urllib.error.URLError, OSError, ValueError, UnicodeError) as e:
            rec["checked"] = False if rec["url"] else None
            log.append("[media-reach] could not read %s (%s)." % (rec["url"] or rec["title"][:60], type(e).__name__))
    if len(todo) > limit:
        log.append("[media-reach] %d articles left to read on later runs." % (len(todo) - limit))

    out = []
    for rec in records.values():
        if rec["url"]:
            rec["key"] = norm_url(rec["url"])
        if rec["key"] in ignore or (rec.get("url") and norm_url(rec["url"]) in ignore):
            continue
        if base_domain(host(rec["url"] or rec["outlet_url"])) in NOT_MEDIA:
            continue
        rec["author"] = ", ".join(a for a in (rec.get("author") or "").split(", ")
                                  if a and not re.fullmatch(r"[a-z0-9_.-]+", a))
        domain = base_domain(host(rec["url"] or rec["outlet_url"]))
        rec["domain"] = domain
        rec["country"] = country_of(domain, names)
        if not rec["outlet"]:
            rec["outlet"] = domain
        file_under(rec)
        out.append(rec)
    # One record per article: the same address found twice (from News and an alert).
    seen, unique = set(), []
    for rec in sorted(out, key=lambda r: (r["checked"] is not True, r["key"])):
        k = rec["key"] if rec["url"] else (rec["domain"], rec["title"].lower())
        if k not in seen:
            seen.add(k)
            unique.append(rec)
    return unique, dropped


# --- analysis ----------------------------------------------------------------------

def plural(n, word, words=None):
    return "%s %s" % ("{:,}".format(n), word if n == 1 else (words or word + "s"))


def nice_step(n):
    raw = max(n, 1) / 4
    k = 10 ** math.floor(math.log10(raw))
    for m in (1, 2, 5, 10):
        if raw <= m * k:
            return int(m * k) or 1


def when(date):
    try:
        d = datetime.date.fromisoformat(date)
        return "%d %s %d" % (d.day, MONTHS[d.month - 1], d.year)
    except (TypeError, ValueError):
        return ""


def analyse(records, names, as_of):
    this_year = datetime.date.today().year

    def cname(code):
        return NO_COUNTRY.get(code) or names.get(code, code)

    def item(r):
        meta = [when(r["date"]), "%s (%s)" % (r["outlet"], cname(r["country"])), r.get("author") or ""]
        return {"date": r["date"], "when": when(r["date"]), "title": r["title"], "url": r["url"] or r["google_url"],
                "outlet": r["outlet"], "country": cname(r["country"]), "author": r.get("author") or "",
                "language": r.get("language") or "", "checked": r.get("checked") is True,
                "meta": " · ".join(m for m in meta if m)}

    newest = sorted(records, key=lambda r: (r["date"], r["title"]), reverse=True)

    # Countries: articles by the outlet's country; pan-European and international apart.
    by_c = Counter(r["country"] for r in records)
    outlets_by_c = defaultdict(set)
    for r in records:
        outlets_by_c[r["country"]].add(r["domain"])

    def colour_for(n):
        return RAMP[max(i for i, lo in enumerate(BINS) if n >= lo)]

    country_rows = []
    for c, n in sorted(by_c.items(), key=lambda kv: (-kv[1], cname(kv[0]))):
        country_rows.append({"code": c, "name": cname(c), "articles": n, "outlets": len(outlets_by_c[c]),
                             "on_map": c not in NO_COUNTRY, "colour": colour_for(n),
                             "tip": "%s: %s in %s" % (cname(c), plural(n, "article"), plural(len(outlets_by_c[c]), "outlet"))})
    top = country_rows[0]["articles"] if country_rows else 1
    for r in country_rows:
        r["w"] = round(r["articles"] / top, 4)
    bins = []
    for i, lo in enumerate(BINS):
        hi = BINS[i + 1] - 1 if i + 1 < len(BINS) else None
        bins.append({"label": ("%d+" % lo) if hi is None else ("%d" % lo if hi == lo else "%d–%d" % (lo, hi)),
                     "colour": RAMP[i]})

    # Outlets, by domain, under the name Google News gives most often (the
    # shortest among equals: "EL PAÍS", not "El País Brasil").
    names_of, outlet_country = defaultdict(Counter), {}
    by_o = Counter()
    for r in records:
        by_o[r["domain"]] += 1
        names_of[r["domain"]][r["outlet"]] += 1
        outlet_country[r["domain"]] = cname(r["country"])
    outlet_name = {d: min(c, key=lambda n: (-c[n], len(n))) for d, c in names_of.items()}
    outlet_rows = [{"name": outlet_name[d], "domain": d, "country": outlet_country[d], "articles": n, "colour": SERIES,
                    "tip": "%s (%s): %s" % (outlet_name[d], outlet_country[d], plural(n, "article"))}
                   for d, n in sorted(by_o.items(), key=lambda kv: (-kv[1], outlet_name[kv[0]].lower()))]
    o_top = outlet_rows[0]["articles"] if outlet_rows else 1
    for r in outlet_rows[:TOP_OUTLETS]:
        r["w"] = round(r["articles"] / o_top, 4)

    # Languages.
    by_l = Counter(r.get("language") or "Unknown" for r in records)
    lang_rows = [{"name": l, "articles": n, "colour": SERIES, "tip": "%s: %s" % (l, plural(n, "article"))}
                 for l, n in sorted(by_l.items(), key=lambda kv: (-kv[1], kv[0]))]
    l_top = lang_rows[0]["articles"] if lang_rows else 1
    for r in lang_rows:
        r["w"] = round(r["articles"] / l_top, 4)

    # Years: one stacked column per year, by category.
    dated = [r for r in records if r["date"][:4].isdigit()]
    first_year = min((int(r["date"][:4]) for r in dated), default=this_year)
    by_year = defaultdict(Counter)
    for r in dated:
        by_year[int(r["date"][:4])][r["category"]] += 1
    years = list(range(first_year, this_year + 1))
    totals = {y: sum(by_year[y].values()) for y in years}
    peak = max(totals.values()) if totals else 1
    step = nice_step(peak)
    axis_max = step * math.ceil(max(peak, 1) / step)
    ticks = [{"value": v, "label": "{:,}".format(v), "pos": round(v / axis_max, 4)} for v in range(0, axis_max + 1, step)]
    used = [k for k, _, _ in CATEGORIES if any(by_year[y].get(k) for y in years)]
    legend = [{"name": CAT_LABEL[k], "colour": CAT_COLOUR[k]} for k in used]
    year_rows = []
    for y in years:
        segs = [{"name": CAT_LABEL[k], "count": by_year[y][k], "colour": CAT_COLOUR[k],
                 "tip": "%s, %s: %s" % (y, CAT_LABEL[k], plural(by_year[y][k], "article"))} for k in used if by_year[y].get(k)]
        label = "%d (to %s)" % (y, datetime.date.today().strftime("%B")) if y == this_year else str(y)
        year_rows.append({"year": y, "label": label, "count": totals[y], "h": round(totals[y] / axis_max, 4),
                          "minor": y % 5 != 0,
                          "tip": "%s: %s" % (label, plural(totals[y], "article"))
                                 + ("" if not segs else " (" + ", ".join("%s %d" % (s["name"], s["count"]) for s in segs) + ")"),
                          "segments": segs})

    # Lists: the key ideas, commentary by year (newest first), research.
    ideas = []
    for key, label in KEY_IDEAS:
        rs = [item(r) for r in newest if r["category"] == "ideas" and r["idea"] == key]
        ideas.append({"key": key, "idea": label, "articles": len(rs), "summary": plural(len(rs), "article"), "items": rs})
    commentary = []
    for y in sorted({int(r["date"][:4]) for r in dated if r["category"] == "commentary"}, reverse=True):
        rs = [item(r) for r in newest if r["category"] == "commentary" and r["date"][:4] == str(y)]
        commentary.append({"year": y, "summary": "%d: %s" % (y, plural(len(rs), "article")), "items": rs})
    undated = [item(r) for r in newest if r["category"] == "commentary" and not r["date"][:4].isdigit()]
    if undated:
        commentary.append({"year": None, "summary": "Undated: %s" % plural(len(undated), "article"), "items": undated})
    research = [item(r) for r in newest if r["category"] == "research"]

    cats = Counter(r["category"] for r in records)
    return {
        "_about": "Data behind the Media reach page, from Google News search and Google Alerts. Written by"
                  " scripts/update-media-reach.py; see that file for what is counted and how.",
        "as_of": as_of,
        "totals": {"articles": len(records), "outlets": len(by_o), "countries": sum(1 for r in country_rows if r["on_map"]),
                   "languages": sum(1 for r in lang_rows if r["name"] != "Unknown"), "first_year": first_year,
                   "unchecked": sum(1 for r in records if r.get("checked") is not True),
                   "commentary": cats["commentary"], "ideas": cats["ideas"], "research": cats["research"]},
        "categories": [{"key": k, "name": label, "colour": colour, "articles": cats[k]} for k, label, colour in CATEGORIES],
        "countries": country_rows, "bins": bins, "no_articles_colour": NO_ARTICLES,
        "outlets": outlet_rows, "top_outlets": TOP_OUTLETS,
        "languages": lang_rows,
        "years": {"axis_max": axis_max, "ticks": ticks, "rows": year_rows, "legend": legend},
        "ideas": ideas, "commentary": commentary, "research": {"summary": plural(len(research), "article"), "items": research},
    }


# --- main ----------------------------------------------------------------------------

def alerts():
    """Print each query once, as a link that opens Google Alerts with it filled in."""
    seen = set()
    print("| Category | Query | Language |\n|---|---|---|")
    for cat, idea, q, eds, _ in QUERIES:
        if q in seen:
            continue
        seen.add(q)
        label = dict(KEY_IDEAS).get(idea, CAT_LABEL[cat])
        print("| %s | [%s](https://www.google.com/alerts?q=%s) | %s |"
              % (label, q.replace("|", "\\|"), urllib.parse.quote(q), ED[eds[0]][3]))


def write_csv(records, names):
    """data/media-mentions.csv: the articles the page shows, newest first, for a
    spreadsheet. UTF-8 with a byte-order mark, so Excel reads the accents."""
    idea = dict(KEY_IDEAS)
    with open(CSV, "w", encoding="utf-8-sig", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["Date", "Title", "Outlet", "Country or region", "Language", "Author", "Category", "Key idea",
                    "Checked", "Link"])
        for r in sorted(records, key=lambda r: (r["date"], r["title"]), reverse=True):
            w.writerow([r["date"], r["title"], r["outlet"], NO_COUNTRY.get(r["country"]) or names.get(r["country"], r["country"]),
                        r.get("language") or "", r.get("author") or "", CAT_LABEL[r["category"]],
                        idea.get(r.get("idea"), ""), "yes" if r.get("checked") is True else "no",
                        r["url"] or r["google_url"]])


def main():
    args = sys.argv[1:]
    if "--alerts" in args:
        alerts()
        return 0
    dry, backfill, offline, resolve_only = ("--dry-run" in args, "--backfill" in args, "--offline" in args,
                                            "--resolve" in args)
    original = open(DATA, encoding="utf-8").read() if os.path.exists(DATA) else None
    old = json.loads(original) if original else {}
    sync = json.load(open(SYNC, encoding="utf-8")) if os.path.exists(SYNC) else {}
    names = json.load(open(MAP, encoding="utf-8"))["names"]
    log = []
    try:
        records, dropped = process(old, sync, backfill, names, log, offline, resolve_only)
    except Exception as e:
        print("[media-reach] warning: could not read Google News (%s: %s); %s."
              % (type(e).__name__, e, "keeping the previous data" if old else "no data to show yet"))
        return 0
    for line in log:
        print(line)
    # Articles the checks dropped this run are meant to go; any other fall means trouble.
    newly_dropped = len(set(dropped) - set(old.get("dropped", {})))
    if len(records) + newly_dropped < 0.75 * len(old.get("records", [])):
        print("[media-reach] needs attention: %d articles now, down from %d; keeping the previous data."
              % (len(records), len(old.get("records", []))))
        return 0
    data = analyse(records, names, datetime.date.today().strftime("%Y-%m"))
    data["records"] = sorted(records, key=lambda r: (r["date"], r["key"]), reverse=True)
    data["dropped"] = dropped
    t = data["totals"]
    print("[media-reach] %s in %s, %s and %s (%d unchecked)."
          % (plural(t["articles"], "article"), plural(t["outlets"], "outlet"),
             plural(t["countries"], "country", "countries"), plural(t["languages"], "language"), t["unchecked"]))
    content = json.dumps(data, indent=1, ensure_ascii=False) + "\n"
    if dry:
        print("[media-reach] dry run: nothing written.")
        return 0
    write_csv(records, names)
    if content != original:
        with open(DATA, "w", encoding="utf-8") as fh:
            fh.write(content)
    if os.path.isdir(os.path.dirname(MIRROR)):
        mirrored = open(MIRROR, encoding="utf-8").read() if os.path.exists(MIRROR) else None
        if mirrored != content:
            with open(MIRROR, "w", encoding="utf-8") as fh:
                fh.write(content)
            print("[media-reach] copied to quarto-site/data/media-reach.json.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
