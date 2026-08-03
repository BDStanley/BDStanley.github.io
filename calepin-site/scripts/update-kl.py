#!/usr/bin/env python3
"""Refresh data/kultura-liberalna.yml from the Kultura Liberalna author page.

Why this is a build *script* and not an R chunk in pages/media.typ:

  * Calepin caches chunk output against a fingerprint of the page source, so an
    inline scraper only re-runs when media.typ itself changes. The previous
    version of this page silently served a snapshot frozen on 21 June 2026.
  * https://kulturaliberalna.pl/autor/ben-stanley is a single `autor` custom
    post type page, not a paginated author archive: it renders the 15 most
    recent pieces and /page/2/ 301s back to /page/1/. Anything older simply
    falls off the end.

So the YAML file is the archive and this script only ever ADDS to it: entries
are keyed by URL and existing ones are never modified, which means older pieces
survive after they drop off the author page, and hand-edits stick.

Run by build.sh. Network failures are non-fatal -- the build just uses the
existing file. Usage:  python3 scripts/update-kl.py [path/to/kultura-liberalna.yml]
"""

import os
import re
import ssl
import sys
import urllib.error
import urllib.request

AUTHOR_URL = "https://kulturaliberalna.pl/autor/ben-stanley/"
AUTHOR_SLUG = "/autor/ben-stanley/"
UA = "Mozilla/5.0 (compatible; CalepinSiteBuild/1.0)"

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_YML = os.path.join(HERE, "..", "data", "kultura-liberalna.yml")

TEASE = re.compile(r'<article class="tease.*?</article>', re.S)
LINK = re.compile(
    r'<a class="h5"\s+href="(https://kulturaliberalna\.pl/(\d{4})/(\d{2})/(\d{2})/[^"]+)"\s*>\s*(.*?)\s*</a>',
    re.S,
)


def unescape(s):
    for a, b in (("&#8211;", "–"), ("&#8212;", "—"), ("&#8222;", "„"),
                 ("&#8221;", "”"), ("&#8220;", "“"), ("&#8217;", "’"),
                 ("&#8216;", "‘"), ("&#8230;", "…"), ("&nbsp;", " "),
                 ("&amp;", "&"), ("&quot;", '"'), ("&lt;", "<"), ("&gt;", ">")):
        s = s.replace(a, b)
    return s


def scrape():
    """Return {url: (title, iso_date)} for the pieces on the author page."""
    req = urllib.request.Request(AUTHOR_URL, headers={"User-Agent": UA})
    try:
        html = urllib.request.urlopen(req, timeout=30).read().decode("utf-8", "replace")
    except (urllib.error.URLError, ssl.SSLError, OSError) as e:
        # Some macOS Pythons have no CA bundle wired up; retry unverified rather
        # than failing the build over a certificate path problem.
        ctx = ssl._create_unverified_context()
        html = urllib.request.urlopen(req, timeout=30, context=ctx).read().decode("utf-8", "replace")

    found = {}
    for tease in TEASE.findall(html):
        if AUTHOR_SLUG not in tease:
            continue
        m = LINK.search(tease)
        if not m:
            continue
        url, y, mo, d, title = m.groups()
        title = unescape(re.sub(r"\s+", " ", re.sub(r"<[^>]+>", "", title)).strip())
        if title:
            found[url] = (title, "%s-%s-%s" % (y, mo, d))
    return found


def read_existing(path):
    """Parse the url/title/date triples we wrote. Deliberately minimal: no PyYAML
    dependency, and the file only ever has the shape this script emits."""
    entries, header = {}, []
    if not os.path.exists(path):
        return entries, header
    with open(path, encoding="utf-8") as fh:
        lines = fh.read().splitlines()
    for line in lines:
        if line.startswith("articles:"):
            break
        header.append(line)
    url = title = date = None
    for line in lines:
        m = re.match(r'\s*-\s*url:\s*"(.*)"\s*$', line)
        if m:
            url, title, date = m.group(1), None, None
            continue
        m = re.match(r'\s*title:\s*"(.*)"\s*$', line)
        if m and url:
            title = m.group(1).replace('\\"', '"')
            continue
        m = re.match(r'\s*date:\s*"(.*)"\s*$', line)
        if m and url and title is not None:
            entries[url] = (title, m.group(1))
            url = title = date = None
    return entries, header


def write(path, header, entries):
    out = list(header) + ["articles:"]
    for url, (title, date) in sorted(entries.items(), key=lambda kv: (kv[1][1], kv[0]), reverse=True):
        out.append('  - url: "%s"' % url)
        out.append('    title: "%s"' % title.replace('"', '\\"'))
        out.append('    date: "%s"' % date)
    with open(path, "w", encoding="utf-8") as fh:
        fh.write("\n".join(out) + "\n")


def main():
    path = os.path.abspath(sys.argv[1] if len(sys.argv) > 1 else DEFAULT_YML)
    entries, header = read_existing(path)
    if not header:
        header = ["# Kultura Liberalna articles by Ben Stanley."
                  " Maintained by scripts/update-kl.py; see that file.", ""]

    try:
        found = scrape()
    except Exception as e:
        print("[kl] warning: could not reach kulturaliberalna.pl (%s);"
              " keeping the %d entries already in %s"
              % (e, len(entries), os.path.basename(path)))
        return 0

    if not found:
        print("[kl] warning: author page returned no articles -- the page markup may"
              " have changed. Keeping the %d existing entries; check scripts/update-kl.py."
              % len(entries))
        return 0

    added = [u for u in found if u not in entries]
    for u in added:
        entries[u] = found[u]

    if added:
        write(path, header, entries)
        for u in sorted(added, key=lambda u: found[u][1], reverse=True):
            print("[kl] + %s  %s" % (found[u][1], found[u][0]))
    print("[kl] %d article(s) total, %d new." % (len(entries), len(added)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
