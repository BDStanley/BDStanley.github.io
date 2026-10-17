#!/usr/bin/env python3
"""Refresh the citation counts in data/citations.json, which the Citations
section of the Publications page reads.

Where the numbers come from:

  * Google Scholar: citations, h-index and i10-index (the "All" column) from
    the metrics table on Ben's profile page.
  * OpenAlex: citations, h-index and i10-index from its author record, looked
    up by Ben's ORCID iD (the open database update-publications.py also uses).
  * Sage Policy Profiles: policy citations and the number of policy documents
    they come from, from the JSON the public profile page itself loads
    (Overton's data). Citations are summed over Ben's DOIs, as the profile does.
  * Scopus: total citations and h-index, worked out from the citation counts
    of the documents under Ben's Scopus author ID in Elsevier's Scopus Search
    API. (The author profile API has the totals ready-made, but only for
    subscribing institutions; the search is open to any key.) The API key is
    read from SCOPUS_API_KEY or else from the macOS Keychain (service
    "scopus-api-key") and never stored in the repo. Without a key, Scopus is
    skipped and its last numbers, if any, stay.

Each source records the month its numbers date from ("as_of"). The page shows
the latest month and names the month next to any source that is older.

A source keeps its previous numbers when it can't be reached, when its page no
longer has the numbers where this script looks (e.g. Scholar answering with a
CAPTCHA), or when any of its numbers has fallen by more than a quarter, which is
far more likely to be a misread page than a real loss. If a fall is real, enter
the new numbers by hand; later runs carry on from them.

It also writes data/publication-citations.json: OpenAlex's citation count for
each publication in cv.yml, which the Publications page shows at the end of
the entry, before its full stop, as "(12 citations)", linked to OpenAlex's
list of the works citing it. Entries are matched by DOI or, for those without one, by
title among the works OpenAlex links to Ben's ORCID iD (strictly, as
update-publications.py matches them). The page looks them up by the entry's
"doi:" DOI, or by the whole entry when it has none, so they are keyed that way.
If OpenAlex can't be reached, or matches far fewer publications than last time,
the previous counts stay.

Both files are copied to quarto-site/data/ so the Quarto backup shows the same
numbers.

Run by build.sh. Network failures are non-fatal: the build then uses the
committed counts. Standard library only.
Usage:  python3 scripts/update-citations.py [--dry-run]
"""

import datetime
import html
import importlib.util
import json
import os
import re
import ssl
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

SCHOLAR_URL = "https://scholar.google.com/citations?user=8AO7H9gAAAAJ&hl=en"
SAGE_URL = "https://policyprofiles.sagepub.com/spa/public/citation-data/for-user/21087"
OPENALEX_URL = "https://api.openalex.org/authors/orcid:0000-0001-6932-1046?select=cited_by_count,summary_stats"
# Scopus author ID, as listed on Ben's ORCID record. Pages of 25, the most a
# non-subscriber key may ask for.
SCOPUS_URL = ("https://api.elsevier.com/content/search/scopus"
              "?query=AU-ID(23493968300)&field=citedby-count&count=25&start=%d")
UA = "benstanley.eu citation check (https://benstanley.eu)"
MAX_FALL = 0.25

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.abspath(os.path.join(HERE, "..", "data", "citations.json"))
MIRROR = os.path.abspath(os.path.join(HERE, "..", "..", "quarto-site", "data", "citations.json"))
PUB_DATA = os.path.abspath(os.path.join(HERE, "..", "data", "publication-citations.json"))
PUB_MIRROR = os.path.abspath(os.path.join(HERE, "..", "..", "quarto-site", "data", "publication-citations.json"))
OPENALEX_WORKS = "https://api.openalex.org/works?per-page=200&select=id,doi,title,cited_by_count"
# A "doi:" DOI as pages/publications.typ finds it, so both sides key an entry alike.
PAGE_DOI = re.compile(r"doi:(10\.\S+)")

# Reading cv.yml and matching titles come from update-publications.py, so
# publications are recognised exactly as there. No .pyc: scripts/__pycache__ is
# not git-ignored.
sys.dont_write_bytecode = True
_spec = importlib.util.spec_from_file_location("update_publications", os.path.join(HERE, "update-publications.py"))
pubs = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(pubs)

SCHOLAR_ROW = re.compile(r'<td class="gsc_rsb_sc1">.*?>([^<>]+)</a></td><td class="gsc_rsb_std">(\d+)</td>', re.S)


def fetch(url, accept="text/html", headers=None):
    req = urllib.request.Request(url, headers=dict({"User-Agent": UA, "Accept": accept}, **(headers or {})))
    try:
        body = urllib.request.urlopen(req, timeout=30).read()
    except urllib.error.HTTPError:
        raise
    except (urllib.error.URLError, ssl.SSLError, OSError):
        # As in update-kl.py: some macOS Pythons have no CA bundle wired up.
        ctx = ssl._create_unverified_context()
        body = urllib.request.urlopen(req, timeout=30, context=ctx).read()
    return body.decode("utf-8", "replace")


def google_scholar():
    table = re.search(r'<table id="gsc_rsb_st">(.*?)</table>', fetch(SCHOLAR_URL), re.S)
    if not table:
        raise ValueError("no metrics table on the page (a CAPTCHA, or changed markup)")
    rows = {html.unescape(label).strip(): int(n) for label, n in SCHOLAR_ROW.findall(table.group(1))}
    return {"citations": rows["Citations"], "h_index": rows["h-index"], "i10_index": rows["i10-index"]}


def openalex():
    data = json.loads(fetch(OPENALEX_URL, "application/json"))
    return {"citations": data["cited_by_count"], "h_index": data["summary_stats"]["h_index"],
            "i10_index": data["summary_stats"]["i10_index"]}


def sage_policy_profiles():
    data = json.loads(fetch(SAGE_URL, "application/json"))["data"]
    return {"citations": sum(len(c) for c in data["citations"].values()),
            "documents": data["metadata"]["total_document_results"]}


def secret(env, service):
    """A credential from the environment, else from the macOS Keychain, else None."""
    if os.environ.get(env):
        return os.environ[env]
    try:
        out = subprocess.run(["security", "find-generic-password", "-s", service, "-w"],
                             capture_output=True, text=True, timeout=10)
    except (OSError, subprocess.SubprocessError):
        return None
    if out.returncode != 0:
        return None
    return out.stdout.strip() or None


def scopus():
    """None when there is no API key, so the source is skipped rather than failed."""
    key = secret("SCOPUS_API_KEY", "scopus-api-key")
    if not key:
        return None
    counts, total = [], 1
    while len(counts) < total:
        try:
            page = json.loads(fetch(SCOPUS_URL % len(counts), "application/json",
                                    {"X-ELS-APIKey": key}))["search-results"]
        except urllib.error.HTTPError as e:
            if e.code in (401, 403):
                # Elsevier's reason ("Invalid API Key", ...) can quote the key, so
                # blank it out before printing.
                why = re.sub(r"\s+", " ", e.read().decode("utf-8", "replace"))[:400].replace(key, "<redacted>")
                raise ValueError("Elsevier refused the request (HTTP %d): %s"
                                 % (e.code, why or "no reason given")) from None
            raise
        total = int(page["opensearch:totalResults"])
        entries = [e for e in page.get("entry", []) if "error" not in e]   # "Result set was empty"
        if not entries:
            break
        counts += [int(e.get("citedby-count") or 0) for e in entries]
        if len(counts) < total:
            time.sleep(0.5)                             # Elsevier allows 3 requests a second
    if not counts:
        raise ValueError("Scopus found no documents for the author ID")
    counts.sort(reverse=True)
    return {"citations": sum(counts), "h_index": sum(1 for i, n in enumerate(counts, 1) if n >= i)}


def publication_counts():
    """{"by_doi": {doi: record}, "by_entry": {entry: record}} for the cv.yml
    publications OpenAlex knows, where a record is {"citations": n, "openalex":
    work ID} (the page links the count to https://openalex.org/works/<ID>)."""
    def record(w):
        return (w["cited_by_count"], w["id"].rsplit("/", 1)[-1])

    with open(pubs.DEFAULT_YML, encoding="utf-8") as fh:
        sections = pubs.parse(fh.read().split("\n"))
    entries = [t for key in pubs.PUB_SECTIONS for _, t in sections.get(key, {}).get("items", [])]
    dois = sorted({d for t in entries for d in pubs.dois_in(t)})
    found = {}
    for i in range(0, len(dois), 50):                   # OpenAlex takes up to 100 values per filter
        query = "|".join(urllib.parse.quote(d, safe="/") for d in dois[i:i + 50])
        for w in pubs.fetch_json(OPENALEX_WORKS + "&filter=doi:" + query)["results"]:
            found[pubs.clean_doi(w["doi"])] = record(w)
    mine = pubs.fetch_json(OPENALEX_WORKS + "&filter=author.orcid:" + pubs.ORCID)["results"]

    counts = {"by_doi": {}, "by_entry": {}}
    for t in entries:
        n = [found[d] for d in pubs.dois_in(t) if d in found]
        if not pubs.dois_in(t):                         # no DOI: match the title instead
            title = pubs.entry_title(t)
            n = [record(w) for w in mine
                 if title and w.get("title") and pubs.same_title(title, w["title"])]
        if n:
            citations, work = max(n)                    # the most-cited record, if several match
            m = PAGE_DOI.search(t)
            if m:
                counts["by_doi"][m.group(1).rstrip(".").lower()] = {"citations": citations, "openalex": work}
            else:
                counts["by_entry"][t] = {"citations": citations, "openalex": work}
    return counts


SOURCES = (("scopus", "Scopus", scopus),
           ("google_scholar", "Google Scholar", google_scholar),
           ("openalex", "OpenAlex", openalex),
           ("sage_policy_profiles", "Sage Policy Profiles", sage_policy_profiles))


def month_name(ym):
    return datetime.date(int(ym[:4]), int(ym[5:7]), 1).strftime("%B %Y")


def summary(nums):
    return ", ".join("%s %s" % (k, v) for k, v in nums.items() if k != "as_of")


def write(path, content):
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(content)


def save(path, mirror, original, content):
    """Write content to path, and to its quarto-site copy, where they differ."""
    if content != original:
        write(path, content)
    if os.path.isdir(os.path.dirname(mirror)):
        mirrored = open(mirror, encoding="utf-8").read() if os.path.exists(mirror) else None
        if mirrored != content:
            write(mirror, content)
            print("[citations] copied to quarto-site/data/%s." % os.path.basename(mirror))


def main():
    dry = "--dry-run" in sys.argv[1:]
    with open(DATA, encoding="utf-8") as fh:
        original = fh.read()
    data = json.loads(original)
    this_month = datetime.date.today().strftime("%Y-%m")

    for key, label, get in SOURCES:
        old = data.get(key, {})
        try:
            new = get()
        except Exception as e:
            print("[citations] warning: could not read %s (%s: %s); %s."
                  % (label, type(e).__name__, e,
                     "keeping its %s numbers" % month_name(old["as_of"]) if old else "nothing to show yet"))
            continue
        if new is None:
            print("[citations] %s: skipped (no API key in SCOPUS_API_KEY or the Keychain)." % label)
            continue
        fallen = [k for k, n in new.items() if n < (1 - MAX_FALL) * old.get(k, 0)]
        if fallen:
            print("[citations] needs attention: %s reports %s, down from %s. Keeping the old"
                  " numbers; if the fall is real, enter the new ones in data/citations.json."
                  % (label, summary(new), summary(old)))
            continue
        data[key] = dict(as_of=this_month, **new)
        if summary(new) != summary(old):
            print("[citations] %s: %s (was %s)" % (label, summary(new), summary(old) or "none"))
        else:
            print("[citations] %s: unchanged (%s)" % (label, summary(new)))

    pub_original = open(PUB_DATA, encoding="utf-8").read() if os.path.exists(PUB_DATA) else "{}"
    pub = json.loads(pub_original)
    before = len(pub.get("by_doi", {})) + len(pub.get("by_entry", {}))
    try:
        counts = publication_counts()
    except Exception as e:
        print("[citations] warning: could not read the per-publication counts from OpenAlex"
              " (%s: %s); keeping the previous ones." % (type(e).__name__, e))
    else:
        matched = len(counts["by_doi"]) + len(counts["by_entry"])
        if matched < (1 - MAX_FALL) * before:
            print("[citations] needs attention: OpenAlex matched %d publications, down from %d;"
                  " keeping the previous per-publication counts." % (matched, before))
        else:
            pub = dict(_about="Citation counts per publication, from OpenAlex, shown at the end of each"
                              " entry on the Publications page. Written by scripts/update-citations.py; see that file.",
                       **counts)
            print("[citations] OpenAlex counts for %d of the CV's publications." % matched)

    content = json.dumps(data, indent=2, ensure_ascii=False) + "\n"
    pub_content = json.dumps(pub, indent=2, ensure_ascii=False) + "\n"
    if dry:
        print("[citations] dry run: nothing written.")
        return 0
    save(DATA, MIRROR, original, content)
    save(PUB_DATA, PUB_MIRROR, pub_original, pub_content)
    return 0


if __name__ == "__main__":
    sys.exit(main())
