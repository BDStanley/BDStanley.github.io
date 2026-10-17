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

The file is copied to quarto-site/data/ so the Quarto backup shows the same
numbers.

Run by build.sh. Network failures are non-fatal: the build then uses the
committed counts. Standard library only.
Usage:  python3 scripts/update-citations.py [--dry-run]
"""

import datetime
import html
import json
import os
import re
import ssl
import subprocess
import sys
import time
import urllib.error
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

    content = json.dumps(data, indent=2, ensure_ascii=False) + "\n"
    if dry:
        print("[citations] dry run: nothing written.")
        return 0
    if content != original:
        write(DATA, content)
    if os.path.isdir(os.path.dirname(MIRROR)):
        mirrored = open(MIRROR, encoding="utf-8").read() if os.path.exists(MIRROR) else None
        if mirrored != content:
            write(MIRROR, content)
            print("[citations] copied to quarto-site/data/citations.json.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
