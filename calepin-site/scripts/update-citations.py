#!/usr/bin/env python3
"""Refresh the citation counts in data/citations.json, which the Citations
section of the Publications page reads.

Where the numbers come from:

  * Google Scholar: citations, h-index and i10-index (the "All" column) from
    the metrics table on Ben's profile page.
  * Sage Policy Profiles: policy citations and the number of policy documents
    they come from, from the JSON the public profile page itself loads
    (Overton's data). Citations are summed over Ben's DOIs, as the profile does.
  * Web of Science: not fetched. The author record is free to view, but its
    data endpoints need an API key and the page shows automated browsers a
    CAPTCHA, so these numbers are entered by hand in citations.json, with the
    month they date from.

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
import sys
import urllib.error
import urllib.request

SCHOLAR_URL = "https://scholar.google.com/citations?user=8AO7H9gAAAAJ&hl=en"
SAGE_URL = "https://policyprofiles.sagepub.com/spa/public/citation-data/for-user/21087"
UA = "benstanley.eu citation check (https://benstanley.eu)"
MAX_FALL = 0.25

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.abspath(os.path.join(HERE, "..", "data", "citations.json"))
MIRROR = os.path.abspath(os.path.join(HERE, "..", "..", "quarto-site", "data", "citations.json"))

SCHOLAR_ROW = re.compile(r'<td class="gsc_rsb_sc1">.*?>([^<>]+)</a></td><td class="gsc_rsb_std">(\d+)</td>', re.S)


def fetch(url, accept="text/html"):
    req = urllib.request.Request(url, headers={"User-Agent": UA, "Accept": accept})
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


def sage_policy_profiles():
    data = json.loads(fetch(SAGE_URL, "application/json"))["data"]
    return {"citations": sum(len(c) for c in data["citations"].values()),
            "documents": data["metadata"]["total_document_results"]}


SOURCES = (("google_scholar", "Google Scholar", google_scholar),
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
            print("[citations] warning: could not read %s (%s: %s); keeping its %s numbers."
                  % (label, type(e).__name__, e, month_name(old["as_of"]) if old else "previous"))
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

    wos = data.get("web_of_science")
    if wos:
        print("[citations] Web of Science is entered by hand: %s, as of %s."
              % (summary(wos), month_name(wos["as_of"])))

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
