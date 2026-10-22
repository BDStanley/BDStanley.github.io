#!/usr/bin/env python3
"""Copy the data behind the Polish political analyses pages from that repo into the site.

READ-ONLY with respect to the Polish political analyses repo
(BDStanley/Polish-political-analyses). Each analysis there writes JSON for its web
page; this script copies it beside the page's chart under assets/:

    Party tracker/output/figures/genealogy.json  -> assets/party-genealogy/genealogy.json
        (written by Party tracker/R/63_export_genealogy_json.R, run by run_all.R)
    Polls/web/polls.json                          -> assets/polling-tracker/polls.json
    Polls/web/draws.json                          -> assets/polling-tracker/draws.json
        (written by Polls/R/export_web.R, which PTP.R runs)
    Party system analysis/web/party_system.json   -> assets/party-system-indicators/party_system.json
        (written by Party system analysis/analysis/export_web.R, which
        "Party system analysis.R" runs)
    Party ideology/web/ideology.json              -> assets/party-ideology/ideology.json
        (written by "Party ideology/analysis/Party ideology.R")
    Political attitudes/web/attitudes.json        -> assets/political-attitudes/attitudes.json
    Political attitudes/web/<slug>.json           -> assets/political-attitudes/<slug>.json
        (written by "Political attitudes/web/build_web.R"; attitudes.json lists the
        attitudes, and each one it lists is copied too)

build.sh runs this before `calepin compile`, so re-running an analysis and rebuilding
the site updates its page. The repo's location can be overridden with the env var
PPA_SRC. A file that is missing, unreadable or lacks the fields its chart needs is
skipped with a warning and the copy already committed here is kept, so the build
never fails because of it. draws.json is optional: without it the Polling tracker
shows scenarios as single allocations.
"""

from __future__ import annotations

import json
import os
import shutil
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent          # calepin-site/scripts
SITE_DIR = SCRIPT_DIR.parent                           # calepin-site

# Default: the sibling checkout (…/Positron/Polish political analyses).
DEFAULT_SRC = SITE_DIR.parent.parent / "Polish political analyses"
PPA_SRC = Path(os.environ.get("PPA_SRC", str(DEFAULT_SRC)))

# (source in the repo, destination in the site, fields the file must have)
FILES = [
    ("Party tracker/output/figures/genealogy.json", "assets/party-genealogy/genealogy.json",
     {"asOf", "parties", "runs", "pres", "conn", "elections", "cabNames", "clubNames"}),
    ("Polls/web/polls.json", "assets/polling-tracker/polls.json",
     {"meta", "rules", "constituencies", "trend", "polls", "weeks"}),
    ("Polls/web/draws.json", "assets/polling-tracker/draws.json",
     {"nDraws", "categories", "weeks"}),
    ("Party system analysis/web/party_system.json", "assets/party-system-indicators/party_system.json",
     {"meta", "elections", "lists", "lineages", "governments", "closureIndex"}),
    ("Party ideology/web/ideology.json", "assets/party-ideology/ideology.json",
     {"meta", "countries", "dims", "parties", "rows", "system"}),
    ("Political attitudes/web/attitudes.json", "assets/political-attitudes/attitudes.json",
     {"meta", "attitudes"}),
]

# Files listed inside a synced index: (index in the repo, field with the list, key naming each
# file, fields each file must have). Each listed file sits beside its index in both places.
LISTED = [
    ("Political attitudes/web/attitudes.json", "attitudes", "file",
     {"meta", "national", "groupings", "items", "readings", "events"}),
]


def sync(rel_src: str, rel_dest: str, required: set[str]) -> None:
    source, dest = PPA_SRC / rel_src, SITE_DIR / rel_dest
    if not source.is_file():
        if dest.is_file():
            print(f"  warning: {source} not found; keeping the committed {dest.name}", file=sys.stderr)
        else:
            print(f"  note: {source} not found yet, so there is no {rel_dest}", file=sys.stderr)
        return
    try:
        data = json.loads(source.read_text(encoding="utf-8"))
    except (OSError, ValueError) as err:
        print(f"  warning: could not read {source} ({err}); keeping the committed {dest.name}", file=sys.stderr)
        return
    missing = required - set(data)
    if missing:
        print(f"  warning: {source.name} lacks {sorted(missing)}; keeping the committed {dest.name}", file=sys.stderr)
        return
    if dest.is_file() and dest.read_bytes() == source.read_bytes():
        print(f"  {rel_dest}: unchanged")
        return
    dest.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(source, dest)
    print(f"  {rel_dest}: updated")


def sync_listed(rel_index: str, field: str, key: str, required: set[str]) -> None:
    index = PPA_SRC / rel_index
    try:
        entries = json.loads(index.read_text(encoding="utf-8")).get(field, [])
    except (OSError, ValueError):
        return  # sync() has already reported the index
    dest_dir = next(dest for src, dest, _ in FILES if src == rel_index).rsplit("/", 1)[0]
    src_dir = rel_index.rsplit("/", 1)[0]
    for entry in entries:
        name = entry.get(key) if isinstance(entry, dict) else None
        if not name or "/" in name or not name.endswith(".json"):
            print(f"  warning: {index.name} lists an invalid file {name!r}; skipped", file=sys.stderr)
            continue
        sync(f"{src_dir}/{name}", f"{dest_dir}/{name}", required)


def main() -> int:
    for rel_src, rel_dest, required in FILES:
        sync(rel_src, rel_dest, required)
    for rel_index, field, key, required in LISTED:
        sync_listed(rel_index, field, key, required)
    return 0


if __name__ == "__main__":
    sys.exit(main())
