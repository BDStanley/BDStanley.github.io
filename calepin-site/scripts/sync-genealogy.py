#!/usr/bin/env python3
"""Copy the party genealogy data from the Polish political analyses repo into the site.

READ-ONLY with respect to that repo (BDStanley/Polish-political-analyses). Its Party
tracker writes output/figures/genealogy.json (R/63_export_genealogy_json.R, run by
run_all.R); this script copies it to

    assets/party-genealogy/genealogy.json

which the interactive chart beside it (assets/party-genealogy/index.html) loads. The
chart appears on pages/polish-political-analyses/party-genealogy.typ.

build.sh runs this before `calepin compile`, so re-running the Party tracker and
rebuilding the site updates the chart. The repo's location can be overridden with the
env var PPA_SRC. If the repo or the file is missing, or the file does not look like
the chart's data, the script warns and keeps the copy already committed here, so the
build never fails because of it.
"""

from __future__ import annotations

import json
import os
import shutil
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent          # calepin-site/scripts
SITE_DIR = SCRIPT_DIR.parent                           # calepin-site
DEST = SITE_DIR / "assets" / "party-genealogy" / "genealogy.json"

# Default: the sibling checkout (…/Positron/Polish political analyses).
DEFAULT_SRC = SITE_DIR.parent.parent / "Polish political analyses"
PPA_SRC = Path(os.environ.get("PPA_SRC", str(DEFAULT_SRC)))
SOURCE = PPA_SRC / "Party tracker" / "output" / "figures" / "genealogy.json"

REQUIRED_KEYS = {"asOf", "parties", "runs", "pres", "conn", "elections", "cabNames", "clubNames"}


def main() -> int:
    if not SOURCE.is_file():
        print(f"  warning: {SOURCE} not found; keeping the committed genealogy data", file=sys.stderr)
        return 0
    try:
        data = json.loads(SOURCE.read_text(encoding="utf-8"))
    except (OSError, ValueError) as err:
        print(f"  warning: could not read {SOURCE} ({err}); keeping the committed genealogy data", file=sys.stderr)
        return 0
    missing = REQUIRED_KEYS - set(data)
    if missing or not data.get("parties"):
        print(f"  warning: {SOURCE.name} lacks {sorted(missing) or 'parties'}; keeping the committed genealogy data",
              file=sys.stderr)
        return 0
    if DEST.is_file() and DEST.read_bytes() == SOURCE.read_bytes():
        print("  genealogy data unchanged")
        return 0
    DEST.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(SOURCE, DEST)
    print(f"  copied genealogy data ({len(data['parties'])} parties, {len(data['conn'])} links)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
