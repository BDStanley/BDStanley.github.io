#!/usr/bin/env python3
"""Copy the data behind the Polish political analyses pages from that repo into the site.

READ-ONLY with respect to the Polish political analyses repo
(BDStanley/Polish-political-analyses). Each analysis there writes JSON for its web
page; this script copies it beside the page's chart under assets/:

    Party tracker/output/figures/genealogy.json  -> assets/party-genealogy/genealogy.json
        (written by Party tracker/R/63_export_genealogy_json.R, run by run_all.R)
    Party tracker/output/figures/defections.json -> assets/party-genealogy/defections.json
        (written by Party tracker/R/64_export_defections_json.R, run by run_all.R)
    Polls/web/polls.json                          -> assets/polling-tracker/polls.json
    Polls/web/draws.json                          -> assets/polling-tracker/draws.json
        (written by Polls/current term/R/export_web.R, which PTP.R runs)
    Polls/web/terms.json                          -> assets/polling-tracker/terms.json
    Polls/web/term-<term>.json                    -> assets/polling-tracker/term-<term>.json
        (the past Sejm terms, written by Polls/R/terms/30_export.R; terms.json lists
        the terms, and each one it lists is copied too)
    Voting behaviour/web/joint_penalty.json       -> assets/polling-tracker/joint_penalty.json
        (the voters' response to joint lists, written by "Voting behaviour/analysis/79b_joint_penalty_export.R")
    Party system analysis/web/party_system.json   -> assets/party-system-indicators/party_system.json
        (written by Party system analysis/analysis/export_web.R, which
        "Party system analysis.R" runs)
    Party ideology/web/ideology.json              -> assets/party-ideology/ideology.json
        (written by "Party ideology/analysis/Party ideology.R")
    Party manifestos/web/manifestos.json          -> assets/party-manifestos/manifestos.json
        (written by "Party manifestos/analysis/30_export.R")
    Political attitudes/web/attitudes.json        -> assets/political-attitudes/attitudes.json
    Political attitudes/web/<slug>.json           -> assets/political-attitudes/<slug>.json
        (written by "Political attitudes/web/build_web.R"; attitudes.json lists the
        attitudes, and each one it lists is copied too)
    Election results/web/elections.json           -> assets/election-results/elections.json
    Election results/web/<file>.json              -> assets/election-results/<file>.json
        (written by "Election results/analysis/90_export.R"; elections.json's `files`
        lists the others: geo.json, covariates.json, one covariates-<group>.json per
        group of the scatter's characteristics, one file per election and one
        profiles file per voivodeship)
    Quality of democracy/web/democracy.json       -> assets/quality-of-democracy/democracy.json
    Quality of democracy/web/democracy-countries.json
                                                  -> assets/quality-of-democracy/democracy-countries.json
    Quality of democracy/web/political-context.json
                                                  -> assets/quality-of-democracy/political-context.json
        (written by "Quality of democracy/analysis/export_web.R", which
        "Quality of democracy.R" runs)

    Constitutional Tribunal/web/composition.json  -> assets/constitutional-tribunal/composition.json
    Constitutional Tribunal/web/chronology.json   -> assets/constitutional-tribunal/chronology.json
    Constitutional Tribunal/web/activity.json     -> assets/constitutional-tribunal/activity.json
    Constitutional Tribunal/web/rulings.json      -> assets/constitutional-tribunal/rulings.json
    Constitutional Tribunal/web/rulings-<year>.json, rulings-pending.json
                                                  -> assets/constitutional-tribunal/
        (written by "Constitutional Tribunal/analysis/90_export.R" and "91_export_rulings.R";
        rulings.json's `years` lists the yearly files, and each one it lists is copied too)

    Voting behaviour/web/voting.json              -> assets/voting-behaviour/voting.json
    Voting behaviour/web/polarisation.json        -> assets/voting-behaviour/polarisation.json
    Voting behaviour/web/calc-<year>.json         -> assets/voting-behaviour/calc-<year>.json
        (written by "Voting behaviour/analysis/70_export_web.R"; voting.json's `files`
        lists the calculator files, one per election, and each one it lists is copied too)

    Constituencies/web/constituencies.json         -> assets/constituencies/constituencies.json
    Constituencies/web/{geometry,mps}.json         -> assets/constituencies/<same name>
        (written by "Constituencies/analysis/90_export.R"; constituencies.json's `files` lists the other two, and each one it lists is copied too)
    Sejm analysis/Legislative activity/web/legislative.json -> assets/legislative-activity/legislative.json
    Sejm analysis/Legislative activity/web/{mps,profile,roles,rebels,votes}-<term>[-<chunk>].json
                                                  -> assets/legislative-activity/<same name>
        (written by "Sejm analysis/Legislative activity/R/la90_export_web.R", which run_all.R runs;
        legislative.json's `files` lists the per-term files, and each one it lists is copied too)

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
     {"asOf", "parties", "runs", "pres", "conn", "elections", "cabNames", "clubNames",
      "seatGroups", "seatRuns", "partnerNames"}),
    ("Party tracker/output/figures/defections.json", "assets/party-genealogy/defections.json",
     {"asOf", "terms", "types", "parties", "clubs", "people", "moves", "spells"}),
    ("Polls/web/polls.json", "assets/polling-tracker/polls.json",
     {"meta", "rules", "constituencies", "trend", "polls", "weeks"}),
    ("Polls/web/draws.json", "assets/polling-tracker/draws.json",
     {"nDraws", "categories", "weeks"}),
    ("Polls/web/terms.json", "assets/polling-tracker/terms.json",
     {"version", "terms"}),
    ("Voting behaviour/web/joint_penalty.json", "assets/polling-tracker/joint_penalty.json",
     {"meta", "parties", "pairs", "levels", "abstain", "splits", "splitsOn"}),
    ("Party system analysis/web/party_system.json", "assets/party-system-indicators/party_system.json",
     {"meta", "elections", "lists", "lineages", "governments", "closureIndex"}),
    ("Party ideology/web/ideology.json", "assets/party-ideology/ideology.json",
     {"meta", "countries", "dims", "parties", "rows", "system"}),
    ("Governments/web/governments.json", "assets/governments/governments.json",
     {"meta", "cabinets", "posts", "ministries", "stats"}),
    ("Party manifestos/web/manifestos.json", "assets/party-manifestos/manifestos.json",
     {"meta", "elections", "programmes", "categories", "domains", "ches", "chesCorrelations"}),
    ("Political attitudes/web/attitudes.json", "assets/political-attitudes/attitudes.json",
     {"meta", "attitudes"}),
    ("Election results/web/elections.json", "assets/election-results/elections.json",
     {"meta", "elections", "contests", "labels", "files"}),
    ("Quality of democracy/web/democracy.json", "assets/quality-of-democracy/democracy.json",
     {"meta", "regions", "measures", "years", "poland", "regional", "regime", "countries", "rollcall"}),
    ("Quality of democracy/web/democracy-countries.json", "assets/quality-of-democracy/democracy-countries.json",
     {"meta", "years", "countries"}),
    ("Quality of democracy/web/political-context.json", "assets/quality-of-democracy/political-context.json",
     {"meta", "elections", "cabinets", "presidents", "events"}),
    ("Constitutional Tribunal/web/composition.json", "assets/constitutional-tribunal/composition.json",
     {"meta", "judges", "seat_runs", "elected_not_on_lists"}),
    ("Constitutional Tribunal/web/chronology.json", "assets/constitutional-tribunal/chronology.json",
     {"meta", "events"}),
    ("Constitutional Tribunal/web/activity.json", "assets/constitutional-tribunal/activity.json",
     {"meta", "long", "years"}),
    ("Constitutional Tribunal/web/rulings-pending.json", "assets/constitutional-tribunal/rulings-pending.json",
     {"year", "cases"}),
    ("Constitutional Tribunal/web/rulings.json", "assets/constitutional-tribunal/rulings.json",
     {"meta", "judges", "years"}),
    ("Voting behaviour/web/voting.json", "assets/voting-behaviour/voting.json",
     {"meta", "files", "elections", "categories", "vs_official", "groupings", "groups", "composition", "effects"}),
    ("Voting behaviour/web/polarisation.json", "assets/voting-behaviour/polarisation.json",
     {"meta", "affective", "ideological"}),
    ("Constituencies/web/constituencies.json", "assets/constituencies/constituencies.json",
     {"files", "meta", "elections", "constituencies", "list_results", "list_costs", "national_lists", "magnitude_bands", "palette", "covariates", "projection"}),
    ("Sejm analysis/Legislative activity/web/legislative.json", "assets/legislative-activity/legislative.json",
     {"files", "meta", "terms", "clubs", "family_colours", "cohesion", "government_opposition", "agreement_between_families",
      "ministry_delays", "topics", "distributions", "wnominate"}),
]

# Files listed inside a synced index: (index in the repo, field with the list, key naming each
# file, fields each file must have). Each listed file sits beside its index in both places.
LISTED = [
    ("Political attitudes/web/attitudes.json", "attitudes", "file",
     {"meta", "national", "groupings", "items", "readings", "events"}),
    ("Polls/web/terms.json", "terms", "file",
     {"term", "meta", "parties", "colours", "trend", "pollsters", "polls", "results", "house", "final"}),
    ("Constitutional Tribunal/web/rulings.json", "years", "file", {"year", "partial", "cases"}),
]


# Indexes whose `files` field is a plain list of file names beside them: (index in the repo,
# field, [(name prefix, fields a file with that prefix must have)], the first prefix that
# matches applying).
LISTED_NAMES = [
    ("Election results/web/elections.json", "files", [
        ("geo.json", {"meta", "gminas", "sets", "partitions"}),
        ("covariates.json", {"meta", "groups", "vars", "years", "zones"}),
        ("covariates-", {"group", "values"}),
        ("profiles-", {"units"}),
        ("", {"facts", "results", "units"}),
    ]),
    ("Voting behaviour/web/voting.json", "files", [
        ("calc-", {"year", "ballot", "design", "n_draws", "coef", "shifts", "support", "rules", "attitudes"}),
    ]),
    ("Constituencies/web/constituencies.json", "files", [
        ("geometry.json", {"meta", "sets"}),
        ("mps.json", {"note", "mps"}),
    ]),
    ("Sejm analysis/Legislative activity/web/legislative.json", "files", [
        ("mps-", {"term", "n_mps", "rebels_files", "vote_chunk_first_index", "n_roll_calls", "mps"}),
        ("profile-", {"term", "mps"}),
        ("roles-", {"term", "mps"}),
        ("rebels-", {"term", "chunk", "mps"}),
        ("votes-", {"term", "chunk", "first_index", "votes"}),
    ]),
]


# Site-wide spellings, applied to the data as it is copied so they survive the next sync (the
# repo the data comes from is read-only here). Each rule replaces a whole JSON string, so
# the lists' full names ("Komitet Wyborczy Samoobrona ...") are left alone.
RENAMES = {'"Samoobrona"': '"SRP"'}
# Variable names that leak into the explanatory text of the data files, put into words
TEXT_FIXES = {
    "(tally_ok FALSE)": "(failed the tally check)",
    "As psns_constituency, gminas": "As the constituency measure, but for gminas",
    "over enp_district_mean, minus 1": "over the mean ENP of the districts, minus 1",
}
# Political attitudes also names the party at the start of its events ("Samoobrona enters the Sejm")
EVENT_RENAMES = {'"Samoobrona enters': '"SRP enters', '"Samoobrona and ': '"SRP and ', '"Samoobrona \'sex': '"SRP \'sex'}


def apply_names(text: str, rel_dest: str) -> str:
    for old, new in RENAMES.items():
        text = text.replace(old, new)
    for old, new in TEXT_FIXES.items():
        text = text.replace(old, new)
    if "political-attitudes" in rel_dest:
        for old, new in EVENT_RENAMES.items():
            text = text.replace(old, new)
    return text


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
    out = apply_names(source.read_text(encoding="utf-8"), rel_dest).encode("utf-8")
    if dest.is_file() and dest.read_bytes() == out:
        print(f"  {rel_dest}: unchanged")
        return
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_bytes(out)
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


def sync_named(rel_index: str, field: str, rules: list[tuple[str, set[str]]]) -> None:
    index = PPA_SRC / rel_index
    try:
        names = json.loads(index.read_text(encoding="utf-8")).get(field, [])
    except (OSError, ValueError):
        return  # sync() has already reported the index
    dest_dir = next(dest for src, dest, _ in FILES if src == rel_index).rsplit("/", 1)[0]
    src_dir = rel_index.rsplit("/", 1)[0]
    for name in names:
        if not isinstance(name, str) or "/" in name or not name.endswith(".json"):
            print(f"  warning: {index.name} lists an invalid file {name!r}; skipped", file=sys.stderr)
            continue
        required = next(req for prefix, req in rules if name.startswith(prefix))
        sync(f"{src_dir}/{name}", f"{dest_dir}/{name}", required)


def main() -> int:
    for rel_src, rel_dest, required in FILES:
        sync(rel_src, rel_dest, required)
    for rel_index, field, key, required in LISTED:
        sync_listed(rel_index, field, key, required)
    for rel_index, field, rules in LISTED_NAMES:
        sync_named(rel_index, field, rules)
    return 0


if __name__ == "__main__":
    sys.exit(main())
