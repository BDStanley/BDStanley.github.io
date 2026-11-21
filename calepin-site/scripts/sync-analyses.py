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
    Polls/web/groups.json                         -> assets/polling-tracker/groups.json
        (the By group view; written by Polls' export_web.R from current term/R/group_trends.R)
    Polls/web/terms.json                          -> assets/polling-tracker/terms.json
    Polls/web/term-<term>.json                    -> assets/polling-tracker/term-<term>.json
        (the past Sejm terms, written by Polls/R/terms/30_export.R; terms.json lists
        the terms, and each one it lists is copied too)
    Voting behaviour/web/joint_penalty.json       -> assets/polling-tracker/joint_penalty.json
        (the voters' response to joint lists, written by "Voting behaviour/analysis/79b_joint_penalty_export.R")
    Election 2027/web/cells.json                  -> assets/polling-tracker/cells.json
        (the cells, exit-poll targets and polls-by-group starting point for the tracker's Scenarios page,
        written by "Election 2027/analysis/90_export.R")
    Election 2027/web/events.json                 -> assets/polling-tracker/events.json
        (the political events on the tracker's trend chart, each with its source: an array of
        {date, event, url}, written by "Election 2027/blog/publish.R" from blog/events.csv)
    Election 2027/web/engine.js                   -> assets/polling-tracker/engine.js
        (the raking of the 2023 cells for the tracker's Groups and Who votes views; a script, copied as it is
        when it defines E27Engine; its checks are "Election 2027/web/test_engine.js")
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
        group of the scatter's characteristics, one file per election, one
        profiles file per voivodeship, senate-<year>.json for each Senate election
        people.json and people-sejm.json, the careers of senators, MEPs and deputies; the Senate files and
        the people files are written by "Election results/analysis/92_senate_export.R")
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

    Presidents/web/presidents.json                -> assets/presidents/presidents.json
        (written by "Presidents/analysis/90_export.R", which run_all.R runs)

    Voting behaviour/web/voting.json              -> assets/voting-behaviour/voting.json
    Voting behaviour/web/polarisation.json        -> assets/voting-behaviour/polarisation.json
    Voting behaviour/web/calc-<year>.json         -> assets/voting-behaviour/calc-<year>.json
        (written by "Voting behaviour/analysis/70_export_web.R"; voting.json's `files`
        lists the calculator files, one per election, and each one it lists is copied too)

    Constituencies/web/constituencies.json         -> assets/constituencies/constituencies.json
    Constituencies/web/{geometry,mps,scenarios}.json         -> assets/constituencies/<same name>
        (written by "Constituencies/analysis/90_export.R"; constituencies.json's `files` lists the other two, and each one it lists is copied too)
    Sejm analysis/Legislative activity/web/legislative.json -> assets/legislative-activity/legislative.json
    Sejm analysis/Legislative activity/web/{mps,profile,roles,rebels,votes}-<term>[-<chunk>].json
                                                  -> assets/legislative-activity/<same name>
        (written by "Sejm analysis/Legislative activity/R/la90_export_web.R", which run_all.R runs;
        legislative.json's `files` lists the per-term files, and each one it lists is copied too;
        senate-legislative.json and ep-legislative.json and the senate- and ep- files they list come the
        same way from la40_senate.R and la45_ep.R. One app reads them all, embedded by three pages since
        21 November 2026: sejm.typ, senate.typ and european-parliament.typ)

Election 2027 (the blog; PLAN.md decision 18 in that project). Its posts are written in
"Election 2027/blog/posts/" and published by "Election 2027/blog/publish.R", which writes
"Election 2027/web/blog/": posts.json (the published posts, newest first), one <date>-<slug>.typ body
per post, figures/<date>/ (frozen PNGs and CSVs), feed.xml (Atom) and page.typ (the standfirst).
From these this script copies
    Election 2027/web/blog/posts.json             -> assets/election-2027/posts.json
    Election 2027/web/blog/feed.xml               -> assets/election-2027/feed.xml
    Election 2027/web/blog/figures/<date>/*       -> assets/election-2027/figures/<date>/*
    Election 2027/web/election2027.json `facts`   -> assets/election-2027/facts.json
and GENERATES the pages, as sync-teaching.py does the Teaching pages:
    pages/polish-political-analyses/election-2027.typ              the main page: standfirst, the live
        "Where the polls stand" panel (assets/election-2027/index.html, which reads
        ../polling-tracker/polls.json in the browser), the latest post in full and the earlier ones
    pages/polish-political-analyses/election-2027/<date>-<slug>.typ one page per published post
Generated post pages that are no longer published are removed; the hand-written
election-2027/organisation.typ (Timing and organisation, which reads facts.json) is left alone.
Drafts never reach this repo. In a body, "e27base:" stands for the path to
pages/polish-political-analyses/ and is replaced here ("" on the main page, "../" on a post's page).

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
import re
import shutil
import sys
from datetime import date, timedelta
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
    ("Polls/web/groups.json", "assets/polling-tracker/groups.json",
     {"meta", "months", "lists", "national", "dims"}),
    ("Voting behaviour/web/joint_penalty.json", "assets/polling-tracker/joint_penalty.json",
     {"meta", "parties", "pairs", "levels", "abstain", "splits", "splitsOn"}),
    ("Election 2027/web/cells.json", "assets/polling-tracker/cells.json",
     {"meta", "dims", "cells", "splits", "parties", "tracker"}),
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
    ("Presidents/web/presidents.json", "assets/presidents/presidents.json",
     {"meta", "presidents", "governments", "heads", "vetoes", "early", "bills", "tribunal", "gazette", "key_acts",
      "prime_ministers", "nbp", "pardons", "secondary", "compare", "by_relation", "validation"}),
    ("Voting behaviour/web/voting.json", "assets/voting-behaviour/voting.json",
     {"meta", "files", "elections", "categories", "vs_official", "groupings", "groups", "composition", "effects"}),
    ("Voting behaviour/web/polarisation.json", "assets/voting-behaviour/polarisation.json",
     {"meta", "affective", "ideological"}),
    ("Constituencies/web/constituencies.json", "assets/constituencies/constituencies.json",
     {"files", "meta", "elections", "constituencies", "list_results", "list_costs", "national_lists", "magnitude_bands", "palette", "covariates", "projection"}),
    ("Sejm analysis/Legislative activity/web/legislative.json", "assets/legislative-activity/legislative.json",
     {"files", "meta", "terms", "clubs", "family_colours", "cohesion", "government_opposition", "agreement_between_families",
      "ministry_delays", "topics", "distributions", "wnominate"}),
    ("Sejm analysis/Legislative activity/web/senate-legislative.json", "assets/legislative-activity/senate-legislative.json",
     {"files", "meta", "terms", "clubs", "family_colours", "cohesion", "government_opposition", "agreement_between_families"}),
    ("Sejm analysis/Legislative activity/web/ep-legislative.json", "assets/legislative-activity/ep-legislative.json",
     {"files", "meta", "terms", "clubs", "family_colours", "cohesion", "agreement_between_families"}),
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
        ("senate-activity.json", {"meta", "persons"}),
        ("senate-", {"facts", "districts"}),
        ("people.json", {"meta", "parties", "persons", "moves"}),
        ("people-sejm.json", {"meta", "persons"}),
        ("ep-activity.json", {"meta", "persons"}),
        ("sejm-activity.json", {"meta", "persons"}),
        ("", {"facts", "results", "units"}),
    ]),
    ("Voting behaviour/web/voting.json", "files", [
        ("calc-", {"year", "ballot", "design", "n_draws", "coef", "shifts", "support", "rules", "attitudes"}),
    ]),
    ("Constituencies/web/constituencies.json", "files", [
        ("geometry.json", {"meta", "sets"}),
        ("mps.json", {"note", "mps"}),
        ("scenarios.json", {"meta", "palette", "constituencies", "measures", "parties", "gallagher", "by_constituency"}),
    ]),
    ("Sejm analysis/Legislative activity/web/legislative.json", "files", [
        ("mps-", {"term", "n_mps", "rebels_files", "vote_chunk_first_index", "n_roll_calls", "mps"}),
        ("profile-", {"term", "mps"}),
        ("roles-", {"term", "mps"}),
        ("rebels-", {"term", "chunk", "mps"}),
        ("votes-", {"term", "chunk", "first_index", "votes"}),
        ("bills-", {"term", "bills"}),
        ("bv-", {"term", "chunk", "mp_ids", "clubs", "rows"}),
    ]),
    ("Sejm analysis/Legislative activity/web/senate-legislative.json", "files", [
        ("senate-mps-", {"term", "n_mps", "rebels_files", "vote_chunk_first_index", "n_roll_calls", "mps"}),
        ("senate-profile-", {"term", "mps"}),
        ("senate-roles-", {"term", "mps"}),
        ("senate-rebels-", {"term", "chunk", "mps"}),
        ("senate-votes-", {"term", "chunk", "first_index", "votes"}),
        ("senate-bills-", {"term", "bills"}),
        ("senate-bv-", {"term", "chunk", "mp_ids", "clubs", "rows"}),
    ]),
    ("Sejm analysis/Legislative activity/web/ep-legislative.json", "files", [
        ("ep-mps-", {"term", "n_mps", "rebels_files", "vote_chunk_first_index", "n_roll_calls", "mps"}),
        ("ep-profile-", {"term", "mps"}),
        ("ep-roles-", {"term", "mps"}),
        ("ep-rebels-", {"term", "chunk", "mps"}),
        ("ep-votes-", {"term", "chunk", "first_index", "votes"}),
        ("ep-bills-", {"term", "bills"}),
        ("ep-bv-", {"term", "chunk", "mp_ids", "clubs", "rows"}),
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
    if not isinstance(data, dict):
        print(f"  warning: {source.name} is not a JSON object; keeping the committed {dest.name}", file=sys.stderr)
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


# ---- Election 2027: the tracker's events, and the blog --------------------------------------------------------------

def sync_events(rel_src: str, rel_dest: str) -> None:
    """events.json is an array of {date, event, url}: checked item by item (sync() takes objects only)."""
    source, dest = PPA_SRC / rel_src, SITE_DIR / rel_dest
    if not source.is_file():
        print(f"  note: {source} not found yet, so there is no {rel_dest}" if not dest.is_file()
              else f"  warning: {source} not found; keeping the committed {dest.name}", file=sys.stderr)
        return
    try:
        data = json.loads(source.read_text(encoding="utf-8"))
    except (OSError, ValueError) as err:
        print(f"  warning: could not read {source} ({err}); keeping the committed {dest.name}", file=sys.stderr)
        return
    if not isinstance(data, list) or not all(isinstance(e, dict) and {"date", "event", "url"} <= set(e) for e in data):
        print(f"  warning: {source.name} is not a list of {{date, event, url}}; keeping the committed {dest.name}", file=sys.stderr)
        return
    write_if_changed(dest, apply_names(source.read_text(encoding="utf-8"), rel_dest).encode("utf-8"), rel_dest)


def write_if_changed(dest: Path, out: bytes, label: str) -> None:
    if dest.is_file() and dest.read_bytes() == out:
        print(f"  {label}: unchanged")
        return
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_bytes(out)
    print(f"  {label}: updated")


E27_WEB = "Election 2027/web"
E27_ASSETS = SITE_DIR / "assets" / "election-2027"
E27_PAGES = SITE_DIR / "pages" / "polish-political-analyses"
E27_POST_RE = re.compile(r"^\d{4}-\d{2}-\d{2}-[a-z0-9-]+\.typ$")
E27_GENERATED = ("// AUTO-GENERATED by scripts/sync-analyses.py from \"Election 2027/web/blog/\" in the Polish political\n"
                 "// analyses repo. Do not edit by hand: edit the posts there (Election 2027/blog/), publish them with\n"
                 "// publish.R and rebuild.\n")
MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October",
          "November", "December"]


def long_date(d) -> str:
    if isinstance(d, str):
        d = date.fromisoformat(d)
    return f"{d.day} {MONTHS[d.month - 1]} {d.year}"


def ty(s: str) -> str:
    """A Typst string literal, inserted as text (no markup)."""
    return '"' + str(s).replace("\\", "\\\\").replace('"', '\\"').replace("\n", " ") + '"'


def election_window(facts: dict) -> tuple[list[date], date]:
    """The Sundays on which the election can fall, and the latest date for the President's order."""
    start = date.fromisoformat(facts["termStart"])
    end = start.replace(year=start.year + 4)
    rules = facts["rules"]
    days = [end - timedelta(days=k) for k in range(rules["windowDaysBefore"], 0, -1)]
    return [d for d in days if d.weekday() == 6], end - timedelta(days=rules["orderDaysBefore"])


def e27_nav(prefix: str, on: str | None) -> str:
    items = [("updates", "Updates", prefix + "election-2027.html"),
             ("organisation", "Timing and organisation", prefix + "election-2027/organisation.html"),
             ("tracker", "Polling tracker", prefix + "polling-tracker.html")]
    links = []
    for key, label, href in items:
        cur = ', class: "on", "aria-current": "page"' if key == on else ""
        links.append(f'#html.elem("a", attrs: (href: {ty(href)}{cur}))[{label}]')
    return '#html.elem("div", attrs: (class: "e27-addr", role: "navigation", "aria-label": "Election 2027 pages"))[\n' + "\n".join(links) + "\n]"


def e27_body(post: dict, blog: Path, base: str) -> str:
    body = (blog / post["body"]).read_text(encoding="utf-8")
    return body.replace('"e27base:', '"' + base)


def e27_article(post: dict, blog: Path, base: str, heading: str) -> str:
    """The post itself: (title,) meta line, body, citation and corrections."""
    corr = post.get("corrections") or []
    corr_txt = (" · Corrections: " + "; ".join(f"{long_date(c['date'])}: {c['text']}" for c in corr)) if corr else " · No corrections."
    permalink = base + "election-2027/" + post["id"] + ".html"
    year = post["date"][:4]
    out = ['#html.elem("div", attrs: (class: "e27-article"))[']
    if heading:
        out.append(heading)
    out.append(f'#html.elem("p", attrs: (class: "e27-meta"))[#{ty(long_date(post["date"]) + " · Ben Stanley · Figures for the week of " + long_date(post["data_week"]))}]')
    out.append(e27_body(post, blog, base))
    out.append(f'#html.elem("p", attrs: (class: "e27-cite"))[Cite as: #{ty("Stanley, B. (" + year + "). " + post["title"] + ". ")}'
               f'#emph[Election 2027], Polish Political Analyses, benstanley.eu, #{ty(long_date(post["date"]) + ".")} · '
               f'#link({ty(permalink)})[Permanent link]#{ty(corr_txt)}]')
    out.append("]")
    return "\n".join(out)


def e27_cards(posts: list[dict], current: dict | None, base: str) -> str:
    """The side column: the post's tracker links, the other updates, and how to follow."""
    out = ['#html.elem("div", attrs: (class: "e27-aside"))[']
    if current and current.get("scenarios"):
        out.append('#html.elem("div", attrs: (class: "e27-card"))[#html.elem("p", attrs: (class: "label"))[Scenarios in this update]')
        for sc in current["scenarios"]:
            out.append(f'#html.elem("p", attrs: (class: "e27-cardlink"))[#link({ty(base + sc["href"])})[#{ty(sc["text"])} →]]')
        out.append(f'#html.elem("p", attrs: (class: "e27-help"))[Each opens the Polling tracker at the week of {long_date(current["data_week"])}, so the figures match this update.]')
        out.append("]")
    others = [p for p in posts if not current or p["id"] != current["id"]]
    newest = current is None or (bool(posts) and current["id"] == posts[0]["id"])
    out.append('#html.elem("div", attrs: (class: "e27-card"))[#html.elem("p", attrs: (class: "label"))[' + ("Earlier updates" if newest else "Other updates") + "]")
    if others:
        for p in others:
            out.append(f'#html.elem("p", attrs: (class: "e27-earlier"))[#html.elem("span", attrs: (class: "e27-date"))[{long_date(p["date"])}] '
                       f'#link({ty(base + "election-2027/" + p["id"] + ".html")})[#{ty(p["title"])}] '
                       f'#html.elem("span", attrs: (class: "e27-summary"))[#{ty(p["summary"])}]]')
    elif current:
        out.append('#html.elem("p", attrs: (class: "e27-help"))[This is the first update. Later ones are listed here, newest first.]')
    else:
        out.append('#html.elem("p", attrs: (class: "e27-help"))[Updates are listed here, newest first, each with its date, title and summary.]')
    out.append("]")
    out.append('#html.elem("div", attrs: (class: "e27-card"))[#html.elem("p", attrs: (class: "label"))[Follow]')
    out.append(f'#html.elem("p", attrs: (class: "e27-cardlink"))[#link({ty(base + "../../assets/election-2027/feed.xml")})[Feed of updates (Atom) →]]')
    out.append(f'#html.elem("p", attrs: (class: "e27-cardlink"))[#link({ty(base + "election-2027/organisation.html")})[Timing and organisation →]]')
    out.append(f'#html.elem("p", attrs: (class: "e27-cardlink"))[#link({ty(base + "polling-tracker.html")})[Polling tracker →]]')
    out.append("]")
    out.append("]")
    return "\n".join(out)


E27_FOOT = ('#html.elem("div", attrs: (class: "e27-foot"))[{links}'
            '#html.elem("span", attrs: (class: "e27-credit"))[Ben Stanley, 2026. #emph[Election 2027], Polish Political Analyses, benstanley.eu.]]')


def e27_main_page(posts: list[dict], blog: Path, facts: dict | None) -> str:
    title = "Poland's 2027 parliamentary election"
    standfirst = (blog / "page.typ").read_text(encoding="utf-8").replace('"e27base:', '"') if (blog / "page.typ").is_file() else ""
    out = [E27_GENERATED,
           f"#set document(title: [{title}])",
           f'#metadata((title: {ty(title)}, description: "Updates on the 2027 Sejm election: what the polls say, what they would mean for seats and government, and when and how the election is held")) <website-metadata>',
           "",
           '#html.elem("div", attrs: (class: "e27"))[',
           '#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]',
           '#html.elem("p", attrs: (class: "eyebrow"))[Election 2027]',
           f"#heading(level: 1)[{title}]",
           '#html.elem("div", attrs: (class: "e27-lead"))[', standfirst, "]"]
    if facts:
        sundays, order = election_window(facts)
        # The law sets a window of days free from work (Constitution Art. 98(2)); every Sejm election since 1991 was on a Sunday
        start = date.fromisoformat(facts["termStart"]); end = start.replace(year=start.year + 4)
        first, last = end - timedelta(days=facts["rules"]["windowDaysBefore"]), end - timedelta(days=1)
        suns = ", ".join(long_date(d).rsplit(" ", 1)[0] for d in sundays[:-1]) + " or " + long_date(sundays[-1]).rsplit(" ", 1)[0]
        out.append(f'#html.elem("p", attrs: (class: "e27-when"))[Election day: a day free from work between {long_date(first).rsplit(" ", 1)[0]} and '
                   f'{long_date(last)}, by custom a Sunday ({suns}) · the President must order the election by {long_date(order)}]')
    out.append(e27_nav("", "updates"))
    out.append('#html.elem("iframe", attrs: (src: "../../assets/election-2027/index.html", class: "e27-live", '
               'title: "Where the polls stand: the Polling tracker\'s latest week", loading: "lazy"))[]')
    out.append('#html.elem("div", attrs: (class: "e27-post"))[')
    if posts:
        latest = posts[0]
        heading = ('#html.elem("p", attrs: (class: "label"))[Latest update]\n'
                   f'#html.elem("h2", attrs: (class: "e27-post-title"))[#link({ty("election-2027/" + latest["id"] + ".html")})[#{ty(latest["title"])}]]')
        out.append(e27_article(latest, blog, "", heading))
        out.append(e27_cards(posts, latest, ""))
    else:
        out.append('#html.elem("div", attrs: (class: "e27-article"))[#html.elem("p", attrs: (class: "label"))[Updates]\n'
                   '#html.elem("p", attrs: (class: "e27-first"))[The first update is coming soon: an introduction to the term so far, the polls since 2023, '
                   'the seat arithmetic and what to watch. Updates will follow monthly, then more often as the election approaches.]]')
        out.append(e27_cards(posts, None, ""))
    out.append("]")
    out.append(E27_FOOT.format(links='#link("election-2027/organisation.html")[Timing and organisation] '
                                     '#link("polling-tracker.html")[Polling tracker] '))
    out.append("]")
    return "\n".join(out) + "\n"


def e27_post_page(post: dict, posts: list[dict], blog: Path) -> str:
    out = [E27_GENERATED,
           f"#set document(title: [#{ty(post['title'])}])",
           f'#metadata((title: {ty(post["title"])}, description: {ty(post["summary"])})) <website-metadata>',
           "",
           '#html.elem("div", attrs: (class: "e27"))[',
           '#html.elem("p", attrs: (class: "teaching-back"))[#link("../election-2027.html", "← Election 2027")]',
           '#html.elem("p", attrs: (class: "eyebrow"))[Election 2027]',
           f"#heading(level: 1)[#{ty(post['title'])}]",
           e27_nav("../", "updates"),
           '#html.elem("div", attrs: (class: "e27-post"))[',
           e27_article(post, blog, "../", ""),
           e27_cards(posts, post, "../"),
           "]",
           E27_FOOT.format(links='#link("../election-2027.html")[All updates] #link("organisation.html")[Timing and organisation] '
                                 '#link("../polling-tracker.html")[Polling tracker] '),
           "]"]
    return "\n".join(out) + "\n"


def sync_election_2027() -> None:
    blog = PPA_SRC / E27_WEB / "blog"
    # The Organisation page's facts
    facts = None
    try:
        facts = json.loads((PPA_SRC / E27_WEB / "election2027.json").read_text(encoding="utf-8")).get("facts")
    except (OSError, ValueError) as err:
        print(f"  warning: could not read Election 2027/web/election2027.json ({err}); keeping the committed facts.json", file=sys.stderr)
    if isinstance(facts, dict) and {"termStart", "rules", "rows"} <= set(facts):
        write_if_changed(E27_ASSETS / "facts.json", json.dumps(facts, ensure_ascii=False, separators=(",", ":")).encode("utf-8"),
                         "assets/election-2027/facts.json")
    else:
        if facts is not None:
            print("  warning: election2027.json's facts lack termStart, rules or rows; keeping the committed facts.json", file=sys.stderr)
        try:
            facts = json.loads((E27_ASSETS / "facts.json").read_text(encoding="utf-8"))
        except (OSError, ValueError):
            facts = None
    # The posts
    try:
        index = json.loads((blog / "posts.json").read_text(encoding="utf-8"))
        posts = index["posts"]
        assert isinstance(index.get("meta"), dict) and isinstance(posts, list)
        for p in posts:
            assert {"id", "date", "title", "summary", "data_week", "body"} <= set(p) and E27_POST_RE.match(p["id"] + ".typ")
            assert (blog / p["body"]).is_file(), f"{p['body']} missing"
    except (OSError, ValueError, KeyError, AssertionError) as err:
        print(f"  warning: Election 2027 blog not readable ({err!r}); keeping the committed pages", file=sys.stderr)
        return
    write_if_changed(E27_ASSETS / "posts.json", (blog / "posts.json").read_bytes(), "assets/election-2027/posts.json")
    if (blog / "feed.xml").is_file():
        write_if_changed(E27_ASSETS / "feed.xml", (blog / "feed.xml").read_bytes(), "assets/election-2027/feed.xml")
    # Figures: exactly those of the published posts
    wanted = {}
    for p in posts:
        for f in p.get("figures", []):
            for key in ("png", "csv"):
                rel = f[key]
                if (blog / rel).is_file():
                    wanted[rel] = blog / rel
                else:
                    print(f"  warning: {rel} not found in Election 2027/web/blog", file=sys.stderr)
    for rel, src in wanted.items():
        write_if_changed(E27_ASSETS / rel, src.read_bytes(), "assets/election-2027/" + rel)
    figdir = E27_ASSETS / "figures"
    if figdir.is_dir():
        for f in sorted(figdir.rglob("*"), reverse=True):
            rel = f.relative_to(E27_ASSETS).as_posix()
            if f.is_file() and rel not in wanted:
                f.unlink()
                print(f"  assets/election-2027/{rel}: removed")
            elif f.is_dir() and not any(f.iterdir()):
                f.rmdir()
        if not any(figdir.iterdir()):
            figdir.rmdir()
    # The pages
    post_dir = E27_PAGES / "election-2027"
    post_dir.mkdir(parents=True, exist_ok=True)
    keep = set()
    for p in posts:
        name = p["id"] + ".typ"
        keep.add(name)
        write_if_changed(post_dir / name, e27_post_page(p, posts, blog).encode("utf-8"),
                         f"pages/polish-political-analyses/election-2027/{name}")
    for f in post_dir.iterdir():
        if E27_POST_RE.match(f.name) and f.name not in keep:
            f.unlink()
            print(f"  pages/polish-political-analyses/election-2027/{f.name}: removed (no longer published)")
    write_if_changed(E27_PAGES / "election-2027.typ", e27_main_page(posts, blog, facts).encode("utf-8"),
                     "pages/polish-political-analyses/election-2027.typ")


def main() -> int:
    for rel_src, rel_dest, required in FILES:
        sync(rel_src, rel_dest, required)
    for rel_index, field, key, required in LISTED:
        sync_listed(rel_index, field, key, required)
    for rel_index, field, rules in LISTED_NAMES:
        sync_named(rel_index, field, rules)
    sync_events("Election 2027/web/events.json", "assets/polling-tracker/events.json")
    sync_script("Election 2027/web/engine.js", "assets/polling-tracker/engine.js", "E27Engine")
    sync_election_2027()
    return 0


def sync_script(rel_src: str, rel_dest: str, marker: str) -> None:
    """A script the site loads, copied as it is when it still defines `marker` (else the committed copy stays)."""
    source, dest = PPA_SRC / rel_src, SITE_DIR / rel_dest
    if not source.is_file():
        print(f"  warning: {source} not found; keeping the committed {dest.name}", file=sys.stderr)
        return
    text = source.read_text(encoding="utf-8")
    if marker not in text:
        print(f"  warning: {source.name} no longer defines {marker}; keeping the committed {dest.name}", file=sys.stderr)
        return
    write_if_changed(dest, text.encode("utf-8"), rel_dest)


if __name__ == "__main__":
    sys.exit(main())
