#!/usr/bin/env python3
"""Refresh data/reach.json, the numbers behind the Citation reach page
(pages/citation-reach.typ): where the scholars who cite Ben's work are based,
which disciplines their work belongs to, and how the citations build up year
by year. Everything comes from OpenAlex.

What is counted:

  * Ben's works: every work OpenAlex links to his ORCID iD.
  * Citations: the works that cite them. A work citing two of Ben's works
    counts twice, as in OpenAlex's own citation totals, and works by Ben
    himself are included, as they are there.
  * Disciplines: each citation is put under the field and subfield of the
    citing work's primary topic (OpenAlex's domain > field > subfield > topic
    hierarchy). Citations from works with no topic are left out of those
    charts. The top five fields have colours of their own; the rest are
    grouped as "Other".
  * Years: by the citing work's year of publication, from the year of Ben's
    first work to this year. Citations OpenAlex dates earlier are dating
    errors and are left out.
  * Countries: the scholars who wrote the citing works, other than Ben, each
    counted once per country of the institutions they list on those works.
    Citing works that list no institution have no country.

The page only lays out what is here: every count, share, bar length, colour
and hover text is worked out in this script, so the Calepin page and its
Quarto twin (quarto-site/R/reach-helpers.R) show the same thing.

The file keeps its previous contents if OpenAlex can't be reached, or if the
citations it finds have fallen by more than a quarter since the last run.
It is copied to quarto-site/data/ for the Quarto backup.

Without an API key OpenAlex serves only the first page of results by cursor,
so this pages by number; a full run takes about 15 requests, a small part of
OpenAlex's free daily allowance.

Run by build.sh. Network failures are non-fatal: the build then uses the
committed file. Standard library only.
Usage:  python3 scripts/update-reach.py [--dry-run]
"""

import datetime
import json
import math
import os
import ssl
import sys
import urllib.error
import urllib.parse
import urllib.request
from collections import Counter, defaultdict

ORCID = "0000-0001-6932-1046"
API = "https://api.openalex.org"
UA = "benstanley.eu citation reach (https://benstanley.eu)"
MAX_FALL = 0.25
CHUNK = 25            # work IDs per "cites:" filter, keeping URLs short

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.abspath(os.path.join(HERE, "..", "data", "reach.json"))
MIRROR = os.path.abspath(os.path.join(HERE, "..", "..", "quarto-site", "data", "reach.json"))
MAP = os.path.abspath(os.path.join(HERE, "..", "data", "world-map.json"))

# Colours. Fields take the first five slots of the dataviz reference palette in
# order (validated for colour-blind separation as neighbours in a stack); the
# rest share a grey. Countries are binned on a one-hue red ramp ending in the
# site's dark red, light to dark; countries with no citing scholars are grey.
FIELD_COLOURS = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4"]
OTHER_COLOUR = "#a9a79f"
UNCLASSIFIED_COLOUR = "#d3d1c9"   # citing works with no topic, in the chart by year
RAMP = ["#eda496", "#e38070", "#d55d4b", "#c13d30", "#a72219", "#8b0000"]
NO_SCHOLARS = "#e1e0d9"
BINS = [1, 5, 10, 25, 50, 100]   # lower bounds of the six country classes
TOP_SUBFIELDS = 15
TOP_COUNTRIES = 15


def fetch_json(url):
    req = urllib.request.Request(url, headers={"User-Agent": UA, "Accept": "application/json"})
    try:
        body = urllib.request.urlopen(req, timeout=60).read()
    except urllib.error.HTTPError:
        raise
    except (urllib.error.URLError, ssl.SSLError, OSError):
        # As in update-kl.py: some macOS Pythons have no CA bundle wired up.
        body = urllib.request.urlopen(req, timeout=60, context=ssl._create_unverified_context()).read()
    return json.loads(body.decode("utf-8"))


def all_pages(url):
    results, page = [], 1
    while True:
        data = fetch_json("%s&per-page=200&page=%d" % (url, page))
        results += data["results"]
        if not data["results"] or len(results) >= data["meta"]["count"]:
            return results
        page += 1


def plural(n, word, words=None):
    return "%s %s" % ("{:,}".format(n), word if n == 1 else (words or word + "s"))


def nice_step(n):
    """A round tick step (1, 2 or 5 x 10^k) giving about four steps up to n."""
    raw = max(n, 1) / 4
    k = 10 ** math.floor(math.log10(raw))
    for m in (1, 2, 5, 10):
        if raw <= m * k:
            return int(m * k) or 1


def gather():
    me = fetch_json("%s/authors/orcid:%s?select=id" % (API, ORCID))["id"]
    mine = all_pages("%s/works?filter=author.orcid:%s&select=id,publication_year" % (API, ORCID))
    ids = sorted(w["id"].rsplit("/", 1)[1] for w in mine)
    citing = {}
    select = "id,publication_year,primary_topic,authorships,referenced_works"
    for i in range(0, len(ids), CHUNK):
        url = "%s/works?filter=cites:%s&select=%s" % (API, "|".join(ids[i:i + CHUNK]), select)
        for w in all_pages(url):
            citing[w["id"]] = w
    return me, mine, list(citing.values())


def analyse(me, mine, citing, names):
    my_ids = {w["id"] for w in mine}
    first_year = min(w["publication_year"] for w in mine if w.get("publication_year"))
    this_year = datetime.date.today().year
    fields, subfields = Counter(), Counter()
    subfield_of = {}
    by_year = defaultdict(Counter)
    before, undated, no_topic, self_works, no_country = 0, 0, 0, 0, 0
    scholars, scholars_by_country, works_by_country = set(), defaultdict(set), defaultdict(set)
    total = 0
    for w in citing:
        n = len(my_ids & set(w.get("referenced_works") or [])) or 1
        total += n
        topic = w.get("primary_topic")
        field = topic["field"]["display_name"] if topic else None
        if topic:
            fields[field] += n
            sub = topic["subfield"]["display_name"]
            subfields[sub] += n
            subfield_of[sub] = field
        else:
            no_topic += n
        year = w.get("publication_year")
        if not year:
            undated += n
        elif year < first_year:
            before += n
        else:
            by_year[year][field] += n
        placed = False
        for a in w.get("authorships") or []:
            author = (a.get("author") or {}).get("id")
            if author == me:
                self_works += 1
                continue
            if author:
                scholars.add(author)
            for c in a.get("countries") or []:
                scholars_by_country[c].add(author or (w["id"], a.get("raw_author_name")))
                works_by_country[c].add(w["id"])
                placed = True
        if not placed:
            no_country += 1

    # Fields: the top five keep a colour each, in order; the rest fold into Other.
    ranked = [f for f, _ in sorted(fields.items(), key=lambda kv: (-kv[1], kv[0]))]
    named = ranked[:len(FIELD_COLOURS)]
    colour = {f: FIELD_COLOURS[i] for i, f in enumerate(named)}
    classified = sum(fields.values())
    top = max(fields.values()) if fields else 1

    def share(n, of):
        return round(100 * n / of, 1) if of else 0

    field_rows = [{"name": f, "citations": fields[f], "share": share(fields[f], classified),
                   "colour": colour.get(f, OTHER_COLOUR), "w": round(fields[f] / top, 4),
                   "tip": "%s: %s (%s%% of classified citations)"
                          % (f, plural(fields[f], "citation"), share(fields[f], classified))}
                  for f in ranked]
    other_members = ranked[len(FIELD_COLOURS):]
    legend = [{"name": f, "colour": colour[f]} for f in named]
    if other_members:
        legend.append({"name": "Other", "colour": OTHER_COLOUR})
    if no_topic:
        legend.append({"name": "Unclassified", "colour": UNCLASSIFIED_COLOUR})

    sub_ranked = sorted(subfields.items(), key=lambda kv: (-kv[1], kv[0]))
    sub_top = sub_ranked[0][1] if sub_ranked else 1
    sub_rows = [{"name": s, "field": subfield_of[s], "citations": n, "share": share(n, classified),
                 "colour": colour.get(subfield_of[s], OTHER_COLOUR), "w": round(n / sub_top, 4),
                 "tip": "%s (%s): %s" % (s, subfield_of[s], plural(n, "citation"))}
                for s, n in sub_ranked]

    # Years: one stacked column per year, the named fields bottom-up in rank
    # order, then Other, then citations whose work has no topic (Unclassified).
    years = list(range(first_year, this_year + 1))
    totals = {y: sum(by_year[y].values()) for y in years}
    peak = max(totals.values()) if totals else 1
    step = nice_step(peak)
    axis_max = step * math.ceil(peak / step)
    ticks = [{"value": v, "label": "{:,}".format(v), "pos": round(v / axis_max, 4)}
             for v in range(0, axis_max + 1, step)]
    year_rows = []
    for y in years:
        counts = by_year[y]
        stack = [(f, counts.get(f, 0), colour[f]) for f in named]
        stack.append(("Other", sum(n for f, n in counts.items() if f is not None and f not in colour), OTHER_COLOUR))
        stack.append(("Unclassified", counts.get(None, 0), UNCLASSIFIED_COLOUR))
        segments = [{"name": f, "citations": n, "colour": c,
                     "tip": "%s, %s: %s" % (y, f, plural(n, "citation"))} for f, n, c in stack if n]
        label = "%d (to %s)" % (y, datetime.date.today().strftime("%B")) if y == this_year else str(y)
        year_rows.append({"year": y, "label": label, "citations": totals[y], "h": round(totals[y] / axis_max, 4),
                          "minor": y % 5 != 0,   # unlabelled on narrow screens
                          "tip": "%s: %s" % (label, plural(totals[y], "citation"))
                                 + ("" if not segments else " (" + ", ".join(
                                     "%s %s" % (s["name"], "{:,}".format(s["citations"])) for s in segments) + ")"),
                          "segments": segments})

    # Countries, most citing scholars first, each in its class's colour.
    def colour_for(n):
        return RAMP[max(i for i, lo in enumerate(BINS) if n >= lo)]

    country_rows = []
    for c, people in sorted(scholars_by_country.items(), key=lambda kv: (-len(kv[1]), names.get(kv[0], kv[0]))):
        n, nw = len(people), len(works_by_country[c])
        name = names.get(c, c)
        country_rows.append({"code": c, "name": name, "scholars": n, "works": nw, "colour": colour_for(n),
                             "tip": "%s: %s, in %s" % (name, plural(n, "citing scholar"), plural(nw, "citing work"))})
    c_top = country_rows[0]["scholars"] if country_rows else 1
    for row in country_rows[:TOP_COUNTRIES]:
        row["w"] = round(row["scholars"] / c_top, 4)
    bins = []
    for i, lo in enumerate(BINS):
        hi = BINS[i + 1] - 1 if i + 1 < len(BINS) else None
        bins.append({"label": ("%d+" % lo) if hi is None else ("%d" % lo if hi == lo else "%d–%d" % (lo, hi)),
                     "colour": RAMP[i]})

    return {
        "_about": "Data behind the Citation reach page, from OpenAlex. Written by scripts/update-reach.py;"
                  " see that file for what is counted and how.",
        "as_of": datetime.date.today().strftime("%Y-%m"),
        "totals": {"citations": total, "citing_works": len(citing), "scholars": len(scholars),
                   "countries": len(country_rows), "my_works": len(mine), "first_year": first_year,
                   "self_citing_works": self_works, "works_without_country": no_country,
                   "citations_without_topic": no_topic, "citations_before_first_year": before,
                   "citations_undated": undated, "classified_citations": classified},
        "fields": field_rows,
        "other_fields": other_members,
        "legend": legend,
        "subfields": sub_rows,
        "top_subfields": TOP_SUBFIELDS,
        "years": {"axis_max": axis_max, "ticks": ticks, "rows": year_rows},
        "countries": country_rows,
        "top_countries": TOP_COUNTRIES,
        "bins": bins,
        "no_scholars_colour": NO_SCHOLARS,
    }


def main():
    dry = "--dry-run" in sys.argv[1:]
    original = open(DATA, encoding="utf-8").read() if os.path.exists(DATA) else None
    old = json.loads(original) if original else {}
    names = json.load(open(MAP, encoding="utf-8"))["names"]
    try:
        data = analyse(*gather(), names)
    except Exception as e:
        print("[reach] warning: could not read OpenAlex (%s: %s); %s."
              % (type(e).__name__, e, "keeping the previous data" if old else "no data to show yet"))
        return 0
    before = old.get("totals", {}).get("citations", 0)
    t = data["totals"]
    if t["citations"] < (1 - MAX_FALL) * before:
        print("[reach] needs attention: OpenAlex now finds %d citations, down from %d;"
              " keeping the previous data." % (t["citations"], before))
        return 0
    print("[reach] %s from %s by %s in %s."
          % (plural(t["citations"], "citation"), plural(t["citing_works"], "work"),
             plural(t["scholars"], "scholar"), plural(t["countries"], "country", "countries")))
    content = json.dumps(data, indent=1, ensure_ascii=False) + "\n"
    if dry:
        print("[reach] dry run: nothing written.")
        return 0
    if content != original:
        with open(DATA, "w", encoding="utf-8") as fh:
            fh.write(content)
    if os.path.isdir(os.path.dirname(MIRROR)):
        mirrored = open(MIRROR, encoding="utf-8").read() if os.path.exists(MIRROR) else None
        if mirrored != content:
            with open(MIRROR, "w", encoding="utf-8") as fh:
                fh.write(content)
            print("[reach] copied to quarto-site/data/reach.json.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
