#!/usr/bin/env python3
"""Refresh data/policy-reach.json, the numbers behind the Policy reach page
(pages/policy-reach.typ): the policy organisations that take up Ben's work,
where they are based and when. Two sources:

  * Sage Policy Profiles: Ben's public profile, built on Overton's index of
    policy documents. These are the reports, briefs, blog posts and official
    papers that cite one of Ben's works, each with the organisation that
    published it, its type (think tank, government, international
    organisation) and its country.
  * OpenAlex: works citing Ben's that were written at a think tank,
    foundation, government body or international organisation, judged by the
    institutions their authors list (types "nonprofit", "government" and
    "other"). Research bodies filed under those same types, such as academies
    of sciences, national research councils, CNRS and university presses, are
    left out by name (EXCLUDE_NAMES) or by OpenAlex ID (EXCLUDE_IDS). Add to
    either to drop an organisation.

What is counted:

  * Ben's works: those OpenAlex links to his ORCID iD. Sage's profile also
    lists a few works by other people, and citations of those are ignored.
  * Policy documents: Sage's, less issues of academic journals that Overton
    files as policy documents (because a government site hosts them;
    JOURNAL_ISSUE), copies of Ben's own works (a think tank's repository
    copy of a book he co-wrote) and second copies of the same document (same
    organisation, title and date). Overton's misspelt names are corrected
    (NAME_FIXES) and its garbled characters repaired.
  * Research works: not Ben's own, even where his co-authors are at a policy
    organisation.
  * A document or work counts once, however many of Ben's works it cites; in
    the list by publication it appears under each of them.
  * Organisations: the publisher of each policy document, and each policy
    organisation the authors of each work list. One that appears in both
    sources counts once: names are matched within a country, allowing for
    small spelling differences. Its type is Sage's where Sage has it, since
    Overton classifies organisations by what they do, not their legal form.
  * Countries: the organisations' countries. EU institutions and
    international organisations have none; they are rows of their own in the
    country chart, but not on the map.
  * Years: the year each document or work was published, under the type of
    its first organisation.
  * Key ideas (KEY_IDEAS): the works behind them are listed first, by idea.

The page only lays out what is here: every count, colour, bar length and hover
text is worked out in this script, so the Calepin page and its Quarto twin
(quarto-site/R/policy-helpers.R) show the same thing.

If a source can't be read, or now finds more than a quarter fewer records than
last time, that source's records from the last run are kept (they are stored
in the file under "records"). The file is copied to quarto-site/data/.

Run by build.sh. Network failures are non-fatal: the build then uses the
committed file. Standard library only.
Usage:  python3 scripts/update-policy-reach.py [--dry-run]
"""

import datetime
import difflib
import importlib.util
import json
import math
import os
import re
import ssl
import sys
import urllib.error
import urllib.request
from collections import Counter, defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))

# CV titles and years come from cv/cv.yml through update-publications.py, as
# in update-datasets.py. No .pyc: scripts/__pycache__ is not git-ignored.
sys.dont_write_bytecode = True
_spec = importlib.util.spec_from_file_location("update_publications", os.path.join(HERE, "update-publications.py"))
pubs = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(pubs)

ORCID = "0000-0001-6932-1046"
API = "https://api.openalex.org"
SAGE_URL = "https://policyprofiles.sagepub.com/spa/public/citation-data/for-user/21087"
UA = "benstanley.eu policy reach (https://benstanley.eu)"
MAX_FALL = 0.25
CHUNK = 25            # work IDs per "cites:" filter, keeping URLs short

DATA = os.path.abspath(os.path.join(HERE, "..", "data", "policy-reach.json"))
MIRROR = os.path.abspath(os.path.join(HERE, "..", "..", "quarto-site", "data", "policy-reach.json"))
MAP = os.path.abspath(os.path.join(HERE, "..", "data", "world-map.json"))

# The ideas to list first, each with the work that sets it out (by DOI).
KEY_IDEAS = [
    ("The thin ideology of populism", "10.1080/13569310701822289"),
    ("Forging, bending and breaking", "10.1017/s1537592721001924"),
    ("The post-illiberal trilemma", "10.1353/jod.2025.a964585"),
]

# OpenAlex institutions of the policy types that are research bodies, not
# policy organisations. By name, then one by one where the name doesn't say.
EXCLUDE_NAMES = re.compile(
    r"academ(y|ie|ia)\b|recherche|investigaci|research (council|network|agency|foundation)|"
    r"innovation agency|max planck|helmholtz|fraunhofer|leibniz|\bpress\b|universit|"
    r"facultad|faculty|amparo|technology centre|cultural heritage", re.I)
EXCLUDE_IDS = {
    "I4210149266": "Institute of Political Science, Polish Academy of Sciences",
    "I4405267083": "Albert Hirschman Centre on Democracy, Geneva Graduate Institute",
}
# OpenAlex files these as government bodies; Sage files EU bodies as
# government with "EU" for a country, and other intergovernmental bodies as
# international organisations.
EU_NAMES = re.compile(r"European (Commission|Parliament|Union|Council)|Joint Research Centre|"
                      r"Council of the European Union", re.I)
IGO_NAMES = re.compile(r"United Nations|International Labour|World Bank|International Monetary|"
                       r"\bOECD\b|Organisation for Economic|Council of Europe|\bNATO\b|\bOSCE\b|"
                       r"World Health Organization|UNESCO|UNDP", re.I)
# Sage documents that are issues of an academic journal ("Spanish Journal of
# Political Science v. 52"), and organisation names Overton misspells.
JOURNAL_ISSUE = re.compile(r"\bjournal\b.*\b(v|vol)\.\s*\d+", re.I)
NAME_FIXES = {"Bertelsmann Stifung": "Bertelsmann Stiftung",
              "Norweigan Institute of International Affairs": "Norwegian Institute of International Affairs"}
# Sage's country names that are not the map's.
COUNTRY_ALIASES = {"USA": "US", "UK": "GB", "Czech Republic": "CZ", "South Korea": "KR",
                   "Republic of Korea": "KR", "Russian Federation": "RU", "Türkiye": "TR",
                   "Turkey": "TR", "Taiwan, Province of China": "TW", "Viet Nam": "VN"}
NO_COUNTRY = {"EU": "EU institutions", "IGO": "International organisations"}

# Colours. Organisation types take the first three slots of the dataviz
# reference palette, in order, as the subfields do on the Citation reach page
# (validated for colour-blind separation as neighbours); "other" is grey.
# Countries are binned on the same red ramp as that page's map.
TYPES = [("think_tank", "Think tanks and foundations", "#2a78d6"),
         ("government", "Government", "#eb6834"),
         ("international", "International organisations", "#1baf7a"),
         ("other", "Other", "#a9a79f")]
TYPE_LABEL = {k: label for k, label, _ in TYPES}
TYPE_COLOUR = {k: c for k, _, c in TYPES}
TYPE_SINGULAR = {"think_tank": "think tank", "government": "government",
                 "international": "international organisation", "other": "other"}
RAMP = ["#eda496", "#e38070", "#d55d4b", "#c13d30", "#a72219", "#8b0000"]
NO_RECORDS = "#e1e0d9"
BINS = [1, 2, 5, 10, 20, 50]   # lower bounds of the six country classes
TOP_ORGS = 15
MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"]   # English, whatever the locale
KINDS = {"article": "article", "book-chapter": "book chapter", "book": "book", "report": "report",
         "preprint": "preprint", "dissertation": "thesis", "review": "review", "editorial": "editorial"}


def fetch_json(url, ua=UA):
    req = urllib.request.Request(url, headers={"User-Agent": ua, "Accept": "application/json"})
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


def cv_works():
    """{doi: (title, year)} for the CV's entries, to name Ben's works as the CV does."""
    with open(pubs.DEFAULT_YML, encoding="utf-8") as fh:
        lines = fh.read().split("\n")
    out = {}
    for sec in pubs.parse(lines).values():
        for _, t in sec["items"]:
            title = pubs.entry_title(t)
            for d in pubs.dois_in(t):
                if title:
                    out[d] = (pubs.unquote(title).replace("\\", ""), pubs.entry_year(t))
    return out


def my_works():
    """{OpenAlex ID: {"key", "title", "year"}}, keyed in the file by DOI where there is one."""
    cv = cv_works()
    works = {}
    for w in all_pages("%s/works?filter=author.orcid:%s&select=id,doi,title,publication_year" % (API, ORCID)):
        doi = pubs.clean_doi(w.get("doi"))
        title, year = cv.get(doi, (w.get("title") or "", w.get("publication_year")))
        works[w["id"]] = {"key": doi or w["id"].rsplit("/", 1)[1], "title": title, "year": year or w.get("publication_year")}
    return works


def country_code(name, names):
    if name in NO_COUNTRY:
        return name
    if name in COUNTRY_ALIASES:
        return COUNTRY_ALIASES[name]
    for code, n in names.items():
        if n.lower() == (name or "").lower():
            return code
    return None


def unmangle(s):
    """Repair UTF-8 read as Latin-1, as in Overton's "Europeâs"."""
    if re.search("[âÃÅ][\x80-\xbf]", s or ""):
        try:
            return s.encode("latin-1").decode("utf-8")
        except UnicodeError:
            pass
    return s or ""


def sage_records(mine, names, notes):
    """(records, number of documents Sage lists)."""
    data = fetch_json(SAGE_URL)["data"]
    keys = {w["key"] for w in mine.values()}
    docs = data["documents"]
    found, copies = {}, {}
    for doi, rows in data["citations"].items():
        doi = doi.lower()
        if doi not in keys:
            continue
        for r in rows:
            m = docs.get(r["policy_document_id"]) or {}
            src = m.get("source") or {}
            title = unmangle(m.get("translated_title") or m.get("title") or "")
            if JOURNAL_ISSUE.search(title) or any(pubs.same_title(title, w["title"]) for w in mine.values()):
                continue                       # a journal issue, or a copy of one of Ben's own works
            org = NAME_FIXES.get(src.get("title"), src.get("title") or "?")
            # A second copy of a document (same organisation, title and date) is the first one.
            pid = copies.setdefault((norm(org), norm(title), m.get("published_on")), r["policy_document_id"])
            kind = {"think tank": "think_tank", "government": "government", "igo": "international"}.get(src.get("type"), "other")
            code = country_code(src.get("country"), names)
            if code is None:
                notes.append("[policy-reach] needs attention: Sage gives a country the map doesn't know, %r;"
                             " add it to COUNTRY_ALIASES." % src.get("country"))
            date = (m.get("published_on") or "")[:10]
            rec = found.setdefault(pid, {
                "id": "sage:" + pid, "source": "sage", "title": title,
                "url": m.get("document_url") or r.get("pdf_url") or "",
                "date": date, "year": int(date[:4]) if date[:4].isdigit() else None,
                "kind": (m.get("overton_policy_document_series") or "").strip().lower(),
                "orgs": [{"name": org, "type": kind, "country": code, "from": "sage"}],
                "cites": []})
            if doi not in rec["cites"]:
                rec["cites"].append(doi)
    return list(found.values()), data["metadata"]["total_document_results"]


def openalex_records(mine, me):
    ids = sorted(i.rsplit("/", 1)[1] for i in mine)
    select = "id,doi,title,publication_year,type,authorships,referenced_works"
    found = {}
    for i in range(0, len(ids), CHUNK):
        url = "%s/works?filter=cites:%s,authorships.institutions.type:government|nonprofit|other&select=%s" % (
            API, "|".join(ids[i:i + CHUNK]), select)
        for w in all_pages(url):
            found[w["id"]] = w
    out = []
    for w in found.values():
        authorships = w.get("authorships") or []
        if any((a.get("author") or {}).get("id") == me for a in authorships):
            continue                           # Ben's own, with co-authors at a policy organisation
        orgs, seen = [], set()
        for a in authorships:
            for inst in a.get("institutions") or []:
                iid = (inst.get("id") or "").rsplit("/", 1)[-1]
                name = inst.get("display_name") or ""
                if (inst.get("type") not in ("government", "nonprofit", "other") or iid in seen
                        or iid in EXCLUDE_IDS or EXCLUDE_NAMES.search(name)):
                    continue
                seen.add(iid)
                if EU_NAMES.search(name):
                    kind, code = "government", "EU"
                elif IGO_NAMES.search(name):
                    kind, code = "international", "IGO"
                else:
                    kind = "government" if inst["type"] == "government" else "think_tank"
                    code = inst.get("country_code")
                orgs.append({"name": name, "type": kind, "country": code, "from": "openalex"})
        if not orgs:
            continue
        doi = pubs.clean_doi(w.get("doi"))
        out.append({
            "id": "openalex:" + w["id"].rsplit("/", 1)[1], "source": "openalex",
            "title": re.sub(r"<[^>]+>", "", w.get("title") or ""),
            "url": "https://doi.org/" + doi if doi else w["id"],
            "date": "", "year": w.get("publication_year"),
            "kind": KINDS.get(w.get("type"), w.get("type") or ""),
            "orgs": orgs,
            "cites": sorted({mine[r]["key"] for r in w.get("referenced_works") or [] if r in mine})})
    return out


def norm(name):
    s = re.sub(r"\b(the|e\.?\s?v\.?|inc|ltd)\b", " ", (name or "").lower())
    return " ".join(re.findall(r"[a-z0-9]+", s))


def organisations(records):
    """Merge the organisations of all records: [org], with each record's orgs
    replaced by indices into that list. Same country and nearly the same name
    is the same organisation."""
    orgs = []
    for rec in records:
        refs = []
        for o in rec["orgs"]:
            key = norm(o["name"])
            match = next((i for i, x in enumerate(orgs) if x["country"] == o["country"]
                          and difflib.SequenceMatcher(None, x["key"], key).ratio() >= 0.9), None)
            if match is None:
                orgs.append({"key": key, "name": o["name"], "type": o["type"], "country": o["country"],
                             "sources": {o["from"]}, "documents": set(), "works": set()})
                match = len(orgs) - 1
            x = orgs[match]
            x["sources"].add(o["from"])
            if o["from"] == "openalex":
                x["name"] = o["name"]          # OpenAlex's spelling: Overton's has slips
            else:
                x["type"] = o["type"]          # Overton's type: by function, not legal form
            (x["documents"] if rec["source"] == "sage" else x["works"]).add(rec["id"])
            if match not in refs:
                refs.append(match)
        rec["_orgs"] = refs
    return orgs


def analyse(records, mine, names, as_of):
    this_year = datetime.date.today().year
    orgs = organisations(records)
    docs = [r for r in records if r["source"] == "sage"]
    works = [r for r in records if r["source"] == "openalex"]

    def country_name(code):
        return NO_COUNTRY.get(code) or names.get(code, code or "Unknown")

    # Organisations, most documents and works first, coloured by type.
    org_rows = []
    for x in sorted(orgs, key=lambda x: (-(len(x["documents"]) + len(x["works"])), x["name"])):
        nd, nw = len(x["documents"]), len(x["works"])
        bits = [plural(nd, "policy document")] if nd else []
        bits += [plural(nw, "research work")] if nw else []
        org_rows.append({"name": x["name"], "type": x["type"], "type_label": TYPE_SINGULAR[x["type"]],
                         "country": country_name(x["country"]), "documents": nd, "works": nw,
                         "total": nd + nw, "colour": TYPE_COLOUR[x["type"]],
                         "tip": "%s (%s, %s): %s" % (x["name"], TYPE_SINGULAR[x["type"]],
                                                     country_name(x["country"]), " and ".join(bits))})
    top = org_rows[0]["total"] if org_rows else 1
    for r in org_rows[:TOP_ORGS]:
        r["w"] = round(r["total"] / top, 4)

    # Types: organisations and records of each.
    type_rows = []
    for k, label, colour in TYPES:
        n_org = sum(1 for x in orgs if x["type"] == k)
        n_rec = sum(1 for r in records if orgs[r["_orgs"][0]]["type"] == k)
        if n_org:
            type_rows.append({"key": k, "name": label, "colour": colour, "organisations": n_org, "records": n_rec,
                              "tip": "%s: %s, behind %s" % (label, plural(n_org, "organisation"),
                                                            plural(n_rec, "document or work", "documents and works"))})
    t_top = max((r["organisations"] for r in type_rows), default=1)
    for r in type_rows:
        r["w"] = round(r["organisations"] / t_top, 4)

    # Countries: documents and works with an organisation there (each once),
    # and the organisations. EU and international bodies get rows, no map.
    recs_by_c, orgs_by_c = defaultdict(set), defaultdict(set)
    for r in records:
        for i in r["_orgs"]:
            recs_by_c[orgs[i]["country"]].add(r["id"])
            orgs_by_c[orgs[i]["country"]].add(i)

    def colour_for(n):
        return RAMP[max(i for i, lo in enumerate(BINS) if n >= lo)]

    country_rows = []
    for c in sorted(recs_by_c, key=lambda c: (-len(recs_by_c[c]), country_name(c))):
        n, no = len(recs_by_c[c]), len(orgs_by_c[c])
        country_rows.append({"code": c, "name": country_name(c), "records": n, "organisations": no,
                             "on_map": c not in NO_COUNTRY and c is not None, "colour": colour_for(n),
                             "tip": "%s: %s, from %s" % (country_name(c),
                                                         plural(n, "document or work", "documents and works"),
                                                         plural(no, "organisation"))})
    c_top = country_rows[0]["records"] if country_rows else 1
    for r in country_rows:
        r["w"] = round(r["records"] / c_top, 4)
    bins = []
    for i, lo in enumerate(BINS):
        hi = BINS[i + 1] - 1 if i + 1 < len(BINS) else None
        bins.append({"label": ("%d+" % lo) if hi is None else ("%d" % lo if hi == lo else "%d–%d" % (lo, hi)),
                     "colour": RAMP[i]})

    # Years: one stacked column per year, by the type of the first organisation.
    dated = [r for r in records if r.get("year")]
    first_year = min((r["year"] for r in dated), default=this_year)
    by_year = defaultdict(Counter)
    for r in dated:
        by_year[r["year"]][orgs[r["_orgs"][0]]["type"]] += 1
    years = list(range(first_year, this_year + 1))
    totals = {y: sum(by_year[y].values()) for y in years}
    peak = max(totals.values()) if totals else 1
    step = nice_step(peak)
    axis_max = step * math.ceil(max(peak, 1) / step)
    ticks = [{"value": v, "label": "{:,}".format(v), "pos": round(v / axis_max, 4)}
             for v in range(0, axis_max + 1, step)]
    used = [k for k, _, _ in TYPES if any(by_year[y].get(k) for y in years)]
    legend = [{"name": TYPE_LABEL[k], "colour": TYPE_COLOUR[k]} for k in used]
    year_rows = []
    for y in years:
        segs = [{"name": TYPE_LABEL[k], "count": by_year[y][k], "colour": TYPE_COLOUR[k],
                 "tip": "%s, %s: %s" % (y, TYPE_LABEL[k], plural(by_year[y][k], "document or work",
                                                                "documents and works"))}
                for k in used if by_year[y].get(k)]
        label = "%d (to %s)" % (y, datetime.date.today().strftime("%B")) if y == this_year else str(y)
        year_rows.append({"year": y, "label": label, "count": totals[y], "h": round(totals[y] / axis_max, 4),
                          "minor": y % 5 != 0,
                          "tip": "%s: %s" % (label, plural(totals[y], "document or work", "documents and works"))
                                 + ("" if not segs else " (" + ", ".join(
                                     "%s %d" % (s["name"], s["count"]) for s in segs) + ")"),
                          "segments": segs})

    # By publication: the key ideas first, then every other work with a record.
    by_key = {w["key"]: w for w in mine.values()}
    cited = defaultdict(list)
    for r in records:
        for k in r["cites"]:
            cited[k].append(r)

    def when(r):
        try:
            d = datetime.date.fromisoformat(r["date"])
            return "%d %s %d" % (d.day, MONTHS[d.month - 1], d.year)
        except (TypeError, ValueError):
            return str(r.get("year") or "")

    def item(r):
        os_ = [orgs[i] for i in r["_orgs"]]
        return {"date": r["date"] or str(r.get("year") or ""), "when": when(r), "title": r["title"], "url": r["url"],
                "kind": "" if r["kind"] == "publication" else r["kind"], "source": r["source"],
                "orgs": "; ".join("%s (%s, %s)" % (o["name"], TYPE_SINGULAR[o["type"]], country_name(o["country"]))
                                  for o in os_)}

    def group(key, idea=None):
        w = by_key.get(key, {"title": key, "year": None})
        rs = sorted(cited.get(key, []), key=lambda r: (r["date"] or str(r.get("year") or ""), r["title"]), reverse=True)
        nd = sum(1 for r in rs if r["source"] == "sage")
        nw = len(rs) - nd
        parts = ([plural(nd, "policy document")] if nd else []) + ([plural(nw, "research work")] if nw else [])
        return {"key": key, "idea": idea, "title": w["title"], "year": w["year"], "documents": nd, "works": nw,
                "summary": " and ".join(parts), "items": [item(r) for r in rs]}

    ideas = [group(doi, idea) for idea, doi in KEY_IDEAS]
    key_dois = {doi for _, doi in KEY_IDEAS}
    others = [group(k) for k in sorted(cited, key=lambda k: (-len(cited[k]), str(by_key.get(k, {}).get("title", k))))
              if k not in key_dois]

    countries_on_map = sum(1 for r in country_rows if r["on_map"])
    for r in records:
        r.pop("_orgs", None)
    return {
        "_about": "Data behind the Policy reach page, from Sage Policy Profiles and OpenAlex. Written by"
                  " scripts/update-policy-reach.py; see that file for what is counted and how.",
        "as_of": as_of,
        "totals": {"documents": len(docs), "works": len(works), "organisations": len(orgs),
                   "countries": countries_on_map, "first_year": first_year,
                   "cited_works": len(cited), "my_works": len(mine)},
        "types": type_rows,
        "organisations": org_rows,
        "top_organisations": TOP_ORGS,
        "countries": country_rows,
        "bins": bins,
        "no_records_colour": NO_RECORDS,
        "years": {"axis_max": axis_max, "ticks": ticks, "rows": year_rows, "legend": legend},
        "ideas": ideas,
        "other_publications": others,
        "records": sorted(records, key=lambda r: r["id"]),
    }


def main():
    dry = "--dry-run" in sys.argv[1:]
    original = open(DATA, encoding="utf-8").read() if os.path.exists(DATA) else None
    old = json.loads(original) if original else {}
    old_records = old.get("records", [])
    old_as_of = old.get("as_of", {})
    names = json.load(open(MAP, encoding="utf-8"))["names"]
    month = datetime.date.today().strftime("%Y-%m")
    notes = []
    try:
        me = fetch_json("%s/authors/orcid:%s?select=id" % (API, ORCID))["id"]
        mine = my_works()
    except Exception as e:
        print("[policy-reach] warning: could not read Ben's works from OpenAlex (%s: %s); %s."
              % (type(e).__name__, e, "keeping the previous data" if old else "no data to show yet"))
        return 0

    records, as_of = [], {}
    listed = old.get("totals", {}).get("sage_listed")
    for source, label, get in (("sage", "Sage Policy Profiles", lambda: sage_records(mine, names, notes)),
                               ("openalex", "OpenAlex", lambda: openalex_records(mine, me))):
        previous = [r for r in old_records if r["source"] == source]
        try:
            new = get()
            if source == "sage":
                new, fresh_listed = new
        except Exception as e:
            print("[policy-reach] warning: could not read %s (%s: %s); keeping its %s from last time."
                  % (label, type(e).__name__, e, plural(len(previous), "record")))
            records += previous
            as_of[source] = old_as_of.get(source, month)
            continue
        if len(new) < (1 - MAX_FALL) * len(previous):
            print("[policy-reach] needs attention: %s now gives %d records, down from %d; keeping the previous ones."
                  % (label, len(new), len(previous)))
            records += previous
            as_of[source] = old_as_of.get(source, month)
            continue
        records += new
        as_of[source] = month
        if source == "sage":
            listed = fresh_listed
    for n in sorted(set(notes)):
        print(n)
    if not records:
        print("[policy-reach] warning: no records from either source; %s."
              % ("keeping the previous data" if old else "no data to show yet"))
        return 0

    data = analyse(records, mine, names, as_of)
    t = data["totals"]
    t["sage_listed"] = listed or t["documents"]
    print("[policy-reach] %s and %s from %s in %s."
          % (plural(t["documents"], "policy document"), plural(t["works"], "research work"),
             plural(t["organisations"], "organisation"), plural(t["countries"], "country", "countries")))
    content = json.dumps(data, indent=1, ensure_ascii=False) + "\n"
    if dry:
        print("[policy-reach] dry run: nothing written.")
        return 0
    if content != original:
        with open(DATA, "w", encoding="utf-8") as fh:
            fh.write(content)
    if os.path.isdir(os.path.dirname(MIRROR)):
        mirrored = open(MIRROR, encoding="utf-8").read() if os.path.exists(MIRROR) else None
        if mirrored != content:
            with open(MIRROR, "w", encoding="utf-8") as fh:
                fh.write(content)
            print("[policy-reach] copied to quarto-site/data/policy-reach.json.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
