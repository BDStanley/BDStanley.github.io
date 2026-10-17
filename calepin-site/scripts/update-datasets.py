#!/usr/bin/env python3
"""Keep the Datasets section of cv/cv.yml in step with Ben's Harvard Dataverse
collection (https://dataverse.harvard.edu/dataverse/bdstanley).

What it writes:

  * One entry per published dataset in the collection, in cv.yml's style:
    "Stanley, Ben. 2026. 'Title'. doi:10.7910/DVN/XXXXXX." The authors and the
    year (first publication) come from the citation Dataverse gives for the
    dataset; English titles get the CV's headline capitalisation, as
    update-publications.py gives articles.
  * Newest year first and, within a year, the most recently deposited first.

What it changes afterwards:

  * Dataverse leads. Each run rewrites the entries for the collection's
    datasets, so a dataset renamed on Dataverse is renamed in the CV, and one
    that leaves the collection leaves the CV. Fix a title on Dataverse rather
    than in cv.yml, or the next build puts the Dataverse version back.
  * Entries for datasets outside the collection (any other DOI, or none) are
    left exactly as they are.
  * Deleting a dataset's entry from cv.yml moves its DOI to "ignore" in
    data/publications-sync.json, so it is not added again; it can tell because
    it records the DOIs it wrote under "datasets". To keep out a dataset it has
    not added yet, add its DOI to "ignore" by hand.
  * When quarto-site/cv/cv.yml is identical to cv.yml, the same changes are
    written there too, so the Quarto backup stays in step.

Run by build.sh after update-publications.py, before the CV PDF is compiled.
Network failures are non-fatal: cv.yml is then left as it is. Standard library
only. Usage:  python3 scripts/update-datasets.py [--dry-run]
"""

import importlib.util
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))

# Title casing, Typst escaping, author lists and the cv.yml reading and writing
# come from update-publications.py, so datasets are written exactly as articles
# are. No .pyc: scripts/__pycache__ is not git-ignored.
sys.dont_write_bytecode = True
_spec = importlib.util.spec_from_file_location("update_publications", os.path.join(HERE, "update-publications.py"))
pubs = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(pubs)

SEARCH = ("https://dataverse.harvard.edu/api/search?q=*&type=dataset&subtree=bdstanley"
          "&per_page=100&start=%d")
# Dataverse's citation: 'Stanley, Ben; Żerkowska-Balas, Marta, 2025, "Title", https://doi.org/...'
CITATION = re.compile(r'^(.*?), ((?:19|20)\d\d), "')
SECTION = "datasets"


def entry(authors, year, title, doi):
    people = []
    for person in authors.split("; "):
        family, _, given = person.partition(", ")
        people.append({"family": family, "given": given} if given else {"name": person})
    authors = pubs.typst(pubs.author_list(people))
    if not authors.endswith("."):
        authors += "."
    title = pubs.text(title)
    if pubs.is_english({}, title):
        title = pubs.title_case(title)
    stop = "" if title.endswith(("?", "!")) else "."
    return "%s %d. '%s'%s doi:%s." % (authors, year, pubs.typst(title), stop, doi)


def collection():
    """[(doi as Dataverse writes it, entry, year)] for the published datasets,
    in the order they go in the CV."""
    found, total = [], 1
    while len(found) < total:
        data = pubs.fetch_json(SEARCH % len(found))["data"]
        total, items = data["total_count"], data["items"]
        if not items:
            break
        for it in items:
            doi = (it.get("global_id") or "").partition("doi:")[2]
            m = CITATION.match(it.get("citation") or "")
            if not (doi and m):
                raise ValueError("unexpected record for %s" % (it.get("global_id") or it.get("name")))
            year = int(m.group(2))
            found.append((doi, entry(m.group(1), year, it["name"], doi), year, it.get("createdAt") or ""))
    found.sort(key=lambda d: (d[2], d[3]), reverse=True)
    return [d[:3] for d in found]


def main():
    dry = "--dry-run" in sys.argv[1:]
    path = pubs.DEFAULT_YML
    with open(path, encoding="utf-8") as fh:
        original = fh.read()
    lines = original.split("\n")
    state = {}
    if os.path.exists(pubs.STATE):
        with open(pubs.STATE, encoding="utf-8") as fh:
            state = json.load(fh)
    state_before = json.dumps(state, sort_keys=True)
    ignore = state.setdefault("ignore", {})
    written = set(state.get("datasets", []))

    try:
        datasets = collection()
    except Exception as e:
        print("[datasets] warning: could not read the Harvard Dataverse collection (%s);"
              " cv.yml unchanged." % e)
        return 0
    if not datasets:
        print("[datasets] warning: Dataverse lists no datasets in the collection; cv.yml unchanged.")
        return 0

    sec = pubs.parse(lines).get(SECTION)
    if not sec or sec["entries_at"] is None:
        print("[datasets] needs attention: cv.yml has no '%s' section with entries." % SECTION)
        return 0
    items = sec["items"]
    if items and any(lines[i].strip() and not pubs.ITEM.match(lines[i])
                     for i in range(items[0][0], items[-1][0] + 1)):
        print("[datasets] needs attention: the Datasets section has lines other than entries"
              " between its entries; cv.yml unchanged.")
        return 0

    log = []
    in_dataverse = {doi.lower() for doi, _, _ in datasets}
    in_cv = set().union(*(pubs.dois_in(t) for _, t in items)) if items else set()
    for doi in sorted((written & in_dataverse) - in_cv):
        if doi not in ignore:
            ignore[doi] = "Added by update-datasets.py, then removed from cv.yml by hand."
            log.append("[datasets] %s was deleted from cv.yml, so it is now ignored." % doi)

    # Entries for the collection's datasets are rebuilt; any other entry is kept.
    ours = (in_dataverse | written) - set(ignore)
    old = {d: t for _, t in items for d in pubs.dois_in(t) if d in ours}
    kept = [(t, pubs.entry_year(t)) for _, t in items if not (pubs.dois_in(t) & ours)]
    fresh = [(e, y) for doi, e, y in datasets if doi.lower() in ours]
    for doi, e, _ in datasets:
        d = doi.lower()
        if d not in ours:
            continue
        if d not in old:
            log.append("[datasets] + %s" % e)
        elif old[d] != e:
            log.append("[datasets] ~ %s\n             (was: %s)" % (e, old[d]))
    for d, t in old.items():
        if d not in in_dataverse:
            log.append("[datasets] - %s  (no longer in the Dataverse collection)" % t)

    new_items = [t for t, _ in sorted(fresh + kept, key=lambda e: -e[1])]   # stable: fresh first
    indent = (pubs.ITEM.match(lines[items[0][0]]).group(1) if items
              else re.match(r"^(\s*)", lines[sec["entries_at"]]).group(1) + "  ")
    block = []
    for t in new_items:
        block += ['%s- "%s"' % (indent, pubs.quote(t)), ""]
    block = block[:-1]
    if items:
        lines[items[0][0]:items[-1][0] + 1] = block
    else:
        lines[sec["entries_at"] + 1:sec["entries_at"] + 1] = block + [""]
    state["datasets"] = sorted(d for d in in_dataverse if d in ours)

    content = "\n".join(lines)
    for line in log:
        print(line)
    if dry:
        print("[datasets] dry run: nothing written.")
    else:
        if content != original:
            pubs.write(path, content)
            if os.path.exists(pubs.MIRROR):
                with open(pubs.MIRROR, encoding="utf-8") as fh:
                    in_step = fh.read() == original
                if in_step:
                    pubs.write(pubs.MIRROR, content)
                    print("[datasets] mirrored the changes to quarto-site/cv/cv.yml.")
                else:
                    print("[datasets] needs attention: quarto-site/cv/cv.yml had already diverged"
                          " from cv/cv.yml, so the changes above were not copied to it.")
        if json.dumps(state, sort_keys=True) != state_before:
            with open(pubs.STATE, "w", encoding="utf-8") as fh:
                json.dump(state, fh, indent=2, ensure_ascii=False)
                fh.write("\n")
    print("[datasets] %d dataset(s) from Harvard Dataverse in the CV%s."
          % (len(fresh), "" if content != original or dry else "; no changes"))
    return 0


if __name__ == "__main__":
    sys.exit(main())
