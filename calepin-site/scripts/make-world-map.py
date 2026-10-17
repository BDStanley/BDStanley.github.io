#!/usr/bin/env python3
"""Draw the world map that the Citation reach page colours by country, as
data/world-map.json: one SVG path per country, already projected.

The shapes are Natural Earth's 1:110m Admin 0 countries (public domain),
projected with Equal Earth (an equal-area projection, so a country's share of
the map is its share of the land) and thinned to about a pixel at the map's
1000-unit width. Antarctica is left out. Countries too small for the 1:110m
set (Singapore, Malta, Hong Kong ...) get a point at their Natural Earth label
position from the 1:50m set instead, which the page draws as a dot; that set
also supplies every country's English name for the page's tables.

Not part of the build: the map is committed, so run this only to change it.
It also copies the file to quarto-site/data/ for the Quarto backup.
Standard library only.
Usage:  python3 scripts/make-world-map.py
"""

import json
import math
import os
import shutil
import ssl
import urllib.error
import urllib.request

NE = "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_%s_admin_0_countries.geojson"
HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.abspath(os.path.join(HERE, "..", "data", "world-map.json"))
MIRROR = os.path.abspath(os.path.join(HERE, "..", "..", "quarto-site", "data", "world-map.json"))
WIDTH = 1000          # map units across; the page scales the SVG to fit
MIN_STEP = 0.7        # drop a vertex closer than this to the last one kept
MIN_RING = 1.2        # drop rings (small islands) narrower and shorter than this
# Shorter or more familiar names than Natural Earth's NAME_EN.
NAMES = {"US": "United States", "CZ": "Czechia", "CN": "China", "CD": "DR Congo", "CG": "Republic of the Congo"}

# Equal Earth (Šavrič, Patterson and Jenny 2018).
A1, A2, A3, A4 = 1.340264, -0.081106, 0.000893, 0.003796
M = math.sqrt(3) / 2


def equal_earth(lon, lat):
    lam, phi = math.radians(lon), math.radians(lat)
    t = math.asin(M * math.sin(phi))
    t2, t6 = t * t, t ** 6
    x = 2 * math.sqrt(3) * lam * math.cos(t) / (3 * (A1 + 3 * A2 * t2 + t6 * (7 * A3 + 9 * A4 * t2)))
    y = t * (A1 + A2 * t2 + t6 * (A3 + A4 * t2))
    return x, y


SCALE = WIDTH / (2 * equal_earth(180, 0)[0])
TOP = equal_earth(0, 84)[1] * SCALE
BOTTOM = TOP - equal_earth(0, -57)[1] * SCALE   # screen y of the map's southern edge


def project(lon, lat):
    x, y = equal_earth(lon, lat)
    return x * SCALE + WIDTH / 2, TOP - y * SCALE


def fetch(scale):
    req = urllib.request.Request(NE % scale, headers={"User-Agent": "benstanley.eu map build"})
    try:
        body = urllib.request.urlopen(req, timeout=60).read()
    except (urllib.error.URLError, ssl.SSLError, OSError):
        # As in update-kl.py: some macOS Pythons have no CA bundle wired up.
        body = urllib.request.urlopen(req, timeout=60, context=ssl._create_unverified_context()).read()
    return json.loads(body)["features"]


def code(props):
    # ISO_A2 is -99 for France and Norway; the "EH" field has them.
    c = props.get("ISO_A2_EH") or props.get("ISO_A2")
    return None if c in (None, "-99") else c


def ring_path(ring):
    pts = [project(lon, lat) for lon, lat in ring]
    xs, ys = [p[0] for p in pts], [p[1] for p in pts]
    if max(xs) - min(xs) < MIN_RING and max(ys) - min(ys) < MIN_RING:
        return ""
    kept = [pts[0]]
    for p in pts[1:]:
        if math.hypot(p[0] - kept[-1][0], p[1] - kept[-1][1]) >= MIN_STEP:
            kept.append(p)
    if len(kept) < 3:
        return ""
    return "M" + "L".join("%.1f %.1f" % p for p in kept) + "Z"


def main():
    small, large = fetch("110m"), fetch("50m")
    # A territory can share its country's "EH" code (Ashmore and Cartier
    # Islands has Australia's), so a country's own ISO_A2 entry names it.
    names = {}
    for f in sorted(large, key=lambda f: f["properties"]["ISO_A2"] == code(f["properties"])):
        c = code(f["properties"])
        if c:
            names[c] = NAMES.get(c, f["properties"]["NAME_EN"])
    countries, drawn = [], set()
    for f in small:
        p = f["properties"]
        c = code(p)
        if c == "AQ":
            continue
        g = f["geometry"]
        polys = g["coordinates"] if g["type"] == "MultiPolygon" else [g["coordinates"]]
        d = "".join(ring_path(ring) for poly in polys for ring in poly)
        if d:
            countries.append({"code": c, "name": names.get(c, NAMES.get(c, p["NAME_EN"])), "d": d})
            drawn.add(c)
    points = []
    for f in large:
        p = f["properties"]
        c = code(p)
        if c and c not in drawn and c != "AQ" and p["LABEL_Y"] > -57:
            x, y = project(p["LABEL_X"], p["LABEL_Y"])
            points.append({"code": c, "name": names[c], "x": round(x, 1), "y": round(y, 1)})
    countries.sort(key=lambda c: c["name"])
    points.sort(key=lambda c: c["name"])
    out = {
        "_about": "World map for the Citation reach page: Natural Earth 1:110m countries (public domain),"
                  " Equal Earth projection, Antarctica left out; small countries as label points from the"
                  " 1:50m set. Made by scripts/make-world-map.py; see that file.",
        "viewBox": "0 0 %d %d" % (WIDTH, math.ceil(BOTTOM)),
        "names": dict(sorted(names.items())),
        "countries": countries,
        "points": points,
    }
    with open(OUT, "w", encoding="utf-8") as fh:
        json.dump(out, fh, ensure_ascii=False, separators=(",", ":"))
        fh.write("\n")
    print("[world-map] %d countries, %d points, %d KB -> %s"
          % (len(countries), len(points), os.path.getsize(OUT) // 1024, os.path.relpath(OUT)))
    if os.path.isdir(os.path.dirname(MIRROR)):
        shutil.copyfile(OUT, MIRROR)
        print("[world-map] copied to quarto-site/data/world-map.json.")


if __name__ == "__main__":
    main()
