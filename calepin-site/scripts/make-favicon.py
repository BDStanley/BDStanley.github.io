#!/usr/bin/env python3
"""Make the site's favicons: a white "BS" in bold Jost on the navbar's dark red.

Writes, into assets/:

  * favicon.svg           browsers that take SVG icons (set by `favicon` in calepin.toml)
  * favicon-32.png        the others, Safari among them
  * apple-touch-icon.png  180 x 180, for iOS home screens
  * favicon.ico           16, 32 and 48 px, for anything that asks for /favicon.ico
                          without reading the page (build.sh copies it to the site root)

The two PNGs are linked from themes/site-theme/partials/site-head.html. Typst
draws everything, so the letters are outlines and look the same whether or not
the viewer has Jost (a favicon cannot load web fonts).

Browsers keep favicons cached for a long time, so the links carry a version
(?v=2, in calepin.toml and site-head.html). After changing the design, raise it
in both places, or visitors may go on seeing the old icon.

Not run by build.sh: the icons are committed, so run this only to change the
design. Needs Typst and the Jost font, as the CV does.
Usage:  python3 scripts/make-favicon.py
"""

import os
import struct
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ASSETS = os.path.abspath(os.path.join(HERE, "..", "assets"))
RED = "#8b0000"                     # the navbar's red (themes/site-theme/css/90_site.css)

# A 64pt square. With the text box running from the baseline to the cap height,
# centring it centres the capitals optically.
SOURCE = """\
#set page(width: 64pt, height: 64pt, margin: 0pt, fill: rgb("%s"))
#set text(font: "Jost", weight: "bold", fill: white, size: 38pt,
          top-edge: "cap-height", bottom-edge: "baseline")
#place(center + horizon)[BS]
""" % RED

# (file, format, pixels per inch: 72 ppi makes the 64pt square 64px)
OUTPUTS = (("favicon.svg", "svg", None),
           ("favicon-32.png", "png", 72 * 32 / 64),
           ("apple-touch-icon.png", "png", 72 * 180 / 64))
ICO_SIZES = (16, 32, 48)


def draw(src, out, fmt, ppi=None):
    run = subprocess.run(["typst", "compile", "--format", fmt]
                         + (["--ppi", str(ppi)] if ppi else []) + [src, out],
                         capture_output=True, text=True)
    if run.returncode != 0 or "unknown font" in run.stderr:
        sys.exit("Typst could not draw %s:\n%s" % (os.path.basename(out), run.stderr))


def write_ico(path, images):
    """An .ico holding PNG images, [(size, png bytes)], as every current browser reads."""
    offset = 6 + 16 * len(images)
    head, body = struct.pack("<HHH", 0, 1, len(images)), b""
    for size, png in images:
        head += struct.pack("<BBBBHHII", size % 256, size % 256, 0, 0, 1, 32, len(png), offset + len(body))
        body += png
    with open(path, "wb") as fh:
        fh.write(head + body)


def main():
    with tempfile.TemporaryDirectory() as tmp:
        src = os.path.join(tmp, "favicon.typ")
        with open(src, "w", encoding="utf-8") as fh:
            fh.write(SOURCE)
        for name, fmt, ppi in OUTPUTS:
            draw(src, os.path.join(ASSETS, name), fmt, ppi)
            print("wrote assets/" + name)
        images = []
        for size in ICO_SIZES:
            png = os.path.join(tmp, "favicon-%d.png" % size)
            draw(src, png, "png", 72 * size / 64)
            with open(png, "rb") as fh:
                images.append((size, fh.read()))
        write_ico(os.path.join(ASSETS, "favicon.ico"), images)
        print("wrote assets/favicon.ico")
    return 0


if __name__ == "__main__":
    sys.exit(main())
