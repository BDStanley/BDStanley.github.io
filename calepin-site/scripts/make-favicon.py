#!/usr/bin/env python3
"""Make the site's favicons: a white "BS" in bold Jost on the navbar's dark red.

Writes, into assets/:

  * favicon.svg           browsers that take SVG icons (set by `favicon` in calepin.toml)
  * favicon-32.png        the others, Safari among them
  * apple-touch-icon.png  180 x 180, for iOS home screens

The two PNGs are linked from themes/site-theme/partials/site-head.html. Typst
draws all three, so the letters are outlines and look the same whether or not
the viewer has Jost (a favicon cannot load web fonts).

Not run by build.sh: the icons are committed, so run this only to change the
design. Needs Typst and the Jost font, as the CV does.
Usage:  python3 scripts/make-favicon.py
"""

import os
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


def main():
    with tempfile.TemporaryDirectory() as tmp:
        src = os.path.join(tmp, "favicon.typ")
        with open(src, "w", encoding="utf-8") as fh:
            fh.write(SOURCE)
        for name, fmt, ppi in OUTPUTS:
            out = subprocess.run(["typst", "compile", "--format", fmt]
                                 + (["--ppi", str(ppi)] if ppi else []) + [src, os.path.join(ASSETS, name)],
                                 capture_output=True, text=True)
            if out.returncode != 0 or "unknown font" in out.stderr:
                sys.exit("Typst could not draw %s:\n%s" % (name, out.stderr))
            print("wrote assets/" + name)
    return 0


if __name__ == "__main__":
    sys.exit(main())
