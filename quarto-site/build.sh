#!/usr/bin/env bash
# Build the Quarto BACKUP of benstanley.eu into ./_site (a LOCAL build only).
#
# This does NOT deploy and does NOT touch ../docs (the live site, which is
# produced by ../calepin-site/build.sh). It exists so the site can be returned
# to a Quarto toolchain in future with the same functionality. See README.md.
#
# cv/cv.yml is the single source of truth: it builds the CV PDF *and* feeds the
# Publications and Research projects pages (read with R's `yaml` package).
#
# Workflow:  edit cv/cv.yml (or any page) -> ./build.sh -> preview ./_site.
set -euo pipefail
cd "$(dirname "$0")"            # quarto-site/
SRC="$(pwd)"

# 1) Build the CV PDF from cv/cv.yml (academicv Typst template), so the navbar
#    "CV" link resolves. The original ../CV is never touched.
echo "→ Building CV PDF from cv/cv.yml ..."
typst compile cv/cv.typ cv/cv.pdf

# 2) Regenerate the Teaching pages from the local Teaching repo (read-only).
#    Override the source/host with TEACHING_SRC / TEACHING_NETLIFY_BASE. A
#    missing repo -> the script warns and keeps any previously generated pages.
echo "→ Syncing Teaching pages from the Teaching repo ..."
python3 "$SRC/scripts/sync-teaching.py"

# 3) Render the site into ./_site (output-dir set in _quarto.yml). The live
#    Kultura Liberalna scrape runs during this step (pages/media.qmd).
echo "→ Rendering site with Quarto into ./_site ..."
quarto render

echo "✓ Built into $SRC/_site. Preview with:  quarto preview   (or open _site/index.html)"
