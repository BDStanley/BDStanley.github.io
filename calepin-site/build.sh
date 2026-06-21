#!/usr/bin/env bash
# Build the whole site (CV PDF + all pages) into the GitHub Pages publish folder.
#
# cv/cv.yml is the single source of truth: it builds the CV PDF *and* feeds the
# Publications and Research projects pages (read via Typst's yaml()).
#
# Workflow:  edit cv/cv.yml (or any page) -> ./build.sh -> commit -> push to deploy.
#
# Default output is ../docs (the repo's GitHub Pages source: main -> /docs).
# Pass a different dir to build elsewhere, e.g. for a local preview:
#     ./build.sh _site && calepin serve _site --open
set -euo pipefail
cd "$(dirname "$0")"            # calepin-site/
SRC="$(pwd)"

# Resolve OUT to an absolute path (pagefind rejects paths containing "..").
if [ -n "${1:-}" ]; then
  mkdir -p "$1"; OUT="$(cd "$1" && pwd)"
else
  OUT="$(cd .. && pwd)/docs"
fi

echo "→ Building CV PDF from cv/cv.yml ..."
typst compile cv/cv.typ cv/cv.pdf

echo "→ Cleaning previous build at $OUT ..."
rm -rf "$OUT"

echo "→ Building site into $OUT ..."
calepin compile "$SRC" "$OUT"

# Calepin copies non-page source into the output too; the deployed site doesn't
# need it (CSS/JS are bundled into .calepin/). Drop the theme sources, any stray
# preview build, and the .typ source files.
rm -rf "$OUT/_site" "$OUT/themes"
find "$OUT" -name '*.typ' -delete

# GitHub Pages specifics:
#  - CNAME keeps the custom domain (benstanley.eu).
#  - .nojekyll disables Jekyll, so the .calepin/ folder (CSS bundle, favicon)
#    is served rather than skipped as a dot-folder — without this, styling breaks.
printf 'benstanley.eu\n' > "$OUT/CNAME"
: > "$OUT/.nojekyll"

echo "✓ Built into $OUT (pruned, added CNAME + .nojekyll)."
