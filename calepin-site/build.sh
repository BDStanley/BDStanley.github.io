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

# Resolve the `calepin` binary. Prefer one on PATH; otherwise fall back to the
# copy bundled with the Positron/VS Code "Calepin for Typst" extension, which
# ships `calepin` but does NOT add it to PATH (so `commit.R` -> this script
# can't see it). The glob tolerates extension version bumps; sort -V picks the
# newest installed version. See README.md → Prerequisites.
if command -v calepin >/dev/null 2>&1; then
  CALEPIN="calepin"
else
  CALEPIN="$(ls -d \
    "$HOME"/.positron/extensions/vincentarel-bundock.calepin-*/bin/calepin \
    "$HOME"/.vscode/extensions/vincentarel-bundock.calepin-*/bin/calepin \
    "$HOME"/.cursor/extensions/vincentarel-bundock.calepin-*/bin/calepin \
    2>/dev/null | sort -V | tail -n1 || true)"
  if [ -z "${CALEPIN}" ] || [ ! -x "${CALEPIN}" ]; then
    echo "Error: 'calepin' is not on PATH and no bundled copy was found in the" >&2
    echo "Positron/VS Code 'Calepin for Typst' extension." >&2
    echo "Install it (see README.md → Prerequisites) or put 'calepin' on PATH." >&2
    exit 1
  fi
  echo "→ Using bundled calepin: $CALEPIN"
fi

echo "→ Building CV PDF from cv/cv.yml ..."
typst compile cv/cv.typ cv/cv.pdf

# Regenerate the Teaching pages from the local Teaching repo (read-only). Editing
# the Teaching repo and rebuilding the site reflects the changes here. Override
# the source/host with TEACHING_SRC / TEACHING_NETLIFY_BASE. Missing repo -> the
# script warns and keeps any previously generated pages (doesn't fail the build).
echo "→ Syncing Teaching pages from the Teaching repo ..."
python3 "$SRC/scripts/sync-teaching.py"

echo "→ Cleaning previous build at $OUT ..."
rm -rf "$OUT"

echo "→ Building site into $OUT ..."
"$CALEPIN" compile "$SRC" "$OUT"

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
