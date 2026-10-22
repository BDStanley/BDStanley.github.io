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

# Add new publications from ORCID/OpenAlex to cv/cv.yml, and fill in volume,
# issue and pages for the online-first articles it added earlier. It never edits
# an entry it didn't add (see data/publications-sync.json). Runs before the CV PDF
# is compiled so the PDF and the Publications page both pick up the changes.
# Network failures are non-fatal: the build then uses cv.yml as it stands.
echo "→ Checking ORCID/OpenAlex for new publications ..."
python3 "$SRC/scripts/update-publications.py" || echo "  (skipped: update-publications.py failed; cv.yml unchanged)"

# Bring the Datasets section of cv/cv.yml in step with the Harvard Dataverse
# collection: new datasets, renamed ones, ones that have left. Dataverse leads,
# so dataset titles are fixed there. Entries for datasets elsewhere are kept.
# Network failures are non-fatal: the build then uses cv.yml as it stands.
echo "→ Syncing datasets from Harvard Dataverse ..."
python3 "$SRC/scripts/update-datasets.py" || echo "  (skipped: update-datasets.py failed; cv.yml unchanged)"

# Rewrite the "Peer reviewer for" entry of cv/cv.yml from the peer reviews on
# ORCID (sent there by Web of Science, formerly Publons): journals named from
# OpenAlex, each with its number of reviews. Journals listed by hand that ORCID
# lacks are kept. Network failures are non-fatal: cv.yml is then left as it is.
echo "→ Counting peer reviews on ORCID ..."
python3 "$SRC/scripts/update-reviews.py" || echo "  (skipped: update-reviews.py failed; cv.yml unchanged)"

echo "→ Building CV PDF from cv/cv.yml ..."
typst compile cv/cv.typ cv/cv.pdf

# Regenerate the Teaching pages from the local Teaching repo (read-only). Editing
# the Teaching repo and rebuilding the site reflects the changes here. Override
# the source/host with TEACHING_SRC / TEACHING_NETLIFY_BASE. Missing repo -> the
# script warns and keeps any previously generated pages (doesn't fail the build).
echo "→ Syncing Teaching pages from the Teaching repo ..."
python3 "$SRC/scripts/sync-teaching.py"

# Refresh the Kultura Liberalna article list in data/kultura-liberalna.yml (read
# by pages/media.typ). The author page only exposes the 15 most recent pieces,
# so the script MERGES new ones into the file rather than replacing it — the
# archive keeps growing. Network failures are non-fatal: the build then just
# uses the list already committed.
echo "→ Refreshing the Kultura Liberalna article list ..."
python3 "$SRC/scripts/update-kl.py" || echo "  (skipped: update-kl.py failed; using the committed list)"

# Refresh the citation counts on the Publications page (data/citations.json) from
# Scopus, Google Scholar, OpenAlex and Sage Policy Profiles (Scopus needs an
# Elsevier API key in the Keychain; see the script). A source that can't be read keeps its previous
# numbers; network failures are non-fatal.
echo "→ Refreshing citation counts ..."
python3 "$SRC/scripts/update-citations.py" || echo "  (skipped: update-citations.py failed; using the committed counts)"

# Refresh the Citation reach page's data (data/reach.json): the countries,
# disciplines and years of the works citing Ben's, from OpenAlex. If OpenAlex
# can't be read, or finds far fewer citations than last time, the previous data
# stays; network failures are non-fatal.
echo "→ Refreshing the citation reach data ..."
python3 "$SRC/scripts/update-reach.py" || echo "  (skipped: update-reach.py failed; using the committed data)"

# Refresh the Policy reach page's data (data/policy-reach.json): the policy
# documents citing Ben's work (Sage Policy Profiles) and the research citing it
# from think tanks, government bodies and international organisations
# (OpenAlex). A source that can't be read keeps its previous records; network
# failures are non-fatal.
echo "→ Refreshing the policy reach data ..."
python3 "$SRC/scripts/update-policy-reach.py" || echo "  (skipped: update-policy-reach.py failed; using the committed data)"

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
#  - .nojekyll disables Jekyll, so the .calepin/ folder (CSS bundle, manifest)
#    is served rather than skipped as a dot-folder — without this, styling breaks.
printf 'benstanley.eu\n' > "$OUT/CNAME"
: > "$OUT/.nojekyll"
# Browsers, feed readers and search engines ask for /favicon.ico without reading
# the page's <link rel="icon">, so put the icon there too.
cp "$SRC/assets/favicon.ico" "$OUT/favicon.ico"

echo "✓ Built into $OUT (pruned, added CNAME, .nojekyll and favicon.ico)."
