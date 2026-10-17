# benstanley.eu

The source of [benstanley.eu](https://benstanley.eu), Ben Stanley's academic website: publications, research projects, teaching, media writing, the Pooling the Poles polling tracker, a page for the book *Good Change*, and a downloadable CV. The site is built with [Calepin](https://github.com/vincentarelbundock/calepin), a static-site generator for Typst, and GitHub Pages serves it from the `docs/` folder of the `main` branch.

One file, `calepin-site/cv/cv.yml`, holds every publication and research project. It produces the CV PDF and feeds the Publications and Research projects pages, so each entry is edited in one place. New journal articles, book chapters and books on Ben's ORCID record are added to it automatically on each build, and its Datasets section follows his Harvard Dataverse collection.

## Layout

| Folder | Contents |
|---|---|
| `calepin-site/` | The source of the live site: pages, configuration, theme, CV and build scripts. |
| `docs/` | The built site that GitHub Pages serves. **Do not edit it by hand**: every build deletes and rewrites it. |
| `quarto-site/` | A Quarto build of the same site, kept as a fallback. It is never deployed. See [The Quarto backup](#the-quarto-backup). |
| `.claude/skills/` | A Claude Code skill for adding a teaching course. See [Working with Claude Code](#working-with-claude-code). |

Key files:

| File | Purpose |
|---|---|
| `commit.R` | Build, commit, push and back up in one step. See [Committing and syncing](#committing-and-syncing). |
| `calepin-site/build.sh` | Builds the CV PDF, refreshes the generated content and compiles the site into `docs/`. |
| `calepin-site/calepin.toml` | Site configuration: title, navigation bar, social links, footer. |
| `calepin-site/cv/cv.yml` | All CV data. `cv/cv.typ` turns it into `cv.pdf` with the academicv Typst template. |
| `calepin-site/scripts/sync-teaching.py` | Generates the Teaching pages from Ben's separate Teaching repository. |
| `calepin-site/scripts/update-kl.py` | Adds new Kultura Liberalna articles to `data/kultura-liberalna.yml`, the archive the Media page lists. It never changes existing entries. |
| `calepin-site/scripts/update-publications.py` | Adds new publications from ORCID and OpenAlex to `cv/cv.yml`, formatted from Crossref's metadata, and later fills in volume, issue and pages for the online-first articles it added. It never changes entries it did not add. See [New publications](#new-publications). |
| `calepin-site/scripts/update-datasets.py` | Keeps the Datasets section of `cv/cv.yml` in step with Ben's Harvard Dataverse collection. See [Datasets](#datasets). |
| `calepin-site/data/publications-sync.json` | The record those two scripts keep: DOIs never to add (`ignore`), the publications `update-publications.py` added (`managed`) and the Dataverse DOIs `update-datasets.py` last wrote (`datasets`). |
| `calepin-site/scripts/update-citations.py` | Refreshes the Scopus, Google Scholar, OpenAlex and Sage Policy Profiles numbers in `data/citations.json`, and each publication's OpenAlex count in `data/publication-citations.json`. See [Citation counts](#citation-counts). |
| `calepin-site/data/citations.json` | The numbers in the Citations list on the Publications page, each with the month it dates from. |
| `calepin-site/data/publication-citations.json` | OpenAlex's citation count for each publication, shown at the end of its entry on the Publications page. |
| `calepin-site/scripts/make-favicon.py` | Draws the favicon (a white "BS" in bold Jost on the navbar's red) as `assets/favicon.svg`, `assets/favicon-32.png` and `assets/apple-touch-icon.png`. Not part of the build: the icons are committed, so run it only to change the design. |
| `calepin-site/themes/site-theme/` | A thin overlay on Calepin's built-in `academic` theme: the Jost font, a dark-red navigation bar and footer. |
| `calepin-site/README.md` | Installing Calepin, and notes on the theme and the CV set-up. |

## Editing the site

Edit the sources in `calepin-site/`, then rebuild. Never edit `docs/`.

| To change | Edit (in `calepin-site/`) |
|---|---|
| Publications, research projects, the CV | `cv/cv.yml`. New publications arrive on their own; see [New publications](#new-publications). |
| Datasets | Harvard Dataverse: the Datasets section follows the collection on each build. See [Datasets](#datasets). |
| Citation counts | Nothing: they update on each build (Scopus once its API key is in the Keychain). See [Citation counts](#citation-counts). |
| Home page | `index.typ` |
| Media, Pooling the Poles, Good Change | `pages/media.typ`, `pages/pooling-the-poles.typ`, `pages/good-change.typ` |
| The Kultura Liberalna list | Nothing: it updates on each build. To fix a title or add a missed piece, edit `data/kultura-liberalna.yml`; hand edits are kept. |
| Teaching | The Teaching repository. Course descriptions are in `COURSE_DESCRIPTIONS` in `scripts/sync-teaching.py` and icons in `assets/teaching/<slug>.svg`. The generated `pages/teaching.typ` and `pages/teaching/` are not edited by hand. |
| Navigation bar, social links, footer | `calepin.toml` |
| Fonts and colours | `themes/site-theme/css/90_site.css` |
| Favicon | `scripts/make-favicon.py`, then run it. `favicon` in `calepin.toml` points to the SVG; `themes/site-theme/partials/site-head.html` links the PNGs. |

## Building

```sh
./calepin-site/build.sh                                     # build into docs/
cd calepin-site && ./build.sh _site && calepin serve _site --open   # local preview
```

`build.sh`:

1. adds new publications to `cv/cv.yml` and completes online-first ones it added earlier (see [New publications](#new-publications)), leaving `cv.yml` as it is if ORCID, OpenAlex or Crossref cannot be reached;
2. brings the Datasets section of `cv/cv.yml` in step with the Harvard Dataverse collection (see [Datasets](#datasets)), leaving it as it is if Dataverse cannot be reached;
3. compiles `cv/cv.typ` into `cv/cv.pdf` with Typst;
4. regenerates the Teaching pages from a `Teaching` folder next to this repository (set `TEACHING_SRC` to use another path). If the folder is missing, it warns and keeps the pages already generated;
5. adds any new Kultura Liberalna articles to `data/kultura-liberalna.yml`, falling back to the committed list if the site cannot be reached;
6. refreshes the citation counts in `data/citations.json` (see [Citation counts](#citation-counts)), keeping the previous numbers for any source it cannot read;
7. deletes the output folder and compiles the site into it, removes the theme and `.typ` sources Calepin copies across, and writes `CNAME` and an empty `.nojekyll`.

A preview build into `_site/` (git-ignored) leaves `docs/` alone, but steps 1–6 still update `cv/cv.yml`, the CV PDF, the Teaching pages, the Kultura Liberalna list and the citation counts.

The build needs Typst 0.15 or newer, Calepin, Python 3 (standard library only) and the Jost font, which the CV uses. `build.sh` uses `calepin` from the `PATH`, or else the copy bundled with the "Calepin for Typst" extension for Positron, VS Code or Cursor. Typst downloads the academicv package on the first compile.

## New publications

`scripts/update-publications.py` runs at the start of every build. It collects the DOIs on Ben's ORCID record (0000-0001-6932-1046) and those OpenAlex links to the same iD, and adds to `cv/cv.yml` any that are not there yet:

- **What it adds:** journal articles, book chapters and books where Crossref lists Ben as an author. It leaves out datasets, book reviews, errata, preprints and the separate chapters of a book already under Monographs. Datasets come from Harvard Dataverse instead; see [Datasets](#datasets).
- **How it recognises existing entries:** by DOI, or by title for the older entries that have no DOI.
- **Where new entries go:** each is formatted in `cv.yml`'s style from Crossref's metadata and placed at the top of its year in the right section. The build output shows every addition as a `[publications] +` line.
- **Online-first articles:** these go in without volume or pages. On later builds, once Crossref has the volume and pages, the script writes them in after the journal name and changes the year to the issue's year. It replaces only the text it wrote, so later fixes to the authors or title are kept. If that part was edited by hand too, it prints the new details as `[publications] needs attention` instead.
- **Quarto backup:** when `quarto-site/cv/cv.yml` matches `calepin-site/cv/cv.yml`, the script makes the same changes there.
- **Never touched:** entries the script did not add.

Day-to-day use:

| To | Do |
|---|---|
| Fix a new entry's wording | Edit it in `cv.yml`. |
| Remove an entry it added | Delete it from `cv.yml`. The next build moves its DOI to `ignore` in `data/publications-sync.json`, so it does not come back. |
| Keep out any other DOI | Add it to `ignore`, with a short reason. |
| Stop it completing an entry | Delete that entry from `managed`. |
| Check without changing anything | `python3 calepin-site/scripts/update-publications.py --dry-run` |

## Datasets

`scripts/update-datasets.py` runs on every build, straight after the publications check, and keeps the Datasets section of `cv/cv.yml` in step with Ben's Harvard Dataverse collection (`bdstanley`), through Dataverse's public API:

- **What it writes:** one entry per published dataset in the collection, in the CV's style (`Stanley, Ben. 2026. 'Title'. doi:10.7910/DVN/…`). Authors and year come from the citation Dataverse gives; English titles get the CV's headline capitalisation. Entries run newest year first and, within a year, most recently deposited first.
- **Dataverse leads:** each run rewrites the entries for the collection's datasets, so renaming a dataset on Dataverse renames it in the CV and on the site at the next build, and a dataset that leaves the collection leaves the CV. Fix titles on Dataverse, not in `cv.yml`: a hand edit there is replaced on the next build.
- **Never touched:** entries for datasets outside the collection (another DOI, or none).
- **Quarto backup:** when `quarto-site/cv/cv.yml` matches `calepin-site/cv/cv.yml`, the same changes are made there.

| To | Do |
|---|---|
| Update the datasets now | `python3 calepin-site/scripts/update-datasets.py` |
| Check without changing anything | `python3 calepin-site/scripts/update-datasets.py --dry-run` |
| Leave a dataset out | Delete its entry from `cv.yml`. The next build moves its DOI to `ignore` in `data/publications-sync.json`, so it does not come back. |
| Bring it back | Delete its DOI from `ignore`. |

## Citation counts

The Citations list at the top of the Publications page reads its numbers from `calepin-site/data/citations.json`. `scripts/update-citations.py` runs on every build:

- **Scopus:** total citations and h-index, worked out from the citation counts of the documents under author ID 23493968300 in Elsevier's Scopus Search API. (The author profile API has these totals ready-made, but only for subscribing institutions.) It needs a free Elsevier API key from [dev.elsevier.com](https://dev.elsevier.com), which must never go in this public repo: store it in the macOS Keychain with `security add-generic-password -a "$USER" -s scopus-api-key -w` (it prompts for the key, which it calls "password data"), or set `SCOPUS_API_KEY`. Without a key, or if Elsevier refuses it, Scopus is skipped and its last numbers stay.
- **Google Scholar:** citations, h-index and i10-index (all years), read from the metrics table on the profile page.
- **OpenAlex:** citations, h-index and i10-index from its author record, looked up by Ben's ORCID iD.
- **Sage Policy Profiles:** policy citations and the number of policy documents they appear in, read from the data the public profile page loads.

Web of Science is not listed: its author records are shown only to signed-in users, and to automated visitors only after a CAPTCHA.

Each source records the month its numbers date from. The page's "Updated" line shows the latest month, and any older source shows its own month beside its name. A source keeps its previous numbers if it cannot be reached, if its page no longer has the numbers where the script looks (for instance a Google Scholar CAPTCHA), or if any number has fallen by more than a quarter. The last case prints a `[citations] needs attention` line; if the fall is real, enter the new numbers by hand. The script copies the file to `quarto-site/data/` for the Quarto backup. `--dry-run` shows what it would change without writing anything.

The same script also fetches OpenAlex's citation count for each publication in `cv/cv.yml` into `data/publication-citations.json` (copied to `quarto-site/data/` too). The Publications page shows it at the end of the entry, before the full stop: "… doi:10.xxx (12 citations)." or "(1 citation)". Entries with no citations, or none OpenAlex knows of, get no note. Entries are matched by DOI, and the few without one by title among the works OpenAlex links to Ben's ORCID iD. The CV PDF does not show these counts. If OpenAlex cannot be reached, or matches more than a quarter fewer publications than last time, the previous counts stay.

## Deploying

GitHub Pages publishes the `docs/` folder of `main` to benstanley.eu, with HTTPS enforced, so deploying means pushing a commit that changes `docs/`. `docs/CNAME` keeps the custom domain. `docs/.nojekyll` turns off Jekyll, which would otherwise skip the `.calepin/` folder (favicon and site manifest). `build.sh` writes both on every build; don't delete them.

## Committing and syncing

Run `Rscript commit.R` from anywhere in the repo, or source it in Positron. It:

1. runs `git pull --ff-only` (if that fails, it carries on);
2. runs `calepin-site/build.sh`, rebuilding the CV PDF and the whole site into `docs/`;
3. stages everything and, if anything has changed, has Claude (Sonnet) check this README against everything changed since the last successful check, and update it if needed. If Claude is unavailable, the check is skipped and catches up on a later run;
4. commits with the message `Update <timestamp>: <summary>`, where Claude Haiku writes the summary and a few bullets from the staged changes (if that fails, the message is just the timestamp), and pushes; the site is live a minute or two later. Commits whose README check succeeded end with a `README-reviewed: yes` line. If nothing has changed, it skips the commit;
5. mirrors the folder to `iCloud Drive/Website/` with `rsync --delete`, excluding `.git`, `.quarto`, `.calepin` and `_site`.

The iCloud copy is a one-way backup: edits made there are overwritten on the next run. `commit.R` does not build `quarto-site/`.

## The Quarto backup

`quarto-site/` rebuilds the same site with Quarto, in case the site ever moves off Calepin. `quarto-site/build.sh` renders only into `quarto-site/_site` (git-ignored). It writes no `CNAME` or `.nojekyll` and never touches `docs/`. The Calepin site leads: changes are mirrored into the backup, never the other way round. `cv/cv.yml` is kept in step, but the backup's Teaching pages and Kultura Liberalna list refresh only when its own `build.sh` is run, so they can fall behind. See `quarto-site/README.md`.

## Setting up on a new machine

1. Clone the repo: `git clone https://github.com/BDStanley/BDStanley.github.io.git`. The download is about 800 MB, almost all of it history. For the Teaching pages, put the Teaching repository beside it in a folder named `Teaching`.
2. Install Typst (`brew install typst`), Calepin (see `calepin-site/README.md` → Prerequisites), Python 3 and the Jost font.
3. For the Scopus citation count, add the Elsevier API key to the Keychain (see [Citation counts](#citation-counts)); without it the build skips Scopus.
4. For `commit.R`, install R (the script installs the `here` package itself), the `claude` command-line tool (on the `PATH` or at `~/.npm-global/bin/claude`) and Homebrew's `rsync`, which it expects at `/opt/homebrew/bin/rsync`. It adds `/opt/homebrew/bin` and `~/.cargo/bin` to the `PATH`, so Typst and Calepin installed there are found. The iCloud path in step 5 is hard-coded.

## Working with Claude Code

`.claude/skills/add-teaching-course/SKILL.md` covers adding a course to the Teaching section: drafting its description, making its icon, rebuilding and checking the page, and mirroring both into the Quarto backup. It also sets out the rule that the Calepin site leads and `quarto-site/` follows.
