# benstanley.eu

The source of [benstanley.eu](https://benstanley.eu), Ben Stanley's academic website: publications, research projects, teaching, media writing, the Pooling the Poles polling tracker, a page for the book *Good Change*, and a downloadable CV. The site is built with [Calepin](https://github.com/vincentarelbundock/calepin), a static-site generator for Typst, and GitHub Pages serves it from the `docs/` folder of the `main` branch.

One file, `calepin-site/cv/cv.yml`, holds every publication and research project. It produces the CV PDF and feeds the Publications and Research projects pages, so each entry is edited in one place.

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
| `calepin-site/themes/site-theme/` | A thin overlay on Calepin's built-in `academic` theme: the Jost font, a dark-red navigation bar and footer. |
| `calepin-site/README.md` | Installing Calepin, and notes on the theme and the CV set-up. |

## Editing the site

Edit the sources in `calepin-site/`, then rebuild. Never edit `docs/`.

| To change | Edit (in `calepin-site/`) |
|---|---|
| Publications, research projects, the CV | `cv/cv.yml` |
| Home page | `index.typ` |
| Media, Pooling the Poles, Good Change | `pages/media.typ`, `pages/pooling-the-poles.typ`, `pages/good-change.typ` |
| The Kultura Liberalna list | Nothing: it updates on each build. To fix a title or add a missed piece, edit `data/kultura-liberalna.yml`; hand edits are kept. |
| Teaching | The Teaching repository. Course descriptions are in `COURSE_DESCRIPTIONS` in `scripts/sync-teaching.py` and icons in `assets/teaching/<slug>.svg`. The generated `pages/teaching.typ` and `pages/teaching/` are not edited by hand. |
| Navigation bar, social links, footer | `calepin.toml` |
| Fonts and colours | `themes/site-theme/css/90_site.css` |

## Building

```sh
./calepin-site/build.sh                                     # build into docs/
cd calepin-site && ./build.sh _site && calepin serve _site --open   # local preview
```

`build.sh`:

1. compiles `cv/cv.typ` into `cv/cv.pdf` with Typst;
2. regenerates the Teaching pages from a `Teaching` folder next to this repository (set `TEACHING_SRC` to use another path). If the folder is missing, it warns and keeps the pages already generated;
3. adds any new Kultura Liberalna articles to `data/kultura-liberalna.yml`, falling back to the committed list if the site cannot be reached;
4. deletes the output folder and compiles the site into it, removes the theme and `.typ` sources Calepin copies across, and writes `CNAME` and an empty `.nojekyll`.

A preview build into `_site/` (git-ignored) leaves `docs/` alone, but steps 1–3 still refresh the CV PDF, the Teaching pages and the Kultura Liberalna list.

The build needs Typst 0.15 or newer, Calepin, Python 3 (standard library only) and the Jost font, which the CV uses. `build.sh` uses `calepin` from the `PATH`, or else the copy bundled with the "Calepin for Typst" extension for Positron, VS Code or Cursor. Typst downloads the academicv package on the first compile.

## Deploying

GitHub Pages publishes the `docs/` folder of `main` to benstanley.eu, with HTTPS enforced, so deploying means pushing a commit that changes `docs/`. `docs/CNAME` keeps the custom domain. `docs/.nojekyll` turns off Jekyll, which would otherwise skip the `.calepin/` folder (favicon and site manifest). `build.sh` writes both on every build; don't delete them.

## Committing and syncing

Run `Rscript commit.R` from anywhere in the repo, or source it in Positron. It:

1. runs `git pull --ff-only` (if that fails, it carries on);
2. runs `calepin-site/build.sh`, rebuilding the CV PDF and the whole site into `docs/`;
3. stages everything and, if anything has changed, has Claude (Sonnet) check this README against everything changed since the last successful check, and update it if needed. If Claude is unavailable, the check is skipped and catches up on a later run;
4. commits with the message `Update <timestamp>` and pushes; the site is live a minute or two later. Commits whose README check succeeded end with a `README-reviewed: yes` line. If nothing has changed, it skips the commit;
5. mirrors the folder to `iCloud Drive/Website/` with `rsync --delete`, excluding `.git`, `.quarto`, `.calepin` and `_site`.

The iCloud copy is a one-way backup: edits made there are overwritten on the next run. `commit.R` does not build `quarto-site/`.

## The Quarto backup

`quarto-site/` rebuilds the same site with Quarto, in case the site ever moves off Calepin. `quarto-site/build.sh` renders only into `quarto-site/_site` (git-ignored). It writes no `CNAME` or `.nojekyll` and never touches `docs/`. The Calepin site leads: changes are mirrored into the backup, never the other way round. `cv/cv.yml` is kept in step, but the backup's Teaching pages and Kultura Liberalna list refresh only when its own `build.sh` is run, so they can fall behind. See `quarto-site/README.md`.

## Setting up on a new machine

1. Clone the repo: `git clone https://github.com/BDStanley/BDStanley.github.io.git`. The download is about 800 MB, almost all of it history. For the Teaching pages, put the Teaching repository beside it in a folder named `Teaching`.
2. Install Typst (`brew install typst`), Calepin (see `calepin-site/README.md` → Prerequisites), Python 3 and the Jost font.
3. For `commit.R`, install R (the script installs the `here` package itself), the `claude` command-line tool (on the `PATH` or at `~/.npm-global/bin/claude`) and Homebrew's `rsync`, which it expects at `/opt/homebrew/bin/rsync`. It adds `/opt/homebrew/bin` and `~/.cargo/bin` to the `PATH`, so Typst and Calepin installed there are found. The iCloud path in step 5 is hard-coded.

## Working with Claude Code

`.claude/skills/add-teaching-course/SKILL.md` covers adding a course to the Teaching section: drafting its description, making its icon, rebuilding and checking the page, and mirroring both into the Quarto backup. It also sets out the rule that the Calepin site leads and `quarto-site/` follows.
