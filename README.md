# benstanley.eu

The source of [benstanley.eu](https://benstanley.eu), Ben Stanley's academic website: publications, research projects, teaching, media writing, a page for the book *Good Change*, interactive analyses of Polish politics (among them the Pooling the Poles polling tracker), a downloadable CV, an analysis of where and in which disciplines Ben's work is cited, one of the think tanks and other policy organisations that cite it, and one of the news coverage that quotes, interviews or cites him. The site is built with [Calepin](https://github.com/vincentarelbundock/calepin), a static-site generator for Typst, and GitHub Pages serves it from the `docs/` folder of the `main` branch.

One file, `calepin-site/cv/cv.yml`, holds every publication and research project. It produces the CV PDF and feeds the Research outputs and Research projects pages, so each entry is edited in one place. New journal articles, book chapters and books on Ben's ORCID record are added to it automatically on each build, and its Datasets section follows his Harvard Dataverse collection.

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
| `bio.md` | Ben's canonical academic bio, in the third person, kept in the repo but not published: a full version (under 300 words) and a short one (under 150), each in English and Polish. The home page has the full English text in the first person. |
| `calepin-site/build.sh` | Builds the CV PDF, refreshes the generated content and compiles the site into `docs/`. |
| `calepin-site/calepin.toml` | Site configuration: title, navigation bar, social links, footer. |
| `calepin-site/cv/cv.yml` | All CV data. `cv/cv.typ` turns it into `cv.pdf` with the academicv Typst template, leaving out the publications' DOIs, which only the website shows. Datasets keep theirs, their only reference. A research project's optional `url:` links its title on the Research projects page; the CV PDF does not show it. |
| `calepin-site/scripts/sync-teaching.py` | Generates the Teaching pages from Ben's separate Teaching repository. |
| `calepin-site/scripts/sync-analyses.py` | Copies the data behind the Polish political analyses pages (the party genealogy, the Polling tracker, the Party system indicators, Party ideology, Political attitudes, Election results, Quality of democracy and Voting behaviour) from that repository into `assets/`. See [Polish political analyses](#polish-political-analyses). |
| `calepin-site/assets/party-genealogy/` | Parties in the Sejm (called Party genealogy until 24 October 2026, whose name its files keep), with its Genealogy, Seat strength and Defections views: `index.html`, a standalone page in plain HTML, CSS and JavaScript, and its data, `genealogy.json` and `defections.json` (loaded after the first). |
| `calepin-site/assets/polling-tracker/` | The Polling tracker, the web version of Pooling the Poles: `index.html` and `app.js` (plain HTML, CSS and JavaScript), and its data, `polls.json` and `draws.json` for the current Sejm term, and `terms.json` with one `term-<term>.json` for each past term. |
| `calepin-site/assets/party-system-indicators/` | The Party system indicators: `index.html` and `app.js` (plain HTML, CSS and JavaScript), and `party_system.json`, its data. It also reads the Polling tracker's `polls.json`. |
| `calepin-site/assets/election-results/` | Election results: `index.html` and `app.js` (plain HTML, CSS and JavaScript), and its data: `elections.json` (the index), `geo.json` (the map paths), `covariates.json` (the index of the scatter's characteristics) and one `covariates-<group>.json` per group of them, one `<election-id>.json` per election and one `profiles-<voivodeship>.json` per voivodeship. It also reads the Polling tracker's `polls.json` for "Polls now". |
| `calepin-site/assets/party-ideology/` | Party ideology: `index.html` and `app.js` (plain HTML, CSS and JavaScript), and `ideology.json`, its data. It also reads the Polling tracker's `polls.json`. |
| `calepin-site/assets/political-attitudes/` | Social and political trends (called Political attitudes until 23 October 2026, whose name its files keep): `index.html` and `app.js` (plain HTML, CSS and JavaScript), `attitudes.json` (the list of attitudes) and one `<slug>.json` per attitude. |
| `calepin-site/assets/election-results/` | Election results: `index.html` and `app.js` (plain HTML, CSS and JavaScript), and its data in `elections.json` and the geography, covariate, election and voivodeship-profile files it lists. Not yet linked from a page. |
| `calepin-site/assets/voting-behaviour/` | Voting behaviour: `index.html` and `app.js` (plain HTML, CSS and JavaScript), and its data: `voting.json` (turnout and list shares by election and group, the comparison with the official results, the average marginal effects and the checks) and one `calc-<year>.json` per election (200 posterior draws of the model's coefficients, the calibration shifts, the support tables and the calculator's rules). |
| `calepin-site/assets/quality-of-democracy/` | Quality of democracy: `index.html` and `app.js` (plain HTML, CSS and JavaScript), and its data: `democracy.json` (the V-Dem measures for Poland, the regional statistics and the Sejm's roll-call polarisation), `democracy-countries.json` (the 27 comparison countries, loaded only when a comparison is chosen) and `political-context.json` (elections, cabinets, presidents and events). |
| `calepin-site/scripts/update-kl.py` | Adds new Kultura Liberalna articles to `data/kultura-liberalna.yml`, the archive the Media page lists. It never changes existing entries. |
| `calepin-site/scripts/update-publications.py` | Adds new publications from ORCID and OpenAlex to `cv/cv.yml`, formatted from Crossref's metadata, and later fills in volume, issue and pages for the online-first articles it added. It never changes entries it did not add. See [New publications](#new-publications). |
| `calepin-site/scripts/update-datasets.py` | Keeps the Datasets section of `cv/cv.yml` in step with Ben's Harvard Dataverse collection. See [Datasets](#datasets). |
| `calepin-site/scripts/update-reviews.py` | Rewrites the CV's "Peer reviewer for" entry from the peer reviews on Ben's ORCID record, with the number of reviews for each journal. See [Peer reviewing](#peer-reviewing). |
| `calepin-site/data/publications-sync.json` | The record the publications and datasets scripts keep: DOIs never to add (`ignore`), the publications `update-publications.py` added (`managed`) and the Dataverse DOIs `update-datasets.py` last wrote (`datasets`). |
| `calepin-site/scripts/update-citations.py` | Refreshes the Scopus, Google Scholar, OpenAlex and Sage Policy Profiles numbers in `data/citations.json`, and each publication's OpenAlex count in `data/publication-citations.json`. See [Citation counts](#citation-counts). |
| `calepin-site/data/citations.json` | The Scopus, Google Scholar and OpenAlex numbers in the Citations list on the Research outputs page, each with the month it dates from. |
| `calepin-site/data/publication-citations.json` | OpenAlex's citation count for each publication, shown at the end of its entry on the Research outputs page. |
| `calepin-site/scripts/update-reach.py` | Refreshes `data/reach.json`, the data behind the Citation reach page: the countries, disciplines and years of the works citing Ben's, from OpenAlex. See [Citation reach](#citation-reach). |
| `calepin-site/scripts/update-policy-reach.py` | Refreshes `data/policy-reach.json`, the data behind the Policy reach page: the policy documents citing Ben's work (Sage Policy Profiles) and the research citing it from think tanks, government bodies and international organisations (OpenAlex). See [Policy reach](#policy-reach). |
| `calepin-site/scripts/update-media-reach.py` | Adds news articles quoting, interviewing or citing Ben to `data/media-reach.json`, the archive behind the Media reach page, from Google News searches and Google Alerts feeds. Settings (false matches to ignore, alert feeds) are in `data/media-sync.json`. See [Media reach](#media-reach). |
| `calepin-site/scripts/make-world-map.py` | Draws the Citation reach page's world map as `data/world-map.json` from Natural Earth's country shapes. Not part of the build: the map is committed, so run it only to change it. |
| `calepin-site/scripts/make-favicon.py` | Draws the favicon (a white "BS" in bold Jost on the navbar's red) as `assets/favicon.svg`, `assets/favicon-32.png`, `assets/apple-touch-icon.png` and `assets/favicon.ico` (which `build.sh` also copies to the site root). Not part of the build: the icons are committed, so run it only to change the design. |
| `calepin-site/themes/site-theme/` | A thin overlay on Calepin's built-in `academic` theme: the Jost font, a dark-red navigation bar and footer, and the type of the Polish political analyses (near-black ink, light dark-red headings, small uppercase labels). |
| `calepin-site/README.md` | Installing Calepin, and notes on the theme and the CV set-up. |

## Editing the site

Edit the sources in `calepin-site/`, then rebuild. Never edit `docs/`.

| To change | Edit (in `calepin-site/`) |
|---|---|
| Publications (the Research outputs page), research projects, the CV | `cv/cv.yml`. New publications arrive on their own; see [New publications](#new-publications). |
| Datasets | Harvard Dataverse: the Datasets section follows the collection on each build. See [Datasets](#datasets). |
| Peer reviewing in the CV | Nothing for the counts: they update on each build from ORCID. Journals ORCID lacks, and the spelling of any journal's name, are edited in the entry in `cv/cv.yml`. See [Peer reviewing](#peer-reviewing). |
| Citation counts | Nothing: they update on each build (Scopus once its API key is in the Keychain). See [Citation counts](#citation-counts). |
| The Citation reach page | Nothing for the numbers: they update on each build. The layout and text are in `pages/citation-reach.typ`, what is counted in `scripts/update-reach.py`. See [Citation reach](#citation-reach). |
| The Policy reach page | Nothing for the numbers: they update on each build. The layout and text are in `pages/policy-reach.typ`; what is counted, the key ideas listed first and the organisations left out are in `scripts/update-policy-reach.py`. See [Policy reach](#policy-reach). |
| The Media reach page | Nothing for the numbers: new articles are searched for on the first build of each month (`--search` to search sooner). To drop a false match, add its address under `ignore` in `data/media-sync.json`; to add Google Alerts, paste their feed addresses under `alert_feeds`. The searches are `QUERIES` in `scripts/update-media-reach.py`. See [Media reach](#media-reach). |
| Home page | `index.typ`. Its bio is the first-person version of the full English bio in `bio.md`; change both. |
| Media, Good Change | `pages/media.typ`, `pages/good-change.typ` |
| The Kultura Liberalna list | Nothing: it updates on each build. To fix a title or add a missed piece, edit `data/kultura-liberalna.yml`; hand edits are kept. |
| Polish political analyses | The landing page is `pages/polish-political-analyses.typ` and each analysis a page in `pages/polish-political-analyses/`. The data for the party genealogy, the Polling tracker, the Party system indicators, Party ideology, Political attitudes, Election results, Quality of democracy and Voting behaviour come from the Polish political analyses repository; their pages are `assets/party-genealogy/index.html`, `assets/polling-tracker/index.html`, `assets/party-system-indicators/index.html`, `assets/party-ideology/index.html`, `assets/political-attitudes/index.html`, `assets/election-results/index.html`, `assets/quality-of-democracy/index.html` and `assets/voting-behaviour/index.html`. See [Polish political analyses](#polish-political-analyses). |
| Teaching | The Teaching repository. Course descriptions are in `COURSE_DESCRIPTIONS` in `scripts/sync-teaching.py` and icons in `assets/teaching/<slug>.svg`. The generated `pages/teaching.typ` and `pages/teaching/` are not edited by hand. |
| Navigation bar, social links, footer | `calepin.toml` |
| Fonts and colours | `themes/site-theme/css/90_site.css` |
| Favicon | `scripts/make-favicon.py`, then run it. `favicon` in `calepin.toml` points to the SVG; `themes/site-theme/partials/site-head.html` links the PNGs. Then raise the `?v=` on those links in both files, or browsers go on showing the cached old icon. |

## Building

```sh
./calepin-site/build.sh                                     # build into docs/
cd calepin-site && ./build.sh _site && calepin serve _site --open   # local preview
```

`build.sh`:

1. adds new publications to `cv/cv.yml` and completes online-first ones it added earlier (see [New publications](#new-publications)), leaving `cv.yml` as it is if ORCID, OpenAlex or Crossref cannot be reached;
2. brings the Datasets section of `cv/cv.yml` in step with the Harvard Dataverse collection (see [Datasets](#datasets)), leaving it as it is if Dataverse cannot be reached;
3. rewrites the peer reviewing entry of `cv/cv.yml` from the reviews on ORCID (see [Peer reviewing](#peer-reviewing)), leaving it as it is if ORCID or OpenAlex cannot be reached;
4. compiles `cv/cv.typ` into `cv/cv.pdf` with Typst;
5. regenerates the Teaching pages from a `Teaching` folder next to this repository (set `TEACHING_SRC` to use another path). If the folder is missing, it warns and keeps the pages already generated;
6. copies the party genealogy, Polling tracker, Party system indicators, Party ideology, Political attitudes, Election results, Quality of democracy and Voting behaviour data from the Polish political analyses repository into `assets/party-genealogy/`, `assets/polling-tracker/`, `assets/party-system-indicators/`, `assets/party-ideology/`, `assets/political-attitudes/`, `assets/election-results/`, `assets/quality-of-democracy/` and `assets/voting-behaviour/` (see [Polish political analyses](#polish-political-analyses)), keeping the committed copies if the repository is missing;
7. adds any new Kultura Liberalna articles to `data/kultura-liberalna.yml`, falling back to the committed list if the site cannot be reached;
8. refreshes the citation counts in `data/citations.json` (see [Citation counts](#citation-counts)), keeping the previous numbers for any source it cannot read;
9. refreshes the data behind the Citation reach, Policy reach and Media reach pages, `data/reach.json`, `data/policy-reach.json` and `data/media-reach.json` (see [Citation reach](#citation-reach), [Policy reach](#policy-reach) and [Media reach](#media-reach)), keeping the previous data for any source it cannot read;
10. deletes the output folder and compiles the site into it, removes the theme and `.typ` sources Calepin copies across, and writes `CNAME`, an empty `.nojekyll` and a copy of the favicon at `/favicon.ico`.

A preview build into `_site/` (git-ignored) leaves `docs/` alone, but steps 1–9 still update `cv/cv.yml`, the CV PDF, the Teaching pages, the Polish political analyses data, the Kultura Liberalna list, the citation counts and the reach data.

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

## Peer reviewing

`scripts/update-reviews.py` runs on every build, after the datasets check, and rewrites the entry of the CV's Peer reviewing section that starts "Peer reviewer for". Its source is the peer-review section of Ben's ORCID record, which Web of Science Researcher Profiles (formerly Publons) fills when "Export peer reviews to ORCID" is on in its settings, and to which some journals add reviews directly:

- **What it writes:** the journals reviewed for, each with its number of reviews, most first, then the total and the years they span: "Peer reviewer for *East European Politics* (13 reviews), … : 60 verified reviews, 2013–2026." The CV PDF shows it; the website does not.
- **Journals:** ORCID files each review under the journal's ISSN. The script names the journal from OpenAlex, or from Crossref for an ISSN OpenAlex does not know, and adds up the reviews filed under a journal's print and online ISSNs.
- **Counting:** a review that two sources send under the same identifier counts once. One that Web of Science and the journal both send, each under its own identifier, counts twice: ORCID gives only the year, so the two cannot be told apart. Editorial roles are not counted.
- **Kept from `cv.yml`:** a journal's name as spelt in the entry, if it matches one of its names in OpenAlex, ignoring case, punctuation, "&" and a leading "The" (OpenAlex writes "Europe Asia Studies", and calls *East European Politics and Societies* by its current title). Journals in the entry with no reviews on ORCID stay in a closing "Also peer reviewer for …" sentence; one that later turns up on ORCID moves into the counted list. Other entries in the section are not touched.
- **Quarto backup:** when `quarto-site/cv/cv.yml` matches `calepin-site/cv/cv.yml`, the same change is made there.

| To | Do |
|---|---|
| Update the counts now | `python3 calepin-site/scripts/update-reviews.py` |
| Check without changing anything | `python3 calepin-site/scripts/update-reviews.py --dry-run` |
| Add a journal not on ORCID, or remove one | Edit the "Also peer reviewer for" sentence in `cv.yml`. |
| Rename a journal | Edit its name in the entry. The new spelling is kept if it still matches one of the journal's OpenAlex names; otherwise the script takes it for another journal, moves it to the "Also" sentence and puts OpenAlex's name back in the counted list. |
| Add new reviews | Nothing, once Web of Science's export to ORCID is on; otherwise press "Export peer reviews to ORCID now" in its settings. |

## Citation counts

The Citations list at the top of the Research outputs page (`pages/publications.typ`, still at `publications.html`) is a single list. The Scopus, Google Scholar and OpenAlex lines read their numbers from `calepin-site/data/citations.json`. The policy line, "Think tanks and other policy organisations", with the totals from `data/policy-reach.json` (see [Policy reach](#policy-reach)). The media line, "News media", with the totals from `data/media-reach.json` (see [Media reach](#media-reach)). `scripts/update-citations.py` runs on every build:

- **Scopus:** total citations and h-index, worked out from the citation counts of the documents under author ID 23493968300 in Elsevier's Scopus Search API. (The author profile API has these totals ready-made, but only for subscribing institutions.) It needs a free Elsevier API key from [dev.elsevier.com](https://dev.elsevier.com), which must never go in this public repo: store it in the macOS Keychain with `security add-generic-password -a "$USER" -s scopus-api-key -w` (it prompts for the key, which it calls "password data"), or set `SCOPUS_API_KEY`. Without a key, or if Elsevier refuses it, Scopus is skipped and its last numbers stay.
- **Google Scholar:** citations, h-index and i10-index (all years), read from the metrics table on the profile page.
- **OpenAlex:** citations, h-index and i10-index from its author record, looked up by Ben's ORCID iD.
- **Sage Policy Profiles:** policy citations and the number of policy documents they appear in, read from the data the public profile page loads. These are still recorded but no longer shown: the Policy line uses the wider Policy reach totals instead.

Web of Science is not listed: its author records are shown only to signed-in users, and to automated visitors only after a CAPTCHA.

Each source records the month its numbers date from. The page's "Updated" line shows the latest month, and any older source shows its own month beside its name. A source keeps its previous numbers if it cannot be reached, if its page no longer has the numbers where the script looks (for instance a Google Scholar CAPTCHA), or if any number has fallen by more than a quarter. The last case prints a `[citations] needs attention` line; if the fall is real, enter the new numbers by hand. The script copies the file to `quarto-site/data/` for the Quarto backup. `--dry-run` shows what it would change without writing anything.

The same script also fetches OpenAlex's citation count for each publication in `cv/cv.yml` into `data/publication-citations.json` (copied to `quarto-site/data/` too). The Research outputs page shows it at the end of the entry, before the full stop: "… doi:10.xxx (12 citations)." or "(1 citation)", with the count linking to OpenAlex's list of the works citing that publication (`https://openalex.org/works?filter=cites:W…`; the file stores each work's OpenAlex ID alongside its count). A publication OpenAlex has but nothing cites yet shows "(0 citations)", unlinked; one OpenAlex does not have shows "(no citation count available)". Entries are matched by DOI, and the few without one by title among the works OpenAlex links to Ben's ORCID iD. The CV PDF does not show these counts. If OpenAlex cannot be reached, or matches more than a quarter fewer publications than last time, the previous counts stay.

## Citation reach

`pages/citation-reach.html` shows where, and in which disciplines, Ben's work is cited: a world map of the countries the citing scholars are based in, bar charts of citations by discipline and subdiscipline, and a chart of citations by year, each bar divided by subdiscipline. Each chart has a table under it. The page is not in the navigation bar; the OpenAlex line of the Citations list on the Research outputs page links to it ("reach by country and discipline").

`scripts/update-reach.py` builds its data, `data/reach.json`, from OpenAlex on every build, and copies it to `quarto-site/data/`:

- **Works and citations:** every work OpenAlex links to Ben's ORCID iD, and the works that cite them. A work citing two of his counts twice, as in OpenAlex's own totals.
- **Disciplines:** each citation counts under the field (discipline) and subfield (subdiscipline) of the citing work's primary topic in OpenAlex's classification. The five largest subfields have colours of their own, shared by the subdiscipline chart and the chart by year; the rest share a grey as "Other". The discipline chart is one series, in the site's red. Citations from works with no topic show as "Unclassified" in the chart by year only.
- **Years:** by the citing work's year of publication, from the year of Ben's first work. Citations OpenAlex dates earlier are dating errors and are left out.
- **Countries:** the citing works' authors, other than Ben, each counted once in each country of the institutions they list. About a quarter of citing works list no institution.

The script works out every count, share, colour and hover text, so the page only lays them out; the Quarto backup's page (`quarto-site/R/reach-helpers.R`) reads the same file. The disciplines' colours are a palette checked for colour-blind readers; the map uses a six-step red scale ending in the site's dark red. If OpenAlex cannot be reached, or finds more than a quarter fewer citations than last time, the previous data stays. OpenAlex needs no API key for this (it pages by number, about 15 requests a run), but without one its free daily allowance is shared with everyone on the same network. `--dry-run` reports the totals without writing anything.

The map's shapes, `data/world-map.json`, come from Natural Earth's 1:110m countries (public domain), in the Equal Earth projection, with Antarctica left out; countries too small for that scale are drawn as dots. `scripts/make-world-map.py` makes the file and is run only to change the map.

## Policy reach

`pages/policy-reach.html` shows how Ben's work is taken up beyond academia. The Policy line of the Citations list on the Research outputs page links to it ("reach by organisation and country"); it is not in the navigation bar. The page has:

- a map of the countries the citing organisations are based in;
- charts of the organisations by type (think tank or foundation, government, international organisation) and by year;
- a section for each key idea (the thin ideology of populism; forging, bending and breaking; the post-illiberal trilemma) and then for Ben's other cited publications, each listing the citing documents and works with links.

Each chart has a table under it.

`scripts/update-policy-reach.py` builds its data, `data/policy-reach.json`, on every build, and copies it to `quarto-site/data/`. There are two sources:

- **Policy documents** come from Ben's public Sage Policy Profiles data, built on Overton's index: reports, briefs, blog posts and official papers citing his works, each with the publishing organisation's type and country. Some documents are left out:
  - issues of academic journals that Overton files as policy documents because a government site hosts them;
  - copies of Ben's own works;
  - second copies of the same document;
  - documents citing only works by other authors that Sage attributes to Ben.

  So it counts fewer documents than Sage's own profile lists. Overton's garbled characters and misspelt organisation names (`NAME_FIXES`) are corrected.
- **Research works** come from OpenAlex: works citing Ben's, not written by him, whose authors list an institution of OpenAlex's types `nonprofit`, `government` or `other`. Research bodies of those types (academies of sciences, national research councils, CNRS, university presses and the like) are left out by name (`EXCLUDE_NAMES`) or by OpenAlex ID (`EXCLUDE_IDS`). Add to either to drop an organisation.

An organisation found in both sources is counted once: names are matched within a country, allowing for small spelling differences, and the type is Overton's. The key ideas and the works behind them are set in `KEY_IDEAS`. If a source cannot be reached, or finds more than a quarter fewer records than last time, its records from the last run stay. `--dry-run` reports the totals without writing anything.

Only documents and works that cite Ben's publications are counted, so an idea taken up without a citation does not appear. Tracking that, especially in journalism, needs phrase searches of news archives (for example Media Cloud), which the site does not do yet.

## Media reach

`pages/media-reach.html` shows Ben's presence in the news. The Media line of the Citations list on the Research outputs page links to it ("reach by outlet and country"); it is not in the navigation bar. The page has:

- a map of the countries the outlets are based in;
- charts by outlet, language and year (each year divided into interviews and commentary, ideas, and research in the news);
- lists of the articles, with links, author and date: by key idea, by year for interviews and commentary, and for research in the news.

`scripts/update-media-reach.py` keeps `data/media-reach.json` and copies it to `quarto-site/data/`. It is an archive: articles once found stay, even after Google stops returning them. It also writes `data/media-mentions.csv`, the same articles as a spreadsheet (date, title, outlet, country or region, language, author, category, key idea, whether checked, link); `data/` is not published with the site.

- **Searches:** `QUERIES` covers:
  - Ben's name with a word tying it to his field, in English, Polish (with the case forms "Bena Stanleya", "Benem Stanleyem", "Benowi Stanleyowi"), German, French, Spanish, Italian, Portuguese, Dutch, Czech, Slovak, Hungarian, Ukrainian, Russian, Norwegian, Swedish and Estonian;
  - the key ideas' phrases in several of those languages;
  - the joint work with Stanley Bill, and Pooling the Poles.

  Each runs through Google News's RSS search (no key) in the edition of its language. A normal build asks only for the last 13 months, and searches only on the first build of each month: the date of the last search is kept under `searched` in `data/media-reach.json`, and later builds that month recount the archive and read any articles still waiting without searching. A search Google refuses partway is not recorded, so the next build tries again. `--search` searches anyway. `--backfill` asks again with no dates, then year by year from 2008 for queries with enough results; it was run once to fill the archive and takes about half an hour.
- **Checks:** each article's address is resolved from Google's link and its page read. It counts only if it names Ben (or uses the idea's phrase, or names the work) as its query requires. Its author and language are taken from the page. An unreadable page, or one that comes back as a short paywall stub without his name (as VG and Aftenposten serve automated readers), is kept and marked "not checked". Articles Ben wrote (his columns, listed on the Media page) are left out, as are pages on academic publishers' sites (`NOT_MEDIA`), which are academic citations. Rejected articles are remembered under `dropped`, so they are not fetched again. Google refuses to resolve many links in a row; any left over wait for later builds (up to 60 a build) or `--resolve`, which resolves and checks without searching.
- **False matches** the check misses go under `ignore` in `data/media-sync.json`, with a reason. Then `python3 calepin-site/scripts/update-media-reach.py --offline` recounts without asking Google.
- **Countries** are the outlets' (`OUTLET_COUNTRIES`, then the web address's country code). Pan-European (.eu) and international outlets are listed but not mapped.
- **Coverage:** Google News indexes online news, so radio, television and print-only coverage is mostly missing, as is much before about 2015.

### Google Alerts

Alerts catch new articles as they appear, including some Google News misses. To set them up:

1. Run `python3 calepin-site/scripts/update-media-reach.py --alerts`. It prints a table of the searches, each a link that opens Google Alerts with the search filled in.
2. Open each link while signed in to Google and choose "Show options":
   - **How often:** at most once a day;
   - **Sources:** automatic;
   - **Language:** the search's language;
   - **Region:** any;
   - **How many:** all results;
   - **Deliver to:** RSS feed.
3. Click "Create alert".
4. On the Alerts page, copy each alert's feed address (the RSS icon) into the `alert_feeds` list in `data/media-sync.json`.

The first build of each month then reads the feeds with the searches and files each new article under the search it came from.

## Polish political analyses

The Polish political analyses menu item leads to `pages/polish-political-analyses.typ`, a list of analyses styled like the Teaching list, each with a 480 × 300 px thumbnail in `assets/analyses/`. Each analysis has its own page in `pages/polish-political-analyses/`.

The first is Parties in the Sejm (`party-genealogy.typ`; called Party genealogy until 24 October 2026, whose name its files keep). The page embeds the chart, `assets/party-genealogy/index.html`, in an iframe. The chart is a standalone page in plain HTML, CSS and JavaScript, with no libraries, in the site's colours and Jost, and it can be opened on its own. It reads `genealogy.json` beside it, and then `defections.json`. It has three views: the Genealogy (lifespans, seat ribbons and the links between parties), Seat strength (every party's seats on each day, stacked by party family, where a click on a family or party moves it to the base of the chart) and Defections (MPs' moves between clubs by term and by flow, as in the Party tracker's `fig_defections.png`, with every move listed and each MP's record of clubs and moves). Its settings are in the page's address: `#pt=<Party tracker id>` selects a party in the Genealogy, `#v=seats&b=<base>` opens Seat strength with a family (`f-<family slug>`) or parties at the base, and `#v=def` opens Defections, with `mp=<person id>`, `p=<party id>` and `t=<term>`.

That data comes from the Party tracker in the Polish political analyses repository (`../Polish political analyses`, next to this one; set `PPA_SRC` to use another path). `Party tracker/R/63_export_genealogy_json.R` and `64_export_defections_json.R`, which its `run_all.R` runs with the other figures, write `Party tracker/output/figures/genealogy.json` and `defections.json`, and on every build `scripts/sync-analyses.py` copies them here if they have changed. If the repository or a file is missing, or a file lacks the chart's fields, the build keeps the committed copy. To update the chart: re-run the Party tracker, then `commit.R` here.

The second is the Polling tracker (`polling-tracker.typ`), the web version of the Pooling the Poles poll-of-polls, embedded the same way from `assets/polling-tracker/`. It replaced the Pooling the Poles Shiny app, retired on 22 October 2026 (kept in `Polls/legacy/shiny-app/` in that repository). The old Pooling the Poles page, `pages/pooling-the-poles.typ`, is no longer in the navigation bar; it redirects to the tracker, so old links still work. The tracker's page explains the method below the tracker. Its data comes from `Polls/` in the Polish political analyses repository: `Polls/R/export_web.R`, which `PTP.R` runs after fitting, writes `Polls/web/polls.json` (trends, polls, the weekly estimates, seats, coalition and threshold probabilities, house effects, constituencies and the shared rules from `constants.R`) and `Polls/web/draws.json` (250 of the model's vote-share draws for every week), and `sync-analyses.py` copies both here. Its Sejm term selector also shows every past term from 1991–93 to 2019–23: `Polls/R/terms/30_export.R` writes `Polls/web/terms.json` (the list of terms) and one `term-<term>.json` per past term (its trend, polls, election results, house effects and the polls' final reading against the result), and `sync-analyses.py` copies `terms.json` and every file it lists. A past term shows only the Trends and Pollsters views, with a panel comparing the polls with the result; its shares are of the parties the polls named, from a model anchored on the elections that open and close the term. The past terms are fitted once and frozen, so they change only when `Polls/R/terms/run_all.R` is rerun. The tracker loads `draws.json` only for scenarios: it applies the user's shares to each draw and re-runs the seat simulation in the browser, so scenarios keep seat ranges and probabilities; without `draws.json` they are single allocations. Its seat allocation reproduced the Shiny app's exactly (checked on 300 random scenarios, with joint lists and coalition dynamics), and its simulation matches PTP.R's within simulation noise. To update it: run `PTP.R`, then `commit.R` here.

The third is the Party system indicators (`party-system-indicators.typ`), embedded the same way from `assets/party-system-indicators/`: fragmentation, disproportionality, volatility, nationalisation, polarisation and government closure for every Sejm election since 1991 and for the current polls, in one scrolling page with a scorecard and a section per dimension. Its data comes from `Party system analysis/` in the Polish political analyses repository: `analysis/export_web.R`, which `Party system analysis.R` runs at the end, writes `Party system analysis/web/party_system.json` (the indicators per election, the lists, lineages, nationalisation scores, CHES positions and cabinets behind them, the closure index and a codebook), and `sync-analyses.py` copies it here. The polls column is computed in the browser from the Polling tracker's `polls.json`, so it follows the tracker without a re-export; `party_system.json` carries a snapshot as a fallback. `systemStats()` in `app.js` mirrors `system_stats()` in the analysis (checked to agree to 1e-5), so a change to one needs the other. To update it: run `Party system analysis.R`, then `commit.R` here.

The fourth is Party ideology (`party-ideology.typ`), embedded the same way from `assets/party-ideology/`: where Polish parties stand in European ideological space from the Chapel Hill Expert Survey (1999–2024), with each party's PopuList 4.0 classification, in five views (a map on any two dimensions, a party profile, populism, Polish parties over time, and polarisation and alignment by country) and a panel for the selected party. Its data comes from `Party ideology/` in the Polish political analyses repository: `analysis/Party ideology.R` writes `Party ideology/web/ideology.json` (CHES positions for every party and wave, the spread of the 2024 expert ratings, PopuList periods and status, Party tracker records of the Polish parties, and polarisation and alignment by country), and `sync-analyses.py` copies it here. The panel shows each Polish party's current polling from the Polling tracker's `polls.json`, and links to the party genealogy with `#pt=<Party tracker id>`; the genealogy links back the same way. To update it: run `Party ideology.R`, then `commit.R` here.

The fifth is Social and political trends (`political-attitudes.typ`; called Political attitudes until 23 October 2026), embedded the same way from `assets/political-attitudes/`: long-run trends in Polish society and public opinion since 1990, each a monthly latent estimate pooled from many surveys, with breakdowns by party preference, age, gender, education and place of residence, survey readings as dots and an event timeline. The first attitude is support for EU membership. Its data comes from `Political attitudes/` in the Polish political analyses repository: each attitude's `R/run_all.R` ends with `web/build_web.R`, which writes `Political attitudes/web/attitudes.json` (the list of attitudes) and one `<slug>.json` per attitude, and `sync-analyses.py` copies the index and every file it lists here. The page is generic, so a new attitude needs no change in this repository. To update it: run the attitude's `run_all.R`, then `commit.R` here.

The sixth is Election results (`election-results.typ`), embedded the same way from `assets/election-results/`: every presidential, Sejm, European Parliament and referendum vote since 1990.
- **Each election:** its facts and results, its seats (a hemicycle, and seats by constituency), and maps by today's gminas or by constituency. The measures are the leader, any list's share, a head to head, turnout, and the change since any other election by lineage, candidate or camp.
- **"Widać zabory!"** draws the borders of 1914 between the partitions.
- **Histories:** a gmina search and every gmina's and constituency's history.
- **Votes by local characteristics:** a scatter of the gminas against GUS covariates, chosen from a grouped drop-down (46 in ten groups), with its own list or candidate selector (kept in step with the map's) and points coloured by result, by 1914 partition zone or by side of the 1937 border.
- **All elections:** a table and a national turnout chart.
- **"Polls now":** the Polling tracker's latest estimate, read in the browser from its `polls.json`.

Its data comes from `Election results/` in the Polish political analyses repository. `analysis/run_all.R`'s last stage, `90_export.R`, writes `Election results/web/`. `sync-analyses.py` copies `elections.json` and every file its `files` lists, each checked for its fields: `geo.json`, `covariates.json`, the `covariates-<group>.json` files, one `<election-id>.json` per election and one `profiles-<voivodeship>.json` per voivodeship. The page links to the party genealogy and Party ideology by Party tracker id. The Party system indicators' nationalisation chart and the Polling tracker's constituency card link back to it, by committee id (`#e=sejm-<year>&it=<id>`) and by constituency (`#u=c<number>`). To update it: run `Election results/analysis/run_all.R`, then `commit.R` here.

The seventh is Quality of democracy (`quality-of-democracy.typ`), embedded the same way from `assets/quality-of-democracy/`: Polish democracy from 1990 to 2025 in V-Dem's expert ratings (version 16), against the other Central and Eastern European EU members and Western Europe (the other EU members and the United Kingdom), in five views: an overview of the five democracy indices with Regimes of the World above them, the indicators behind them lens by lens, Poland in context (the regional median and middle half, up to five chosen countries, and a ranked table), polarisation (three V-Dem measures, two of them reversed so that up means more polarised, beside the PiS–PO/KO distance and opposed-line rate in the Sejm's roll calls), and methods and data. Every chart shows V-Dem's 68% intervals, shares one selected year and has a context strip of Sejm elections, cabinets, presidents and events beneath it. Its data comes from `Quality of democracy/` in the Polish political analyses repository: `analysis/Quality of democracy.R` runs `analysis/export_web.R`, which writes `Quality of democracy/web/democracy.json`, `democracy-countries.json` and `political-context.json`, and `sync-analyses.py` copies all three here. Its own settings are in the page's address (`#view=`, `#m=`, `#r=`, `#c=`, `#ev=` and others); its strip links to the Party system indicators (`#e=<year>`, `&s=government`) and the party genealogy (`#pt=`), and its Polarisation view to the Party system indicators and Party ideology. To update it: run `Quality of democracy.R`, then `commit.R` here.

The eighth is Voting behaviour (`voting-behaviour.typ`), embedded the same way from `assets/voting-behaviour/`: who votes for whom in the Sejm elections from 1997 to 2023, and who stays at home, by age, gender, education, the size of the place of residence, religious attendance and socio-economic position, from the Polish General Election Studies, in four views: who votes (turnout and each list's survey share beside the official result, split by any characteristic, and who each list's voters are), what changes the vote (average marginal effects with 90% intervals, in one election and across elections, with an option to adjust to the official results), try a voter (a calculator that computes the probability of staying at home or voting for each list from 200 posterior draws in the browser, with refusal rules for impossible or thinly supported profiles and a chart of the same person at every election) and the model and data (the sample, the survey's gaps from the official results, how the model was chosen and the checks on voters who give no list). Its data comes from `Voting behaviour/` in the Polish political analyses repository: `analysis/70_export_web.R` writes `Voting behaviour/web/voting.json` and `calc-<year>.json`, and `sync-analyses.py` copies them here (the calculator files through the `files` list in `voting.json`). Its own settings are in the page's address (`#view=`, `#show=`, `#sp=`, `#ct=`, `#el=`, `#age=` and others). The citation for the Polish General Election Studies is still to be added to the method note. To update it: run `Voting behaviour/analysis/run_all.R` (or `70_export_web.R` alone), then `commit.R` here.

On a computer screen or a landscape tablet the list is in two columns. Each analysis page has its introduction, then its method ("How this works") as a collapsed note, with any sources listed under "Sources:" and then its suggested citation, then the app. The apps label their charts in thin Jost, and their PNG downloads go through `assets/chart-export.js`, which keeps those labels in Jost in the image (an SVG drawn on a canvas otherwise falls back to a serif face), sets each export's title like the site's headings, and draws it at four times its size on screen (less for a chart so large that Safari would refuse the canvas).

Every analysis is cited the same way: "Ben Stanley, 2026. *Page name*, **Polish Political Analyses**, benstanley.eu." Each page has it at the end of its note (Election results also in the app's "Data and citation" card). Each app names its page in `PAGE` at the top of its `app.js`, and `chart-export.js` sets the citation under every PNG (`drawCite()`, with the data source on the line above where there is one) and as the first line of every CSV (`csvCite()`: `# Ben Stanley, 2026. …`, which R's `fread`, and pandas or readr with `comment = "#"`, skip). The year and series name are `CITE_YEAR` and `SERIES` there; the pages' `.typ` files repeat the citation in full, so change them too.

To add an analysis: a page in `pages/polish-political-analyses/`, a thumbnail in `assets/analyses/`, and a row for it in `pages/polish-political-analyses.typ`. A new app that offers a PNG download should load `../chart-export.js`, build its image from `ChartExport.clone(svg)` sized with `ChartExport.setSize()`, draw it at `ChartExport.scale(W, H)` and end it with `ChartExport.drawCite()`; one that offers a CSV should start it with `ChartExport.csvCite(PAGE)`.

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

1. Clone the repo: `git clone https://github.com/BDStanley/BDStanley.github.io.git`. The download is about 800 MB, almost all of it history. For the Teaching pages, put the Teaching repository beside it in a folder named `Teaching`; for the party genealogy, the Polling tracker, the Party system indicators, Party ideology, Political attitudes and Election results, the Polish political analyses repository in a folder named `Polish political analyses`.
2. Install Typst (`brew install typst`), Calepin (see `calepin-site/README.md` → Prerequisites), Python 3 and the Jost font.
3. For the Scopus citation count, add the Elsevier API key to the Keychain (see [Citation counts](#citation-counts)); without it the build skips Scopus.
4. For `commit.R`, install R (the script installs the `here` package itself), the `claude` command-line tool (on the `PATH` or at `~/.npm-global/bin/claude`) and Homebrew's `rsync`, which it expects at `/opt/homebrew/bin/rsync`. It adds `/opt/homebrew/bin` and `~/.cargo/bin` to the `PATH`, so Typst and Calepin installed there are found. The iCloud path in step 5 is hard-coded.

## Working with Claude Code

`.claude/skills/add-teaching-course/SKILL.md` covers adding a course to the Teaching section: drafting its description, making its icon, rebuilding and checking the page, and mirroring both into the Quarto backup. It also sets out the rule that the Calepin site leads and `quarto-site/` follows.
