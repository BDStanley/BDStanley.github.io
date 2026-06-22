---
name: add-teaching-course
description: Onboard new (or fix incomplete) Teaching courses on benstanley.eu after they appear in the Teaching repo. Use when the user says they added/updated a class in the Teaching repo (BDStanley/Classes, the local ../../Teaching checkout), asks to "sync the teaching page" / "add the new course(s) to the website", or when a build prints "[teaching] needs attention" or the landing shows a course with the placeholder hexsticker or no description. Reads the Teaching repo READ-ONLY, drafts a Polish/English description, creates a hexsticker SVG, then rebuilds and verifies.
---

# Add a Teaching course to benstanley.eu

The Teaching section is generated from the Teaching repo (a Quarto site, `BDStanley/Classes`,
checked out locally at `../../Teaching` relative to `calepin-site/`) by
`calepin-site/scripts/sync-teaching.py`, which `calepin-site/build.sh` runs before the
Calepin compile. **The Teaching repo is the source of truth and must never be modified by
this site.** Everything below happens inside `calepin-site/`.

A course's **slug** is the `.qmd` basename used in the Teaching repo's `index.qmd`
(`### [Foo Bar](foo-bar.qmd)` → slug `foo-bar`). It is the key for every per-course value.

## What's automatic vs. what this skill supplies

Running `./build.sh` already handles, for any course listed under a term in
`../../Teaching/index.qmd`:
- the landing row and the per-course page (slide/handout links → the Netlify teaching site);
- the **term badge(s)** — Polish for `lang: pl` courses ("Zima 2025" / "Lato 2026"), English otherwise; a course relisted under another term just gains another badge;
- the course's **language** — read from the first slide deck's (`<slug>-1-slides.qmd`) `lang:` field — which sets whether the term badges render in Polish or English.

Two things are NOT auto-derivable; the build prints a `needs attention` line listing them
per course, and this skill supplies them:

1. **Description** — authored, one short sentence in the course's language → the
   `COURSE_DESCRIPTIONS` dict in `scripts/sync-teaching.py`.
2. **Hexsticker icon** → a per-course SVG at `assets/teaching/<slug>.svg` (otherwise the
   shared `assets/teaching/_placeholder.svg` is used).

## Procedure

1. **See what's needed.** From `calepin-site/`, run `python3 scripts/sync-teaching.py`
   (or `./build.sh`). Read the `[teaching] needs attention:` lines — each names a slug and
   what it's missing (`description`, `icon`). If the new course isn't listed at all,
   it isn't in `../../Teaching/index.qmd` yet — tell the user to add it there (under the right
   term); the website never edits the Teaching repo.

2. **Description** (if flagged). Read `../../Teaching/<slug>.qmd` (its lecture/slide topics)
   and the first slide deck for `lang:`. Draft ONE concise sentence summarising the topics, in
   the course's language (Polish for `lang: pl`, English for `lang: en`), matching the tone of
   the existing `COURSE_DESCRIPTIONS` entries. Add it there keyed by slug. Tell the user it's a
   draft to review.

3. **Hexsticker** (if flagged). Pick a fitting Lucide icon (https://lucide.dev) — propose 1–2
   options and avoid duplicating an in-use icon unless the user wants it (see list below).
   Fetch its exact path data:
   `curl -s https://cdn.jsdelivr.net/npm/lucide-static@latest/icons/<icon>.svg`
   Create `assets/teaching/<slug>.svg` from the template below, pasting the icon's inner
   elements (the `<path>`/`<circle>`/`<rect>` lines, without their own stroke/fill — they
   inherit) into the `<g>`. Keep the hexagon geometry and `#8b0000`/white colours identical to
   `assets/teaching/_placeholder.svg`.

4. **Rebuild & verify.** Run `./build.sh`; confirm the course is no longer in `needs attention`.
   Then serve and screenshot the landing:
   `cd ../docs && (python3 -m http.server 8920 &) ; sleep 1` then headless Chrome
   `--screenshot` of `http://localhost:8920/pages/teaching.html` (use
   `--force-device-scale-factor=2` for a crisp shot); kill the server after. Check the new row:
   icon, name, description, badge(s). Do not commit unless asked.

## Hexsticker template

White hexagon, `#8b0000` border + red icon. Paste the Lucide inner elements into the `<g>`:

```svg
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 115" role="img" aria-label="<Course title>">
  <!-- icon: lucide <name> · white hex, #8b0000 border + icon (matches _placeholder.svg) -->
  <polygon points="50,3 95,29 95,86 50,112 5,86 5,29"
           fill="#ffffff" stroke="#8b0000" stroke-width="6" stroke-linejoin="round"/>
  <g transform="translate(29.5,37) scale(1.71)" fill="none" stroke="#8b0000"
     stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
    <!-- PASTE Lucide inner <path>/<circle>/<rect> elements here -->
  </g>
</svg>
```

The `translate(29.5,37) scale(1.71)` centres any standard 24×24 Lucide icon inside the hex —
reuse it verbatim.

## Icons already in use (avoid accidental duplicates)

- `landmark` — Unia Europejska i jej instytucje
- `languages` — Język angielski w zastosowaniu zawodowym
- `newspaper` — both Podstawy dziennikarstwa danych & Dziennikarstwo danych (intentional)
- `users-round` — Social Psychology of Democracies in Transition
- `chart-column` — Wprowadzenie do metodologii badań społecznych
- `chart-scatter` — Introduction to Social Research Methodology
- `hand-fist` — Wyzwania życia publicznego: uprzedzenia
- `vote` — Systemy polityczne

## Notes

- Generator output is deterministic — rebuilding with an unchanged Teaching repo produces
  byte-identical pages (no spurious git churn).
- A course's **code and university are no longer rendered** on the landing — rows show
  icon · name · description · badge(s) only. `sync-teaching.py` still derives them (slide
  `footer:` `(CODE)` / the `COURSE_CODES` fallback / author `affiliation:`) but keeps them
  dormant, so you never need to ask the user for a code when onboarding a new course.
- The generated `pages/teaching.typ` + `pages/teaching/*.typ` and the `assets/teaching/*.svg`
  are committed as part of the normal flow; they are not hand-edited.
- Overrides for source/host exist if ever needed: env vars `TEACHING_SRC` (default
  `../../Teaching`) and `TEACHING_NETLIFY_BASE` (default `https://bdstanley.netlify.app`).
