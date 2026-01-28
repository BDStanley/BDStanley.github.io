# Automatic "Recent" Sections

This website has automatic "Recent" sections that display recent media appearances and publications.

## Media (media.qmd)

The media page uses a **Lua filter** (`recent_media.lua`) that automatically generates a "Recent" section with items from the last 6 months.

- **How it works**: The filter runs during Quarto rendering and scans all media items, looking for dates in formats like `(DD.MM.YYYY)` or `(DD/MM/YYYY)` at the end of list items.
- **Automatic**: Updates automatically every time you render the site. No manual intervention needed.
- **Cutoff**: Items from the last 6 months (rolling window).

## Publications (publications.qmd)

The publications page uses an **R script** (`generate_recent_publications.R`) that generates a "Recent" section with publications from the current and previous year (if in first half of year).

### How to update recent publications:

```bash
Rscript generate_recent_publications.R
```

This will:
1. Scan all publication files (journal_articles.qmd, books.qmd, book_chapters.qmd, essays_interviews_reviews.qmd)
2. Extract publications from the cutoff year onwards
3. Generate `recent_publications.qmd` with the results
4. This file is automatically included in the publications page

### When to run it:

- After adding new publications to any of the publication files
- At the start of each new year
- Whenever you want to refresh the recent publications list

### How it works:

- **Current date**: January 2026
- **Cutoff logic**: If in first half of year (Jan-Jun), include previous year. If in second half (Jul-Dec), only include current year.
- **Currently showing**: Publications from 2025 onwards

## Files

- `recent_media.lua` - Lua filter for media page
- `recent_publications.lua` - Lua filter for publications (not used - R script approach used instead)
- `generate_recent_publications.R` - R script to generate recent publications
- `recent_publications.qmd` - Generated file (do not edit manually)
