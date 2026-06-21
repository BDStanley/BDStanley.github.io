# Automatic "Recent" Sections

This website has automatic "Recent" sections that display recent media appearances and publications.

## Media (media.qmd)

The media page uses a **Lua filter** (`recent_media.lua`) that automatically generates a "Recent" section with items from the last 6 months.

- **How it works**: The filter runs during Quarto rendering and scans all media items, looking for dates in formats like `(DD.MM.YYYY)` or `(DD/MM/YYYY)` at the end of list items.
- **Automatic**: Updates automatically every time you render the site. No manual intervention needed.
- **Cutoff**: Items from the last 6 months (rolling window).

## Publications (publications.qmd)

The publications page uses an **R script** (`generate_recent_publications.R`) that generates a "Recent" section with publications from the current and previous year (if in first half of year).

### Automatic Updates:

The script runs **automatically** as a pre-render step whenever you render the site:

```bash
quarto render
```

The script will:
1. Scan all publication files (journal_articles.qmd, books.qmd, book_chapters.qmd, essays_interviews_reviews.qmd)
2. Extract publications from the cutoff year onwards
3. Generate `recent_publications.qmd` with the results
4. This file is automatically included in the publications page

### Manual Updates (optional):

You can also run the script manually if needed:

```bash
Rscript generate_recent_publications.R
```

This is useful for testing or if you want to update just the publications without rendering the entire site.

### How it works:

- **Current date**: January 2026
- **Cutoff logic**: If in first half of year (Jan-Jun), include previous year. If in second half (Jul-Dec), only include current year.
- **Currently showing**: Publications from 2025 onwards

## Files

- `recent_media.lua` - Lua filter for media page
- `recent_publications.lua` - Lua filter for publications (not used - R script approach used instead)
- `generate_recent_publications.R` - R script to generate recent publications
- `recent_publications.qmd` - Generated file (do not edit manually)
