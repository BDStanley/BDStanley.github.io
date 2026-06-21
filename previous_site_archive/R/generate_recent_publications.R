#!/usr/bin/env Rscript
# Script to generate recent_publications.qmd with publications from the last 6 months
# Run this script to update the recent publications list

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cran.r-project.org")
}

pkgs <- c(
  "tidyverse",
  "here"
)
missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  pak::pkg_install(missing_pkgs, ask = FALSE)
}
invisible(lapply(pkgs, library, character.only = TRUE))

# Function to extract year from a publication line
extract_year <- function(text) {
  # Match pattern: author names followed by period/comma, space, 4-digit year, period
  # This works for: "Stanley, Ben. 2025." or "Ben Stanley. 2020."
  year_match <- str_extract(text, "[.,] (\\d{4})\\.")
  if (is.na(year_match)) return(NA)

  year <- str_extract(year_match, "\\d{4}")
  return(as.numeric(year))
}

# Get current date info
current_date <- Sys.Date()
current_year <- as.numeric(format(current_date, "%Y"))
current_month <- as.numeric(format(current_date, "%m"))

# Calculate cutoff year (if we're in first half, include previous year too)
cutoff_year <- if (current_month <= 6) current_year - 1 else current_year

# Read all publication files
pub_files <- c(
  here("includes", "journal_articles.qmd"),
  here("includes", "books.qmd"),
  here("includes", "book_chapters.qmd"),
  here("includes", "essays_interviews_reviews.qmd")
)

all_pubs <- tibble()

for (file in pub_files) {
  if (file.exists(file)) {
    lines <- read_lines(file)

    # Filter out empty lines
    lines <- lines[lines != ""]

    for (line in lines) {
      year <- extract_year(line)
      if (!is.na(year) && year >= cutoff_year) {
        all_pubs <- bind_rows(all_pubs, tibble(
          text = line,
          year = year
        ))
      }
    }
  }
}

# Sort by year (descending)
all_pubs <- all_pubs %>%
  arrange(desc(year))

# Generate the output file
if (nrow(all_pubs) > 0) {
  output <- c(
    "### Recent",
    ""
  )

  # Add publications with blank lines between them
  for (i in 1:nrow(all_pubs)) {
    output <- c(output, all_pubs$text[i], "")
  }

  output <- c(output, "---", "")

  write_lines(output, here("includes", "recent-publications.qmd"))

  cat(sprintf("Generated includes/recent-publications.qmd with %d publications from %d onwards\n",
              nrow(all_pubs), cutoff_year))
} else {
  cat("No recent publications found\n")
}
