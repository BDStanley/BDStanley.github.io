# R Web Scraper for Ben Stanley Articles on kulturaliberalna.pl
# Returns results as QMD format

# Load required libraries
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(xml2)

# Simple, direct scraper function for Ben Stanley articles
scrape_ben_stanley_articles <- function() {
  
  # Base configuration
  base_url <- "https://kulturaliberalna.pl"
  author_url <- "https://kulturaliberalna.pl/autor/ben-stanley"
  
  # User agent to appear more like a browser
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
  
  # Initialize results
  articles <- data.frame(
    title = character(),
    date = character(),
    url = character(),
    stringsAsFactors = FALSE
  )
  
  cat("Scraping Ben Stanley author page directly...\n")
  
  tryCatch({
    # Make request with proper headers
    response <- GET(author_url, 
                    add_headers(`User-Agent` = user_agent),
                    timeout(30))
    
    if (status_code(response) == 200) {
      page <- read_html(response)
      
      # Get all links from the author page
      all_links <- html_nodes(page, "a")
      
      for (link in all_links) {
        href <- html_attr(link, "href")
        title <- html_text(link, trim = TRUE)
        
        # Skip empty or very short titles
        if (is.na(href) || is.na(title) || nchar(title) < 10) next
        
        # Convert relative URLs to absolute
        url <- ifelse(startsWith(href, "http"), href, paste0(base_url, href))
        
        # Check if this looks like an article URL
        if (is_article_url(url)) {
          # Extract date from URL
          date <- extract_date_from_url(url)
          
          # Add to results (since we're on Ben Stanley's author page, we assume all articles are his)
          articles <- bind_rows(articles, data.frame(
            title = title,
            date = date,
            url = url,
            stringsAsFactors = FALSE
          ))
          
          cat("Found article:", title, "\n")
        }
      }
      
    } else {
      cat("Failed to load author page, status:", status_code(response), "\n")
    }
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
  })
  
  # Remove duplicates based on URL and clean up
  articles <- articles %>%
    distinct(url, .keep_all = TRUE) %>%
    filter(!is.na(title), !is.na(url), title != "", url != "")
  
  cat("Found", nrow(articles), "total articles\n")
  return(articles)
}

# Simplified article URL validation - more restrictive
is_article_url <- function(url) {
  # Must be from kulturaliberalna.pl domain
  if (!str_detect(url, "kulturaliberalna\\.pl")) {
    return(FALSE)
  }
  
  # Exclude non-article pages - be more strict
  exclude_patterns <- c(
    "/wp-content/", "/wp-admin/", "\\.(jpg|jpeg|png|gif|pdf|doc|docx)$",
    "/feed/", "/comments/", "/trackback/", "#comment", "/autor/", "/tag/",
    "/category/", "/kategoria/", "/kontakt", "/o-nas", "/regulamin", 
    "/polityka", "/reklama", "/newsletter", "/donacje", "/wsparcie",
    "facebook\\.com", "twitter\\.com", "instagram\\.com", "youtube\\.com",
    "kulturaliberalna\\.pl/$", # homepage
    "kulturaliberalna\\.pl$"   # homepage without slash
  )
  
  for (pattern in exclude_patterns) {
    if (str_detect(tolower(url), pattern)) {
      return(FALSE)
    }
  }
  
  # Only include URLs that clearly look like articles
  # Must contain a year (articles are dated) OR specific article keywords
  include_patterns <- c(
    "/\\d{4}/\\d{2}/\\d{2}/", # YYYY/MM/DD format
    "/\\d{4}/\\d{2}/",        # YYYY/MM format
    "/artykul/", 
    "/tekst/", 
    "/post/", 
    "/publikacja/", 
    "/wywiad/", 
    "/komentarz/",
    "/opinia/",
    "/raport/"
  )
  
  for (pattern in include_patterns) {
    if (str_detect(url, pattern)) {
      return(TRUE)
    }
  }
  
  # Additional check: if URL contains a year (2000-2025) and is reasonably long, likely an article
  if (str_detect(url, "/\\d{4}/") && nchar(url) > 60) {
    year_match <- str_extract(url, "\\d{4}")
    if (!is.na(year_match) && as.numeric(year_match) >= 2015 && as.numeric(year_match) <= 2025) {
      return(TRUE)
    }
  }
  
  # Default to FALSE - be conservative
  return(FALSE)
}

# Extract date from URL (YYYY/MM/DD format)
extract_date_from_url <- function(url) {
  # Look for YYYY/MM/DD pattern in URL
  date_match <- str_extract(url, "\\d{4}/\\d{2}/\\d{2}")
  if (!is.na(date_match)) {
    # Convert to readable format
    date_parts <- str_split(date_match, "/")[[1]]
    return(paste(date_parts[1], date_parts[2], date_parts[3], sep = "-"))
  }
  
  # Look for YYYY/MM pattern
  date_match <- str_extract(url, "\\d{4}/\\d{2}")
  if (!is.na(date_match)) {
    date_parts <- str_split(date_match, "/")[[1]]
    return(paste(date_parts[1], date_parts[2], sep = "-"))
  }
  
  # Look for just YYYY
  year_match <- str_extract(url, "\\d{4}")
  if (!is.na(year_match) && as.numeric(year_match) >= 2000 && as.numeric(year_match) <= 2025) {
    return(year_match)
  }
  
  return("Date not found")
}

# Enhance article details by visiting individual pages
enhance_article_details <- function(articles, user_agent) {
  enhanced_articles <- articles
  
  for (i in 1:nrow(articles)) {
    cat("Enhancing article", i, "of", nrow(articles), "\n")
    
    tryCatch({
      response <- GET(articles$url[i], 
                      add_headers(`User-Agent` = user_agent),
                      timeout(30))
      
      if (status_code(response) == 200) {
        page <- read_html(response)
        
        # Try to get better date if not already found
        current_date <- articles$date[i]
        if (current_date == "Date not found") {
          # Try to extract date from page elements
          date_selectors <- c(
            "time[datetime]", ".date", ".published", 
            "[property='article:published_time']", ".meta-date",
            ".entry-date", ".post-date"
          )
          
          for (sel in date_selectors) {
            date_node <- html_node(page, sel)
            if (!is.null(date_node)) {
              datetime_attr <- html_attr(date_node, "datetime")
              if (!is.na(datetime_attr)) {
                enhanced_articles$date[i] <- datetime_attr
                break
              }
              date_text <- html_text(date_node, trim = TRUE)
              if (!is.na(date_text) && nchar(date_text) > 0) {
                enhanced_articles$date[i] <- date_text
                break
              }
            }
          }
        }
        
        # Try to get better title if current title is very short
        if (nchar(articles$title[i]) < 20) {
          title_selectors <- c("h1", "h2", ".title", ".headline", "[property='og:title']")
          
          for (sel in title_selectors) {
            title_node <- html_node(page, sel)
            if (!is.null(title_node)) {
              title_text <- html_text(title_node, trim = TRUE)
              if (!is.na(title_text) && nchar(title_text) > nchar(articles$title[i])) {
                enhanced_articles$title[i] <- title_text
                break
              }
            }
          }
        }
      }
      
      Sys.sleep(1) # Be respectful
      
    }, error = function(e) {
      cat("Error enhancing article", i, ":", e$message, "\n")
    })
  }
  
  return(enhanced_articles)
}

# Create QMD output
create_qmd_output <- function(articles) {
  if (nrow(articles) == 0) {
    return("No articles found by Ben Stanley.")
  }
  
  # Sort articles by date (newest first)
  articles <- articles %>%
    arrange(desc(date))
  
  # Create QMD header
  qmd_content <- "---\neditor: visual\n---\n\n"
  
  # Add title
  qmd_content <- paste0(qmd_content, "### Kultura Liberalna\n\n")
  
  # Create article list with proper spacing to avoid tight list
  article_lines <- character(nrow(articles))
  
  for (i in 1:nrow(articles)) {
    # Format date
    formatted_date <- format_date_for_qmd(articles$date[i])
    
    # Create the line: -   [Title](URL) (DD.MM.YYYY)
    article_lines[i] <- sprintf(
      "-   [%s](%s) (%s)",
      articles$title[i],
      articles$url[i],
      formatted_date
    )
  }
  
  # Combine all content with double line breaks to prevent tight list
  qmd_content <- paste0(qmd_content, paste(article_lines, collapse = "\n\n"))
  
  return(qmd_content)
}

# Format date for QMD output (DD.MM.YYYY)
format_date_for_qmd <- function(date_string) {
  if (is.na(date_string) || date_string == "Date not found") {
    return("Date not found")
  }
  
  # Try to parse different date formats
  tryCatch({
    # If it's already in YYYY-MM-DD format
    if (str_detect(date_string, "^\\d{4}-\\d{2}-\\d{2}$")) {
      date_parts <- str_split(date_string, "-")[[1]]
      return(paste(date_parts[3], date_parts[2], date_parts[1], sep = "."))
    }
    
    # If it's in YYYY/MM/DD format
    if (str_detect(date_string, "^\\d{4}/\\d{2}/\\d{2}$")) {
      date_parts <- str_split(date_string, "/")[[1]]
      return(paste(date_parts[3], date_parts[2], date_parts[1], sep = "."))
    }
    
    # If it's in DD.MM.YYYY format already
    if (str_detect(date_string, "^\\d{2}\\.\\d{2}\\.\\d{4}$")) {
      return(date_string)
    }
    
    # Try to parse as date and format
    parsed_date <- as.Date(date_string)
    if (!is.na(parsed_date)) {
      return(format(parsed_date, "%d.%m.%Y"))
    }
    
    # If all else fails, return original
    return(date_string)
    
  }, error = function(e) {
    return(date_string)
  })
}

# Main execution function
main <- function() {
  cat("Ben Stanley Article Scraper for kulturaliberalna.pl\n")
  cat(rep("=", 50), "\n")
  
  # Scrape articles using simple method
  articles <- scrape_ben_stanley_articles()
  
  # Display summary
  cat("\nScraping completed!\n")
  cat("Found", nrow(articles), "articles by Ben Stanley\n\n")
  
  # Create and save QMD output
  qmd_output <- create_qmd_output(articles)
  
  # Save QMD to file
  writeLines(qmd_output, "kultura_liberalna_articles.qmd")
  cat("QMD output saved to kultura_liberalna_articles.qmd\n")
  
  return(qmd_output)
}

# Run the scraper
result <- main()