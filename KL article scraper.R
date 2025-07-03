# R Web Scraper for Ben Stanley Articles on kulturaliberalna.pl
# Returns results as HTML list

# Load required libraries
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(xml2)
library(htmltools)

# Main scraper function
scrape_ben_stanley_articles <- function() {
  
  # Base configuration
  base_url <- "https://kulturaliberalna.pl"
  author_name <- "Ben Stanley"
  
  # User agent to appear more like a browser
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
  
  # Initialize results
  articles <- data.frame(
    title = character(),
    date = character(),
    url = character(),
    stringsAsFactors = FALSE
  )
  
  cat("Starting search for Ben Stanley articles...\n")
  
  # Try different search approaches
  search_patterns <- c(
    paste0(base_url, "/szukaj?q=", URLencode("Ben Stanley")),
    paste0(base_url, "/autor/ben-stanley"),
    paste0(base_url, "/tag/ben-stanley"),
    paste0(base_url, "/search/", URLencode("Ben Stanley"))
  )
  
  for (search_url in search_patterns) {
    cat("Trying URL:", search_url, "\n")
    
    tryCatch({
      # Make request with proper headers
      response <- GET(search_url, 
                      add_headers(`User-Agent` = user_agent),
                      timeout(30))
      
      if (status_code(response) == 200) {
        page <- read_html(response)
        new_articles <- extract_articles_from_page(page, base_url)
        
        if (nrow(new_articles) > 0) {
          articles <- bind_rows(articles, new_articles)
          cat("Found", nrow(new_articles), "articles from this page\n")
        }
      } else {
        cat("HTTP status:", status_code(response), "\n")
      }
      
      # Be respectful - wait between requests
      Sys.sleep(2)
      
    }, error = function(e) {
      cat("Error with URL", search_url, ":", e$message, "\n")
    })
  }
  
  # Remove duplicates based on URL
  articles <- articles %>%
    distinct(url, .keep_all = TRUE) %>%
    filter(!is.na(title), !is.na(url), title != "", url != "")
  
  # Enhance article details if we found some
  if (nrow(articles) > 0) {
    cat("Enhancing article details...\n")
    articles <- enhance_article_details(articles, user_agent)
  }
  
  return(articles)
}

# Function to extract articles from a page
extract_articles_from_page <- function(page, base_url) {
  articles <- data.frame(
    title = character(),
    date = character(),
    url = character(),
    stringsAsFactors = FALSE
  )
  
  # Multiple selectors to try for articles
  article_selectors <- c(
    "article",
    ".article-item",
    ".post-item", 
    ".entry",
    "div[class*='article']",
    "div[class*='post']",
    ".content-item",
    ".story"
  )
  
  for (selector in article_selectors) {
    article_nodes <- html_nodes(page, selector)
    
    if (length(article_nodes) > 0) {
      cat("Using selector:", selector, "- found", length(article_nodes), "nodes\n")
      
      for (node in article_nodes) {
        article_info <- extract_single_article(node, base_url)
        if (!is.null(article_info) && is_ben_stanley_article(article_info, node)) {
          articles <- bind_rows(articles, article_info)
        }
      }
      break # Use first working selector
    }
  }
  
  # Fallback: extract from all links
  if (nrow(articles) == 0) {
    articles <- extract_from_links(page, base_url)
  }
  
  return(articles)
}

# Extract information from a single article node
extract_single_article <- function(node, base_url) {
  tryCatch({
    # Extract title
    title_selectors <- c("h1", "h2", "h3", ".title", ".headline", "a")
    title <- NULL
    
    for (sel in title_selectors) {
      title_node <- html_node(node, sel)
      if (!is.null(title_node)) {
        title <- html_text(title_node, trim = TRUE)
        if (!is.na(title) && nchar(title) > 5) break
      }
    }
    
    # Extract URL
    link_node <- html_node(node, "a")
    url <- NULL
    if (!is.null(link_node)) {
      href <- html_attr(link_node, "href")
      if (!is.na(href)) {
        url <- ifelse(startsWith(href, "http"), href, paste0(base_url, href))
      }
    }
    
    # Only proceed if we have valid title and URL, and URL is a valid article
    if (!is.null(title) && !is.null(url) && !is.na(title) && !is.na(url) && is_article_url(url)) {
      
      # Extract date - first try from URL, then from page elements
      date <- extract_date_from_url(url)
      
      # If no date in URL, try page elements
      if (date == "Date not found") {
        date_selectors <- c("time", ".date", ".published", "[datetime]", ".meta-date")
        
        for (sel in date_selectors) {
          date_node <- html_node(node, sel)
          if (!is.null(date_node)) {
            # Try datetime attribute first
            datetime_attr <- html_attr(date_node, "datetime")
            if (!is.na(datetime_attr)) {
              date <- datetime_attr
              break
            }
            # Try text content
            date_text <- html_text(date_node, trim = TRUE)
            if (!is.na(date_text) && nchar(date_text) > 0) {
              date <- date_text
              break
            }
          }
        }
      }
      
      return(data.frame(
        title = title,
        date = date,
        url = url,
        stringsAsFactors = FALSE
      ))
    }
    
  }, error = function(e) {
    return(NULL)
  })
  
  return(NULL)
}

# Check if article is actually by Ben Stanley (strict author verification)
is_ben_stanley_article <- function(article_info, node) {
  # Only check for explicit author mentions in author fields
  author_selectors <- c(".author", ".byline", ".meta-author", "[rel='author']", ".author-name", ".post-author")
  
  for (sel in author_selectors) {
    author_node <- html_node(node, sel)
    if (!is.null(author_node)) {
      author_text <- html_text(author_node, trim = TRUE)
      if (!is.na(author_text) && str_detect(tolower(author_text), "ben stanley")) {
        return(TRUE)
      }
    }
  }
  
  # Do NOT rely on title or URL mentions - only explicit author attribution
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

# Check if URL is a valid kulturaliberalna.pl article (strict filtering)
is_article_url <- function(url) {
  # Must be from kulturaliberalna.pl domain
  if (!str_detect(url, "kulturaliberalna\\.pl")) {
    return(FALSE)
  }
  
  # Exclude non-article pages
  exclude_patterns <- c(
    "/tag/", "/category/", "/autor/", "/search", "/kontakt", "/o-nas",
    "facebook\\.com", "twitter\\.com", "instagram\\.com", "youtube\\.com",
    "/wp-content/", "/wp-admin/", "\\.(jpg|jpeg|png|gif|pdf|doc|docx)$",
    "/feed/", "/comments/", "/trackback/", "#comment"
  )
  
  for (pattern in exclude_patterns) {
    if (str_detect(tolower(url), pattern)) {
      return(FALSE)
    }
  }
  
  # Include only article-like URLs
  include_patterns <- c(
    "/artykul/", "/tekst/", "/\\d{4}/\\d{2}/\\d{2}/", "/\\d{4}/\\d{2}/",
    "/post/", "/publikacja/", "/wywiad/", "/komentarz/"
  )
  
  for (pattern in include_patterns) {
    if (str_detect(url, pattern)) {
      return(TRUE)
    }
  }
  
  # Default to FALSE for safety - only include clearly identified articles
  return(FALSE)
}

# Enhance article details by visiting individual pages (with strict author verification)
enhance_article_details <- function(articles, user_agent) {
  # Check all articles for proper author verification
  verified_articles <- data.frame(
    title = character(),
    date = character(),
    url = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(articles)) {
    cat("Verifying article", i, "of", nrow(articles), "\n")
    
    tryCatch({
      response <- GET(articles$url[i], 
                      add_headers(`User-Agent` = user_agent),
                      timeout(30))
      
      if (status_code(response) == 200) {
        page <- read_html(response)
        
        # Strict author verification
        author_found <- FALSE
        author_selectors <- c(
          ".author", ".byline", ".meta-author", "[rel='author']", 
          ".author-name", ".post-author", ".article-author",
          "[property='article:author']", ".entry-author"
        )
        
        for (sel in author_selectors) {
          author_nodes <- html_nodes(page, sel)
          for (author_node in author_nodes) {
            author_text <- html_text(author_node, trim = TRUE)
            if (!is.na(author_text) && str_detect(tolower(author_text), "ben stanley")) {
              author_found <- TRUE
              break
            }
          }
          if (author_found) break
        }
        
        # Only include if Ben Stanley is confirmed as author
        if (author_found) {
          # Try to get better date if not already extracted from URL
          current_date <- articles$date[i]
          if (current_date == "Date not found") {
            current_date <- extract_date_from_url(articles$url[i])
          }
          
          # If still no date, try page elements
          if (current_date == "Date not found") {
            date_selectors <- c("time[datetime]", ".date", ".published", "[property='article:published_time']")
            
            for (sel in date_selectors) {
              date_node <- html_node(page, sel)
              if (!is.null(date_node)) {
                datetime_attr <- html_attr(date_node, "datetime")
                if (!is.na(datetime_attr)) {
                  current_date <- datetime_attr
                  break
                }
                date_text <- html_text(date_node, trim = TRUE)
                if (!is.na(date_text) && nchar(date_text) > 0) {
                  current_date <- date_text
                  break
                }
              }
            }
          }
          
          # Add to verified articles
          verified_articles <- bind_rows(verified_articles, data.frame(
            title = articles$title[i],
            date = current_date,
            url = articles$url[i],
            stringsAsFactors = FALSE
          ))
          
          cat("✓ Verified: Ben Stanley is author\n")
        } else {
          cat("✗ Skipped: Ben Stanley not found as author\n")
        }
      }
      
      Sys.sleep(2) # Be respectful
      
    }, error = function(e) {
      cat("Error verifying article", i, ":", e$message, "\n")
    })
  }
  
  return(verified_articles)
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
  
  # Scrape articles
  articles <- scrape_ben_stanley_articles()
  
  # Display summary
  cat("\nScraping completed!\n")
  cat("Found", nrow(articles), "articles by Ben Stanley\n\n")
  
  if (nrow(articles) > 0) {
    # Print summary to console
    cat("Articles found:\n")
    for (i in 1:nrow(articles)) {
      cat(sprintf("%d. %s\n", i, articles$title[i]))
      cat(sprintf("   Date: %s\n", articles$date[i]))
      cat(sprintf("   URL: %s\n\n", articles$url[i]))
    }
  }
  
  # Create and save QMD output
  qmd_output <- create_qmd_output(articles)
  
  # Save QMD to file
  writeLines(qmd_output, "kultura_liberalna_articles.qmd")
  cat("QMD output saved to kultura_liberalna_articles.qmd\n")
  
  return(qmd_output)
}

# Run the scraper
result <- main()