# Google Scholar Scraper for Ben Stanley (SWPS University)
# This script scrapes Google Scholar for articles by Ben Stanley affiliated with SWPS University

# Load required libraries
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(xml2)

# Function to scrape Google Scholar
scrape_google_scholar <- function(author = "Ben Stanley", 
                                  affiliation = "SWPS", 
                                  max_pages = 3,
                                  delay_seconds = 2) {
  
  # Construct search query
  search_query <- paste0('"', author, '"', ' ', affiliation)
  base_url <- "https://scholar.google.com/scholar"
  
  # Initialize results dataframe
  all_results <- data.frame(
    title = character(),
    authors = character(),
    journal = character(),
    year = character(),
    citations = character(),
    url = character(),
    stringsAsFactors = FALSE
  )
  
  # User agent to avoid blocking
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
  
  cat("Starting Google Scholar scrape for:", search_query, "\n")
  
  # Loop through pages
  for (page in 1:max_pages) {
    start_index <- (page - 1) * 10
    
    # Construct URL with parameters
    params <- list(
      q = search_query,
      start = start_index,
      hl = "en"
    )
    
    cat("Scraping page", page, "...\n")
    
    # Make request with error handling
    tryCatch({
      response <- GET(
        base_url,
        query = params,
        add_headers("User-Agent" = user_agent),
        timeout(30)
      )
      
      # Check if request was successful
      if (status_code(response) != 200) {
        cat("Warning: HTTP status", status_code(response), "for page", page, "\n")
        next
      }
      
      # Parse HTML content
      page_content <- content(response, "text", encoding = "UTF-8")
      html_doc <- read_html(page_content)
      
      # Extract article containers
      articles <- html_doc %>% 
        html_nodes(".gs_r.gs_or.gs_scl")
      
      if (length(articles) == 0) {
        cat("No articles found on page", page, "\n")
        break
      }
      
      # Extract information from each article
      for (article in articles) {
        # Title and URL
        title_node <- article %>% html_node(".gs_rt a")
        title <- if (!is.na(title_node)) {
          html_text(title_node, trim = TRUE)
        } else {
          article %>% html_node(".gs_rt") %>% html_text(trim = TRUE)
        }
        
        article_url <- if (!is.na(title_node)) {
          html_attr(title_node, "href")
        } else {
          NA
        }
        
        # Authors and publication info
        authors_info <- article %>% 
          html_node(".gs_a") %>% 
          html_text(trim = TRUE)
        
        # Split authors and journal info
        if (!is.na(authors_info)) {
          # Authors are typically before the first " - "
          authors_split <- str_split(authors_info, " - ")[[1]]
          authors <- authors_split[1]
          
          # Journal and year info
          if (length(authors_split) > 1) {
            pub_info <- paste(authors_split[-1], collapse = " - ")
            
            # Extract year (4 digits)
            year_match <- str_extract(pub_info, "\\b(19|20)\\d{2}\\b")
            year <- if (!is.na(year_match)) year_match else ""
            
            # Journal is the remaining text after removing year
            journal <- str_trim(str_remove(pub_info, "\\b(19|20)\\d{2}\\b"))
            journal <- str_remove(journal, "^,\\s*")
          } else {
            journal <- ""
            year <- ""
          }
        } else {
          authors <- ""
          journal <- ""
          year <- ""
        }
        
        # Citations count
        citations_node <- article %>% html_node(".gs_fl a:contains('Cited by')")
        citations <- if (!is.na(citations_node)) {
          citations_text <- html_text(citations_node)
          str_extract(citations_text, "\\d+")
        } else {
          "0"
        }
        
        # Filter for Ben Stanley specifically
        if (str_detect(tolower(authors), "ben stanley|b stanley|stanley.*b")) {
          # Add to results
          all_results <- rbind(all_results, data.frame(
            title = ifelse(is.na(title), "", title),
            authors = ifelse(is.na(authors), "", authors),
            journal = ifelse(is.na(journal), "", journal),
            year = ifelse(is.na(year), "", year),
            citations = ifelse(is.na(citations), "0", citations),
            url = ifelse(is.na(article_url), "", article_url),
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Add delay between requests to be respectful
      Sys.sleep(delay_seconds)
      
    }, error = function(e) {
      cat("Error on page", page, ":", e$message, "\n")
    })
  }
  
  cat("Scraping completed. Found", nrow(all_results), "articles.\n")
  return(all_results)
}

# Function to clean and format results
format_results <- function(results) {
  if (nrow(results) == 0) {
    cat("No results found.\n")
    return(results)
  }
  
  # Clean up data
  results$title <- str_trim(results$title)
  results$authors <- str_trim(results$authors)
  results$journal <- str_trim(results$journal)
  results$citations <- as.numeric(results$citations)
  
  # Remove duplicates based on title
  results <- results[!duplicated(results$title), ]
  
  # Sort by citations (descending) and then by year (descending)
  results <- results %>%
    arrange(desc(citations), desc(year))
  
  return(results)
}

# Function to save results to QMD
save_results <- function(results, filename = "ben_stanley_publications.qmd") {
  # Create QMD content
  qmd_content <- c(
    "---",
    "title: \"Ben Stanley Publications from SWPS University\"",
    "author: \"Google Scholar Scraper\"",
    paste0("date: \"", Sys.Date(), "\""),
    "format:",
    "  html:",
    "    toc: true",
    "    toc-depth: 2",
    "    theme: cosmo",
    "  pdf:",
    "    toc: true",
    "    number-sections: true",
    "---",
    "",
    "# Overview",
    "",
    paste0("This report contains ", nrow(results), " publications by Ben Stanley affiliated with SWPS University, scraped from Google Scholar on ", Sys.Date(), "."),
    "",
    "Publications are sorted by citation count (descending) and then by year (descending).",
    "",
    "# Publications",
    ""
  )
  
  if (nrow(results) == 0) {
    qmd_content <- c(qmd_content, "No publications found matching the search criteria.")
  } else {
    for (i in 1:nrow(results)) {
      # Create publication entry
      pub_entry <- c(
        paste0("## ", i, ". ", results$title[i]),
        "",
        paste0("**Authors:** ", results$authors[i]),
        ""
      )
      
      if (results$journal[i] != "") {
        pub_entry <- c(pub_entry, paste0("**Journal/Publication:** ", results$journal[i]), "")
      }
      
      if (results$year[i] != "") {
        pub_entry <- c(pub_entry, paste0("**Year:** ", results$year[i]), "")
      }
      
      pub_entry <- c(pub_entry, paste0("**Citations:** ", results$citations[i]), "")
      
      if (results$url[i] != "") {
        pub_entry <- c(pub_entry, paste0("**URL:** [Link to publication](", results$url[i], ")"), "")
      }
      
      pub_entry <- c(pub_entry, "---", "")
      
      qmd_content <- c(qmd_content, pub_entry)
    }
    
    # Add summary statistics
    total_citations <- sum(as.numeric(results$citations), na.rm = TRUE)
    avg_citations <- round(mean(as.numeric(results$citations), na.rm = TRUE), 1)
    year_range <- paste(min(results$year[results$year != ""], na.rm = TRUE), 
                        max(results$year[results$year != ""], na.rm = TRUE), 
                        sep = " - ")
    
    qmd_content <- c(qmd_content,
                     "",
                     "# Summary Statistics",
                     "",
                     paste0("- **Total Publications:** ", nrow(results)),
                     paste0("- **Total Citations:** ", total_citations),
                     paste0("- **Average Citations per Publication:** ", avg_citations),
                     paste0("- **Publication Year Range:** ", year_range),
                     paste0("- **Most Cited Publication:** ", results$title[1], " (", results$citations[1], " citations)"),
                     "",
                     "---",
                     "",
                     paste0("*Report generated on ", Sys.Date(), " using Google Scholar data*")
    )
  }
  
  # Write QMD file
  writeLines(qmd_content, filename)
  cat("Results saved to:", filename, "\n")
  cat("To render the QMD file, use: quarto render", filename, "\n")
}

# Function to display results nicely
display_results <- function(results) {
  if (nrow(results) == 0) {
    cat("No publications found.\n")
    return()
  }
  
  cat("\n=== Ben Stanley Publications (SWPS University) ===\n\n")
  
  for (i in 1:nrow(results)) {
    cat("", i, ". ", results$title[i], "\n", sep = "")
    cat("   Authors: ", results$authors[i], "\n", sep = "")
    if (results$journal[i] != "") {
      cat("   Journal: ", results$journal[i], "\n", sep = "")
    }
    if (results$year[i] != "") {
      cat("   Year: ", results$year[i], "\n", sep = "")
    }
    cat("   Citations: ", results$citations[i], "\n", sep = "")
    if (results$url[i] != "") {
      cat("   URL: ", results$url[i], "\n", sep = "")
    }
    cat("\n")
  }
}

# Main execution
main <- function() {
  cat("Google Scholar Scraper for Ben Stanley (SWPS University)\n")
  cat("======================================================\n\n")
  
  # Check if required packages are installed
  required_packages <- c("rvest", "httr", "dplyr", "stringr", "xml2")
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    install.packages(missing_packages, repos = "https://cran.r-project.org/")
  }
  
  # Run the scraper
  raw_results <- scrape_google_scholar(
    author = "Ben Stanley", 
    affiliation = "SWPS",
    max_pages = 3,
    delay_seconds = 2
  )
  
  # Format and clean results
  final_results <- format_results(raw_results)
  
  # Display results
  display_results(final_results)
  
  # Save to QMD
  save_results(final_results)
  
  # Return results invisibly
  invisible(final_results)
}

# Run the main function if script is executed directly
if (!interactive()) {
  main()
}

