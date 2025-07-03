# Bibliography HTML to Markdown Converter
library(rvest)
library(stringr)

# Function to convert bibliography HTML to markdown
convert_bibliography_html_to_md <- function(html_file, md_file) {
  
  cat("Processing:", html_file, "\n")
  
  if (!file.exists(html_file)) {
    cat("Error: File", html_file, "does not exist\n")
    return(FALSE)
  }
  
  tryCatch({
    # Read HTML file
    html_content <- read_html(html_file)
    
    # Extract paragraphs - each paragraph should be one bibliography entry
    paragraphs <- html_nodes(html_content, "p")
    
    if (length(paragraphs) == 0) {
      cat("No paragraphs found, trying div elements\n")
      paragraphs <- html_nodes(html_content, "div")
    }
    
    if (length(paragraphs) == 0) {
      cat("No structured content found\n")
      return(FALSE)
    }
    
    cat("Found", length(paragraphs), "elements\n")
    
    # Convert each paragraph to markdown, preserving original formatting
    md_lines <- character()
    
    for (para in paragraphs) {
      # Get text content while preserving formatting
      text <- convert_html_para_to_markdown(para)
      
      # Only include non-empty entries
      if (!is.na(text) && nchar(str_trim(text)) > 10) {
        # Filter out very long paragraphs that contain multiple entries
        # Individual bibliography entries are typically < 500 characters
        if (nchar(text) < 500) {
          md_lines <- c(md_lines, text)
        } else {
          cat("Skipping long paragraph (", nchar(text), "chars) - likely contains multiple entries\n")
        }
      }
    }
    
    # Remove duplicates
    md_lines <- unique(md_lines)
    
    if (length(md_lines) == 0) {
      cat("No valid entries found\n")
      return(FALSE)
    }
    
    # Join with double line breaks for proper spacing
    md_content <- paste(md_lines, collapse = "\n\n")
    
    # Write to markdown file
    writeLines(md_content, md_file)
    
    cat("✓ Converted", html_file, "to", md_file, "\n")
    cat("✓ Extracted", length(md_lines), "entries\n\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("Error processing", html_file, ":", e$message, "\n")
    return(FALSE)
  })
}

# Function to convert a single HTML paragraph to markdown
convert_html_para_to_markdown <- function(para) {
  # Get the inner HTML to preserve formatting
  inner_html <- html_children(para)
  
  if (length(inner_html) > 0) {
    # There are child elements - convert HTML tags to markdown
    html_string <- as.character(para)
    
    # Convert hyperlinks to markdown format FIRST (before removing other tags)
    # Pattern: <a href="URL">Link Text</a> becomes [Link Text](URL)
    html_string <- str_replace_all(html_string, '<a\\s+href=["\']([^"\']+)["\'][^>]*>([^<]*)</a>', "[\\2](\\1)")
    
    # Convert <em> or <i> to *text*
    html_string <- str_replace_all(html_string, "<(em|i)>([^<]*)</(em|i)>", "*\\2*")
    
    # Convert <strong> or <b> to **text**
    html_string <- str_replace_all(html_string, "<(strong|b)>([^<]*)</(strong|b)>", "**\\2**")
    
    # Remove paragraph tags
    html_string <- str_replace_all(html_string, "</?p[^>]*>", "")
    
    # Remove any remaining HTML tags (but preserve the converted markdown links)
    html_string <- str_replace_all(html_string, "<[^>]+>", "")
    
    # Clean up HTML entities
    html_string <- str_replace_all(html_string, "&amp;", "&")
    html_string <- str_replace_all(html_string, "&lt;", "<")
    html_string <- str_replace_all(html_string, "&gt;", ">")
    html_string <- str_replace_all(html_string, "&quot;", "\"")
    html_string <- str_replace_all(html_string, "&nbsp;", " ")
    
    # Clean up whitespace but preserve line structure
    html_string <- str_replace_all(html_string, "\\s+", " ")
    html_string <- str_trim(html_string)
    
    return(html_string)
  } else {
    # Plain text - just extract and clean
    text <- html_text(para, trim = TRUE)
    text <- str_replace_all(text, "\\s+", " ")
    return(text)
  }
}

# Function to convert HTML formatting to markdown (for plain text)
convert_html_formatting_to_markdown_text <- function(text) {
  # Convert common HTML entities
  text <- str_replace_all(text, "&amp;", "&")
  text <- str_replace_all(text, "&lt;", "<")
  text <- str_replace_all(text, "&gt;", ">")
  text <- str_replace_all(text, "&quot;", "\"")
  
  # For plain text, we can't easily detect what should be italicized
  # But we can clean up the text
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_trim(text)
  
  return(text)
}

# Function to convert HTML formatting to markdown
convert_html_formatting_to_markdown <- function(html_element) {
  # Get the HTML as text
  html_text <- as.character(html_element)
  
  # Convert common HTML tags to markdown
  # Italics: <em> or <i> to *text*
  html_text <- str_replace_all(html_text, "<(em|i)>(.*?)</(em|i)>", "*\\2*")
  
  # Bold: <strong> or <b> to **text**
  html_text <- str_replace_all(html_text, "<(strong|b)>(.*?)</(strong|b)>", "**\\2**")
  
  # Remove paragraph tags
  html_text <- str_replace_all(html_text, "</?p>", "")
  
  # Remove any remaining HTML tags
  html_text <- str_replace_all(html_text, "<[^>]+>", "")
  
  # Clean up extra whitespace
  html_text <- str_replace_all(html_text, "\\s+", " ")
  html_text <- str_trim(html_text)
  
  return(html_text)
}

# Function to convert all bibliography HTML files
convert_all_bibliography_files <- function() {
  # List of HTML files to convert
  html_files <- c(
    "journal_articles.html",
    "books.html", 
    "book_chapters.html",
    "essays_interviews_reviews.html",
    "datasets.html"
  )
  
  # Convert each file
  for (html_file in html_files) {
    if (file.exists(html_file)) {
      md_file <- str_replace(html_file, "\\.html$", ".qmd")
      convert_bibliography_html_to_md(html_file, md_file)
    } else {
      cat("File not found:", html_file, "\n")
    }
  }
}

# Run the conversion
convert_all_bibliography_files()

# Alternative: Convert a single file
# convert_bibliography_html_to_md("essays_interviews_reviews.html", "essays_interviews_reviews.qmd")