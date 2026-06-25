# =============================================================================
# Render the CV-driven website sections from cv/cv.yml — the single source of
# truth that also builds cv/cv.pdf. Mirrors the Calepin site's
# pages/publications.typ and pages/research-projects.typ.
#
# Paths are resolved from the PROJECT ROOT (the project sets
# `execute-dir: project` in _quarto.yml), so cv/cv.yml resolves the same way no
# matter which page sources this file.
# =============================================================================

suppressWarnings(suppressMessages(library(yaml)))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Locate the project root (the directory containing _quarto.yml) in a way that
# does NOT depend on the working directory. Positron/Quarto may render with the
# working dir set to the page's folder, the project root, OR the workspace root
# *above* the project — so walking up from getwd() alone is not enough (from a
# parent dir it can never find a _quarto.yml that lives in a sub-directory).
# Resolution order: Quarto's own QUARTO_PROJECT_DIR env var, then the directory
# of the .qmd being knit (knitr::current_input), then getwd() — each walked
# upward to the nearest _quarto.yml.
proj_root <- function() {
  walk_up <- function(start) {
    d <- normalizePath(start, winslash = "/", mustWork = FALSE)
    for (i in 1:12) {
      if (file.exists(file.path(d, "_quarto.yml"))) return(d)
      parent <- dirname(d)
      if (parent == d) break
      d <- parent
    }
    NULL
  }
  # 1) Quarto sets this to the project directory during a project render.
  pd <- Sys.getenv("QUARTO_PROJECT_DIR", "")
  if (nzchar(pd) && file.exists(file.path(pd, "_quarto.yml"))) return(pd)
  # 2) The directory of the .qmd being knit — file-relative, ignores the cwd.
  ci <- tryCatch(knitr::current_input(dir = TRUE), error = function(e) "")
  if (nzchar(ci)) {
    r <- walk_up(dirname(normalizePath(ci, winslash = "/", mustWork = FALSE)))
    if (!is.null(r)) return(r)
  }
  # 3) Last resort: walk up from the working directory.
  r <- walk_up(getwd())
  if (!is.null(r)) r else getwd()
}

read_cv <- function() {
  # Prefer a root resolved by the calling page (set via options); else find it.
  root <- getOption("cvsite_root", default = proj_root())
  yaml::read_yaml(file.path(root, "cv", "cv.yml"))
}

html_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;",  s, fixed = TRUE)
  s <- gsub(">", "&gt;",  s, fixed = TRUE)
  s
}

# Apply `fn` to every match of `pattern` in `x`, rebuilding the string (base R
# has no regex-replace-with-callback).
gsub_fn <- function(pattern, fn, x) {
  m <- gregexpr(pattern, x, perl = TRUE)[[1]]
  if (m[1] == -1) return(x)
  starts <- as.integer(m)
  ends <- starts + attr(m, "match.length") - 1
  out <- ""
  prev <- 1
  for (i in seq_along(starts)) {
    out <- paste0(out, substr(x, prev, starts[i] - 1), fn(substr(x, starts[i], ends[i])))
    prev <- ends[i] + 1
  }
  paste0(out, substr(x, prev, nchar(x)))
}

# Turn one CV entry string into Markdown, matching the Calepin render-entry:
#   * bare URLs              -> clickable links (URL shown as the link text)
#   * doi:10.xxxx            -> https://doi.org/10.xxxx (shown as "doi:10.xxxx")
#   * *...* (academicv bold) -> **...** (Markdown bold)
# A trailing full stop after a URL/DOI is kept as plain text outside the link.
render_entry <- function(s) {
  # 1) bare URLs first, so DOIs injected below aren't reprocessed
  s <- gsub_fn("https?://\\S+", function(u) {
    trail <- ""
    if (grepl("\\.$", u)) { trail <- "."; u <- sub("\\.$", "", u) }
    paste0("<", u, ">", trail)
  }, s)
  # 2) bare DOIs: doi:10.xxx -> Markdown link
  s <- gsub_fn("doi:10\\.\\S+", function(d) {
    doi <- sub("^doi:", "", d)
    trail <- ""
    if (grepl("\\.$", doi)) { trail <- "."; doi <- sub("\\.$", "", doi) }
    paste0("[doi:", doi, "](https://doi.org/", doi, ")", trail)
  }, s)
  # 3) single-asterisk emphasis means bold in the CV markup -> Markdown bold
  s <- gsub("\\*([^*]+)\\*", "**\\1**", s, perl = TRUE)
  s
}

# Publications: emit the monographs / journal_articles / book_chapters /
# datasets sections in CV order, with the CV's own titles, as bullet lists.
emit_publications <- function(cv = read_cv()) {
  pub_keys <- c("monographs", "journal_articles", "book_chapters", "datasets")
  for (sec in cv$sections) {
    if (!is.null(sec$key) && sec$key %in% pub_keys) {
      entries <- sec$entries %||% list()
      if (length(entries) > 0) {
        cat("\n## ", sec$title, "\n\n", sep = "")
        for (e in entries) cat("- ", render_entry(e), "\n", sep = "")
        cat("\n")
      }
    }
  }
}

# One research project's date range, formatted like the CV's timeline layout.
fmt_range <- function(e) {
  s  <- as.character(e[["start-date"]] %||% "")
  en <- as.character(e[["end-date"]] %||% "")
  if (en != "" && en != s) paste0(s, "–", en) else s
}

# Research projects: emit Current then Previous, each entry as a `.proj` grid
# (date | bold title / italic funder / smaller detail) styled in styles.css.
emit_projects <- function(cv = read_cv()) {
  for (key in c("projects_current", "projects_previous")) {
    sec <- NULL
    for (s in cv$sections) if (!is.null(s$key) && s$key == key) sec <- s
    if (is.null(sec)) next
    entries <- sec$entries %||% list()
    if (length(entries) == 0) next
    cat("\n## ", sec$title, "\n\n", sep = "")
    for (e in entries) {
      cat(
        '<div class="proj">\n',
        '<div class="proj-date">', fmt_range(e), '</div>\n',
        '<div class="proj-body">\n',
        '<div class="proj-title"><strong>', html_escape(e$title %||% ""), '</strong></div>\n',
        '<div class="proj-funder"><em>', html_escape(e$institution %||% ""), '</em></div>\n',
        '<div class="proj-detail">', html_escape(trimws(e$description %||% "")), '</div>\n',
        '</div>\n',
        '</div>\n\n',
        sep = ""
      )
    }
  }
}
