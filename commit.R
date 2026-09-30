#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Rebuild the Calepin site and deploy it.
#
#   1. rebuild the CV PDF + the whole site into /docs   (calepin-site/build.sh)
#   2. have Claude bring README.md up to date (skipped if Claude is unavailable)
#   3. commit everything with a Claude-written message and push to GitHub
#      (-> GitHub Pages -> benstanley.eu); the message is just the timestamp
#      if Claude is unavailable
#   4. mirror the project to the iCloud backup folder
#
# This replaces the old Quarto workflow. There is NO quarto::quarto_render() here
# any more — that step is what made the previous script slow (it re-ran the old
# site's Google Scholar / Kultura Liberalna scrapers). The Calepin build is fast.
#
# Run it from anywhere in the repo:  Rscript commit.R   (or source it in Positron)
# -----------------------------------------------------------------------------

# --- locate the repo root --------------------------------------------------
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cran.r-project.org")
}
project_root <- here::here()
old_wd <- setwd(project_root)
on.exit(setwd(old_wd))

# --- make sure the build tools are on PATH ---------------------------------
# R's system() uses a minimal PATH, so add Homebrew (typst) and Cargo (calepin).
Sys.setenv(PATH = paste(
  "/opt/homebrew/bin",
  file.path(Sys.getenv("HOME"), ".cargo", "bin"),
  Sys.getenv("PATH"),
  sep = ":"
))

# helper: run a shell command, stop with a clear message if it fails
run <- function(cmd, fatal = TRUE) {
  cat("→", cmd, "\n")
  status <- system(cmd)
  if (fatal && status != 0) {
    stop(sprintf("Command failed (exit %d): %s", status, cmd), call. = FALSE)
  }
  invisible(status)
}

# --- Claude helpers (for the README check and the commit message) -----------
claude <- Sys.which("claude")
if (!nzchar(claude)) claude <- path.expand("~/.npm-global/bin/claude")
# A nested `claude -p` refuses to start if this runs inside a Claude Code session
Sys.unsetenv("CLAUDECODE")

# Claude's reply as lines, or character() if Claude is unavailable or fails
ask_claude <- function(prompt, input, model = "haiku", timeout = 120) {
  out <- tryCatch(
    suppressWarnings(system2(
      claude,
      c("-p", "--model", model, "--tools", '""', "--no-session-persistence", shQuote(prompt)),
      input = input, stdout = TRUE, stderr = FALSE, timeout = timeout
    )),
    error = function(e) character()
  )
  if (!is.null(attr(out, "status"))) return(character())
  out[cumsum(nzchar(trimws(out))) > 0]
}

# Staged changes relative to `base` (default HEAD). The built site in docs/ and
# bulky assets appear in the stat only, not the patch
staged_diff <- function(base = "") {
  stat <- system(paste("git diff --cached --stat=160", base), intern = TRUE)
  if (length(stat) > 300) stat <- c(head(stat, 300), "...", tail(stat, 1))
  patch <- system(
    paste(
      "git diff --cached --no-color", base,
      "-- . ':(exclude)docs/*' ':(exclude)*.pdf' ':(exclude)*.html' ':(exclude)*.js' ':(exclude)*.css' ':(exclude)*.json' ':(exclude)*.svg' ':(exclude)*.png' ':(exclude)*.jpg'"
    ),
    intern = TRUE
  )
  patch <- iconv(paste(patch, collapse = "\n"), "UTF-8", "UTF-8", sub = "")
  c(stat, "", substr(patch, 1, 40000))
}

# --- 1. stay in sync (non-fatal: a no-op when there's nothing to pull) ------
run("git pull --ff-only", fatal = FALSE)

# --- 2. rebuild the CV PDF + the site into /docs ----------------------------
run("./calepin-site/build.sh")

# --- 3. have Claude bring README.md up to date (non-fatal) ------------------
# README.md is checked against everything since the last commit whose check
# succeeded (marked by a README-reviewed trailer), so changes committed while
# Claude was unavailable are caught up later. Until the first successful check,
# the base is the commit that added README.md (HEAD if it is not committed yet).
run("git add -A")
changed <- system("git diff --cached --quiet") != 0
readme_reviewed <- FALSE
if (changed && file.exists("README.md")) {
  base <- suppressWarnings(system(
    "git log -1 --format=%H --grep='^README-reviewed: yes$' 2>/dev/null",
    intern = TRUE
  ))
  if (length(base) == 0) base <- suppressWarnings(system("git log -1 --diff-filter=A --format=%H -- README.md 2>/dev/null", intern = TRUE))
  if (length(base) == 0) base <- if (system("git rev-parse -q --verify HEAD", ignore.stdout = TRUE) == 0) "HEAD" else system("git hash-object -t tree /dev/null", intern = TRUE)

  readme <- readLines("README.md", warn = FALSE)
  readme_prompt <- paste(
    "Below are the README.md of Ben Stanley's website repo (the public source of benstanley.eu:",
    "a Calepin site built into docs/ and served by GitHub Pages), the files in the repo,",
    "and the changes made since the README was last checked.",
    "Update the README so that it is accurate and complete: add anything new that belongs in it",
    "(e.g. a new section of the site, a new build step or tool) and correct anything the changes",
    "have made out of date (e.g. a changed step in build.sh or commit.R, a removed folder).",
    "Routine content updates (new pages, posts, publications or CV entries) need no README change.",
    "Keep everything else exactly as it is: do not rephrase, reorder or reformat untouched text.",
    "Never include credentials, tokens or IDs. British English.",
    "If nothing needs changing, output only NO_CHANGE.",
    "Otherwise output only the complete updated README.md: no preamble, no code fences."
  )
  ai <- ask_claude(
    readme_prompt,
    c(
      "=== README.md ===", readme, "",
      "=== Files in the repository ===", system("git -c core.quotePath=false ls-files", intern = TRUE), "",
      "=== Changes since the README was last checked ===", staged_diff(base)
    ),
    model = "sonnet", timeout = 300
  )
  if (length(ai) > 0 && startsWith(trimws(ai[1]), "NO_CHANGE")) {
    readme_reviewed <- TRUE
    cat("✓ README.md checked: no change needed\n")
  } else if (length(ai) >= length(readme) / 2 && startsWith(ai[1], "# ")) {
    # Anything shorter or not starting with the title is a truncated or wrapped reply, not a README
    writeLines(ai, "README.md")
    run("git add README.md", fatal = FALSE)
    readme_reviewed <- TRUE
    cat("✓ README.md updated\n")
  } else {
    cat("README.md check skipped (Claude unavailable or reply unusable); will retry on the next commit\n")
  }
}

# --- 4. commit & push, but only if something actually changed ---------------
if (changed) {
  msg <- sprintf("Update %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  msg_prompt <- paste(
    "Write a git commit message for the staged changes below, in Ben Stanley's website repo",
    "(the source of benstanley.eu: a Calepin site in calepin-site/, built into docs/, with a",
    "Quarto backup of the same site in quarto-site/).",
    "Line 1: a summary of at most 60 characters, no trailing full stop.",
    "Then a blank line, then up to 6 short '- ' bullets, grouped by part of the site",
    "(e.g. CV, Teaching, Media, theme, build scripts),",
    "saying what changed in substance (e.g. 'added 2026 article to the CV', 'new course page for POL101'),",
    "not just which files were touched.",
    "Do not mention the rebuilt docs/ folder unless nothing else changed.",
    "If README.md changed only to record the other changes, do not mention it. British English.",
    "Output only the message: no preamble, no code fences."
  )
  ai <- ask_claude(msg_prompt, staged_diff())
  if (length(ai) > 0) msg <- c(paste0(msg, ": ", trimws(ai[1])), ai[-1])
  if (readme_reviewed) msg <- c(msg, "", "README-reviewed: yes")

  msg_file <- tempfile(fileext = ".txt")
  writeLines(msg, msg_file)
  run(paste("git commit -F", shQuote(msg_file)))
  run("git push")
  cat(sprintf("\n✓ Deployed: %s  (live in ~1–2 min at https://benstanley.eu)\n", msg[1]))
} else {
  cat("\n✓ Build is unchanged — nothing to commit.\n")
}

# --- 5. mirror to the iCloud backup folder (non-fatal) ----------------------
icloud <- "/Users/benstanley/Library/Mobile Documents/com~apple~CloudDocs/Website/"
run(sprintf(
  paste(
    "/opt/homebrew/bin/rsync -av --delete --iconv=utf-8-mac,utf-8",
    "--exclude='.git' --exclude='.quarto' --exclude='.calepin'",
    "--exclude='_site' %s/ %s"
  ),
  shQuote(project_root), shQuote(icloud)
), fatal = FALSE)
