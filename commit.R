#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Rebuild the Calepin site and deploy it.
#
#   1. rebuild the CV PDF + the whole site into /docs   (calepin-site/build.sh)
#   2. commit everything and push to GitHub  (-> GitHub Pages -> benstanley.eu)
#   3. mirror the project to the iCloud backup folder
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

# --- 1. stay in sync (non-fatal: a no-op when there's nothing to pull) ------
run("git pull --ff-only", fatal = FALSE)

# --- 2. rebuild the CV PDF + the site into /docs ----------------------------
run("./calepin-site/build.sh")

# --- 3. commit & push, but only if something actually changed ---------------
run("git add -A")
changed <- system("git diff --cached --quiet") != 0
if (changed) {
  msg <- sprintf("Update %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  run(sprintf("git commit -m %s", shQuote(msg)))
  run("git push")
  cat(sprintf("\n✓ Deployed: %s  (live in ~1–2 min at https://benstanley.eu)\n", msg))
} else {
  cat("\n✓ Build is unchanged — nothing to commit.\n")
}

# --- 4. mirror to the iCloud backup folder (non-fatal) ----------------------
icloud <- "/Users/benstanley/Library/Mobile Documents/com~apple~CloudDocs/Website/"
run(sprintf(
  paste(
    "/opt/homebrew/bin/rsync -av --delete --iconv=utf-8-mac,utf-8",
    "--exclude='.git' --exclude='.quarto' --exclude='.calepin'",
    "--exclude='_site' %s/ %s"
  ),
  shQuote(project_root), shQuote(icloud)
), fatal = FALSE)
