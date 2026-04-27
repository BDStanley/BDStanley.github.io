library(here)

# Run all git/render operations from the project root
project_root <- here()
old_wd <- setwd(project_root)
on.exit(setwd(old_wd))

system("git pull")
quarto::quarto_render()
system("git add -A")
system('git commit -m "Update $(date +"%Y-%m-%d %H:%M:%S")"')
system("git push")
system(sprintf(
  '/opt/homebrew/bin/rsync -av --delete --iconv=utf-8-mac,utf-8 --exclude=".quarto" --exclude=".git" --exclude="_site" %s/ "/Users/benstanley/Library/Mobile Documents/com~apple~CloudDocs/Website/"',
  shQuote(project_root)
))
