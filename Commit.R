system("git pull")
quarto::quarto_render()
system("git add -A")
system('git commit -m "Update $(date +"%Y-%m-%d %H:%M:%S")"')
system("git push")
system(
  '/opt/homebrew/bin/rsync -av --delete --iconv=utf-8-mac,utf-8 --exclude=".git" --exclude="_site" \\
  "/Users/benstanley/Positron/Website/" \\
  "/Users/benstanley/Library/Mobile Documents/com~apple~CloudDocs/Website/"'
)
