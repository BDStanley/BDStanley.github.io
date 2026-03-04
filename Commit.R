system("git pull")
quarto::quarto_render()
system("git add -A")
system('git commit -m "Update $(date +"%Y-%m-%d %H:%M:%S")"')
system("git push")
system(
  '/opt/homebrew/bin/rsync -av --delete --modify-window=1 --exclude=".git" --exclude="_site" \\
  --protect-args \\
  "/Users/benstanley/Positron/Website/" \\
  "/Users/benstanley/Library/Mobile Documents/com~apple~CloudDocs/Website/"'
)
