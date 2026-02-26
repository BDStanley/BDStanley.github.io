system("git pull")
quarto::quarto_render()
system("git add -A")
system('git commit -m "Update $(date +"%Y-%m-%d %H:%M:%S")"')
system("git push")
system(
    'rsync -av --delete --size-only --exclude=".git" --exclude="_site" "/Users/benstanley/Positron/Website/" "/Users/benstanley/Library/Mobile Documents/com~apple~CloudDocs/Website/"'
)
