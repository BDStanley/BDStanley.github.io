#set document(title: [Polish parties in the Sejm, 1989–2026])
#metadata((
  title: "Polish parties in the Sejm, 1989–2026",
  description: "Interactive genealogy of Polish parties in the Sejm: lifespans, seats, government status, splits and mergers",
)) <website-metadata>

// The chart is a standalone page, assets/party-genealogy/index.html, embedded
// here like the Polling tracker. Its data, assets/party-genealogy/genealogy.json,
// comes from the Party tracker in the Polish political analyses repo and is
// copied in by scripts/sync-analyses.py on each build. .embed-frame--app
// sizing lives in themes/site-theme/css/90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Polish political analyses")]

#heading(level: 1)[Polish parties in the Sejm, 1989–2026]

Every party that has held seats in the Sejm since June 1989: its organisational lifespan, its seats (ribbon height proportional to the square root of its seats, solid when in government) and the splits, mergers, transformations and alliance entries that link it to other parties. Hover over a party, a seat ribbon or an arrow for its record; click a party to trace its lineage. The data come from my Polish party tracker.

#html.elem("iframe", attrs: (
  src: "../../assets/party-genealogy/index.html",
  class: "embed-frame embed-frame--app",
  title: "Interactive genealogy of Polish parties in the Sejm, 1989–2026",
  loading: "lazy",
))[]
