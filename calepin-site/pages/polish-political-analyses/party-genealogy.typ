#set document(title: [Parties in the Sejm])
#metadata((
  title: "Parties in the Sejm",
  description: "Polish parties in the Sejm since 1989: their genealogy, the seat strength of each party and party family day by day, and MPs' defections between clubs",
)) <website-metadata>

// The chart is a standalone page, assets/party-genealogy/index.html, embedded
// here like the Polling tracker, with three views: Genealogy, Seat strength and
// Defections. The page was called Party genealogy until 24 October 2026, and its
// files keep that name. Its data, assets/party-genealogy/genealogy.json and
// defections.json, comes from the Party tracker in the Polish political analyses
// repo and is copied in by scripts/sync-analyses.py on each build.
// .embed-frame--app sizing lives in themes/site-theme/css/90_site.css. The chart
// reads and writes #pt=<Party tracker id> in this page's address, which the Party
// ideology page links to (its IDEO list names the parties that link back there),
// v=seats with b=<base> for Seat strength, and v=def with mp=<person id>,
// p=<party id> and t=<term> for Defections.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Polish political analyses")]

#heading(level: 1)[Parties in the Sejm]

Every party that has held seats in the Sejm since June 1989, in three views. The Genealogy shows each party's organisational lifespan, its seats (ribbon height proportional to the square root of its seats, solid when in government) and the splits, mergers, transformations and alliance entries that link it to other parties. Hover over a party, a seat ribbon or an arrow for its record; click a party to trace its lineage. For parties rated in the Chapel Hill Expert Survey, the record links to their positions on the #link("party-ideology.html")[Party ideology] page. Seat strength stacks every party's seats on each day, ordered from left to right by party family. Hover over it for a party's seats on that day; click a party or a party family, in the chart or in the list beside it, to move it to the base of the chart and read its seats off the axis. Defections counts the MPs who changed clubs, by Sejm term and by the parties they left and joined, and lists every move: who moved, when, from which club to which, and on what evidence. Click a bar, a flow or a party to narrow the list, and a move for the MP's record. The data come from my Polish party tracker.

#html.elem("iframe", attrs: (
  src: "../../assets/party-genealogy/index.html",
  class: "embed-frame embed-frame--app",
  title: "Parties in the Sejm: the genealogy, seat strength and defections of Polish parties in the Sejm, 1989–2026",
  loading: "lazy",
))[]
