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
// The method note (How this works) is a collapsed <details class="method"> under the
// introduction; its styles are with the other method notes in 90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Polish political analyses")]

#heading(level: 1)[Parties in the Sejm]

Every party that has held seats in the Sejm since June 1989, in three views. The Genealogy traces each party's lifespan, seats and time in government, and the splits, mergers, transformations and alliances that link it to other parties; Seat strength shows the seats of every party and party family day by day; and Defections follows every MP who changed clubs. For parties rated in the Chapel Hill Expert Survey, a party's record links to its positions on the #link("party-ideology.html")[Party ideology] page.

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

The Genealogy gives each party a lane from its founding to its end. Its seats are drawn as a ribbon whose height is proportional to the square root of its seats, solid while it was in government, and arrows mark the splits, mergers, transformations and alliance entries that link it to other parties. Hover over a party, a seat ribbon or an arrow for its record; click a party to trace its lineage.

Seat strength stacks every party's seats on each day, ordered from left to right by party family. Hover over it for a party's seats on that day; click a party or a party family, in the chart or in the list beside it, to move it to the base of the chart and read its seats off the axis.

Defections counts the MPs who changed clubs, by Sejm term and by the parties they left and joined, and lists every move: who moved, when, from which club to which, and on what evidence. Click a bar, a flow or a party to narrow the list, and a move for the MP's record.

Seats are the seats of the clubs in the Sejm, day by day, and a club's seats go to the party that leads it, so a party inside an alliance's club has no seats of its own while it sits there (as PO had none from 2019 to 2025). An alliance with no spell in government of its own takes its member parties' status. Since 2005 every roll call records each MP's club; from 1997 to 2005 the first and last votes of each voting day do, so a move and a return between them could be missed. For 1989–97 the clubs come from lists of MPs, the histories of the clubs and the Sejm archive's lists at the end of each term; most moves in those years can be dated only within a range, so their seats on any one day are less certain, though they add up to the Sejm's 460 less its vacancies on every day.

Sources:

- Clubs and seats: the Sejm's roll calls (1997–2026); Polish Wikipedia's lists of MPs and articles on the clubs, MPs' biographies and the Sejm archive's end-of-term club lists (1989–97)
- Parties, their histories and cabinets: mostly Polish and English Wikipedia
- Elections: the National Electoral Commission (PKW)
- All of these compiled in my Polish party tracker

Ben Stanley, 2026. _Parties in the Sejm_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/party-genealogy/index.html",
  class: "embed-frame embed-frame--app",
  title: "Parties in the Sejm: the genealogy, seat strength and defections of Polish parties in the Sejm, 1989–2026",
  loading: "lazy",
))[]
