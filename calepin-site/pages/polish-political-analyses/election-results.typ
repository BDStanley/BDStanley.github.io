#set document(title: [Election results])
#metadata((
  title: "Election results",
  description: "Every presidential, Sejm, European Parliament and referendum vote in Poland since 1990: facts, results and maps of every gmina and constituency",
)) <website-metadata>

// The page is a standalone app, assets/election-results/index.html (+ app.js),
// embedded like the other analyses. Its data (elections.json, geo.json,
// covariates.json, one <election-id>.json per election and one profiles file per
// voivodeship) comes from Election results/ in the Polish political analyses repo
// (analysis/run_all.R, whose 90_export.R writes web/) and is copied in by
// scripts/sync-analyses.py on each build. "Polls now" reads the Polling tracker's
// ../polling-tracker/polls.json in the browser.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Polish political analyses")]

#heading(level: 1)[Election results]

Every presidential, Sejm, European Parliament and referendum vote in Poland since 1990: the facts of each election, its national results and seats, and maps of every gmina and constituency. Choose an election in the strip at the top. The map shows who came first, any list's or candidate's share, a head to head, turnout, or the change since any other election, by gmina or by constituency, and the "Widać zabory!" button draws the borders of 1914 between the partitions. Search for a gmina, or click one, for its history in every election, and see how the results line up with the make-up of the gminas in Who votes where. "Polls now" shows the latest estimate of the #link("polling-tracker.html")[Polling tracker]. For how views differ by place of residence and education among voters themselves, see #link("political-attitudes.html")[Social and political trends]; for the party system behind the results, #link("party-system-indicators.html")[Party system indicators].

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

The results are the National Electoral Commission's (PKW) files, by gmina and by polling station. National figures are PKW's announced results in Dziennik Ustaw and Monitor Polski, corrected where PKW corrected them after court rulings (the referendums of 2003 and 2015); where the files differ from the announcements, the maps follow the files. Turnout is PKW's own figure. The semi-free Sejm election of 1989 is not in PKW's files, so it has its facts and seats only.

Gminas are today's (the boundaries of 2022, with Warsaw as one). Results from before the 1999 reform are mapped onto them through a crosswalk built for this page from the gminas' codes, names and electorates: where a gmina of the 1990s later split, its result covers all of its successors, and the few that split in two are shared between them by electorate. Constituencies are each election's own, with presidential elections and referendums on the Sejm constituencies of the time; "Today's 41" adds every election up by the current Sejm constituencies, without votes abroad, so that any two elections can be compared like for like. Seats by constituency come from PKW's lists of the candidates elected.

Lists and candidates take the colours of the parties behind them; a candidate whom no party with a colour backed is grey. Change maps follow each Sejm list through the lineages of Party system indicators, the same candidate between elections, or a camp: the lists or candidates of one colour added up. The borders of 1914 are those of CShapes 2.0, and each gmina belongs to the empire its centre lay in. Who votes where sets each gmina's result against its characteristics from Statistics Poland's Local Data Bank, kept every five years and taken at the year nearest the election; its correlations are weighted by electorate and describe places, not voters.

Sources: PKW (danewyborcze.kbw.gov.pl) and its announcements (through the Sejm's ELI service); Statistics Poland's Local Data Bank; the national register of boundaries (PRG, GUGiK); G. Schvitz, L. Girardin, S. Rüegger, N. B. Weidmann, L.-E. Cederman and K. S. Gleditsch, "Mapping the International System, 1886–2019: The CShapes 2.0 Dataset", Journal of Conflict Resolution 66 (1), 2022; my Polish party tracker; Polish Wikipedia for candidates' backing parties and the presidents' oaths of office.
]

#html.elem("iframe", attrs: (
  src: "../../assets/election-results/index.html",
  class: "embed-frame embed-frame--app",
  title: "Election results: every Polish presidential, Sejm, European Parliament and referendum vote since 1990",
  loading: "lazy",
))[]
