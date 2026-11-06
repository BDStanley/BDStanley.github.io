#set document(title: [Election results])
#metadata((
  title: "Election results",
  description: "Every presidential, Sejm, Senate, European Parliament and referendum vote in Poland since 1990: facts, results and maps of every gmina and constituency, and the careers of senators and MEPs",
)) <website-metadata>

// The page is a standalone app, assets/election-results/index.html (+ app.js),
// embedded like the other analyses. Its data (elections.json, geo.json,
// covariates.json, one <election-id>.json per election, one profiles file per
// voivodeship, and people.json, the careers of senators and MEPs) comes from Election results/ in the Polish political analyses repo
// (analysis/run_all.R, whose 90_export.R writes web/) and is copied in by
// scripts/sync-analyses.py on each build. "Polls now" reads the Polling tracker's
// ../polling-tracker/polls.json in the browser.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Election results]

Every presidential, Sejm, Senate, European Parliament and referendum vote in Poland since 1990: the facts of each election, its national results and seats, and maps of every gmina and constituency. Choose an election in the strip at the top. A Senate election has its own view: the seats by party, a map of the districts (or, where PKW's files allow, of the gminas) coloured by who won, every district's candidates, and, for each senator, a career across the Sejm, the Senate and the European Parliament, with the moves between them. A Sejm or European Parliament election also lists everyone who sat in that term, with the party each was elected for, and a click on a name opens the same career. The map shows who came first, any list's or candidate's share, a head to head, turnout, or the change since any other election, by gmina or by constituency, and the "Widać zabory!" button draws the borders of 1914 between the partitions. Search for a gmina, or click one, for its history in every election, and see how the results line up with the make-up of the gminas in Votes by local characteristics. "Polls now" shows the latest estimate of the #link("polling-tracker.html")[Polling tracker]. For how views differ by place of residence and education among voters themselves, see #link("political-attitudes.html")[Social and political trends]\; for the party system behind the results, #link("party-system-indicators.html")[Party system indicators].

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

The results are the National Electoral Commission's (PKW) files, by gmina and by polling station. National figures are PKW's announced results in Dziennik Ustaw and Monitor Polski, corrected where PKW corrected them after court rulings (the referendums of 2003 and 2015); where the files differ from the announcements, the maps follow the files. Turnout is PKW's own figure. The semi-free Sejm election of 1989 is not in PKW's files, so it has its facts and seats only, and the Senate of 1989 is not yet on the page.

Gminas are today's (the boundaries of 2022, with Warsaw as one). Results from before the 1999 reform are mapped onto them through a crosswalk built for this page from the gminas' codes, names and electorates: where a gmina of the 1990s later split, its result covers all of its successors, and the few that split in two are shared between them by electorate. Constituencies are each election's own, with presidential elections and referendums on the Sejm constituencies of the time; "Today's 41" adds every election up by the current Sejm constituencies, without votes abroad, so that any two elections can be compared like for like. Seats by constituency come from PKW's lists of the candidates elected.

The Senate is voted for by candidate, not by list. From 1991 to 1997 each of the 49 voivodeships elected two senators (three in Warsaw and Katowice), from 2001 to 2007 forty districts elected two to four, voters having as many votes as there were seats, and from 2011 there are 100 single-member districts. A senator takes the colour of the party whose committee nominated them, from the committee's name; a committee of electors, or one that names no party, is grey. PKW's files give candidates' votes by gmina for 1993, 1997, 2001, 2007, 2011, 2015 and 2019 (polling districts added up), by powiat only for 2005, and not below the district for 1991 and 2023, so the map of gminas is not offered for those. Eight districts of 2011 to 2023 lie inside one city (Warsaw, Wrocław, Łódź, Kraków) and share one shape on the map; the cartogram shows each. The careers come from the Senate's own pages on its senators, the European Parliament's open data and my Polish party tracker; where no official source gives the date a senator joined or left in mid-term, Polish Wikipedia's lists of senators supply it, citing the act, and such dates are marked on the career. Senators are matched to deputies and MEPs only where the name and a birth date, birth year, birthplace or the page's own statement of earlier service agree. Where records of the same name carry no such detail, I checked them by hand against Polish Wikipedia, and a few I could not settle are left as separate people. For a deputy, the party is the lead party of the club they started the term in; for an MEP, their national party when the mandate began. A move is a person's next mandate in another body; it is "straight on" when it began within 31 days of the last one ending.

Lists and candidates take the colours of the parties behind them; a candidate whom no party with a colour backed is grey. Change maps follow each Sejm list through the lineages of Party system indicators, the same candidate between elections, or a camp: the lists or candidates of one colour added up. The borders of 1914 are those of CShapes 2.0, and each gmina belongs to the empire its centre lay in. Votes by local characteristics sets each gmina's result against its characteristics from Statistics Poland's Local Data Bank, kept every five years and taken at the year nearest the election; its correlations are weighted by electorate and describe places, not voters.

Sources: PKW (danewyborcze.kbw.gov.pl) and its announcements (through the Sejm's ELI service); Statistics Poland's Local Data Bank; the national register of boundaries (PRG, GUGiK); G. Schvitz, L. Girardin, S. Rüegger, N. B. Weidmann, L.-E. Cederman and K. S. Gleditsch, "Mapping the International System, 1886–2019: The CShapes 2.0 Dataset", Journal of Conflict Resolution 66 (1), 2022; my Polish party tracker; the Senate's own pages on its senators (senat.gov.pl and its archive) and the European Parliament's open data (data.europarl.europa.eu); Polish Wikipedia for candidates' backing parties, the presidents' oaths of office and, flagged where used, the dates of senators who joined or left in mid-term.
]

#html.elem("iframe", attrs: (
  src: "../../assets/election-results/index.html",
  class: "embed-frame embed-frame--app",
  title: "Election results: every Polish presidential, Sejm, Senate, European Parliament and referendum vote since 1990",
  loading: "lazy",
))[]
