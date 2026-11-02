#set document(title: [Governments])
#metadata((
  title: "Governments",
  description: "Who sat in each of Poland's 23 cabinets since 1988: ministers, reshuffles, ministries and their changes, and the party, age and gender of those who held them",
)) <website-metadata>

// The page is a standalone app, assets/governments/index.html (+ app.js), embedded here
// like Party manifestos. Its data, governments.json, comes from Governments/ in the Polish
// political analyses repo (analysis/90_export.R) and is copied in by
// scripts/sync-analyses.py on each build.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Governments]

Who sat in each of Poland's 23 cabinets from September 1988 to 2026, and when. For every government the page shows each minister's time in post, coloured by party, and the changes made during its term: who joined, who left, who moved and who took on a second post. A second view follows each ministry through its mergers, splits and renamings, and lists everyone who headed it. A third follows one minister across governments. Coalitions shows how long each cabinet lasted, what share of its Sejm term it served, which parties formed it, whether it had a majority of seats when it took office, and how it ended: after an election, by resignation or forced out by the Sejm. It also follows the governing parties' seats against a majority day by day, who joined and left each coalition, and how public support for the government rose and fell around each change of government. The statistics compare cabinets by the share of women, the age of ministers at appointment, the pace of change and the portfolios held by each party. Titles are in English, with the Polish ones a click away. See also #link("party-genealogy.html")[Parties in the Sejm], whose cabinets these are, and #link("election-results.html")[Election results].

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

How cabinets last and end comes from my #link("party-genealogy.html")[Parties in the Sejm] tracker: its dates, the reason each cabinet ended, whether it had a majority, and the seats of the governing parties' clubs on the day of appointment. A cabinet's Sejm term is the term in which it was appointed, and it outlasted the term if it was still in office when the term ended; a cabinet that began in a term still running has no share of term. A cabinet is forced out by the Sejm if it fell to a vote of no confidence, including a constructive one, or lost a confidence vote. Rakowski's cabinet took office before the Contract Sejm and so has no term, and Waldemar Pawlak's first government, which was never formed, is left out of the counts.

The parliamentary majority is the sum of the seats of the clubs that sit for the governing parties, day by day, against the 231 that make a majority of the Sejm's 460. A club's seats go to the party that leads it, external support is not counted, and a cabinet that stays on after an election has the seats its parties hold in the new Sejm until its successor is sworn in. The coalition partners are each governing party's time in a cabinet; a party that leaves the day another joins (a rename or a merger) is shown as a handover, not as a departure. Public support is my #link("political-attitudes.html")[Government support] trend: the average position of Poles, from 0 (opponent) to 10 (supporter), from a model that pools every survey question on support for the government of the day, with the average for the voters of the governing parties and for the voters of the other parties in the months each group exists. The levels for a cabinet are the means of its first and last three full calendar months, and a cabinet in office for less than six full months has none. These are descriptions, not estimates of what a government did to its own support: governments take office after elections and often when opinion is high, and support tends to fall whoever governs.

The page covers the prime minister, deputy prime ministers, ministers heading a ministry, ministers without portfolio and heads of central offices and committees of ministerial rank. Deputy ministers (secretaries and undersecretaries of state) are not included.

Every appointment and dismissal is checked against Monitor Polski, the official gazette, through the Sejm's open API: of the 1,065 appointments and departures, 1,055 are confirmed by an act, and each post on the page links to the acts that confirm it. For the remaining ten, and for the few periods where the gazette is missing from the interface, the dates are those of the cabinet's article in Polish Wikipedia, and the page marks them as unconfirmed.

A minister who stays through a reshuffle, a change of government or a ministry's renaming is not counted as leaving and rejoining: the page counts a continuous tenure in one post, joining the dates of a minister who is re-appointed with a gap of three days or less. Moving to another post is a move, and taking a second post is shown as an additional one.

A ministry's lineage links the names a portfolio has had (for example, Transport and Infrastructure) so that its holders can be followed across renamings, mergers and splits. The grouping is my reading of the changes. For each name the page gives the Prime Minister's or Council of Ministers' regulation on the ministry's scope of action that is nearest in date in the Sejm's legal database, and, for a merger or transfer of functions, the amendments to the act on departments of government administration dated within two years of it whose text names the ministry. They show the legal setting of each change and do not prove that a given act created the ministry: for many names before 1997 the database holds no such regulation, and the page says so. The English titles are my translations of the Polish names; the 2026 ministries use the government's own English names.

A minister's party is the one printed beside their name in the cabinet's Wikipedia article, or in another cabinet's article within four years, or their party membership in Wikidata at the time. A few are from a source I read and checked by hand, which the minister's page names. Non-party ministers are counted as such, including those nominated by a party. Where none of these exists the party is shown as not recorded, which is not the same as non-party.

Sources: #link("https://monitorpolski.gov.pl")[Monitor Polski] and the #link("https://api.sejm.gov.pl")[Sejm's open API] for appointments, dismissals, dates and regulations; the Chancellery of the Prime Minister for the current composition; #link("https://pl.wikipedia.org")[Polish Wikipedia] (CC BY-SA 4.0) for party labels and for dates no act confirms; #link("https://www.wikidata.org")[Wikidata] (CC0) for gender, birth dates and party memberships.

Ben Stanley, 2026. _Governments_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/governments/index.html",
  class: "embed-frame embed-frame--app",
  title: "Governments: who sat in Poland's cabinets, 1988–2026",
  loading: "lazy",
))[]
