#set document(title: [Governments and ministers])
#metadata((
  title: "Governments and ministers",
  description: "Who sat in each of Poland's 23 cabinets since 1988: ministers, reshuffles, ministries and their changes, and the party, age and gender of those who held them",
)) <website-metadata>

// The page is a standalone app, assets/governments/index.html (+ app.js), embedded here
// like Party manifestos. Its data, governments.json, comes from Governments/ in the Polish
// political analyses repo (analysis/90_export.R) and is copied in by
// scripts/sync-analyses.py on each build.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Governments and ministers]

Who sat in each of Poland's 23 cabinets from September 1988 to 2026, and when. For every government the page shows each minister's time in post, coloured by party or by type of post, and the changes made during its term: who joined, who left, who moved and who took on a second post. A second view follows each ministry through its mergers, splits and renamings, and lists everyone who headed it. A third follows one minister across governments. The statistics compare cabinets by the share of women, the age of ministers at appointment, the pace of change and the portfolios held by each party. Titles are in English, with the Polish ones a click away. See also #link("party-genealogy.html")[Parties in the Sejm], whose cabinets these are, and #link("election-results.html")[Election results].

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

The page covers the prime minister, deputy prime ministers, ministers heading a ministry, ministers without portfolio and heads of central offices and committees of ministerial rank. Deputy ministers (secretaries and undersecretaries of state) are not included.

The skeleton of each cabinet is the table in its Polish Wikipedia article (a pinned revision). Every appointment and dismissal is checked against Monitor Polski, the official gazette, through the Sejm's open API: of the 1,065 appointments and departures, 1,055 are confirmed by an act, and each post on the page links to the acts that confirm it. The remaining ten, and the few periods where the gazette is missing from the interface, are marked as unconfirmed. Gender and birth dates are from Wikidata.

A minister who stays through a reshuffle, a change of government or a ministry's renaming is not counted as leaving and rejoining: the page counts a continuous tenure in one post, joining the dates of a minister who is re-appointed with a gap of three days or less. Moving to another post is a move, and taking a second post is shown as an additional one.

A ministry's lineage links the names a portfolio has had (for example, Transport and Infrastructure) so that its holders can be followed across renamings, mergers and splits. The grouping is my reading of the changes. For each name the page gives the Prime Minister's or Council of Ministers' regulation on the ministry's scope of action that is nearest in date in the Sejm's legal database, and, for a merger or transfer of functions, the amendments to the act on departments of government administration dated within a year. They show the legal setting of each change and do not prove that a given act created the ministry: for many names before 1997 the database holds no such regulation, and the page says so. The English titles are my translations of the Polish names; the 2026 ministries use the government's own English names.

A minister's party is the one printed beside their name in the cabinet's Wikipedia article, or in another cabinet's article within four years, or their party membership in Wikidata at the time. A few are from a source I read and checked by hand, which the minister's page names. Non-party ministers are counted as such, including those nominated by a party. Where none of these exists the party is shown as not recorded, which is not the same as non-party.

Sources: #link("https://monitorpolski.gov.pl")[Monitor Polski] and the #link("https://api.sejm.gov.pl")[Sejm's open API]; #link("https://pl.wikipedia.org")[Polish Wikipedia] (CC BY-SA 4.0); #link("https://www.wikidata.org")[Wikidata] (CC0); the Chancellery of the Prime Minister for the current composition.

Ben Stanley, 2026. _Governments and ministers_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/governments/index.html",
  class: "embed-frame embed-frame--app",
  title: "Governments and ministers: who sat in Poland's cabinets, 1988–2026",
  loading: "lazy",
))[]
