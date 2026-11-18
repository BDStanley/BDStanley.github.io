#set document(title: [Polling tracker: Geography])
#metadata((
  title: "Polling tracker: Geography",
  description: "Where the seats would fall this week, constituency by constituency, and what a vote is worth in each Sejm constituency, 1991–2023: electors, votes and residents per seat, what a list needs for a seat, and a profile of each constituency",
)) <website-metadata>

// The Geography address of the Polling tracker (see ../polling-tracker.typ): the app with ?s=geography. It took over the
// Constituencies page (removed on Ben's decision of 18 November 2026; constituencies.typ now redirects here). Its data are the
// tracker's and the Constituencies project's (assets/constituencies/*.json, written by Constituencies/analysis/90_export.R and
// copied in by scripts/sync-analyses.py). Views: Map, What a vote is worth, If seats followed population. It links to Election
// results (#e=, #u=c<n>) and to Legislative activity (#view=mps).

#html.elem("p", attrs: (class: "teaching-back"))[#link("../../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Polling tracker]

Where the seats would fall if an election were held this week, constituency by constituency, and whether a vote is worth the same everywhere. Seats are shared out among constituencies by their number of residents, with at least seven in each, and then among the lists by the votes they get in each one; both steps make some votes count for more than others. The term selector shows every Sejm election since 1991 on its own constituencies. Select a constituency for its projected seats, its results and MPs, and how it compares with the rest of Poland.

#html.elem("nav", attrs: (class: "tracker-nav", "aria-label": "Polling tracker sections"))[
  #html.elem("a", attrs: (href: "../polling-tracker.html", "data-s": "polls"))[Polls]
  #html.elem("a", attrs: (href: "seats.html", "data-s": "seats"))[Seats]
  #html.elem("a", attrs: (href: "geography.html", "data-s": "geography", class: "on", "aria-current": "page"))[Geography]
  #html.elem("a", attrs: (href: "scenarios.html", "data-s": "scenarios"))[Scenarios]
]

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

For the current term, the projected seats and their certainty are the tracker's (see #link("seats.html")[Seats]): this week's estimates, or a scenario that arrives in the link, spread across the 41 constituencies by each party's regional pattern and allocated by D'Hondt in each of 250 simulations.

The Electoral Code (art. 201 to 203) gives every constituency at least seven seats and divides the rest by a uniform norm of representation: the country's residents divided by the number of seats, rounded to the nearest whole seat. The Polish Electoral Commission proposes new boundaries and seat numbers when the population changes, and the Sejm decides. The 41 constituencies and their seat numbers have been the same since 2001, with only a few seats moved, while the population has not. A vote's worth is counted three ways. Electors per seat is the registered electorate divided by the seats, which is what malapportionment means. Votes cast per seat adds turnout. Residents per seat is the basis the law uses. Each is shown as an index: the national average divided by the constituency's figure, so that 1 is average and above 1 means a vote counts for more. The malapportionment index adds up how far the seats are from the electors (or residents) over all constituencies, as half the sum of the differences between each constituency's share of the seats and of the electors.

Electors registered abroad or on ships are counted by the Commission in the Warsaw constituency: 637,000 in 2023, against 43,000 in 2001. They vote for Warsaw's seats, so the headline measures count them, and every measure is also shown without them. Most of the jump in the malapportionment of electors in 2023, from 2.6 per cent to 4.0 per cent, is this. Residents are the population on 31 December of the year before the election, from Statistics Poland, added up over today's gminas, and are not available on this basis before 2001.

Within a constituency, seats go to lists by D'Hondt's rule in 1993 to 1997 and from 2005, by a modified Sainte-Laguë rule in 2001 and by the largest remainder in 1991. The votes behind the last seat won are the quotient of the last seat; under D'Hondt a list with more than 1 / (seats + 1) of the votes in contention is sure to win a seat. Lists below the national threshold, 5 per cent (8 for a coalition), take no part. The 69 seats filled from national lists in 1991 to 1997 are left out of the constituency measures. The 1991 allocation cannot be reproduced from the list votes, so 1991 has no last-seat measures, and one constituency (Sieradz, 2007) does not reproduce exactly from the votes in the Commission's file. The MPs shown are those who held a seat for the constituency in the term after the election, replacements included, with no ranking.

Sources:

- Polish National Electoral Commission (PKW): results by constituency and list, 1991–2023, and the Electoral Code (Dz.U.2026.1261, art. 201–203).
- Statistics Poland (GUS), Local Data Bank: population on 31 December.
- My #link("../election-results.html")[Election results] (constituency geometry and results) and #link("../legislative-activity.html")[Legislative activity] (MPs).

Ben Stanley, 2026. _Polling tracker_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../../assets/polling-tracker/index.html?s=geography",
  class: "embed-frame embed-frame--app",
  title: "Polling tracker: the constituencies, their projected seats and what a vote is worth",
  loading: "lazy",
))[]
