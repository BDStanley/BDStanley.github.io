#set document(title: [Constituencies])
#metadata((
  title: "Constituencies",
  description: "What a vote is worth in each Sejm constituency, 1991–2023: electors, votes and residents per seat, what a list needs for a seat by district magnitude, and a profile of each constituency with its results, MPs, projection and covariates",
)) <website-metadata>

// The page is a standalone app, assets/constituencies/index.html (+ app.js), embedded like the Legislative activity page. Its data,
// constituencies.json, geometry.json and mps.json, comes from Constituencies/ in the Polish political analyses repo (analysis/90_export.R)
// and is copied in by scripts/sync-analyses.py on each build. The app keeps its state in this page's address (#view=, #y=, #c=). It links to
// Election results (#e=, #u=c<n>) and to Legislative activity (#view=mps). The method note (How this works) is a collapsed
// <details class="method"> under the introduction; its styles are with the other method notes in 90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Constituencies]

Is a vote in Poland's Sejm elections worth the same everywhere? Seats are shared out among constituencies by their number of residents, with at least seven in each, and then among the lists by the votes they get in each one. Both steps make some votes count for more than others. This page measures it for every Sejm election from 1991 to 2023: how many registered electors, votes cast and residents each seat stands for, how that has drifted since the 41 constituencies were drawn in 2001, and what a list needs for a seat in a small or a large constituency. Select a constituency to see its results, the MPs elected there, the Polling tracker's projection for it and how it compares with the rest of Poland.

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

The Electoral Code (art. 201 to 203) gives every constituency at least seven seats and divides the rest by a uniform norm of representation: the country's residents divided by the number of seats, rounded to the nearest whole seat. The Polish Electoral Commission proposes new boundaries and seat numbers when the population changes, and the Sejm decides. The 41 constituencies and their seat numbers have been the same since 2001, with only a few seats moved, while the population has not. The page counts a vote's worth three ways. Electors per seat is the registered electorate divided by the seats, which is what malapportionment means. Votes cast per seat adds turnout. Residents per seat is the basis the law uses. Each is shown as an index: the national average divided by the constituency's figure, so that 1 is average and above 1 means a vote counts for more. The malapportionment index adds up how far the seats are from the electors (or residents) over all constituencies, as half the sum of the differences between each constituency's share of the seats and of the electors.

Electors registered abroad or on ships are counted by the Commission in the Warsaw constituency: 637,000 in 2023, against 43,000 in 2001. They vote for Warsaw's seats, so the headline measures count them, and every measure is also shown without them. Most of the jump in the malapportionment of electors in 2023, from 2.6 per cent to 4.0 per cent, is this. Residents are the population on 31 December of the year before the election, from Statistics Poland, added up over today's gminas, and are not available on this basis before 2001.

Within a constituency, seats go to lists by D'Hondt's rule in 1993 to 1997 and from 2005, by a modified Sainte-Laguë rule in 2001 and by the largest remainder in 1991. The votes behind the last seat won are the quotient of the last seat; under D'Hondt a list with more than 1 / (seats + 1) of the votes in contention is sure to win a seat. Lists below the national threshold, 5 per cent (8 for a coalition), take no part. The 69 seats filled from national lists in 1991 to 1997 are left out of the constituency measures. The 1991 allocation cannot be reproduced from the list votes, so 1991 has no last-seat measures, and one constituency (Sieradz, 2007) does not reproduce exactly from the votes in the Commission's file. The MPs shown are those who held a seat for the constituency in the term after the election, replacements included, with no ranking.

Sources:

- Polish National Electoral Commission (PKW): results by constituency and list, 1991–2023, and the Electoral Code (Dz.U.2026.1261, art. 201–203).
- Statistics Poland (GUS), Local Data Bank: population on 31 December.
- My #link("election-results.html")[Election results] (constituency geometry and results) and #link("legislative-activity.html")[Legislative activity] (MPs), and the #link("polling-tracker.html")[Polling tracker] (projection).

Ben Stanley, 2026. _Constituencies_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/constituencies/index.html",
  class: "embed-frame embed-frame--app",
  title: "Constituencies: what a vote is worth in the Sejm, 1991–2023",
  loading: "lazy",
))[]
