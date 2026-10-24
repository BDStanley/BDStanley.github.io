#set document(title: [Quality of democracy])
#metadata((
  title: "Quality of democracy",
  description: "Polish democracy since 1990 in the V-Dem project's expert ratings, against Central and Eastern and Western Europe, with polarisation in V-Dem and in the Sejm's roll calls",
)) <website-metadata>

// The page is a standalone app, assets/quality-of-democracy/index.html (+ app.js), embedded
// like the Polling tracker. Its data, democracy.json, democracy-countries.json and
// political-context.json, comes from Quality of democracy/ in the Polish political analyses repo
// (analysis/export_web.R) and is copied in by scripts/sync-analyses.py on each build. The app
// links to Party system indicators (#e=, #s=), the party genealogy (#pt=), Party ideology and the
// Polling tracker, and keeps its own state in this page's address (#view=, #m=, #r=, #c=, #ev=…).
// The method note (How this works) is a collapsed <details class="method"> under the
// introduction; its styles are with the other method notes in 90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Polish political analyses")]

#heading(level: 1)[Quality of democracy]

How democratic has Poland been since the end of communism, by the measures political scientists use, and how does it compare with the rest of the European Union? This page follows Polish democracy from 1990 to 2025 in the ratings of the Varieties of Democracy (V-Dem) project: its elections, courts and checks on power, media, civil society, deliberation and polarisation, set against Central and Eastern Europe and Western Europe and against the elections, governments and events of the period. Compare Poland with up to five countries, rank it among them on any measure, or follow polarisation through V-Dem's ratings and the Sejm's own votes.

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

V-Dem asks country experts, several for each country, to rate it year by year on hundreds of questions about how its politics works in practice. A statistical measurement model combines their ratings, allowing for the fact that experts use the same scale differently, and gives every score a measure of its uncertainty. The bands on the charts show V-Dem's 68% intervals, about one standard deviation either side: where two years' bands overlap, the change between them is within the uncertainty. The page uses V-Dem's version 16, released in March 2026, which runs to 2025, and never mixes it with earlier versions, because each release revises past estimates.

The high-level indices (electoral, liberal, participatory, deliberative and egalitarian democracy) run from 0 to 1 and are built from the indicators beneath them; Explain the score shows those indicators on V-Dem's own response scales, with the answers at the two ends written on each chart. Each year's score is V-Dem's average over the whole year, so 2015 is mostly months before the PiS government took office in November, and 2023 mostly months before the Tusk government took office in December. Regimes of the World, shown above the overview, is V-Dem's classification of each country as a closed or electoral autocracy, or an electoral or liberal democracy.

Poland is compared with two groups: Central and Eastern Europe, the 11 post-communist members of the EU, and Western Europe, the other 16 EU members and the United Kingdom. The regional lines are the median and middle half of the other countries in each group; Poland is never included in its own benchmark, and the median describes the countries rather than measuring a regional level of democracy.

The Polarisation view sets three of V-Dem's measures side by side: whether society is divided into hostile political camps, how deep differences of opinion on major issues run, and how often parties use hate speech. On the second and third, V-Dem's scale runs from most to least polarised, so they are reversed there, and labelled as such, so that up means more polarised on all three. Below them are two measures from my analysis of the Sejm's roll calls since 2005: the distance between PiS and PO/KO MPs on the main dimension of a dynamic ideal-point model of every contested vote, and the share of contested votes on which the two clubs' lines opposed each other.

The strip under each chart shows Sejm elections, cabinets (in the colours of their prime ministers' party families, as on my party genealogy), presidents and a short list of institutional events, each with its source. It places events beside the scores for context; it does not show that any of them caused a change.

Sources:

- Michael Coppedge, John Gerring, Carl Henrik Knutsen, Staffan I. Lindberg, Jan Teorell et al. (2026), "V-Dem [Country-Year/Country-Date] Dataset v16", Varieties of Democracy (V-Dem) Project (#link("https://doi.org/10.23696/vdemds26")[doi:10.23696/vdemds26]), CC BY-SA 4.0.
- Daniel Pemstein, Kyle L. Marquardt, Eitan Tzelgov, Yi-ting Wang, Juraj Medzihorsky, Joshua Krusell, Farhad Miri and Johannes von Römer (2026), "The V-Dem Measurement Model: Latent Variable Analysis for Cross-National and Cross-Temporal Expert-Coded Data", V-Dem Working Paper No. 21, 11th edition.
- Elections and cabinets: my Polish party tracker (see #link("party-genealogy.html")[Parties in the Sejm]).
- Sejm roll calls: my analysis of every recorded vote in the Sejm since 2005, from the Sejm's API.

Suggested citation: Stanley, Ben. 2026. _Quality of democracy_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/quality-of-democracy/index.html",
  class: "embed-frame embed-frame--app",
  title: "Quality of democracy: Poland in V-Dem, 1990–2025",
  loading: "lazy",
))[]
