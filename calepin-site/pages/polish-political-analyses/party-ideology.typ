#set document(title: [Party ideology])
#metadata((
  title: "Party ideology",
  description: "Where Polish parties stand in European ideological space, from the Chapel Hill Expert Survey and The PopuList, and how they compare with their own voters",
)) <website-metadata>

// The page is a standalone app, assets/party-ideology/index.html (+ app.js),
// embedded here like the Polling tracker. Its data, ideology.json, comes from
// Party ideology/ in the Polish political analyses repo (analysis/Party
// ideology.R) and is copied in by scripts/sync-analyses.py on each build. The
// app also reads the Polling tracker's polls.json for current polling, and
// links to the party genealogy (#pt=<Party tracker id>) and the tracker.
// The method note (How this works) is a collapsed <details class="method"> under the
// introduction; its styles are with the other method notes in 90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Polish political analyses")]

#heading(level: 1)[Party ideology]

Where Polish parties stand on the economy, on cultural questions, on European integration and on democracy, as rated by political scientists in the Chapel Hill Expert Survey, against parties across Europe; how those ratings compare with the classification of parties as populist, far right or far left in The PopuList; how each Polish party compares with its own voters, from my #link("political-attitudes.html")[Social and political trends]; and how polarised and how aligned party systems are across Europe. Pick a party to follow it through two decades of surveys.

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

Positions come from the Chapel Hill Expert Survey (CHES), which asks political scientists who study each country to place its parties' leaderships on a set of scales. Each position shown is the mean of the experts' ratings. The trend file covers seven waves, from 1999 to 2024; Poland is included from 2002. The latest wave was fielded between October and December 2024, when 609 experts rated 279 parties in 31 countries. For 2024, the spread of the experts' ratings (their standard deviation) is shown for the selected party. Most scales run from 0 to 10; European integration runs from 1 (strongly opposed) to 7 (strongly in favour). Some questions were asked only in some waves, and a wave is labelled by its survey year although its fieldwork can run into the next (the 2019 wave was fielded in February–May 2020).

The Map places parties on any two dimensions. Polish parties are drawn in their own colours, sized by the vote for the list they ran on at the election before each wave, and their paths join their positions at earlier waves; the other parties, paler, take the colour of their PopuList category or of their party family (blue for the right, red for the left, gold for liberals, green for greens). Two smoothed fit lines, which can be switched off, show how the dimension up the side varies with the one across: a solid line through the Polish parties and a dashed grey one through the other parties compared. The Profile shows one party on every dimension against a comparison group, with the change since the previous wave. Over time follows the Polish parties wave by wave against the middle half of the parties compared. The closest parties are those with the smallest typical gap on economic left–right, GAL–TAN (libertarian–authoritarian), European integration and, from 2014, anti-elite rhetoric.

Parties and voters sets each Polish party, as the experts place it, beside its own voters, from my #link("political-attitudes.html")[Social and political trends] at the time of each wave's fieldwork. On left–right, the voters' figure is their average self-placement from 0 (left) to 10 (right). On the other 24 dimensions with a voters' measure, from economic left–right and GAL–TAN to immigration, European integration and judicial independence, it is the voters' average position on 0–10, pooled from survey questions on that issue; a party's position on a 1–7 scale is rescaled to 0–10. The voters' scales are not the experts' scales, so the order of the parties and the size of the gaps say more than any single pair of numbers. A party's voters are those who intended to vote for it, or voted for it at the last Sejm election, followed by party lineage. On the Map, the Party voters switch places each Polish party's voters in the same two-dimensional space, joined to the party by a dashed line, whenever both axes have a voters' measure.

The PopuList classifies European parties as populist, far right, far left and Eurosceptic, period by period, on the judgement of country experts; it covers national elections from 1989 to 1 May 2026. Parties whose classification is uncertain, or on which experts disagree, are marked as borderline. The Populism view sets these categories against the experts' CHES ratings of how important anti-elite rhetoric is to each party, and lists the parties on which the two disagree. CHES and PopuList parties are linked through PartyFacts, with corrections by hand.

Party systems compares Poland with other countries. Polarisation is Dalton's index (0–10), the vote-weighted spread of parties around the mean position. Alignment is the vote-weighted correlation between parties' economic and cultural positions: positive where the economic right is also the traditional–authoritarian side, as in most of Western Europe; negative where the economic left is, as in much of Central and Eastern Europe. Polish parties are weighted by the vote for the lists they ran on, from my Polish party tracker; other countries by CHES's vote shares.

CHES rates parties, but Polish elections are fought by lists, so two parties on one list share a list's vote. The 2024 wave rated Konfederacja before KKP left it in 2025, and predates R+.

Sources:

- Seth Jolly, Ryan Bakker, Liesbet Hooghe, Gary Marks, Jonathan Polk, Jan Rovny, Marco Steenbergen and Milada Anna Vachudova (2022), "Chapel Hill Expert Survey trend file, 1999–2019", Electoral Studies 75: 102420.
- Jan Rovny, Jonathan Polk, Ryan Bakker, Liesbet Hooghe, Seth Jolly, Gary Marks, Marco Steenbergen and Milada Anna Vachudova (2025), "The 2024 Chapel Hill Expert Survey on political party positioning in Europe: Twenty-five years of party positional data", Electoral Studies 97: 102981 (#link("https://www.chesdata.eu")[chesdata.eu]).
- Matthijs Rooduijn, Andrea L. P. Pirro, Daphne Halikiopoulou, Caterina Froio, Stijn van Kessel, Sarah L. de Lange, Cas Mudde and Paul Taggart (2024), "The PopuList: A Database of Populist, Far-Left, and Far-Right Parties Using Expert-Informed Qualitative Comparative Classification (EiQCC)", British Journal of Political Science 54(3): 969–978, and The PopuList 4.0 (#link("https://popu-list.org")[popu-list.org]).
- Voters' positions: my Social and political trends, pooled from CBOS, the Eurobarometers, the European Social Survey, the Polish National Election Study and other surveys (see that page).

Suggested citation: Stanley, Ben. 2026. _Party ideology_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/party-ideology/index.html",
  class: "embed-frame embed-frame--app",
  title: "Party ideology: Polish parties in European ideological space",
  loading: "lazy",
))[]
