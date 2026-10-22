#set document(title: [Party system indicators])
#metadata((
  title: "Party system indicators",
  description: "Fragmentation, disproportionality, volatility, nationalisation, polarisation and government closure in the Polish party system, 1991 to the current polls",
)) <website-metadata>

// The page is a standalone app, assets/party-system-indicators/index.html (+ app.js),
// embedded like the Polling tracker. Its data, party_system.json, comes from
// Party system analysis/ in the Polish political analyses repo (analysis/export_web.R,
// run at the end of "Party system analysis.R") and is copied in by
// scripts/sync-analyses.py on each build. The polls column is computed in the
// browser from the Polling tracker's ../polling-tracker/polls.json, falling back
// to the snapshot in party_system.json; app.js's systemStats() mirrors
// system_stats() in the analysis, so a change to one needs the other.
// The method note (How these are measured) is a collapsed <details class="method"> under the
// introduction; its styles are with the other method notes in 90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Polish political analyses")]

#heading(level: 1)[Party system indicators]

The standard measures of a party system for every Sejm election since 1991, and for the current polls: how fragmented the vote and the Sejm are, how disproportionally votes become seats, how much support moves between elections, how evenly parties are supported across the country, how far apart they stand, and how closed competition for government has been. Choose an election in the strip at the top and every section shows the lists, lineages, party positions and cabinets behind its numbers. The polls column follows the latest estimate of the #link("polling-tracker.html")[Polling tracker].

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How these are measured]

Election results are the PKW's national results by electoral committee (list), as recorded in my Polish party tracker; constituency and gmina results come from the PKW (KBW) files. Each list is linked to the list or lists it continues at the next election through the party tracker's record of parties, splits, mergers and alliances, and each belongs to one of the tracker's 14 party families. The polls column uses the Polling tracker's latest median vote shares (without Other, rescaled to sum to 100%) and its median seat projection; only the measures that need national shares and seats are available for it.

Fragmentation: the Laakso–Taagepera effective number of parties is one over the sum of squared list shares, of votes or of seats; Golosov's (2010) index compares each list with the largest, so that many small lists inflate it less. Disproportionality: the Gallagher index is the square root of half the sum of squared differences between each list's vote and seat shares; wasted votes are the share cast for lists that won no seats, and the seat bonus is the largest list's seat share minus its vote share.

Volatility is the Pedersen index, half the sum of absolute changes in vote share, computed on lineages rather than lists, so that a list counts as continuing when its parties stand again under another name or in another alliance. Following Powell and Tucker (2014), it is split into Type A (lists that are new or withdrawn, including lists with no party in the tracker) and Type B (change among continuing lineages). It is also split between and within party families, in two ways: with each list at the earlier election taking the family of the list it continues into, which sums to the total, and with each list in its own family at both elections (plain), where a lineage that changes family counts as moving between families.

Nationalisation is Bochsler's (2010) standardised party nationalisation score, a Gini-based measure corrected for the number and size of territorial units, for each list across the 41 constituencies and across gminas; the system score is the vote-weighted mean over seat-winning lists. For 1991, only the lists in the KBW file are covered. The inflation of the national effective number of parties over the average constituency (Moenius and Kasuya 2004) shows how far national fragmentation comes from parties being strong in different places.

Polarisation is Dalton's index, 10 × √(Σ vᵢ((pᵢ − p̄)/5)²), where pᵢ is a party's position on a 0–10 scale in the Chapel Hill Expert Survey, p̄ the vote-weighted mean position and vᵢ the party's vote share at the latest Sejm election in or before the survey year, among the lists the survey covers. It is available for the general left–right, economic left–right and GAL–TAN dimensions.

Government: the closure index of Casal Bértoa and Mair scores each change of government on alternation, formula and access, and averages the three over all changes since the first cabinet of the 1991 Sejm, either counting each change once or weighting it by the days the new cabinet was in office. Cabinets that never won a confidence vote are left out.

The CSV button downloads every indicator for every election and the polls in one file.
]

#html.elem("iframe", attrs: (
  src: "../../assets/party-system-indicators/index.html",
  class: "embed-frame embed-frame--app",
  title: "Party system indicators: Polish Sejm elections since 1991 and the current polls",
  loading: "lazy",
))[]
