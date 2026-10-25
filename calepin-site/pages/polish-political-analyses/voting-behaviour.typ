#set document(title: [Voting behaviour])
#metadata((
  title: "Voting behaviour",
  description: "Who votes for whom in Poland's Sejm elections, 1997–2023, and who stays at home, by age, gender, education, residence, religious attendance and socio-economic position, with a calculator for a hypothetical voter",
)) <website-metadata>

// The page is a standalone app, assets/voting-behaviour/index.html (+ app.js), embedded like the
// Quality of democracy page. Its data, voting.json and one calc-<year>.json per election (the
// calculator's posterior draws), comes from Voting behaviour/ in the Polish political analyses repo
// (analysis/70_export_web.R) and is copied in by scripts/sync-analyses.py on each build. The app keeps its
// own state in this page's address (#view=, #show=, #sp=, #ct=, #el=, #age= and others).
// The method note (How this works) is a collapsed <details class="method"> under the introduction; its
// styles are with the other method notes in 90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Polish political analyses")]

#heading(level: 1)[Voting behaviour]

Who votes for whom in Poland's Sejm elections, and who stays at home? This page follows age, gender, education, the size of the place people live in, how often they go to church and how well off they are across the eight elections from 1997 to 2023, in the Polish General Election Studies, the national surveys fielded after each election. Who votes shows each election's turnout and each list's support, split by any of these characteristics. What changes the vote takes one characteristic at a time and shows how much more or less likely each choice is. Hypothetical voter lets you describe a person and see how likely a person like that was, at an election, to stay at home or to vote for each list, with the model's uncertainty.

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

The surveys interview about 15,000 people, and about 13,500 are used here. Everyone is asked whether they voted and, if so, for which list. People who say they voted but will not say for which list (4 to 9 per cent of voters), and those with a missing answer to any of the characteristics, are left out of the main estimates; a check that brings them back, filling in the missing list from what else is known about them, changes none of the conclusions.

One statistical model covers all eight elections. For each election it estimates the probability that a person stayed at home and the probability that they voted for each list on the ballot, from the six characteristics. For the five lists that run through many elections (PiS, KO, Lewica, PSL and Konfederacja) the effect of each characteristic is allowed to change gradually from one election to the next. Six combinations of characteristics, such as education mattering more or less depending on age, are held fixed across elections. This version was chosen over simpler and more flexible ones by asking how well each predicts an election it has not seen.

Socio-economic position is a score within each election, built from a person's occupation and household income, and shown as five equal groups from low to high. It is a relative position, not the same real income in 1997 and in 2023, and it is measured with error, which pulls its estimated effect towards zero; its effect is net of education. Place of residence has five sizes, from a village to a city of 500,000 or more.

Surveys overstate turnout, by 8 to 15 points at these elections, and people tend to recall voting for the winner, so the survey's shares for the winning list are up to 10 points above the official result. The page shows both. The effects and the calculator can be adjusted so that the survey reproduces the official turnout and list shares; the effects hardly change, but the probabilities do, and the unadjusted version is the default.

A list has a category of its own at an election if it won seats and had at least 40 voters in the survey, or if it is one of the five long-running lists; every other list is "Other lists", which means different lists at different elections and is never drawn as a series. The calculator gives no estimate where a profile is impossible (a degree at 20 or under) or where fewer than about 15 respondents resemble the person on a pair of characteristics the model combines, and warns below about 50.

The model treats respondents as independent, though the surveys are clustered. I checked this against a version that resamples whole clusters of respondents (the sampling point in 2023 and otherwise the municipality, locality or interviewer, the best the data allow; 2005 records none): allowing for clustering changes the standard errors of the effects by about 1 per cent on average, so the intervals are not widened. They do rest on the assumption that effects change gradually from one election to the next, which is what lets the model use all eight elections together; separate models for each election would give intervals about 1.6 times as wide.

These are associations measured at eight elections, not causes, and not the same people followed over time. The page says nothing about the current term.

Sources:

- Polish General Election Study (Polskie Generalne Studia Wyborcze), post-election surveys 1997–2023.
- Official turnout and list shares: the National Electoral Commission (PKW), as in my #link("election-results.html")[Election results] and #link("party-genealogy.html")[party tracker].
- Occupational status: the International Socio-Economic Index of occupational status (ISEI); Harry B. G. Ganzeboom and Donald J. Treiman (1996), "Internationally comparable measures of occupational status for the 1988 International Standard Classification of Occupations", _Social Science Research_ 25: 201–239.
- Population of municipalities for the 2023 place of residence: Statistics Poland's Local Data Bank (BDL).

Ben Stanley, 2026. _Voting behaviour_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/voting-behaviour/index.html",
  class: "embed-frame embed-frame--app",
  title: "Voting behaviour: who votes for whom in Polish Sejm elections, 1997–2023",
  loading: "lazy",
))[]
