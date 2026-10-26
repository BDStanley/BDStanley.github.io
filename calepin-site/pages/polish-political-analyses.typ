#set document(title: [Polish political analyses])
#metadata((
  title: "Polish political analyses",
  description: "Interactive analyses of Polish politics: parties, elections and public opinion",
)) <website-metadata>

// Landing page for the analyses built from the Polish political analyses repo
// (BDStanley/Polish-political-analyses). One row per analysis, styled like the
// Teaching course list (.course-*), with a thumbnail in place of the hexsticker.
// On a computer screen or a landscape tablet the page widens and the rows sit
// in two columns (.analysis-landing / .analysis-list in 90_site.css).
// To add one: a page in pages/polish-political-analyses/, a thumbnail in
// assets/analyses/ (480 × 300 px), and a row below.

#html.elem("div", attrs: (class: "analysis-landing"))[
#html.elem("div", attrs: (class: "course-list analysis-list"))[
#html.elem("div", attrs: (class: "course-row"))[
#html.elem("a", attrs: (class: "analysis-thumb", href: "polish-political-analyses/polling-tracker.html", tabindex: "-1", "aria-hidden": "true"))[
#html.elem("img", attrs: (src: "../assets/analyses/polling-tracker.png", alt: "", width: "480", height: "300", loading: "lazy"))
]
#html.elem("div", attrs: (class: "course-body"))[
#html.elem("a", attrs: (class: "course-name", href: "polish-political-analyses/polling-tracker.html"))[Polling tracker]
#html.elem("div", attrs: (class: "course-desc"))[A Bayesian poll-of-polls for the Sejm: voting intention since the October 2023 election, seat projections for every week, polling-house effects, and a scenario builder for vote shares, joint lists and government options that keeps the model's uncertainty. The polls of every earlier Sejm term since 1991 are there too, set against the elections that opened and closed each term.]
]
]
#html.elem("div", attrs: (class: "course-row"))[
#html.elem("a", attrs: (class: "analysis-thumb", href: "polish-political-analyses/election-results.html", tabindex: "-1", "aria-hidden": "true"))[
#html.elem("img", attrs: (src: "../assets/analyses/election-results.png", alt: "", width: "480", height: "300", loading: "lazy"))
]
#html.elem("div", attrs: (class: "course-body"))[
#html.elem("a", attrs: (class: "course-name", href: "polish-political-analyses/election-results.html"))[Election results]
#html.elem("div", attrs: (class: "course-desc"))[Every presidential, Sejm, European Parliament and referendum vote since 1990: the facts, results and seats of each, maps of every gmina and constituency, the change between any two elections, the partitions of 1914, each gmina's history, and how the results line up with the make-up of the gminas.]
]
]
#html.elem("div", attrs: (class: "course-row"))[
#html.elem("a", attrs: (class: "analysis-thumb", href: "polish-political-analyses/voting-behaviour.html", tabindex: "-1", "aria-hidden": "true"))[
#html.elem("img", attrs: (src: "../assets/analyses/voting-behaviour.png", alt: "", width: "480", height: "300", loading: "lazy"))
]
#html.elem("div", attrs: (class: "course-body"))[
#html.elem("a", attrs: (class: "course-name", href: "polish-political-analyses/voting-behaviour.html"))[Voting behaviour]
#html.elem("div", attrs: (class: "course-desc"))[Who votes for whom in Sejm elections from 1997 to 2023, and who stays at home, by age, gender, education, where people live, how often they go to church and how well off they are; how much each characteristic changes the choice; and a calculator for a hypothetical voter with the model's uncertainty, from the national post-election surveys.]
]
]
#html.elem("div", attrs: (class: "course-row"))[
#html.elem("a", attrs: (class: "analysis-thumb", href: "polish-political-analyses/political-attitudes.html", tabindex: "-1", "aria-hidden": "true"))[
#html.elem("img", attrs: (src: "../assets/analyses/political-attitudes.png", alt: "", width: "480", height: "300", loading: "lazy"))
]
#html.elem("div", attrs: (class: "course-body"))[
#html.elem("a", attrs: (class: "course-name", href: "polish-political-analyses/political-attitudes.html"))[Social and political trends]
#html.elem("div", attrs: (class: "course-desc"))[How Poles' attitudes, values and lives have changed since 1990, from left–right self-placement and support for EU membership to trust in institutions, religion and living conditions, each trend pooled from every comparable survey into one monthly estimate, with breakdowns by party preference, age, gender, education and place of residence.]
]
]
#html.elem("div", attrs: (class: "course-row"))[
#html.elem("a", attrs: (class: "analysis-thumb", href: "polish-political-analyses/party-system-indicators.html", tabindex: "-1", "aria-hidden": "true"))[
#html.elem("img", attrs: (src: "../assets/analyses/party-system-indicators.png", alt: "", width: "480", height: "300", loading: "lazy"))
]
#html.elem("div", attrs: (class: "course-body"))[
#html.elem("a", attrs: (class: "course-name", href: "polish-political-analyses/party-system-indicators.html"))[Party system indicators]
#html.elem("div", attrs: (class: "course-desc"))[Fragmentation, disproportionality, volatility, nationalisation, polarisation and the closure of government competition for every Sejm election since 1991 and for the current polls, with the lists, lineages, party positions and cabinets behind each number.]
]
]
#html.elem("div", attrs: (class: "course-row"))[
#html.elem("a", attrs: (class: "analysis-thumb", href: "polish-political-analyses/party-ideology.html", tabindex: "-1", "aria-hidden": "true"))[
#html.elem("img", attrs: (src: "../assets/analyses/party-ideology.png", alt: "", width: "480", height: "300", loading: "lazy"))
]
#html.elem("div", attrs: (class: "course-body"))[
#html.elem("a", attrs: (class: "course-name", href: "polish-political-analyses/party-ideology.html"))[Party ideology]
#html.elem("div", attrs: (class: "course-desc"))[Where Polish parties stand on the economy, cultural questions, European integration and democracy, set against parties across Europe in the Chapel Hill Expert Survey since 2002; how each party compares with its own voters; polarisation and alignment in party systems across Europe; and how expert ratings of anti-elite rhetoric compare with The PopuList's classification of parties as populist, far right or far left.]
]
]
#html.elem("div", attrs: (class: "course-row"))[
#html.elem("a", attrs: (class: "analysis-thumb", href: "polish-political-analyses/quality-of-democracy.html", tabindex: "-1", "aria-hidden": "true"))[
#html.elem("img", attrs: (src: "../assets/analyses/quality-of-democracy.png", alt: "", width: "480", height: "300", loading: "lazy"))
]
#html.elem("div", attrs: (class: "course-body"))[
#html.elem("a", attrs: (class: "course-name", href: "polish-political-analyses/quality-of-democracy.html"))[Quality of democracy]
#html.elem("div", attrs: (class: "course-desc"))[How Polish democracy has fared since 1990 in the V-Dem project's expert ratings of elections, courts, media, civil society and polarisation, set against Central and Eastern and Western Europe, with the elections, governments and events around them and the polarisation of the Sejm's roll calls.]
]
]
#html.elem("div", attrs: (class: "course-row"))[
#html.elem("a", attrs: (class: "analysis-thumb", href: "polish-political-analyses/party-genealogy.html", tabindex: "-1", "aria-hidden": "true"))[
#html.elem("img", attrs: (src: "../assets/analyses/party-genealogy.png", alt: "", width: "480", height: "300", loading: "lazy"))
]
#html.elem("div", attrs: (class: "course-body"))[
#html.elem("a", attrs: (class: "course-name", href: "polish-political-analyses/party-genealogy.html"))[Parties in the Sejm]
#html.elem("div", attrs: (class: "course-desc"))[The lifespans, Sejm seats, government status and genealogy of every party that has sat in the Sejm since 1989: the splits, mergers, renames and alliances that link them. Trace any party's predecessors and successors, follow the seat strength of each party and party family day by day, and see every MP who changed clubs: who, when, and from which party to which.]
]
]
]
]
