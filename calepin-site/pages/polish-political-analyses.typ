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
#heading(level: 1)[Polish political analyses]

Data and interactive charts on Polish politics, from my ongoing work on the party system, elections and public opinion. Each analysis is rebuilt from its data whenever the site is updated.

#html.elem("div", attrs: (class: "course-list analysis-list"))[
#html.elem("div", attrs: (class: "course-row"))[
#html.elem("a", attrs: (class: "analysis-thumb", href: "polish-political-analyses/polling-tracker.html", tabindex: "-1", "aria-hidden": "true"))[
#html.elem("img", attrs: (src: "../assets/analyses/polling-tracker.png", alt: "", width: "480", height: "300", loading: "lazy"))
]
#html.elem("div", attrs: (class: "course-body"))[
#html.elem("a", attrs: (class: "course-name", href: "polish-political-analyses/polling-tracker.html"))[Polling tracker]
#html.elem("div", attrs: (class: "course-desc"))[A Bayesian poll-of-polls for the Sejm: voting intention since January 2025, seat projections for every week, polling-house effects, and a scenario builder for vote shares, joint lists and government options that keeps the model's uncertainty.]
]
]
#html.elem("div", attrs: (class: "course-row"))[
#html.elem("a", attrs: (class: "analysis-thumb", href: "polish-political-analyses/election-results.html", tabindex: "-1", "aria-hidden": "true"))[
#html.elem("img", attrs: (src: "../assets/analyses/election-results.png", alt: "", width: "480", height: "300", loading: "lazy"))
]
#html.elem("div", attrs: (class: "course-body"))[
#html.elem("a", attrs: (class: "course-name", href: "polish-political-analyses/election-results.html"))[Election results]
#html.elem("div", attrs: (class: "course-desc"))[Every presidential, Sejm, European Parliament and referendum vote since 1990: the facts and results of each, maps of every gmina and constituency, the change between any two elections, the partitions of 1914, each gmina's history, and how the results line up with the make-up of the gminas.]
]
]
#html.elem("div", attrs: (class: "course-row"))[
#html.elem("a", attrs: (class: "analysis-thumb", href: "polish-political-analyses/political-attitudes.html", tabindex: "-1", "aria-hidden": "true"))[
#html.elem("img", attrs: (src: "../assets/analyses/political-attitudes.png", alt: "", width: "480", height: "300", loading: "lazy"))
]
#html.elem("div", attrs: (class: "course-body"))[
#html.elem("a", attrs: (class: "course-name", href: "polish-political-analyses/political-attitudes.html"))[Social and political trends]
#html.elem("div", attrs: (class: "course-desc"))[How Poles' attitudes, values and lives have changed since 1990, each trend pooled from every comparable survey into one monthly estimate, with breakdowns by party preference, age, gender, education and place of residence. So far: support for membership of the European Union, and where Poles place themselves between left and right.]
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
#html.elem("div", attrs: (class: "course-desc"))[Where Polish parties stand on the economy, cultural questions, European integration and democracy, set against parties across Europe in the Chapel Hill Expert Survey since 2002, and how expert ratings of anti-elite rhetoric compare with The PopuList's classification of parties as populist, far right or far left.]
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
#html.elem("a", attrs: (class: "course-name", href: "polish-political-analyses/party-genealogy.html"))[Polish parties in the Sejm, 1989–2026]
#html.elem("div", attrs: (class: "course-desc"))[The lifespans, Sejm seats, government status and genealogy of every party that has sat in the Sejm since 1989: the splits, mergers, renames and alliances that link them. Trace any party's predecessors and successors.]
]
]
]
]
