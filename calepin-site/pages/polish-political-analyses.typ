#set document(title: [Polish political analyses])
#metadata((
  title: "Polish political analyses",
  description: "Interactive analyses of Polish politics: parties, elections and public opinion",
)) <website-metadata>

// Landing page for the analyses built from the Polish political analyses repo
// (BDStanley/Polish-political-analyses). One row per analysis, styled like the
// Teaching course list (.course-*), with a thumbnail in place of the hexsticker.
// To add one: a page in pages/polish-political-analyses/, a thumbnail in
// assets/analyses/ (480 × 300 px), and a row below.

#heading(level: 1)[Polish political analyses]

Data and interactive charts on Polish politics, from my ongoing work on the party system, elections and public opinion. Each analysis is rebuilt from its data whenever the site is updated.

#html.elem("div", attrs: (class: "course-list analysis-list"))[
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
