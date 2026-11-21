#set document(title: [Polling tracker: Scenarios])
#metadata((
  title: "Polling tracker: Scenarios",
  description: "Change vote shares, put parties on joint lists, or shift turnout and support in any group of voters, and see what follows for the seats and the options for government, with the model's uncertainty",
)) <website-metadata>

// The Scenarios address of the Polling tracker (see ../polling-tracker.typ): the app with ?s=scenarios. Views: Parties and lists
// (shares, joint lists and the voters' response), Groups (changes by group, raked on Voting behaviour's 2023 electorate cells, cells.json) and
// Who votes. The 2023 targets table below is read from assets/polling-tracker/cells.json at build time.

#let cells = json("/assets/polling-tracker/cells.json")
#let lab(g, l) = {
  let d = cells.dims.at(g)
  let i = d.levels.position(x => x == l)
  if i == none { l } else { d.labels.at(i) }
}
#let listname(id) = {
  let m = cells.lists2023.filter(x => x.id == id)
  if m.len() == 0 { id } else { m.first().name }
}
#let num(x) = if x == none { "" } else { str(calc.round(x, digits: 1)) }

#html.elem("p", attrs: (class: "teaching-back"))[#link("../../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Polling tracker]

What follows if support or turnout shifts: change any party's share, put parties on joint lists, or move turnout and support in any group of voters (men aged 18–29 in towns, say), and see the seats and the options for government, with the model's uncertainty. Every scenario has a link of its own.

#html.elem("nav", attrs: (class: "tracker-nav", "aria-label": "Polling tracker sections"))[
  #html.elem("a", attrs: (href: "../polling-tracker.html", "data-s": "polls"))[Polls]
  #html.elem("a", attrs: (href: "seats.html", "data-s": "seats"))[Seats]
  #html.elem("a", attrs: (href: "geography.html", "data-s": "geography"))[Geography]
  #html.elem("a", attrs: (href: "scenarios.html", "data-s": "scenarios", class: "on", "aria-current": "page"))[Scenarios]
  #html.elem("a", attrs: (href: "forecast.html", "data-s": "forecast"))[Forecast]
]

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

Parties and lists: when you edit a vote share or build a joint list, your shares are applied to each of the model's simulated draws for that week (each party's draws are scaled so that their median is the share you entered), the voters' response to joint lists (below) is applied draw by draw, and the seat simulation is run again in your browser. A joint list's total has a slider of its own: moving it scales the list's parties in proportion, and moving one of its parties resizes the others so that the list keeps its total. Scenarios therefore keep seat ranges and probabilities: they describe what would follow if support stood where you set it, with the same uncertainty as the model. Government options follow the compatibility rules described on #link("seats.html")[Seats]; with joint lists, two lists can govern together only if no party on one is excluded from a party on the other.

Voter response to joint lists: when parties stand on one list, some of their voters will not follow. For each party on a joint list, the tracker takes the share of its voters who would rate each partner in the bottom quarter of a 0 to 10 scale, from a model that combines surveys of 2023 to 2026: the Polish General Election Study (2023 and 2025), the European Election Study (2024), CBOS and SWPS University's online surveys. Each survey is read through its own scale, the 2023 Polish General Election Study's ratings of the parties set the scale, and feelings across the left-right divide can move over time with CBOS's monthly trust in politicians. A share of those reluctant voters defect: 40% in the Medium setting, calibrated on CBOS's questions about joint opposition lists in February 2023, with 30% and 55% (Low and High) at the 10th and 90th percentiles of that estimate. Defectors abstain or vote for the parties outside the list that their electorate would most consider (from the European Election Study 2024). The reluctance of each pair and the abstention share vary from draw to draw, so the scenario keeps its uncertainty. R+, which no survey rated, takes the figures of PiS. The abstention share and the extra reluctance between parties that split recently (PiS and R+, Polska 2050 and PSL) are judgement, not estimates, and the advanced settings let you change them.

Groups: the 2023 electorate is #str(cells.cells.w.len()) cells of age, sex, education and place of residence, from the Polish General Election Study of 2023 (#cells.meta.frame.respondents respondents, through the vote model of my Voting behaviour analysis). Each cell has a chance of voting and a vote among the 2023 lists, raked to PKW's turnout and list shares and to the pattern across groups in Ipsos's exit poll for TVP, TVN and Polsat, which is first moved onto the official result (the table below). The starting point is this week's estimates, with each group moved as Opinia24's and CBOS's tables show it has moved since December 2023 (or, if you choose, every group moved alike), each splinter taking a share of its 2023 list that differs by group as those tables show, and each group's turnout moved as its stated intention to vote has moved, with national turnout kept at 2023's. A change sets turnout, or one party's share of the voters, in a group or an intersection of groups; the cells are raked in log-odds until every change is met, everything not set moving as little as possible, and the national result follows from the groups' sizes. Those national shares become the scenario's shares, and the seats follow as for any scenario. Parties set by hand under Parties and lists stay where they were set. The regional patterns do not yet follow the groups, so a change among the young moves every constituency alike. Changes by group start from the week the groups were last fitted to the polls.

Who votes: for every group, everyone in it votes (turnout raised to 100%) or only it votes (everyone else stays at home), from the same starting point; the extra voters are assumed to vote like the voters already in their cell, so the figures show what composition alone can do. Seats there are a single allocation by the tracker's rules, without joint lists.

The figures describe what would follow if support stood where you set it, not a forecast. How the polls are pooled is on #link("../polling-tracker.html")[Polls].

#html.elem("details")[
#html.elem("summary")[The 2023 targets: Ipsos's exit poll, moved onto the official result]

Each row is one figure the cells are raked to: the exit poll as printed, the target after moving the exit poll's pattern onto PKW's result, and the survey model before raking. Turnout by residence comes from the late exit poll (90% of stations); everything else from the 21:00 exit poll. Each figure links to the report it was read from.

#html.elem("div", attrs: (style: "overflow-x:auto"))[
#html.elem("table", attrs: (class: "method-table"))[
  #html.elem("tr")[#html.elem("th")[Measure] #html.elem("th")[Group] #html.elem("th")[List] #html.elem("th")[Exit poll] #html.elem("th")[Target] #html.elem("th")[Survey model] #html.elem("th")[Source]]
  #for r in cells.exitPoll [
    #html.elem("tr")[
      #html.elem("td")[#if r.measure == "turnout" [Turnout] else [Vote]]
      #html.elem("td")[#lab(r.grouping, r.level)]
      #html.elem("td")[#if r.list == "" [] else [#listname(r.list)]]
      #html.elem("td")[#num(r.exit)]
      #html.elem("td")[#num(r.target)]
      #html.elem("td")[#num(r.model)]
      #html.elem("td")[#if r.url == "" or r.url == none [] else [#link(r.url)[#if r.poll == "late90" [late poll] else [report]]]]
    ]
  ]
]
]
]

Ben Stanley, 2026. _Polling tracker_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../../assets/polling-tracker/index.html?s=scenarios",
  class: "embed-frame embed-frame--app",
  title: "Polling tracker: scenarios for vote shares, joint lists and groups of voters",
  loading: "lazy",
))[]
