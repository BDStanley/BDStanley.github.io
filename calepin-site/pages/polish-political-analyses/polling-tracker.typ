#set document(title: [Polling tracker])
#metadata((
  title: "Polling tracker",
  description: "A Bayesian poll-of-polls for elections to the Polish Sejm: voting intention since 2023, by group and by pollster, with every Sejm term since 1991",
)) <website-metadata>

// The tracker is one app at four addresses (Election 2027/PLAN.md, decision 18 in the Polish political analyses repo):
// this page (Polls) and polling-tracker/seats.typ, geography.typ and scenarios.typ each embed
// assets/polling-tracker/index.html with ?s=<section>, which fixes the views it offers. The section row (nav.tracker-nav,
// styled in 90_site.css) is plain links; the app adds the week and the scenario to them (data-s). Its data, polls.json and
// draws.json, come from Polls/ (current term/R/export_web.R, run by PTP.R), the past terms' terms.json and term-<term>.json
// from Polls/R/terms/; scripts/sync-analyses.py copies them in on each build. It replaced the Shiny app, retired on 22 October
// 2026; pages/pooling-the-poles.typ redirects here. A link from before the four addresses that names a seat view or a scenario
// is sent on by the app to Seats or Scenarios. The method note covers this page's views only; each address has its own.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Polling tracker]

A Bayesian poll-of-polls for elections to the Polish Sejm: voting intention since the October 2023 election, what it would mean for seats, where those seats would fall, and what changes if support or turnout shifts. This page has the trends, the picture by group and the pollsters; the term selector also shows the polls of every Sejm term since 1991, measured against the elections that opened and closed it.

#html.elem("nav", attrs: (class: "tracker-nav", "aria-label": "Polling tracker sections"))[
  #html.elem("a", attrs: (href: "polling-tracker.html", "data-s": "polls", class: "on", "aria-current": "page"))[Polls]
  #html.elem("a", attrs: (href: "polling-tracker/seats.html", "data-s": "seats"))[Seats]
  #html.elem("a", attrs: (href: "polling-tracker/geography.html", "data-s": "geography"))[Geography]
  #html.elem("a", attrs: (href: "polling-tracker/scenarios.html", "data-s": "scenarios"))[Scenarios]
]

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

Every poll of Sejm voting intention listed on Wikipedia is pooled in a Bayesian model (brms/Stan). Party shares follow a smooth trend over time, and each polling house has its own systematic lean (house effect) and its own precision, so noisier houses count for less. CBOS counts as three houses, divided at its documented changes of method (an online share added in November 2024, a fixed mix from January 2026). The trend shown is the average across the houses observed, with 80% credible bands; dots are individual polls. Undecided respondents are set aside before modelling.

Four parties split during the series: Razem from Lewica (October 2024), KKP from Konfederacja (June 2025), Polska 2050 and PSL ending their Trzecia Droga alliance (June 2025), and R+ from PiS (July 2026). Because houses did not all start polling the parts separately at once, the model estimates each whole bloc from every poll and the split within it from the polls that offered it. The parent's line steps down when its splinter is founded; before that, Lewica's line includes Razem and Polska 2050's is the whole Trzecia Droga figure.

By group: Opinia24's monthly and CBOS Flash's tables by sex, age, education and town size are pooled into each group's trend since December 2023, as decided voters' choice among the five lists of 2023 (each splinter counted with its 2023 list). Each group's distance from the national line follows a monthly random walk, the two houses' leans on it are estimated as equal and opposite, and the groups are pinned to this tracker's national trend, so that they always average to it: the polls give the pattern across groups, not the level. Stated intention to vote is modelled the same way and not pinned, since people overstate it; the change between groups is the reliable part.

Estimates describe voting intention now, not a forecast of election day. They assume pollsters are, on average, unbiased.

Past terms (1991–93 to 2019–23) are drawn from the polls Wikipedia lists for each election, with a model of their own. Before the current term the tables lump the other parties with the undecided, so each line is a share of the vote for the parties the polls named. The trend starts and ends at the two election results, and each house's lean is measured against them, so the "polls against the result" card shows how far the houses that polled last missed. Parties join the chart when the polls first named them; a splinter is counted inside its parent until it split, and a joint list replaces its members from when the polls began reporting it. Past terms show trends and pollsters only.

Seats, the constituencies and scenarios have notes of their own on #link("polling-tracker/seats.html")[Seats], #link("polling-tracker/geography.html")[Geography] and #link("polling-tracker/scenarios.html")[Scenarios].

Ben Stanley, 2026. _Polling tracker_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/polling-tracker/index.html?s=polls",
  class: "embed-frame embed-frame--app",
  title: "Polling tracker: Polish Sejm voting intention, by group and by pollster",
  loading: "lazy",
))[]
