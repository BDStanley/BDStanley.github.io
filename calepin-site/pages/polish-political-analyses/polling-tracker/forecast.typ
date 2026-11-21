#set document(title: [Polling tracker: Forecast])
#metadata((
  title: "Polling tracker: Forecast",
  description: "A forecast of the 2027 Sejm election from the latest polls: each party's seats with their ranges on election day, the chance of each majority, and every one of the 460 seats with each list's chance of holding it",
)) <website-metadata>

// The Forecast address of the Polling tracker (see ../polling-tracker.typ): the app with ?s=forecast, set apart from the
// polling pages (Polls/current term/docs/forecast_plan.md in the analyses repository). Views: Sejm (with Every seat),
// Senate and Sliders.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Polling tracker]

A forecast of the next Sejm and Senate elections from the latest polls: how many seats each party could win on election day, the chance of each majority, every one of the 460 Sejm seats with each list's chance of holding it, the 100 Senate districts under any pacts you choose, and sliders for how groups of voters might turn out or shift. Unlike the other pages of the tracker, which describe an election held this week, this one allows for how far the polls can still move, and miss, before the vote.

#html.elem("nav", attrs: (class: "tracker-nav", "aria-label": "Polling tracker sections"))[
  #html.elem("a", attrs: (href: "../polling-tracker.html", "data-s": "polls"))[Polls]
  #html.elem("a", attrs: (href: "seats.html", "data-s": "seats"))[Seats]
  #html.elem("a", attrs: (href: "geography.html", "data-s": "geography"))[Geography]
  #html.elem("a", attrs: (href: "scenarios.html", "data-s": "scenarios"))[Scenarios]
  #html.elem("a", attrs: (href: "forecast.html", "data-s": "forecast", class: "on", "aria-current": "page"))[Forecast]
]

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

The forecast starts from the tracker's latest weekly estimate, 250 simulated sets of vote shares that reflect what the polls say now. It then asks how far the polls have stood from the result at the same distance before past elections. For every Sejm term since 1991, the polls of each four-week window are compared with the result party by party (shares of the parties each poll named, averaged within each polling house and then across houses, with the houses' own sampling noise taken out). How far they missed depends on the party's size: parties over 15% missed by little in the last weeks and by more the further out, while smaller parties missed by much the same at any distance, and middle-sized ones (7–15%) most of all: Samoobrona and LPR in 2001, Ruch Palikota in 2011 and Trzecia Droga in 2023 all polled well short of their results. Each of the 250 sets of shares is given four random shifts of that size, one per party, making 1,000 simulated elections, and each party's expected share is kept at this week's estimate: the forecast says how uncertain the result is, not which way it will move.

Each party's regional pattern can also shift by election day, by as much as such patterns have shifted between Sejm elections since 2001: in each simulation a party's whole pattern is sharpened or flattened at once, and each voivodeship and each constituency moves a little on its own. PSL, standing alone, takes its own pattern of 2015, and Polska 2050 that of Szymon Hołownia's first round in 2020.

The seats then follow as on #link("seats.html")[Seats], with those patterns in place of the 2023 ones: D'Hondt in each constituency, the 5% threshold applied in each simulation, and the German minority in Opole. Seats are medians with 80% ranges, and a majority's chance is the share of simulations in which it has 231 seats or more. "If held this week" shows the Seats page's figures for comparison.

The Senate has no polls of its own. Its forecast rests on how candidates in the 100 single-member districts turned the Sejm vote of the parties behind them into votes at the elections of 2011–2023: the more Sejm votes behind a candidate, the more votes, though less than in proportion, since Senate candidates draw votes across party lines; sitting senators ran about a quarter ahead; and a pact's single candidate got about what the pact parties' Sejm votes together predict. The forecast's Sejm vote in each district then decides each simulated contest, with each candidate's own random vote and one shared by each camp's candidates. Every party stands alone unless you put it in a pact. Checked by predicting each election from the other three, it named 87–89 of the 100 winners in 2011, 2019 and 2023 and 71 in 2015, when PiS's candidates ran well ahead everywhere. It cannot foresee strong local independents, who win a few seats at every election.

The sliders ask what follows if groups of voters (by age, sex, education and place of residence) turn out more or less, or move economically or on social and cultural questions. They rest on the 2023 Polish National Election Study: a model of the 2023 vote, not voting included, on each voter's economic position (state-led to market-liberal) and socio-cultural position (progressive to conservative) as well as age, sex, education and residence. Moving a group's positions applies the model's difference to that group's 2023 vote, the change in each party's national share is carried over to every simulated election, and the Sejm and Senate forecasts follow. These are associations from 2023, not measured effects of opinion on the vote; a check against the 2025 study found the economic link unchanged but a conservative shift worth less to PiS, and more to Konfederacja and KKP, than in 2023.

The election date is not yet set: the President orders it by 15 August 2027, on a day between 14 October and 12 November 2027. Until then the forecast is for 24 October 2027, the middle of that window; a week either way changes little.

Ben Stanley, 2026. _Polling tracker_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../../assets/polling-tracker/index.html?s=forecast",
  class: "embed-frame embed-frame--app",
  title: "Polling tracker: forecast of the Sejm election",
  loading: "lazy",
))[]
