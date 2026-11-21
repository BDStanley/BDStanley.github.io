#set document(title: [Polling tracker: Seats])
#metadata((
  title: "Polling tracker: Seats",
  description: "What the polls would mean for the Sejm this week: seats with their ranges, the chance of each majority, and every one of the 460 seats with each list's chance of holding it",
)) <website-metadata>

// The Seats address of the Polling tracker (see ../polling-tracker.typ): the app with ?s=seats, showing the model's figures,
// or a scenario that arrives in the link with a banner and a way back to the model. Views: Sejm and Every seat.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Polling tracker]

What the polls would mean for the Sejm if an election were held this week: each party's seats with their ranges, the chance of each majority, which governments are possible, and every one of the 460 seats with each list's chance of holding it.

#html.elem("nav", attrs: (class: "tracker-nav", "aria-label": "Polling tracker sections"))[
  #html.elem("a", attrs: (href: "../polling-tracker.html", "data-s": "polls"))[Polls]
  #html.elem("a", attrs: (href: "seats.html", "data-s": "seats", class: "on", "aria-current": "page"))[Seats]
  #html.elem("a", attrs: (href: "geography.html", "data-s": "geography"))[Geography]
  #html.elem("a", attrs: (href: "scenarios.html", "data-s": "scenarios"))[Scenarios]
  #html.elem("a", attrs: (href: "forecast.html", "data-s": "forecast"))[Forecast]
]

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

National shares are spread across the 41 constituencies using each party's regional pattern at the 2023 election (for KKP and Razem, their leaders' 2025 presidential first-round results, and for Razem adjusted voivodeship by voivodeship from Opinia24's tables), and allocated by D'Hondt. Parties need 5% nationally and joint lists 8%; the German minority (MN) is exempt and stands only in Opole. The model's seat figures come from 250 simulations a week that apply the threshold in each draw and let each party's regional pattern shift in every constituency, which is where the seat ranges and the probabilities come from. Seats are medians, with 80% ranges.

Each seat's certainty is the share of simulations in which the list projected to hold it wins it: certain at 97.5% or more, probable from 66%, possible from 50% (in play, but more likely than not), and a toss-up below that. On the Sejm view each constituency's seats are dealt to the lists most likely to hold them. On Every seat a constituency's seats are its most probable list-seats, a list's k-th seat there being won in the share of simulations in which it wins at least k; when the projected list loses a seat in a simulation, the seat goes to the list that wins one beyond its projection there, so each seat's chances add up to 100%. An outside possibility is a seat projected for another list where the list in question is the next most likely to take it, with a chance of 33% or more.

Possible governments are every combination with at least a 5% chance of a majority (231 seats), under fixed compatibility rules (for example, PiS does not govern with KO or Lewica), which the coalition configurations let you change. They are ordered from the most to the least plausible: coalitions whose parties are ideologically adjacent come first, then those with fewer parties; a majority with a party it does not need ranks below the smaller majority inside it. Chances of 276 (overriding a presidential veto) and 307 (a constitutional majority) are shown where they reach 5%.

The figures describe an election held this week, not a forecast of election day. A scenario set on #link("scenarios.html")[Scenarios] can be opened here from its link: the page then shows it with a banner. How the polls are pooled is on #link("../polling-tracker.html")[Polls].

Ben Stanley, 2026. _Polling tracker_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../../assets/polling-tracker/index.html?s=seats",
  class: "embed-frame embed-frame--app",
  title: "Polling tracker: seats in the Sejm, majorities and every seat",
  loading: "lazy",
))[]
