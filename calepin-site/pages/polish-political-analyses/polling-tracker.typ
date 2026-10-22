#set document(title: [Polling tracker])
#metadata((
  title: "Polling tracker",
  description: "A Bayesian poll-of-polls for elections to the Polish Sejm, with seat projections and a scenario builder",
)) <website-metadata>

// The tracker is a standalone page, assets/polling-tracker/index.html (+ app.js),
// embedded here like the party genealogy. Its data, polls.json and draws.json,
// comes from Polls/ in the Polish political analyses repo (R/export_web.R, run by
// PTP.R) and is copied in by scripts/sync-analyses.py on each build. It replaced
// the Shiny app, retired on 22 October 2026; pages/pooling-the-poles.typ now
// redirects here.
// The method note (How this works) is a collapsed <details class="method"> under the
// introduction; its styles are with the other method notes in 90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Polish political analyses")]

#heading(level: 1)[Polling tracker]

A Bayesian poll-of-polls for elections to the Polish Sejm: voting intention since January 2025, seat projections for every week, and a scenario builder in which you can change vote shares, put parties on joint lists and see how the seats and the options for government change, with the model's uncertainty.

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

Every poll of Sejm voting intention listed on Wikipedia is pooled in a Bayesian model (brms/Stan). Party shares follow a smooth trend over time, and each polling house has its own systematic lean (house effect) and its own precision, so noisier houses count for less. The trend shown is the average across the houses observed, with 80% credible bands; dots are individual polls. Undecided respondents are set aside before modelling.

Three parties split during the series: KKP from Konfederacja (June 2025), Polska 2050 and PSL ending their Trzecia Droga alliance (June 2025), and R+ from PiS (July 2026). Because houses did not all start polling the parts separately at once, the model estimates each whole bloc from every poll and the split within it from the polls that offered it. The parent's line steps down when its splinter is founded; before that, Polska 2050's line is the whole Trzecia Droga figure.

Seats: national shares are spread across the 41 constituencies using each party's regional pattern at the 2023 election (for KKP and Razem, their leaders' 2025 presidential first-round results), and allocated by D'Hondt. Parties need 5% nationally and joint lists 8%; the German minority (MN) is exempt and stands only in Opole. The model's seat figures come from simulations that apply the threshold in each draw and let each party's regional pattern shift, which is where the seat ranges and probabilities come from.

Scenarios: when you edit a vote share or build a joint list, your shares are applied to each of the model's simulated draws for that week (each party's draws are scaled so that their median is the share you entered), the coalition-dynamics penalty is applied draw by draw, and the seat simulation is run again in your browser. Scenarios therefore keep seat ranges and probabilities: they describe what would follow if support stood where you set it, with the same uncertainty as the model. Government options follow fixed compatibility rules (for example, PiS does not govern with KO or Lewica) until you build a joint list, when any lists may combine; the minimum winning coalition is the majority with the fewest parties, then the fewest seats.

Estimates describe voting intention now, not a forecast of election day. They assume pollsters are, on average, unbiased.
]

#html.elem("iframe", attrs: (
  src: "../../assets/polling-tracker/index.html",
  class: "embed-frame embed-frame--app",
  title: "Polling tracker: Polish Sejm voting intention, seats and scenarios",
  loading: "lazy",
))[]
