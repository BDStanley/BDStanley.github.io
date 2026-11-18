#set document(title: [Timing and organisation])
#metadata((
  title: "Timing and organisation",
  description: "When the 2027 election to the Sejm and Senate can be held, who calls and runs it, and the rules that turn votes into seats, with the legal basis of each",
)) <website-metadata>

// Election 2027's subpage (PLAN.md decision 18 in the Polish political analyses repo). Hand-written, not generated:
// scripts/sync-analyses.py leaves it alone when it regenerates the posts beside it. Every fact and rule comes from
// /assets/election-2027/facts.json, which the sync takes from "Election 2027/web/election2027.json" (`facts`, from
// the hand-kept data-raw/election_facts.csv: each row with its article and a link to the consolidated text); the
// dates for each possible election day are computed here from its `rules`. Styles: .e27-* in 90_site.css.
// When the President's order and PKW's calendar are published, add them here (the "Added as they happen" card).

#let F = json("/assets/election-2027/facts.json")
#let R = F.rules
#let el(tag, ..attrs, body) = html.elem(tag, attrs: attrs.named(), body)
#let ymd(s) = datetime(year: int(s.slice(0, 4)), month: int(s.slice(5, 7)), day: int(s.slice(8, 10)))
#let long(d) = d.display("[day padding:none] [month repr:long] [year]")
#let short(d) = d.display("[day padding:none] [month repr:short] [year]")
#let dm(d) = d.display("[day padding:none] [month repr:long]")
#let dms(d) = d.display("[day padding:none] [month repr:short]")
// The term runs four years from the Sejm's first sitting; the election falls on a day free from work in the last
// 30 days of it (every Sejm election since 1991 has been on a Sunday)
#let start = ymd(F.termStart)
#let end = datetime(year: start.year() + 4, month: start.month(), day: start.day())
#let window = range(R.windowDaysBefore, 0, step: -1).map(k => end - duration(days: k))
#let sundays = window.filter(d => d.weekday() == 7)
#let order = end - duration(days: R.orderDaysBefore)
#let ec = "https://api.sejm.gov.pl/eli/acts/DU/2026/1261/text.pdf"
#let con = "https://api.sejm.gov.pl/eli/acts/DU/1997/483/text.pdf"
#let today = datetime.today()

#el("div", class: "e27")[
#el("p", class: "teaching-back")[#link("../election-2027.html", "← Election 2027")]
#el("p", class: "eyebrow")[Election 2027]

#heading(level: 1)[Timing and organisation]

#el("div", class: "e27-lead")[When the 2027 election to the Sejm and Senate can be held, who calls and runs it, and the rules that turn votes into seats, each with the article of the Constitution or the Electoral Code it rests on.]

#html.elem("div", attrs: (class: "e27-addr", role: "navigation", "aria-label": "Election 2027 pages"))[
#html.elem("a", attrs: (href: "../election-2027.html"))[Updates]
#html.elem("a", attrs: (href: "organisation.html", class: "on", "aria-current": "page"))[Timing and organisation]
#html.elem("a", attrs: (href: "../polling-tracker.html"))[Polling tracker]
]

#el("p", class: "e27-status")[Checked on 18 November 2026 against the Constitution and the consolidated Electoral Code (Journal of Laws 2026, item 1261). The President has not yet ordered the election; this page will be updated when the order and the National Electoral Commission's calendar are published.]

#html.elem("ol", attrs: (class: "e27-line", "aria-label": "Timeline"))[
#for (d, when, what) in (
  (start, short(start), "The Sejm's first sitting: the term begins"),
  (order, "by " + short(order), "The President orders the election and sets its date"),
  (none, str(R.listsDaysBefore) + "th day before", "Candidate lists due, by 16:00"),
  (none, dms(window.first()) + " – " + short(window.last()), "Election day, a day free from work"),
  (none, "within " + str(R.firstSittingDays) + " days", "The new Sejm's first sitting"),
  (none, "by the " + str(R.courtDays) + "th day", "The Supreme Court rules on validity"),
) {
  el("li", class: if d != none and d < today { "e27-ms done" } else { "e27-ms" }, {
    el("span", class: "e27-ms-date")[#when]
    el("span", class: "e27-ms-what")[#what]
  })
}
]

#heading(level: 2)[Dates for each possible election day]

#el("p", class: "e27-help")[The Sundays in the window are #sundays.map(dm).slice(0, -1).join(", ") and #long(sundays.last()). These are the legal limits for each; the President's order will fix the actual calendar.]

#el("div", class: "e27-scroll")[
#el("table", class: "e27-table e27-dates")[
#el("thead")[#el("tr")[
  #el("th")[Step]
  #for d in sundays { el("th")[#long(d)] }
  #el("th")[Basis]
]]
#el("tbody")[
// A row whose cell is the same for every Sunday (`same`) spans the Sundays' columns
#let row(step, cell, basis, url, same: false) = el("tr", {
  el("td", class: "k")[#step]
  if same { html.elem("td", attrs: (colspan: str(sundays.len()), class: "span"))[#cell(none)] } else { for d in sundays { el("td")[#cell(d)] } }
  el("td", class: "b")[#link(url)[#basis]]
})
#row("The President's order", d => [by #long(order), whichever Sunday is chosen], "Electoral Code Art. 194", ec, same: true)
#row("Candidate lists filed", d => [by 16:00 on #long(d - duration(days: R.listsDaysBefore))], "Electoral Code Art. 208, 210, 211", ec)
#row("Election day", d => el("span", class: "red")[Sunday #long(d)], "Constitution Art. 98(2)", con)
#row("First sitting of the new Sejm", d => [by #long(d + duration(days: R.firstSittingDays))], "Constitution Art. 109(2)", con)
#row("Election protests", d => [within #R.protestDays days of the Commission's announcement of the results], "Electoral Code Art. 241", ec, same: true)
#row("Ruling on validity", d => [by #long(d + duration(days: R.courtDays))], "Electoral Code Art. 244", ec)
]
]
]

#for (key, title) in (
  ("timing", "When and by whom the election is called"),
  ("who", "Who runs it and signs it off"),
  ("rules", "The rules that decide seats"),
) [
#heading(level: 2)[#title]

#el("div", class: "e27-scroll")[
#el("table", class: "e27-table e27-facts")[
#el("tbody")[
#for r in F.rows.filter(r => r.section == key) {
  el("tr", {
    el("td", class: "k")[#r.label]
    el("td")[#r.value]
    el("td", class: "b")[#if r.url != "" { link(r.url)[#r.basis] } else { r.basis }]
  })
}
]
]
]
]

#el("div", class: "e27-card e27-later")[
#el("p", class: "label")[Added as they happen]
- The President's order and the National Electoral Commission's electoral calendar.
- The registered election committees, and which parties stand together.
- The candidate lists in each constituency.
- After the vote: the result as announced, and the steps for forming a government (Constitution Art. 154–155).
]

#el("div", class: "e27-foot")[#link("../election-2027.html")[Updates] #link("../polling-tracker.html")[Polling tracker] #el("span", class: "e27-credit")[Ben Stanley, 2026. #emph[Election 2027], Polish Political Analyses, benstanley.eu.]]
]
