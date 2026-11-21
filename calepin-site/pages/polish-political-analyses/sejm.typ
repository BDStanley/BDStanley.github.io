#set document(title: [Sejm])
#metadata((
  title: "Sejm",
  description: "How the Sejm votes: club cohesion, every MP's record, positions in the roll calls, how late ministries answer and the topics of the laws passed, 1997–2026",
)) <website-metadata>

// The Sejm's page. It embeds the Legislative activity app, assets/legislative-activity/index.html (+ app.js), with ?b=sejm,
// like the Quality of democracy page; senate.typ and european-parliament.typ embed the same app with ?b=senate and ?b=ep.
// (Until 21 November 2026 one page, legislative-activity.typ, showed all three bodies with a switch; it now redirects here,
// or to the Senate or European Parliament page when the old link names that body with #b=.) Its data, legislative.json and
// the per-term files it lists (mps-, profile-, roles-, rebels-, votes-, bills- and bv-<term>[-<chunk>].json, loaded when a
// term, an MP or a bill is chosen), comes from Sejm analysis/Legislative activity/ in the Polish political analyses repo
// (R/la90_export_web.R) and is copied in by scripts/sync-analyses.py on each build. The app keeps its state in this page's
// address (#view=, #t=, #mp=, #q=, #topic=, #bill=, #rc=); the Presidents, Election results and Polling tracker pages link in
// with it. The method note (How this works) is a collapsed <details class="method"> under the introduction; its styles are
// with the other method notes in 90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Sejm]

What do Polish MPs do, and how disciplined are their clubs? The page covers every term of the Sejm from 1997 to 2026. Clubs shows how united each club is and how often government and opposition take opposite sides. MPs gives every MP a profile: attendance, the votes they cast against their club, committee and club roles, questions, speech and rapporteur work. All MPs puts every MP in one chart, Positions places them by how they voted, Questions shows how late ministries answer MPs' questions, Bills finds any bill since 2005 by topic or by words in its title, with how every MP voted on it, and Legislation shows what kind of laws the Sejm passes. There are no ranked lists of members: the page shows where a member stands among colleagues. For the laws the president vetoed and the bills he submitted, see #link("presidents.html#v=leg")[Presidents]. The #link("senate.html")[Senate] and the Polish members of the #link("european-parliament.html")[European Parliament] have pages of their own.

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

Everything comes from the Sejm's own API: every recorded vote, each MP's attendance by sitting day, committee, club and bilateral-group membership, interpellations and written questions, the stages of every bill, and the statement index of the plenary sittings. A club's line on a vote is the position most of its MPs took (yes, no or abstain); a tie leaves the line undefined, and unaffiliated MPs have no club. An MP rebels when they vote differently from a defined line, and a club's cohesion is the agreement index of Hix, Noury and Roland on contested votes, those where the winning side had no more than 90 per cent of the votes cast. Clubs that hold more than one party are marked, because rebellion in them often reflects a party faction rather than individual dissent. Attendance is the share of roll calls an MP voted in; days with missed votes, excused and unexcused, come from the API's sitting-day statistics, which also count personnel-list ballots, so the two sources count slightly differently. Term 9 includes remote voting from 2020, and the types of vote (final passage, amendments, procedural) are reliable from term 4.

Ministers, secretaries and under-secretaries of state who spoke as such (terms 6 to 10) and the Marshals of the Sejm are marked and left out of the medians, because their attendance and speech differ by office. Questions are counted as first-signed (the first name listed) and co-signed; whether the first name is the lead signatory is not checked. A reply is late if the first answer that is not a request for more time arrives more than 21 days after the question was sent. The extended version, which gives extra time when more time was requested, assumes 14 days, because the API does not give the length. Reply times can be measured for terms 8 to 10 only, and only for questions with one recipient.

The positions are W-NOMINATE estimates fitted to each term separately for terms 5 to 10, so they cannot be compared across terms; the MP profiles also show a position from a model that links the terms. The topics of bills are model-coded: two language models coded every bill's topic (Comparative Agendas Project major topics) from its title, and the analysis uses the bills on whose topic they agreed (93 per cent). There is no hand-coded accuracy figure; the check made is that a bill's topic fits the committee it was referred to, which it does for 85 per cent of the agreed bills, against 12 per cent by chance. The API does not name the MPs who sponsored a bill, so bills are shown by who introduced them (government or MPs), not by MP. The sources end on different dates, given under each chart; the term still running is incomplete.

Sources:

- The Sejm's API, api.sejm.gov.pl (roll calls, attendance, committees, clubs, interpellations and written questions, bills and legislative processes, plenary statements).
- Party tracker, for the composite clubs and government status (see #link("party-genealogy.html")[Parties in the Sejm]).
- Roll-call positions: my #link("quality-of-democracy.html")[roll-call polarisation analysis].

Ben Stanley, 2026. _Sejm_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/legislative-activity/index.html?b=sejm",
  class: "embed-frame embed-frame--app",
  title: "Sejm: clubs and MPs, positions, questions, bills and legislation",
  loading: "lazy",
))[]
