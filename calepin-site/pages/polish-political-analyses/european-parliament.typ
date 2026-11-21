#set document(title: [European Parliament])
#metadata((
  title: "European Parliament",
  description: "How Polish MEPs vote in the European Parliament: group cohesion, every Polish MEP's record, positions in the roll calls and the items voted on, 2004–2026",
)) <website-metadata>

// The European Parliament's page. It embeds the Legislative activity app, assets/legislative-activity/index.html (+ app.js),
// with ?b=ep; sejm.typ and senate.typ embed the same app with ?b=sejm and ?b=senate. (Until 21 November 2026 one page,
// legislative-activity.typ, showed all three bodies with a switch; an old link to it with #b=ep is sent here.) Its data,
// ep-legislative.json and the per-term files it lists under the prefix ep- (mps-, profile-, roles-, rebels-, votes-, bills-
// and bv-<term>[-<chunk>].json), comes from Sejm analysis/Legislative activity/R/la45_ep.R in the Polish political analyses
// repo and is copied in by scripts/sync-analyses.py on each build. The app keeps its state in this page's address (#view=,
// #t=, #mp=, #q=, #topic=, #bill=, #rc=); the Election results career card links in with it. The method note (How this works)
// is a collapsed <details class="method"> under the introduction; its styles are with the other method notes in 90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]

#heading(level: 1)[European Parliament]

What do Poland's members of the European Parliament do, and how disciplined are their groups? The page covers terms 6 to 10 (July 2004 onward) and lists the Polish MEPs. Groups shows how united each political group is. MEPs gives every Polish MEP a profile: attendance, the votes they cast against their group, committee and delegation roles and, from 2019, speeches. All MEPs puts every Polish MEP in one chart, Positions places them by how they voted, and Items finds any report, resolution or legislative proposal the Parliament voted on by topic or by words in its title, with how every Polish MEP voted on it. There are no ranked lists of members: the page shows where an MEP stands among colleagues. The #link("sejm.html")[Sejm] and the #link("senate.html")[Senate] have pages of their own.

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

The European Parliament's roll-call votes come from two secondary compilations of the Parliament's own minutes: VoteWatch Europe's dataset (Hix, Frantescu and Hagemann 2022) for terms 6 to 8 and HowTheyVote.eu (Open Database License) from July 2019. Where they overlap, they agree on Polish MEPs' attendance within one percentage point. The MEPs listed are the Polish ones, but a group's line, its cohesion and the contested votes are taken over every MEP, so a Polish MEP's rebellion is against the whole group's line. A group's line on a vote is the position most of its MEPs took (yes, no or abstain); a tie, or fewer than three voting, leaves the line undefined, and non-attached members have no group. An MEP rebels when they vote differently from a defined line, and a group's cohesion is the agreement index of Hix, Noury and Roland on contested votes, those where the winning side had no more than 90 per cent of the votes cast. Attendance is the share of the roll calls held while the MEP sat in which they voted for, against or abstained. A group is shown under the name it had in the term, so the line of a renamed group does not continue across terms. The Parliament has no government and opposition, so that comparison is left out.

Committees, delegations and working groups come from the Parliament's open data; plenary speeches, from the same source, exist by person only from July 2019, and are counted, not read. The positions are W-NOMINATE estimates fitted to each term separately for the Polish MEPs only, so they show the Polish delegation, not the whole Parliament, and cannot be compared across terms. The topics of the items are model-coded: two language models coded each item's topic (Comparative Agendas Project major topics) from its title, and a third decided where they disagreed. There is no hand-coded accuracy figure. The sources end on different dates, given under each chart; the term still running is incomplete.

Sources:

- VoteWatch Europe (Hix, Frantescu and Hagemann 2022) and HowTheyVote.eu, for the European Parliament's roll calls.
- The European Parliament's open data, for committees, delegations and speeches.

Ben Stanley, 2026. _European Parliament_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/legislative-activity/index.html?b=ep",
  class: "embed-frame embed-frame--app",
  title: "European Parliament: groups and Polish MEPs, positions and the items voted on",
  loading: "lazy",
))[]
