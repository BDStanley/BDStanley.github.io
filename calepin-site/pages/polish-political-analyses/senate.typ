#set document(title: [Senate])
#metadata((
  title: "Senate",
  description: "How the Senate votes: club cohesion, government against opposition, every senator's record, positions in the votes and the items voted on, 2011–2026",
)) <website-metadata>

// The Senate's page. It embeds the Legislative activity app, assets/legislative-activity/index.html (+ app.js), with
// ?b=senate; sejm.typ and european-parliament.typ embed the same app with ?b=sejm and ?b=ep. (Until 21 November 2026 one
// page, legislative-activity.typ, showed all three bodies with a switch; an old link to it with #b=senate is sent here.)
// Its data, senate-legislative.json and the per-term files it lists under the prefix senate- (mps-, profile-, roles-, rebels-,
// votes-, bills- and bv-<term>[-<chunk>].json), comes from Sejm analysis/Legislative activity/R/la40_senate.R in the Polish
// political analyses repo and is copied in by scripts/sync-analyses.py on each build. The app keeps its state in this page's
// address (#view=, #t=, #mp=, #q=, #topic=, #bill=, #rc=); the Election results career card links in with it. The method note
// (How this works) is a collapsed <details class="method"> under the introduction; its styles are with the other method notes
// in 90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Senate]

What do Polish senators do, and how disciplined are their clubs? The page covers the Senate's terms VIII to XI (2011 onward), the terms for which the Senate publishes named votes. Clubs shows how united each club is and how often government and opposition take opposite sides. Senators gives every senator a profile: attendance, the votes they cast against their club, committee roles and speeches. All senators puts every senator in one chart, Positions places them by how they voted, and Items finds anything the Senate voted on (a law passed by the Sejm, a resolution or an election to a post) by topic or by words in its title, with how every senator voted on it. There are no ranked lists of members: the page shows where a senator stands among colleagues. The #link("sejm.html")[Sejm] and the Polish members of the #link("european-parliament.html")[European Parliament] have pages of their own.

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

The Senate's data are its own pages of named votes, read for every sitting of terms VIII to XI (earlier terms have none online). A senator's club is the party of the club in Party tracker, so a club's line is taken over all its members however the sources spell its name, and independents' circles have no line. A club's line on a vote is the position most of its senators took (yes, no or abstain); a tie, or fewer than three voting, leaves the line undefined, and unaffiliated senators have no club. A senator rebels when they vote differently from a defined line, and a club's cohesion is the agreement index of Hix, Noury and Roland on contested votes, those where the winning side had no more than 90 per cent of the votes cast. Attendance is the share of recorded votes in which a senator voted for, against or abstained; absence and not voting are printed alike in term IX, so attendance there is the share of votes cast. Government and opposition follow Party tracker's cabinets through each club's party.

Committees, with roles and dates, and the number of plenary speeches come from the Senate's own page on each senator; speeches are counted, not read, so there are no word counts. The positions are W-NOMINATE estimates fitted to each term separately, so they cannot be compared across terms. The topics of the items are model-coded: two language models coded each item's topic (Comparative Agendas Project major topics) from its title, and a third decided where they disagreed. There is no hand-coded accuracy figure. The sources end on different dates, given under each chart; the term still running is incomplete.

Sources:

- The Senate's pages of named votes and its pages on each senator, senat.gov.pl.
- Party tracker, for the clubs' parties and government status (see #link("party-genealogy.html")[Parties in the Sejm]).

Ben Stanley, 2026. _Senate_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/legislative-activity/index.html?b=senate",
  class: "embed-frame embed-frame--app",
  title: "Senate: clubs and senators, positions and the items voted on",
  loading: "lazy",
))[]
