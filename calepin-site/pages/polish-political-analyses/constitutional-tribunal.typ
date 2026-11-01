#set document(title: [Constitutional Tribunal])
#metadata((
  title: "Constitutional Tribunal",
  description: "Who has sat on Poland's Constitutional Tribunal since 1985 and how they were chosen, what it has ruled since 1997, how its work has changed, and the disputes about it since 2015",
)) <website-metadata>

// The page is a standalone app, assets/constitutional-tribunal/index.html (+ app.js),
// embedded here like Governments. Its data (composition.json, chronology.json, rulings.json,
// one rulings-<year>.json per year, rulings-pending.json and activity.json) comes from
// Constitutional Tribunal/ in the Polish political analyses repo (analysis/90_export.R and
// 91_export_rulings.R) and is copied in by scripts/sync-analyses.py on each build.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Constitutional Tribunal]

Who has sat on Poland's Constitutional Tribunal since it began work in 1985, how the Sejm chose them, and what each judge's own party ties or political offices were before or alongside the Tribunal. A second view lists the Sejm's votes on every judge. A third lists every case the Tribunal has handled since 1997, with its outcome, bench and dissents, and links each one to the Tribunal's own page. A fourth charts its activity: cases received, rulings by outcome, how long cases take, dissents, delay in publication and vacant seats. The last sets out, event by event and with sources, the disputes about the Tribunal since 2015. See also #link("party-genealogy.html")[Parties in the Sejm], whose clubs elect the judges, and #link("quality-of-democracy.html")[Quality of democracy].

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

*Composition.* The terms are those the Tribunal itself lists for its judges, from the oath to the end of the term. The Sejm's resolutions electing each judge come from the Monitor Polski gazette through the Sejm's open API from 1993 (I have not found the earlier ones), and the votes from the Sejm's voting records from 2001. Where a vote was a secret ballot, the page gives the turnout and says so. The club that put a candidate forward is taken from the Sejm's own prints, which exist from 2007, and is left blank for earlier elections. Ten people the Sejm elected are not on the Tribunal's lists; the page names them, and the chronology gives the circumstances.

*Judges' own ties.* A judge's page shows their own party membership, list or office before or alongside the Tribunal, each with its source. Official biographies are used where they exist, and they are few: the Tribunal's own page for a retired judge gives only a name. The rest come from the Sejm's records of club membership for judges who were MPs, and from Polish Wikipedia, which the page marks as a secondary source. A judge with nothing shown has nothing recorded in these sources, which is not the same as having no ties. I coded each entry by hand from sentences that mention a party or an office, so a tie described without those words is missed.

*Rulings.* The case data are from the Tribunal's Internet Portal of Judgments (ipo.trybunal.gov.pl), which I read case by case for every series from 1997: abstract review (K), legal questions from courts (P), constitutional complaints (SK), reviews of lower acts (U), preventive review (Kp), competence disputes (Kpt), interpretation (W), signals (S) and political parties (Pp). I keep the metadata and the operative part of each ruling, not the texts, and link to the Tribunal's page for each. A case joined to another is decided with it, and the page shows its lead case's ruling. The outcome is my reading of the operative part: unconstitutional, constitutional, mixed (some provisions unconstitutional and others not), proceedings discontinued, or other, which includes the Tribunal's signals to the legislature. Cases filed before 1997 are not collected yet, so rulings in the first years are slightly low.

*Checks.* The Tribunal's annual information on its work gives the number of judgments and of discontinuance orders each year. For 2003 to 2017, the years I compared, my count of judgments matches in every year, and discontinuance orders match exactly in eight of fifteen years and within five in the rest. The Tribunal's catalogue of published decisions (OTK ZU), which covers 2016 onward, contains every one of the 151 entries I sampled and the number of published decisions it lists for a year equals mine in seven of the eleven years from 2016 to 2026 and differs by at most three in the rest. I have not compared the years 1997 to 2002 or 2018 to 2024 with the annual reports.

*The disputes since 2015.* Each event in the chronology has a date, a source and, where there is one, a link to the document. Where institutions disagree about whether a judge was validly elected or sworn in, the page states each side's position and takes no side. A judge's standing is shown only where it was disputed.

Sources: the #link("https://trybunal.gov.pl")[Constitutional Tribunal] (lists of judges, the portal of judgments, annual information, the catalogue of published decisions), #link("https://monitorpolski.gov.pl")[Monitor Polski] and the #link("https://api.sejm.gov.pl")[Sejm's open API] (resolutions, votes, prints, club records), the #link("https://www.prezydent.pl")[Chancellery of the President] and other official records for the oaths and the chronology, and #link("https://pl.wikipedia.org")[Polish Wikipedia] (CC BY-SA 4.0), flagged where used.

Ben Stanley, 2026. _Constitutional Tribunal_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/constitutional-tribunal/index.html",
  class: "embed-frame embed-frame--app",
  title: "Constitutional Tribunal: its judges, rulings and activity, and the disputes since 2015",
  loading: "lazy",
))[]
