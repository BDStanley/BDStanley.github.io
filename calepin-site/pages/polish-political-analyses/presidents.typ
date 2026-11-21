#set document(title: [Presidents])
#metadata((
  title: "Presidents",
  description: "Poland's presidents since 1989: their parties, Chancellery and governments, and how they used their powers — vetoes, bills, applications to the Constitutional Tribunal, appointments and pardons — compared across presidencies",
)) <website-metadata>

// The page is a standalone app, assets/presidents/index.html (+ app.js), embedded here like
// Governments. Its data, presidents.json, comes from Presidents/ in the Polish political analyses
// repo (analysis/90_export.R) and is copied in by scripts/sync-analyses.py on each build.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Presidents]

Poland's presidents from Wojciech Jaruzelski, elected by the National Assembly in July 1989, to Karol Nawrocki, and the acting presidents of 2010. For each the page shows when and how he came to office, his party ties, the governments he served beside and whether his own party sat in them, and the heads of his Chancellery and of the National Security Bureau. It follows how each president used his powers: the laws he vetoed and what the Sejm did with them, the bills he submitted, the laws and other acts he asked the Constitutional Tribunal to rule on, the appointments and other decisions published in the official gazette, the prime ministers he appointed, and the people he pardoned. A comparison view sets the presidents side by side on every measure, as totals or per year in office, and splits vetoes and bills by whether the president's own party was in government. See also #link("governments.html")[Governments], #link("constitutional-tribunal.html")[Constitutional Tribunal], the #link("sejm.html")[Sejm] and the presidential elections in #link("election-results.html")[Election results].

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

The presidents' terms are those of their oaths of office. A president's own party is the party he belonged to when first elected or, for Karol Nawrocki, who stood as a non-party candidate, the party that backed him; a government is a cohabitation when that party was not in it. Lech Wałęsa had no party of his own, and Wojciech Jaruzelski none after his party dissolved itself in January 1990, so their terms are not classified. Governments, their parties and their dates come from my #link("party-genealogy.html")[Parties in the Sejm] tracker. Election results are the National Electoral Commission's, from my #link("election-results.html")[Election results] page, and so is each president's party membership when he stood. Whether he left his party on taking office, and the heads of the Chancellery and of the National Security Bureau with their party ties, come from Polish Wikipedia; a head with no party shown has none recorded there, which is not the same as having none.

Vetoes and bills since October 1997 come from the Sejm's own records of every legislative process, through its open API; the records start with the term elected in September 1997. A law counts as vetoed when the record shows the president's motion for its reconsideration, the Sejm's vote on that motion or the president's message as a Sejm print, or when a law sent to the president was neither signed, published nor sent to the Constitutional Tribunal: the Constitution leaves him no other course, and the records leave out the veto of several laws of 2015–23. Each law is counted once, though bills considered together can each carry its record. A veto stood when the Sejm voted and failed to reach three fifths, and was overridden when it reached them or the law was later published; a veto the Sejm never voted on ended the law with the term. The totals for 1989–97, and Kwaśniewski's totals per term, are the Chancellery's or the press's as reported on Polish Wikipedia, and so are the totals the page uses to check the records: they agree for Lech Kaczyński (18) and Bronisław Komorowski (4). Bill topics are the Comparative Agendas Project codes of my #link("sejm.html")[Sejm] page, which cover bills from 2005.

Applications to the Constitutional Tribunal are every case since 1997 in which the president is named as an applicant in the Tribunal's portal, from my #link("constitutional-tribunal.html")[Constitutional Tribunal] page, with the result the portal records. Some were joint applications with others.

Appointments and other decisions are the president's acts published in Monitor Polski, the official gazette, read from the Sejm's legal database and sorted by kind from their titles. They are counts of acts, not of people: one decision can appoint several judges, and one can decorate dozens. Monitor Polski carries few presidential acts before 1993, and appointments of ambassadors and judges, promotions to general and professors' titles only from 2001, so each kind is counted from its first year of regular publication and the rate per year uses only those years. Prime ministers come from the Parties in the Sejm tracker; the presidents of the National Bank of Poland are those the Sejm appointed on the president's motion, from its resolutions in Monitor Polski.

Pardon totals are the Chancellery's, as reported on Polish Wikipedia, for each president's whole term; the Chancellery's own statistics could not be read directly, no yearly figures are published, and for Andrzej Duda the period the figure covers is not stated.

Sources: the #link("https://api.sejm.gov.pl")[Sejm's open API] for legislative processes, roll calls and Monitor Polski; the #link("https://ipo.trybunal.gov.pl")[Constitutional Tribunal's portal]; the National Electoral Commission; #link("https://pl.wikipedia.org")[Polish Wikipedia] (CC BY-SA 4.0) for party ties, the heads of the Chancellery and the National Security Bureau, pardon totals and vetoes and bills before 1997, citing the Chancellery of the President and the press.

Ben Stanley, 2026. _Presidents_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/presidents/index.html",
  class: "embed-frame embed-frame--app",
  title: "Presidents: Poland's presidents and their powers, 1989–2026",
  loading: "lazy",
))[]
