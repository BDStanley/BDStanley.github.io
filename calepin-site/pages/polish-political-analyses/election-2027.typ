#set document(title: [Election 2027])
#metadata((
  title: "Election 2027",
  description: "The Election 2027 page is now the Polling tracker's Scenarios page",
)) <website-metadata>

// The Election 2027 page was retired on 21 November 2026 (Ben's decision): its tools are the Polling tracker's Scenarios page
// (Groups and Who votes), and its blog and Timing and organisation page were withdrawn. This page stays, out of the analyses
// index, so that old links land on Scenarios.
#html.elem("script", "location.replace('polling-tracker/scenarios.html');")
#html.elem("meta", attrs: (http-equiv: "refresh", content: "0; url=polling-tracker/scenarios.html"))

#heading(level: 1)[Election 2027]

The Election 2027 page is now part of the Polling tracker: see its #link("polling-tracker/scenarios.html")[Scenarios] page for changes in turnout or support by group of voters, and what they would mean for seats.
