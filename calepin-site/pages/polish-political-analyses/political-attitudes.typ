#set document(title: [Political attitudes])
#metadata((
  title: "Political attitudes",
  description: "Long-run trends in Polish public opinion since 1990, pooled from many surveys, with breakdowns by party, age, gender, education and place of residence",
)) <website-metadata>

// The chart is a standalone page, assets/political-attitudes/index.html (+ app.js), embedded
// like the Polling tracker. Its data, attitudes.json and one <slug>.json per attitude, comes from
// "Political attitudes/web/" in the Polish political analyses repo (written by web/build_web.R
// from each attitude's subfolder) and is copied in by scripts/sync-analyses.py on each build.
// A new attitude needs no change here: add it to Political attitudes/web/attitudes.csv.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Polish political analyses")]

#heading(level: 1)[Political attitudes]

How Poles' attitudes have changed since the end of communism. Each trend pools every comparable survey reading I could find, from the early 1990s to the latest polls, into one monthly estimate, and can be broken down by party preference, age, gender, education and place of residence. The first attitude is support for Poland's membership of the European Union; more will follow. The method is explained below the chart.

#html.elem("iframe", attrs: (
  src: "../../assets/political-attitudes/index.html",
  class: "embed-frame embed-frame--app",
  title: "Political attitudes: trends in Polish public opinion since 1990",
  loading: "lazy",
))[]

#heading(level: 2)[How this works]

No single survey has asked the same question about the European Union from 1990 to today. CBOS has asked whether people support Poland's integration with (since 2004, membership of) the EU since 1994; Eurobarometer, the European Social Survey, the Polish National Election Study, the Polish General Social Survey, the European Election Study and commercial pollsters have asked other questions at other times. The trend combines them all.

Every reading is turned into the share of respondents taking a side who give the pro-EU answer (those who say "don't know", or choose a neutral middle option, are set aside). A Bayesian measurement model (Stan) then treats each question as a noisy measure of one underlying level of support that changes from month to month. Each question has its own level and its own sensitivity to that underlying support, estimated from the periods in which it overlaps with other questions, and its own allowance for error beyond sampling error, which absorbs differences of wording, fieldwork organisation and mode. The underlying support follows a random walk, so it is smooth when little happens but can move quickly when many surveys show a change. The line is expressed on the scale of CBOS's question in its face-to-face surveys (up to April 2020): it reads as the share of supporters among those with a view who would have been recorded by that question. CBOS's surveys since May 2020, which mix face-to-face, telephone and online interviews, are treated as a separate question, because the change of mode may have shifted answers. Dots show each reading placed on the same scale, after allowing for its question's level and sensitivity. Bands are 80% and 95% credible intervals.

For the breakdowns, each group's line is the national trend plus a deviation that changes slowly over time, estimated from every survey that reports that group. Where surveys report categories that do not match exactly (for example, Eurobarometer's "small or middle-sized town"), the reading is modelled as a mix of the groups it covers. Between the surveys that report a breakdown, a group's line keeps its last known distance from the national trend and its interval widens. Party preference is the party respondents intend to vote for, or voted for at the last Sejm election, followed across elections by party lineage; a party's line is shown while it existed and its voters were surveyed.

The data come from microdata (Central and Eastern Eurobarometer 1990–97, PGSS 1995 and 2005, PGSW 2001 and 2005, ESS 2016–23, Standard Eurobarometer 2022–24, European Election Study 2024) and from published results (CBOS's trend series and its breakdowns by party electorate, Eurobarometer reports and commercial polls). Survey readings are estimates too, and the early 1990s rest on few of them, so the intervals there are wide.
