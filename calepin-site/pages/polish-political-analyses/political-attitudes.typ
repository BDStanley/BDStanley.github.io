#set document(title: [Social and political trends])
#metadata((
  title: "Social and political trends",
  description: "Long-run trends in Polish society and public opinion since 1990, pooled from many surveys, from left–right self-placement and support for EU membership to trust in institutions, religion and living conditions, with breakdowns by party, age, gender, education and place of residence",
)) <website-metadata>

// The page was called Political attitudes until 23 October 2026; its file names, the asset folder
// and the repo folder keep that name. The chart is a standalone page, assets/political-attitudes/index.html (+ app.js), embedded
// like the Polling tracker. Its data, attitudes.json and one <slug>.json per attitude, comes from
// "Political attitudes/web/" in the Polish political analyses repo (written by web/build_web.R
// from each attitude's subfolder) and is copied in by scripts/sync-analyses.py on each build.
// A new attitude needs no change here: add it to Political attitudes/web/attitudes.csv.
// The method note (How this works) is a collapsed <details class="method"> under the
// introduction; its styles are with the other method notes in 90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Social and political trends]

How Poles' attitudes, values and lives have changed since the end of communism. Each trend pools every comparable survey reading I could find, from the early 1990s to the latest polls, into one monthly estimate, and can be broken down by party preference, age, gender, age and gender together, education and place of residence. The trends are grouped under ideology, the economy, society and morality, nation and immigration, Europe and the world, democracy and government, trust, the state of the country, religion and values, the communist past and living conditions.

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

No single survey has asked the same question from 1990 to today. Each trend therefore combines every survey that asked about the attitude, in its own words: CBOS's monthly surveys, the Eurobarometers, the European Social Survey, the Polish National Election Study, the Polish General Social Survey, the European Election Study and others.

Every reading is turned into a single number. Where every survey offers a pro and an anti answer, as on EU membership, it is the share of respondents taking a side who give the pro answer; those who say "don't know", or choose a neutral middle option, are set aside. Where surveys ask people to place themselves on scales of different lengths, as on left and right, every answer is placed on one scale from 0 to 10 and the reading is the average position of those who answer. A Bayesian measurement model (Stan) then treats each question as a noisy measure of one underlying level that changes from month to month. Each question has its own level and its own sensitivity to that underlying level, estimated from the periods in which it overlaps with other questions, and its own allowance for error beyond sampling error, which absorbs differences of wording, fieldwork organisation and mode. The underlying level follows a random walk, so it is smooth when little happens but can move quickly when many surveys show a change. Each trend is expressed on the scale of one anchor question, CBOS's in its face-to-face surveys: it reads as the share, or the average position, that question would have recorded. Dots show each reading placed on the same scale, after allowing for its question's level and sensitivity. Bands are 80% and 95% credible intervals.

For the breakdowns, each group's line is the national trend plus a deviation that changes slowly over time, estimated from every survey that reports that group. Where surveys report categories that do not match exactly (for example, Eurobarometer's "small or middle-sized town"), the reading is modelled as a mix of the groups it covers. Between the surveys that report a breakdown, a group's line keeps its last known distance from the national trend and its interval widens. Party preference is the party respondents intend to vote for, or voted for at the last Sejm election, followed across elections by party lineage; a party's line is shown while it existed and its voters were surveyed. Age and gender splits each age group into men and women.

The data come from survey microdata wherever they are available, and published results fill in the rest. Survey readings are estimates too, and the early 1990s rest on few of them, so the intervals there are wide.

*Support for EU membership.* CBOS has asked whether people support Poland's integration with (since 2004, membership of) the EU since 1994; the other surveys and commercial pollsters have asked other questions at other times, about membership, a referendum or whether Poland would be better off outside the EU. The line is the share of supporters among those with a view, on the scale of CBOS's question in its face-to-face surveys up to April 2020. CBOS's surveys since May 2020, which mix face-to-face, telephone and online interviews, are treated as a separate question, because the change of mode may have shifted answers. That is why the line since then sits a few points below CBOS's own published figures: the other surveys suggest the mixed-mode question reads somewhat higher.

Sources for support for EU membership:

- CBOS's monthly surveys, 1995–2017 (microdata, from the Polish Social Data Archive)
- Central and Eastern Eurobarometer, 1990–97 (microdata)
- Candidate Countries Eurobarometer, 2000–04 (microdata)
- Polish General Social Survey, 1995 and 2005 (microdata)
- Polish National Election Study, 2001 and 2005 (microdata)
- European Social Survey, 2016–23 (microdata)
- Standard Eurobarometer, 2018–24 (microdata)
- European Election Study, 2024 (microdata)
- CBOS's published trend series and its breakdowns by party electorate
- Eurobarometer reports
- Commercial polls

*Left–right self-placement.* Most surveys ask people where they would place their own political views on a scale from left to right: CBOS on a line of seven points, from left-wing (1) to right-wing (7); the European Social Survey, the Polish National Election Study and the European Election Study from 0 to 10; the Eurobarometers and the Polish General Social Survey from 1 to 10. Every answer is placed on one scale from 0 (left) to 10 (right), so that the middle of each scale (4 on CBOS's line, halfway between 5 and 6 on a ten-point scale) falls at 5, and the line is the average position of those who place themselves; "don't know" is set aside. It is read on the scale of CBOS's question in its face-to-face monthly surveys. Because most people place themselves near the middle, averages move less than shares: a shift of a few tenths of a point is a sizeable change in the balance between left and right.

The share giving no position at all varies, between surveys and over time: in the Eurobarometer it fell from about a quarter of Polish respondents in 2018–19 to a few per cent from 2020, so the waves before and after are treated as separate questions. CBOS's monthly microdata end in 2017, apart from a survey in April 2021 that mixed interview modes and is treated as a separate question. Since 2018 the line therefore rests on the European Social Survey, the Eurobarometer, the Polish National Election Study, the European Election Study and online surveys run at SWPS University, each series of which is a question of its own.

Sources for left–right self-placement:

- CBOS's monthly surveys, 1994–2017 (microdata, from the Polish Social Data Archive), and its survey of April 2021 (microdata)
- Central and Eastern Eurobarometer, 1990–92 (microdata)
- Consolidation of Democracy in Central and Eastern Europe, 1991 and 2000 (microdata)
- Polish General Social Survey, 1992–2010 (microdata)
- Polish National Election Study, 1997–2025 (microdata)
- European Social Survey, 2002–23 (microdata)
- Candidate Countries Eurobarometer, 2004 (microdata)
- Standard Eurobarometer, 2018–25 (microdata)
- European Election Study, 2024 (microdata)
- SWPS University's online surveys: the COVID tracker, 2020–21; the NCN Opus panel, 2023–24; FNW SWPS surveys, 2025–26 (microdata)

Ben Stanley, 2026. _Social and political trends_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/political-attitudes/index.html",
  class: "embed-frame embed-frame--app",
  title: "Social and political trends: Polish society and public opinion since 1990",
  loading: "lazy",
))[]
