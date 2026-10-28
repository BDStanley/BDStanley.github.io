#set document(title: [Party manifestos])
#metadata((
  title: "Party manifestos",
  description: "What Polish parties stressed in their Sejm election manifestos from 1991 to 2019, from the Manifesto Project's coding of each manifesto",
)) <website-metadata>

// The page is a standalone app, assets/party-manifestos/index.html (+ app.js),
// embedded here like Party ideology. Its data, manifestos.json, comes from Party
// manifestos/ in the Polish political analyses repo (analysis/30_export.R) and is
// copied in by scripts/sync-analyses.py on each build. The method note is a
// collapsed <details class="method">; its styles are with the other method notes
// in 90_site.css.

#html.elem("p", attrs: (class: "teaching-back"))[#link("../polish-political-analyses.html", "← Back")]

#heading(level: 1)[Party manifestos]

What each party's election manifesto stressed at the Sejm elections from 1991 to 2019: how much of it went to the economy, welfare, the political system, national identity and the other domains of political debate, which categories it stressed most, and how the emphasis of each party changed from one election to the next. From the coded categories I build measures of the positions manifestos imply on the economy, culture, the European Union, morality, law and order, deregulation, protectionism, public spending, redistribution, growth against the environment and regional autonomy, and of how much they stress corruption, agriculture and minorities, and set them beside the ratings of experts in the Chapel Hill Expert Survey. A heat map shows one election's manifestos side by side across every category, any two manifestos can be compared, a further view plots each dimension against the experts' rating of the same party, and #link("party-ideology.html")[Party ideology] shows the manifesto behind each Polish party's ratings.

#html.elem("details", attrs: (class: "method"))[
#html.elem("summary")[How this works]

The Manifesto Project reads each party's election manifesto and assigns every sentence, or part of a sentence (a quasi-sentence), to one of about 56 categories, such as free-market economy, welfare state expansion, traditional morality or the military. A category's share is the percentage of a manifesto's coded sentences that fall in it. The shares are those of the Manifesto Project dataset (version 2026a), which I reproduced from the sentences of the Manifesto Corpus (version 2026-1) for all 25 manifestos shown.

A position is the log of how much a manifesto stresses one pole over the other, with half a sentence added to each side so that a pole with no sentences does not break the ratio. For the economy, for example, the market-liberal pole is free-market economy, incentives, anti-protectionism, economic orthodoxy and limits on welfare, and the other pole is market regulation, planning, protectionism, a controlled economy, nationalisation, welfare and education expansion and labour groups. Each dimension's categories are listed in the data file, and its direction follows the Chapel Hill scale it is compared with. A salience is simply the percentage of a manifesto's sentences in the categories. I built these from the categories rather than using the Manifesto Project's own right–left index, which is shown only for comparison with the experts.

A position here is emphasis, not a promise. A manifesto that says little about the economy sits near the middle of the economy scale whatever it would do in office, and the scales are not those of the experts, so the order of parties says more than the numbers. The intervals come from resampling each manifesto's sentences 1,000 times; they show how much a short manifesto's shares could move by chance, not how far coders might disagree. The comparison view flags a category gap when a two-proportion test on sentence counts finds it unlikely by chance; with 56 categories compared, a few flags are expected by chance, and sentences are not independent draws, so it is a guide only.

Some manifestos are special. The Left and Democrats (2007), The Left, the Polish Coalition, Confederation and the Civic Coalition (all 2019) are coalitions' manifestos. Law and Justice and Modern in 2015 are general manifestos, the latter adopted in 2016. The German Minority's manifestos are short, two of them under 100 sentences. The Manifesto Project has not yet coded the 2023 election. Its corpus has the full text only for some earlier manifestos, so the page's shares for the others are the dataset's own; the Chapel Hill survey rated Polish parties only from 2002, so earlier manifestos have no expert comparison.

Sources (the Manifesto Project's terms ask for these citations):

- The #link("https://manifesto-project.wzb.eu")[Manifesto Project], WZB Berlin Social Science Center and the Institute for Democracy Research (IfDem), Göttingen, whose coders read and categorised every manifesto used here.
- Lehmann, Pola, Simon Franzmann, Denise Al-Gaddooa, Christoph Ivanusch, Felicia Riethmüller, Svenja Samstag, Andrea Volkens, Bernhard Weßels and Lisa Zehnter (2026), _The Manifesto Data Collection_, Manifesto Project (MRG/CMP/MARPOR), version 2026a, WZB Berlin Social Science Center and IfDem Göttingen, #link("https://doi.org/10.25522/manifesto.mpds.2026a")[doi.org/10.25522/manifesto.mpds.2026a].
- Lehmann, Pola, Simon Franzmann, Denise Al-Gaddooa, Christoph Ivanusch, Jirka Lewandowski, Felicia Riethmüller, Svenja Samstag and Lisa Zehnter (2026), _Manifesto Corpus_, version 2026-1.
- The Chapel Hill Expert Survey, as on my #link("party-ideology.html")[Party ideology] page.

Ben Stanley, 2026. _Party manifestos_, *Polish Political Analyses*, benstanley.eu.
]

#html.elem("iframe", attrs: (
  src: "../../assets/party-manifestos/index.html",
  class: "embed-frame embed-frame--app",
  title: "Party manifestos: what Polish parties stressed, 2007–2019",
  loading: "lazy",
))[]
