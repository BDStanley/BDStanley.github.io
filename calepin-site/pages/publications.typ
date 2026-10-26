#set document(title: [Publications])
#metadata((title: "Publications")) <website-metadata>

// No on-page title heading (kept off, like the other pages); the nav label and
// browser-tab title still come from the metadata / #set document above.

= Citations

// One list. Academic: the counts in data/citations.json, which scripts/update-citations.py
// refreshes from Scopus, Google Scholar and OpenAlex on every build. Policy:
// the totals in data/policy-reach.json (scripts/update-policy-reach.py), the
// policy documents from Sage Policy Profiles and the research from policy
// organisations in OpenAlex. Media: the totals in data/media-reach.json
// (scripts/update-media-reach.py), from Google News and Google Alerts.
// "Updated" is the most recent month; a source whose numbers are older (one a
// script couldn't read) names its own month. The OpenAlex line links to the
// Citation reach page (pages/citation-reach.typ), the Policy line to the Policy
// reach page (pages/policy-reach.typ) and the Media line to the Media reach
// page (pages/media-reach.typ); none of them is in the navigation bar.
#let cites = json("/data/citations.json")
#let scopus = cites.scopus
#let gs = cites.google_scholar
#let oa = cites.openalex
#let pr = json("/data/policy-reach.json")
#let policy = (as_of: pr.as_of.values().sorted().last(), ..pr.totals)
#let md = json("/data/media-reach.json")
#let media = (as_of: md.as_of, ..md.totals)
#let latest = (scopus, gs, oa, policy, media).map(s => s.as_of).sorted().last()
#let month(ym) = datetime(year: int(ym.slice(0, 4)), month: int(ym.slice(5, 7)), day: 1).display("[month repr:long] [year]")
#let when(s) = if s.as_of != latest [ (#month(s.as_of))]
// 4044 -> "4,044"
#let num(n) = {
  let s = str(n)
  let out = ""
  for (i, c) in s.clusters().enumerate() {
    if i > 0 and calc.rem(s.len() - i, 3) == 0 { out += "," }
    out += c
  }
  out
}

_Updated: #month(latest)_

- #link("https://www.scopus.com/authid/detail.uri?authorId=23493968300")[Scopus]#when(scopus): #num(scopus.citations) (h-index = #scopus.h_index)
- #link("https://scholar.google.com/citations?user=8AO7H9gAAAAJ&hl=en")[Google Scholar]#when(gs): #num(gs.citations) (h-index = #gs.h_index, i10-index = #gs.i10_index)
- #link("https://openalex.org/authors/A5015716464")[OpenAlex]#when(oa): #num(oa.citations) (h-index = #oa.h_index, i10-index = #oa.i10_index\; #link("citation-reach.html")[reach by country and discipline])
- Think tanks and other policy organisations#when(policy): #num(policy.documents) policy documents and #num(policy.works) research works, from #num(policy.organisations) organisations in #num(policy.countries) countries (#link("https://policyprofiles.sagepub.com/profile/21087/ben-stanley")[Sage Policy Profiles] and #link("https://openalex.org/authors/A5015716464")[OpenAlex]\; #link("policy-reach.html")[reach by organisation and country])
- News media#when(media): #num(media.articles) articles quoting, interviewing or citing me, in #num(media.outlets) outlets in #num(media.countries) countries and #num(media.languages) languages (Google News and Google Alerts\; #link("media-reach.html")[reach by outlet and country])

// ============================================================================
// Single source of truth: the CV. These lists are read straight from
// cv/cv.yml (the same file that builds cv/cv.pdf), so editing a publication in
// the CV updates the CV PDF *and* this page. Sections appear in the same order
// as in the CV (Monographs first), with the CV's own section titles.
//
// Entry strings use the academicv markup convention (`*...*` = bold, as in the
// CV); we additionally turn `doi:...` and bare URLs into clickable links.
// ============================================================================
#let cv = yaml("/cv/cv.yml")

#let render-entry(s) = {
  // bare URLs first (so the doi links injected below aren't reprocessed)
  let t = s.replace(regex("(https?://[^\s]+)"), m => {
    let u = m.captures.at(0)
    let tr = ""
    if u.ends-with(".") { tr = "."; u = u.slice(0, -1) }
    "#link(\"" + u + "\")" + tr
  })
  // then bare DOIs: doi:10.xxxx -> https://doi.org/10.xxxx
  t = t.replace(regex("doi:(10\.[^\s]+)"), m => {
    let d = m.captures.at(0)
    let tr = ""
    if d.ends-with(".") { tr = "."; d = d.slice(0, -1) }
    "#link(\"https://doi.org/" + d + "\")[doi:" + d + "]" + tr
  })
  eval(t, mode: "markup")
}

// OpenAlex's citation count at the end of each entry, before its full stop:
// "… doi:10.xxx (12 citations).", linked to OpenAlex's list of the works citing
// it; "(0 citations)", unlinked, when nothing cites it yet; and "(no citation
// count available)" when OpenAlex doesn't have the publication. From
// data/publication-citations.json (scripts/update-citations.py), keyed by the
// entry's DOI, or by the whole entry for those without one.
#let pub-cites = json("/data/publication-citations.json")
#let with-count(s) = {
  let m = s.match(regex("doi:(10\.[^\s]+)"))
  let rec = if m != none {
    pub-cites.by_doi.at(lower(m.captures.at(0).trim(".", at: end)), default: none)
  } else {
    pub-cites.by_entry.at(s, default: none)
  }
  let note = if rec == none {
    [ (no citation count available)]
  } else if rec.citations == 0 {
    [ (0 citations)]
  } else {
    let n = rec.citations
    let label = if n == 1 [1 citation] else [#num(n) citations]
    [ (#link("https://openalex.org/works?filter=cites:" + rec.openalex, label))]
  }
  if s.ends-with(".") [#render-entry(s.slice(0, -1))#note.] else [#render-entry(s)#note]
}

#{
  let pub-keys = ("monographs", "journal_articles", "book_chapters", "datasets")
  for sec in cv.sections {
    if pub-keys.contains(sec.key) {
      // Tolerate an empty/removed section (`entries:` -> none) without erroring.
      let entries = sec.at("entries", default: ())
      if entries == none { entries = () }
      if entries.len() > 0 {
        heading(level: 1, sec.title)
        list(..entries.map(with-count))
      }
    }
  }
}
