#set document(title: [Publications])
#metadata((title: "Publications")) <website-metadata>

// No on-page title heading (kept off, like the other pages); the nav label and
// browser-tab title still come from the metadata / #set document above.

= Citations

// The counts live in data/citations.json, which scripts/update-citations.py
// refreshes from Scopus, Google Scholar, OpenAlex and Sage Policy Profiles on
// every build. "Updated" is the most recent month; a source whose numbers are
// older (one the script couldn't read) names its own month.
#let cites = json("/data/citations.json")
#let scopus = cites.scopus
#let gs = cites.google_scholar
#let oa = cites.openalex
#let sage = cites.sage_policy_profiles
#let latest = (scopus, gs, oa, sage).map(s => s.as_of).sorted().last()
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
- #link("https://openalex.org/authors/A5015716464")[OpenAlex]#when(oa): #num(oa.citations) (h-index = #oa.h_index, i10-index = #oa.i10_index)
- #link("https://policyprofiles.sagepub.com/profile/21087/ben-stanley")[Sage Policy Profiles]#when(sage): #num(sage.citations) citations across #num(sage.documents) policy documents.

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

// OpenAlex's citation count at the end of each entry, before its full stop and
// linked to the publication's OpenAlex record: "… doi:10.xxx (12 citations)."
// From data/publication-citations.json (scripts/update-citations.py), keyed by
// the entry's DOI, or by the whole entry for those without one. Entries with no
// citations, or none that OpenAlex knows of, are left as they are.
#let pub-cites = json("/data/publication-citations.json")
#let with-count(s) = {
  let m = s.match(regex("doi:(10\.[^\s]+)"))
  let rec = if m != none {
    pub-cites.by_doi.at(lower(m.captures.at(0).trim(".", at: end)), default: none)
  } else {
    pub-cites.by_entry.at(s, default: none)
  }
  if rec == none or rec.citations == 0 { return render-entry(s) }
  let n = rec.citations
  let label = if n == 1 [1 citation] else [#num(n) citations]
  let note = [ (#link("https://openalex.org/works/" + rec.openalex, label))]
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
