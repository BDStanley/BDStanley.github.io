#set document(title: [Publications])
#metadata((title: "Publications")) <website-metadata>

// No on-page title heading (kept off, like the other pages); the nav label and
// browser-tab title still come from the metadata / #set document above.

= Citations

_Updated: March 2026_

- #link("https://www.webofscience.com/wos/author/record/J-7051-2013")[Web of Science]: 1,153 (h-index = 10)
- #link("https://scholar.google.com/citations?user=8AO7H9gAAAAJ&hl=en")[Google Scholar]: 3,845 (h-index = 18, i10-index = 20)
- #link("https://policyprofiles.sagepub.com/profile/21087/ben-stanley")[Sage Policy Profiles]: 64 citations across 52 policy documents.

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

#{
  let pub-keys = ("monographs", "journal_articles", "book_chapters", "datasets")
  for sec in cv.sections {
    if pub-keys.contains(sec.key) {
      // Tolerate an empty/removed section (`entries:` -> none) without erroring.
      let entries = sec.at("entries", default: ())
      if entries == none { entries = () }
      if entries.len() > 0 {
        heading(level: 1, sec.title)
        list(..entries.map(render-entry))
      }
    }
  }
}
