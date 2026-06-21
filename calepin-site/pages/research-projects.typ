#set document(title: [Research projects])
#metadata((title: "Research projects")) <website-metadata>

// No on-page title heading; nav label / tab title come from the metadata above.
// Read straight from cv/cv.yml — the same source as the CV PDF. Editing a
// project in the CV updates the CV and this page. Sections and order follow the
// CV (Current research projects, then Previous research projects).
#let cv = yaml("/cv/cv.yml")

#let fmt-range(e) = {
  let s = str(e.at("start-date", default: ""))
  let en = if "end-date" in e { str(e.end-date) } else { "" }
  if en != "" and en != s { s + "–" + en } else { s }
}

// One entry, formatted like the CV: italic date + bold title on the first line,
// italic funder on its own line, smaller details below. Classes are styled in
// assets/site.css (HTML-only build, so html.elem is safe here).
#let render-projects(entries) = {
  for e in entries {
    html.elem("div", attrs: (class: "proj"))[
      #html.elem("div", attrs: (class: "proj-title"))[
        #html.elem("span", attrs: (class: "proj-date"))[#emph(fmt-range(e))]#strong(e.title)
      ]
      #html.elem("div", attrs: (class: "proj-funder"))[#emph(e.at("institution", default: ""))]
      #html.elem("div", attrs: (class: "proj-detail"))[#e.at("description", default: "").trim()]
    ]
  }
}

#{
  for key in ("projects_current", "projects_previous") {
    let sec = cv.sections.find(x => x.key == key)
    if sec != none {
      // A section can have `entries:` empty/removed in cv.yml (-> none); treat
      // that as no entries and skip the heading entirely.
      let entries = sec.at("entries", default: ())
      if entries == none { entries = () }
      if entries.len() > 0 {
        heading(level: 1, sec.title)
        render-projects(entries)
      }
    }
  }
}
