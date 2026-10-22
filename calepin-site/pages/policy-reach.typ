#set document(title: [Policy reach])
#metadata((title: "Policy reach")) <website-metadata>

// Not in the navigation bar: the Sage Policy Profiles line of the Citations
// list on the Publications page links here. Every number, colour, bar length,
// hover text and list entry comes from data/policy-reach.json
// (scripts/update-policy-reach.py, run by build.sh) and the map shapes from
// data/world-map.json; this page only lays them out. It reuses the Citation
// reach page's .reach-* styles, plus .policy-* for the lists, in
// themes/site-theme/css/90_site.css. The Quarto backup builds the same markup
// in quarto-site/R/policy-helpers.R.

#let pr = json("/data/policy-reach.json")
#let world = json("/data/world-map.json")
#let t = pr.totals
#let month(ym) = datetime(year: int(ym.slice(0, 4)), month: int(ym.slice(5, 7)), day: 1).display("[month repr:long] [year]")
#let latest = pr.as_of.values().sorted().last()
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
#let el(tag, ..attrs, body) = html.elem(tag, attrs: attrs.named(), body)
// Bodies with several elements are code blocks ({...}), not markup: markup
// would put a space, as a <span>, between inline elements.

// One horizontal bar per row: label, then a bar of length w (a share of the
// longest) in the row's colour, with the value at its tip.
#let bars(rows, label, value) = el("div", class: "reach-bars", {
  for r in rows {
    el("div", class: "reach-bar-row", title: r.tip, {
      el("span", class: "reach-bar-label")[#r.at(label)]
      el("span", class: "reach-bar-area", {
        el("span", class: "reach-bar", style: "--w: " + str(r.w) + "; background: " + r.colour)[]
        el("span", class: "reach-bar-value")[#num(r.at(value))]
      })
    })
  }
})
// A collapsed table under each chart, so every value can be read without hovering.
#let data-table(summary, head, rows) = el("details", class: "reach-table")[
  #el("summary")[#summary]
  #el("div", class: "reach-table-scroll")[
    #el("table")[
      #el("thead")[#el("tr")[#for (i, h) in head.enumerate() { el("th", class: if i > 0 { "num" } else { "" })[#h] }]]
      #el("tbody")[#for r in rows {
        el("tr")[#for (i, c) in r.enumerate() { el("td", class: if i > 0 and type(c) == int { "num" } else { "" })[#if type(c) == int { num(c) } else { c }] }]
      }]
    ]
  ]
]
#let swatch(colour, name) = el("span", class: "reach-legend-item", {
  el("span", class: "reach-swatch", style: "background: " + colour)[]
  name
})
// The documents and works citing one publication, newest first, collapsed.
#let items(summary, rows) = el("details", class: "reach-table policy-details")[
  #el("summary")[#summary]
  #el("ul", class: "policy-items")[#for it in rows {
    el("li", {
      el("span", class: "policy-item-title")[#link(it.url)[#it.title]]
      el("span", class: "policy-item-meta")[#(it.when, it.orgs, it.kind).filter(s => s != "").join(" · ")]
    })
  }]
]

#el("p", class: "reach-back")[#link("publications.html")[← Publications]]

= Policy reach

_Updated: #month(latest)_

Where my work is taken up beyond academia: the policy documents that cite it,
and the research by think tanks, foundations, government bodies and
international organisations that cites it. The policy documents come from
#link("https://policyprofiles.sagepub.com/profile/21087/ben-stanley")[Sage Policy Profiles],
which draws on Overton's index of policy documents; the research from
#link("https://openalex.org/authors/A5015716464")[OpenAlex], an open index of
scholarly works.

#el("div", class: "reach-kpis", {
  for (n, label) in ((t.documents, "policy documents"), (t.works, "research works"), (t.organisations, "organisations"), (t.countries, "countries")) {
    el("div", class: "reach-kpi", {
      el("span", class: "reach-kpi-value")[#num(n)]
      el("span", class: "reach-kpi-label")[#label]
    })
  }
})

= Countries

Where the organisations are based. Hover over a country to see its numbers. EU
institutions and international organisations have no country: they are in the
chart below the map, but not on it.

#let by-code = pr.countries.filter(c => c.on_map).fold((:), (acc, c) => { acc.insert(c.code, c); acc })
#el("figure", class: "reach-map")[
  #el("svg", viewBox: world.viewBox, class: "reach-map-svg", role: "img",
      aria-label: "World map shading each country by the number of policy documents and research works from organisations there that cite my work")[
    #for c in world.countries {
      let row = if c.code == none { none } else { by-code.at(c.code, default: none) }
      el("path", d: c.d, fill: if row == none { pr.no_records_colour } else { row.colour })[
        #el("title")[#if row == none [#c.name: none] else [#row.tip]]
      ]
    }
    #for p in world.points {
      let row = by-code.at(p.code, default: none)
      if row != none {
        el("circle", class: "reach-map-dot", cx: str(p.x), cy: str(p.y), r: "4", fill: row.colour)[#el("title")[#row.tip]]
      }
    }
  ]
  #el("figcaption", class: "reach-legend", {
    el("span", class: "reach-legend-title")[Documents and works]
    for b in pr.bins { swatch(b.colour, b.label) }
    swatch(pr.no_records_colour, "None")
  })
]

#bars(pr.countries, "name", "records")
#data-table([As a table], ("Country", "Documents and works", "Organisations"),
  pr.countries.map(c => (c.name, c.records, c.organisations)))

= Organisations

The #num(t.organisations) organisations, by type:

#bars(pr.types, "name", "organisations")

The #pr.top_organisations organisations with the most documents and works, by
type:

#el("div", class: "reach-legend")[#for r in pr.types { swatch(r.colour, r.name) }]
#bars(pr.organisations.slice(0, calc.min(pr.top_organisations, pr.organisations.len())), "name", "total")
#data-table([All #num(t.organisations) organisations], ("Organisation", "Type", "Country", "Policy documents", "Research works"),
  pr.organisations.map(o => (o.name, o.type_label, o.country, o.documents, o.works)))

= By year

Documents and works each year, by the year they were published, divided by the
type of organisation. Hover over a bar to see its numbers.

#let years = pr.years
#el("div", class: "reach-legend")[#for l in years.legend { swatch(l.colour, l.name) }]
#el("div", class: "reach-years", role: "img",
    aria-label: "Bar chart of policy documents and research works per year from " + str(years.rows.first().year) + " to " + str(years.rows.last().year) + ", each bar divided by type of organisation")[
  #el("div", class: "reach-years-plot")[
    #for tk in years.ticks {
      el("div", class: "reach-tick", style: "bottom: " + str(tk.pos * 100) + "%")[#el("span")[#tk.label]]
    }
    #el("div", class: "reach-cols")[
      #for y in years.rows {
        el("div", class: "reach-col", title: y.tip)[
          #el("div", class: "reach-stack", style: "height: " + str(y.h * 100) + "%")[
            #for s in y.segments {
              el("span", class: "reach-seg", title: s.tip, style: "flex-grow: " + str(s.count) + "; background: " + s.colour)[]
            }
          ]
        ]
      }
    ]
  ]
  #el("div", class: "reach-years-axis")[
    #for y in years.rows { el("span", class: if y.minor { "reach-year minor" } else { "reach-year" })[#y.year] }
  ]
]
#data-table([As a table], ("Year", "Documents and works", ..years.legend.map(l => l.name)),
  years.rows.map(y => (y.label, y.count, ..years.legend.map(l => {
    let s = y.segments.find(s => s.name == l.name)
    if s == none { 0 } else { s.count }
  }))))

= Key ideas

#for g in pr.ideas {
  heading(level: 2, g.idea)
  if g.items.len() == 0 [
    Set out in _#g.title;_ (#g.year). No policy documents or research works
    from policy organisations cite it yet.
  ] else [
    Set out in _#g.title;_ (#g.year), which is cited in #g.summary.
    #items([The documents and works], g.items)
  ]
}

= Other publications

My other publications cited in policy documents or in research from policy
organisations, the most cited first.

#for g in pr.other_publications {
  items([_#g.title;_ (#g.year): #g.summary], g.items)
}

= About these figures

- Policy documents are those Sage Policy Profiles finds citing one of my works.
  Sage lists #t.sage_listed, of which #t.documents are counted here. Left out are issues
  of academic journals that Overton files as policy documents, copies of my own
  works, second copies of the same document, and documents citing only works by
  other authors that Sage attributes to me.
- Research works are works in OpenAlex that cite mine and whose authors list a
  think tank, foundation, government body or international organisation, but
  not works I wrote myself. Research bodies OpenAlex files the same way, such as
  academies of sciences and national research councils, are left out.
- An organisation in both sources is counted once. Its type is Overton's where
  Sage has it, and otherwise follows OpenAlex. Each document or work counts once
  in each country of its organisations, and in the chart by year under the type
  of its first organisation.
- Only documents and works that cite my publications are counted: an idea taken
  up without a citation does not appear. Overton takes time to index new
  documents, so the most recent work is undercounted.
- The figures are refreshed from Sage Policy Profiles and OpenAlex whenever the
  site is rebuilt#if pr.as_of.sage != latest [ (those from Sage Policy Profiles date from #month(pr.as_of.sage))]#if pr.as_of.openalex != latest [ (those from OpenAlex date from #month(pr.as_of.openalex))].
