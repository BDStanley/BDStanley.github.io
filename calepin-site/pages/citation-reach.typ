#set document(title: [Citation reach])
#metadata((title: "Citation reach")) <website-metadata>

// Not in the navigation bar: the OpenAlex line of the Citations list on the
// Research page (pages/publications.typ) links here. Every number, colour, bar length and hover
// text comes from data/reach.json (scripts/update-reach.py, run by build.sh)
// and the map shapes from data/world-map.json (scripts/make-world-map.py); this
// page only lays them out. Styles: .reach-* in themes/site-theme/css/90_site.css.
// The Quarto backup builds the same markup in quarto-site/R/reach-helpers.R.

#let reach = json("/data/reach.json")
#let world = json("/data/world-map.json")
#let t = reach.totals
#let month(ym) = datetime(year: int(ym.slice(0, 4)), month: int(ym.slice(5, 7)), day: 1).display("[month repr:long] [year]")
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

#el("p", class: "reach-back")[#link("publications.html")[← Research]]

= Citation reach

_Updated: #month(reach.as_of)_

Where, and in which disciplines, my work is cited. The figures come from
#link("https://openalex.org/authors/A5015716464")[OpenAlex], an open index of
scholarly works, and cover the works it records as citing my publications.

#el("div", class: "reach-kpis", {
  for (n, label) in ((t.citations, "citations"), (t.citing_works, "citing works"), (t.scholars, "citing scholars"), (t.countries, "countries")) {
    el("div", class: "reach-kpi", {
      el("span", class: "reach-kpi-value")[#num(n)]
      el("span", class: "reach-kpi-label")[#label]
    })
  }
})

= Countries

Where the scholars citing my work are based, by the institutions they list on
the citing works. Hover over a country to see its numbers.

#let by-code = reach.countries.fold((:), (acc, c) => { acc.insert(c.code, c); acc })
#el("figure", class: "reach-map")[
  #el("svg", viewBox: world.viewBox, class: "reach-map-svg", role: "img",
      aria-label: "World map shading each country by the number of scholars there who cite my work")[
    #for c in world.countries {
      let row = if c.code == none { none } else { by-code.at(c.code, default: none) }
      el("path", d: c.d, fill: if row == none { reach.no_scholars_colour } else { row.colour })[
        #el("title")[#if row == none [#c.name: no citing scholars] else [#row.tip]]
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
    el("span", class: "reach-legend-title")[Citing scholars]
    for b in reach.bins { swatch(b.colour, b.label) }
    swatch(reach.no_scholars_colour, "None")
  })
]

The #reach.top_countries countries with the most citing scholars:

#bars(reach.countries.slice(0, calc.min(reach.top_countries, reach.countries.len())), "name", "scholars")
#data-table([All #t.countries countries], ("Country", "Citing scholars", "Citing works"),
  reach.countries.map(c => (c.name, c.scholars, c.works)))

= Disciplines

Citations by the discipline of the citing work: the field of its main topic in
OpenAlex's classification of research.

#bars(reach.fields, "name", "citations")
#data-table([As a table], ("Discipline", "Citations", "Share"),
  reach.fields.map(f => (f.name, f.citations, str(f.share) + "%")))

= Subdisciplines

The #reach.top_subfields subdisciplines (OpenAlex's subfields) with the most
citations. The colours are those of the chart by year below, where the smaller
subdisciplines are grouped as Other.

#bars(reach.subfields.slice(0, calc.min(reach.top_subfields, reach.subfields.len())), "name", "citations")
#data-table([All #reach.subfields.len() subdisciplines], ("Subdiscipline", "Discipline", "Citations"),
  reach.subfields.map(s => (s.name, s.field, s.citations)))

= Citations by year

Citations each year, by the year the citing work was published, divided by
subdiscipline. Hover over a bar to see its numbers.

#let years = reach.years
#el("div", class: "reach-legend")[#for l in reach.legend { swatch(l.colour, l.name) }]
#el("div", class: "reach-years", role: "img",
    aria-label: "Bar chart of citations per year from " + str(years.rows.first().year) + " to " + str(years.rows.last().year) + ", each bar divided by subdiscipline")[
  #el("div", class: "reach-years-plot")[
    #for tk in years.ticks {
      el("div", class: "reach-tick", style: "bottom: " + str(tk.pos * 100) + "%")[#el("span")[#tk.label]]
    }
    #el("div", class: "reach-cols")[
      #for y in years.rows {
        el("div", class: "reach-col", title: y.tip)[
          #el("div", class: "reach-stack", style: "height: " + str(y.h * 100) + "%")[
            #for s in y.segments {
              el("span", class: "reach-seg", title: s.tip, style: "flex-grow: " + str(s.citations) + "; background: " + s.colour)[]
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
#data-table([As a table], ("Year", "Citations", ..reach.legend.map(l => l.name)),
  years.rows.map(y => (y.label, y.citations, ..reach.legend.map(l => {
    let s = y.segments.find(s => s.name == l.name)
    if s == none { 0 } else { s.citations }
  }))))

= About these figures

- OpenAlex links #t.my_works works to my ORCID iD. A citation is a work that
  cites one of them; a work citing two of them counts twice, as in OpenAlex's own
  totals. #t.self_citing_works of the #num(t.citing_works) citing works are my own.
- Disciplines and subdisciplines are the fields and subfields of each citing
  work's main topic in OpenAlex's classification (domains, fields, subfields and
  topics). #num(t.citations_without_topic) citations come from works OpenAlex has
  not classified: they are left out of the discipline charts and shown as
  Unclassified in the chart by year.
- Citing scholars are the authors of the citing works, other than me, each counted
  once in each country of the institutions they list. #num(t.works_without_country)
  of the citing works list no institution, so their authors have no country.
  Countries too small to see on the map are shown as dots.
- The years run from #t.first_year, when my first work appeared; the
  #t.citations_before_first_year citations OpenAlex dates earlier are dating
  errors and are left out. The latest year is counted to #month(reach.as_of).
- The figures are refreshed from OpenAlex whenever the site is rebuilt.
