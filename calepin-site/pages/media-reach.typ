#set document(title: [Media reach])
#metadata((title: "Media reach")) <website-metadata>

// Not in the navigation bar: the Media line of the Citations list on the
// Research outputs page (pages/publications.typ) links here. Every number, colour,
// bar length, hover text and list entry comes from data/media-reach.json
// (scripts/update-media-reach.py, run by build.sh) and the map shapes from
// data/world-map.json; this page only lays them out. It reuses the reach
// pages' .reach-* and .policy-* styles in themes/site-theme/css/90_site.css.
// The Quarto backup builds the same markup in quarto-site/R/media-helpers.R.

#let md = json("/data/media-reach.json")
#let world = json("/data/world-map.json")
#let t = md.totals
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
// Articles, newest first, collapsed: title, then date, outlet and author.
#let items(summary, rows) = el("details", class: "reach-table policy-details")[
  #el("summary")[#summary]
  #el("ul", class: "policy-items")[#for it in rows {
    el("li", {
      el("span", class: "policy-item-title")[#link(it.url)[#it.title]]
      el("span", class: "policy-item-meta")[#it.meta#if not it.checked [ · not checked]]
    })
  }]
]

#el("p", class: "reach-back")[#link("publications.html")[← Research outputs]]

= Media reach

_Updated: #month(md.as_of)_

Where my work and expertise appear in the news: articles that quote or
interview me, that take up my ideas, or that report on my research. They were
found by searching Google News, in thirteen languages, from 2008 onwards, and
are kept up to date with Google Alerts.

#el("div", class: "reach-kpis", {
  for (n, label) in ((t.articles, "articles"), (t.outlets, "outlets"), (t.countries, "countries"), (t.languages, "languages")) {
    el("div", class: "reach-kpi", {
      el("span", class: "reach-kpi-value")[#num(n)]
      el("span", class: "reach-kpi-label")[#label]
    })
  }
})

= Countries

Where the outlets are based. Hover over a country to see its numbers.
Pan-European and international outlets have no country of their own: they are
in the chart below the map, but not on it.

#let by-code = md.countries.filter(c => c.on_map).fold((:), (acc, c) => { acc.insert(c.code, c); acc })
#el("figure", class: "reach-map")[
  #el("svg", viewBox: world.viewBox, class: "reach-map-svg", role: "img",
      aria-label: "World map shading each country by the number of articles in outlets there that quote, interview or cite me")[
    #for c in world.countries {
      let row = if c.code == none { none } else { by-code.at(c.code, default: none) }
      el("path", d: c.d, fill: if row == none { md.no_articles_colour } else { row.colour })[
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
    el("span", class: "reach-legend-title")[Articles]
    for b in md.bins { swatch(b.colour, b.label) }
    swatch(md.no_articles_colour, "None")
  })
]

#bars(md.countries, "name", "articles")
#data-table([As a table], ("Country", "Articles", "Outlets"),
  md.countries.map(c => (c.name, c.articles, c.outlets)))

= Outlets

The #md.top_outlets outlets with the most articles:

#bars(md.outlets.slice(0, calc.min(md.top_outlets, md.outlets.len())), "name", "articles")
#data-table([All #num(t.outlets) outlets], ("Outlet", "Country", "Articles"),
  md.outlets.map(o => (o.name, o.country, o.articles)))

= Languages

#bars(md.languages, "name", "articles")

= By year

Articles each year, divided into interviews and commentary, ideas, and research
in the news. Hover over a bar to see its numbers.

#let years = md.years
#el("div", class: "reach-legend")[#for l in years.legend { swatch(l.colour, l.name) }]
#el("div", class: "reach-years", role: "img",
    aria-label: "Bar chart of articles per year from " + str(years.rows.first().year) + " to " + str(years.rows.last().year) + ", each bar divided by kind of article")[
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
#data-table([As a table], ("Year", "Articles", ..years.legend.map(l => l.name)),
  years.rows.map(y => (y.label, y.count, ..years.legend.map(l => {
    let s = y.segments.find(s => s.name == l.name)
    if s == none { 0 } else { s.count }
  }))))

= Key ideas

#for g in md.ideas {
  heading(level: 2, g.idea)
  if g.items.len() == 0 [
    No articles found yet.
  ] else [
    #items([#g.summary], g.items)
  ]
}

= Interviews and commentary

Articles that quote or interview me, by year.

#for g in md.commentary {
  items([#g.summary], g.items)
}

= Research in the news

Articles about my research: _Good Change_ and other work with Stanley Bill, and
Pooling the Poles.

#if md.research.items.len() == 0 [No articles found yet.] else [#items([#md.research.summary], md.research.items)]

= About these figures

- The articles were found with Google News searches for my name, in the forms
  the press uses (Polish case endings and Cyrillic included), together with a
  word that ties it to my field; for the key ideas' phrases; and for my work
  with Stanley Bill and Pooling the Poles. The searches are in thirteen
  languages, each in the Google News edition of its language.
- Each article's page is read to check that it names me, or uses the idea's
  phrase, before it is counted. #num(t.unchecked) articles whose pages could
  not be read (paywalls, for instance) are counted unchecked. Articles I wrote
  myself are left out: my columns are on the #link("media.html")[Media] page.
- An outlet's country is where it is based; its language is the article's.
  An article matching several searches counts once, under an idea first, then
  research, then interviews and commentary.
- Google News indexes online news, so radio, television and print-only
  articles are mostly missing, as is much of the coverage before about 2015.
- The figures are refreshed whenever the site is rebuilt, from Google News and
  from Google Alerts set up with the same searches.
