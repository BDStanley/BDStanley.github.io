#set document(title: [Good Change])
#metadata((title: "Good Change")) <website-metadata>

// Set like the Polish political analyses: small uppercase labels, a large light
// dark-red title. On a computer screen the page is wider than the reading
// column: the cover and the publication details beside the title and
// description, then the reviews in two columns. Below 56rem it all stacks.
// Styles: .gc-* in themes/site-theme/css/90_site.css. The Quarto backup builds
// the same structure in quarto-site/pages/good-change.qmd.

#let el(tag, ..attrs, body) = html.elem(tag, attrs: attrs.named(), body)
#let fact(term, value) = el("div")[
  #el("dt")[#term]
  #el("dd")[#value]
]
#let review(quote, who, where) = el("figure", class: "gc-review")[
  #el("blockquote")[#quote]
  #el("figcaption")[
    #el("span", class: "gc-who")[#who]
    #el("span", class: "gc-where")[#where]
  ]
]

#el("div", class: "gc-book")[
  #el("div", class: "gc-top")[
    #el("div", class: "gc-side")[
      #image("/assets/good-change.png", alt: "Cover of Good Change: The Rise and Fall of Poland's Illiberal Revolution, by Stanley Bill and Ben Stanley")

      #el("div", class: "eyebrow gc-facts-label")[Publication details]
      #el("dl", class: "gc-facts")[
        #fact[Published][September 2025]
        #fact[Length][344 pages · 6×9 · 14 figures]
        #fact[Paper][\$35.00 (£27.99) · SDT 9781503643680]
        #fact[Cloth][\$140.00 (£110.00) · SDT 9781503634565]
        #fact[eBook][9781503643697]
        #fact[Subject][International Affairs]
      ]
    ]
    #el("div", class: "gc-main")[
      #el("div", class: "eyebrow")[Book · Stanford University Press]
      #el("h1", class: "gc-title")[Good Change]
      #el("p", class: "gc-subtitle")[The Rise and Fall of Poland's Illiberal Revolution]
      #el("p", class: "gc-authors")[Stanley Bill and Ben Stanley]

      Few countries serve as a more useful case study for understanding the global
      tension between liberal and illiberal conceptions of democracy than Poland.
      Under the populist Law and Justice (PiS)-led government, a large part of the
      Polish electorate welcomed the party's “Good Change”—as it described its
      program—despite accusations of democratic backsliding. PiS offered voters
      neglected by previous governments a combination of economic redistributionism
      and cultural traditionalism, supplemented with narratives of bolstering
      Poland's national prestige and sovereignty. Yet, after eight years of success,
      it was defeated in the October 2023 elections by a “pro-democratic” coalition.
      The history of PiS shows both the strengths and weaknesses of democratic
      illiberalism as a challenge to liberal democracy.

      Bill and Stanley analyze the course and causes of the party's successes and
      failures. The authors deftly outline PiS's assault on democratic institutions,
      its paradigm-changing redistributive programs, cultural backlash agenda,
      politics of history, and the reasons for its fall from power. Poland's
      democracy has proven resilient to the specter of autocratization, but its
      future development under a new government raises fresh questions. This
      essential book considers what the rise and fall of Poland's illiberal
      government reveals about the future of liberal democracy and its ongoing
      transformations in the 21st century.

      #el("div", class: "eyebrow gc-authors-label")[The authors]

      *Stanley Bill* is Professor of Polish Studies, University of Cambridge. He is
      the Chair of the Cambridge Committee for Russian and East European Studies
      (CamCREES). *Ben Stanley* is Associate Professor at the Centre for the Study
      of Democracy, SWPS University of Social Sciences and Humanities.
    ]
  ]

  #el("section", class: "gc-reviews")[
    #el("div", class: "eyebrow")[Reviews]
    #el("div", class: "gc-review-grid")[
      #review[
        “This nuanced and data-driven analysis shows how support for the right was
        undermined by widespread opposition to conservative policies on abortion and
        other issues, along with Law and Justice's failure to deliver low-inflation
        growth, benefits for farmers, and smooth relations with the EU. … This is a
        must-read for all who follow central European politics.”
      ][Andrew Moravcsik][Foreign Affairs]

      #review[
        “By refusing to write off the dominant Polish political force of the past
        two decades as an aberration, Stanley Bill and Ben Stanley's _Good Change_
        helps us to discern how illiberals in countries other than the US have
        responded to genuine demands from within the liberal mould, changing
        liberalism in the process.”
      ][Piotr H. Kosicki][Times Literary Supplement]

      #review[
        “A book as topic-specific and detailed about a particular case study as this
        one is, and as far-reaching and theoretically complex, is usually a rare
        event.”
      ][Ray Taras][Slavic Review]

      #review[
        “_Good Change_ is an exceptional work in the literature on populism and
        post-communist transformation. Its strength lies in its ability to combine
        different levels of analysis – empirical, symbolic and historical – into a
        coherent whole.”
      ][Mikołaj Cześnik][Studia Socjologiczne]

      #review[
        “_Good Change_ is admirably comprehensive, compelling, and well-researched,
        amply backing up its claims with numbers and charts.”
      ][Ana Oppenheim][Renewal]

      #review[
        “_Good Change_ provides an insightful analysis of recent political
        developments in Poland, with particular attention to the emergence and
        subsequent decline of illiberalism and democratic backsliding. Authors
        Stanley Bill and Ben Stanley adopt a nuanced approach, offering a
        comprehensive examination of the political, institutional, economic, and
        societal dynamics underpinning these processes.”
      ][Anna Gwiazda][Political Science Quarterly]
    ]
  ]
]
