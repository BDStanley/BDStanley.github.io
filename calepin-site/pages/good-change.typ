#set document(title: [Good Change])
#metadata((title: "Good Change")) <website-metadata>

// No on-page title heading; nav label / tab title come from the metadata above.
// On a computer screen the page is wider than the reading column: the cover and
// the publication details beside the description, then the reviews in two
// columns. Below 56rem it all stacks. Styles: .gc-* in
// themes/site-theme/css/90_site.css. The Quarto backup builds the same
// structure in quarto-site/pages/good-change.qmd.

#let el(tag, ..attrs, body) = html.elem(tag, attrs: attrs.named(), body)
#let review(quote, by) = el("div", class: "gc-review")[
  #quote

  —#by
]

#el("div", class: "gc-book")[
  #el("div", class: "gc-top")[
    #el("div", class: "gc-side")[
      #image("/assets/good-change.png", alt: "Cover of Good Change: The Rise and Fall of Poland's Illiberal Revolution, by Stanley Bill and Ben Stanley")

      #el("div", class: "gc-details")[
        September 2025 \
        344 pages | 6x9 \
        14 figures \
        Paper \
        \$35.00 (£27.99) SDT 9781503643680 \
        Cloth \
        \$140.00 (£110.00) SDT 9781503634565 \
        eBook 9781503643697 \
        International Affairs
      ]
    ]
    #el("div", class: "gc-main")[
      _The Rise and Fall of Poland's Illiberal Revolution_

      Stanley Bill and Ben Stanley

      Stanford University Press

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

      *Stanley Bill* is Professor of Polish Studies, University of Cambridge. He is
      the Chair of the Cambridge Committee for Russian and East European Studies
      (CamCREES). *Ben Stanley* is Associate Professor at the Centre for the Study
      of Democracy, SWPS University of Social Sciences and Humanities.
    ]
  ]

  = Reviews

  #el("div", class: "gc-reviews")[
    #review[
      “This nuanced and data-driven analysis shows how support for the right was
      undermined by widespread opposition to conservative policies on abortion and
      other issues, along with Law and Justice's failure to deliver low-inflation
      growth, benefits for farmers, and smooth relations with the EU. … This is a
      must-read for all who follow central European politics.”
    ][Andrew Moravcsik, _Foreign Affairs_]

    #review[
      “By refusing to write off the dominant Polish political force of the past
      two decades as an aberration, Stanley Bill and Ben Stanley's _Good Change_
      helps us to discern how illiberals in countries other than the US have
      responded to genuine demands from within the liberal mould, changing
      liberalism in the process.”
    ][Piotr H. Kosicki, _Times Literary Supplement_]

    #review[
      “A book as topic-specific and detailed about a particular case study as this
      one is, and as far-reaching and theoretically complex, is usually a rare
      event.”
    ][Ray Taras, _Slavic Review_]

    #review[
      “_Good Change_ is an exceptional work in the literature on populism and
      post-communist transformation. Its strength lies in its ability to combine
      different levels of analysis – empirical, symbolic and historical – into a
      coherent whole.”
    ][Mikołaj Cześnik, _Studia Socjologiczne_]

    #review[
      “_Good Change_ is admirably comprehensive, compelling, and well-researched,
      amply backing up its claims with numbers and charts.”
    ][Ana Oppenheim, _Renewal_]
  ]
]
