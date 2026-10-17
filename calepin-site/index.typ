#set document(title: [Ben Stanley])

// Home page: portrait floated left, the bio wrapping round and under it. The
// .hero* classes are defined in themes/site-theme/css/90_site.css. Built HTML-only (pdf = false),
// so html.elem is safe here. The bio is the first-person version of /bio.md at
// the repository root; change them together.

#html.elem("section", attrs: (class: "hero"))[
  #html.elem("div", attrs: (class: "hero-portrait"))[
    #html.elem("img", attrs: (
      src: "assets/profile.jpg",
      alt: "Ben Stanley",
    ))
  ]
  #html.elem("div", attrs: (class: "hero-bio"))[
    I am Associate Professor of Social Science at the Centre for the Study of
    Democracy, SWPS University, Warsaw. A political scientist and sociologist, I
    study populism, illiberalism, democratic backsliding and electoral behaviour
    in Central and Eastern Europe, especially Poland.

    My most recent book,
    #link("pages/good-change.html")[_Good Change: The Rise and Fall of Poland's Illiberal Revolution_]
    (Stanford University Press, 2025), written with Stanley Bill, explains how
    the Law and Justice government won and lost power, and what its eight years
    reveal about the illiberal challenge to liberal democracy. I am also a
    co-author of _External Voting_ (Palgrave Macmillan, 2023), on the electoral
    participation of Central European emigrants. My articles have appeared in
    _Comparative Political Studies_, _Perspectives on Politics_, the _Journal of
    Democracy_, _Party Politics_ and _East European Politics_, and my 2008
    article "The Thin Ideology of Populism" is widely cited.

    My current research examines how firmly citizens hold to liberal democracy
    when partisan polarisation runs high, the rise of postliberal attitudes, and
    the difficulties of rebuilding democracy after illiberal rule. I led the
    National Science Centre project "Partisan Bias or a Climate of
    Indifference?" (2021–2026) and was a co-investigator on AUTHLIB, a Horizon
    Europe project on neo-authoritarianism in Europe. I now lead the work package
    on the economic dimensions of enlargement in CitiSense, a Horizon Europe
    project on EU enlargement (2026–2029).

    I hold a PhD in Government from the University of Essex and a habilitation
    in sociology from the Polish Academy of Sciences. I previously worked at the
    Institute for Public Affairs in Bratislava, Cardinal Stefan Wyszyński
    University in Warsaw and the University of Sussex, where I was a Marie Curie
    Fellow. I am deputy editor of the _Polish Sociological Review_ and a regular
    contributor to _Kultura Liberalna_.
  ]
]
