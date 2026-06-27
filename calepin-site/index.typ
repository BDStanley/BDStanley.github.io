#set document(title: [Ben Stanley])

// Home page: portrait beside the bio, bio top-aligned with the photo. The
// .hero* classes are defined in themes/site-theme/css/90_site.css. Built HTML-only (pdf = false),
// so html.elem is safe here.

#html.elem("section", attrs: (class: "hero"))[
  #html.elem("div", attrs: (class: "hero-portrait"))[
    #html.elem("img", attrs: (
      src: "assets/profile.jpg",
      alt: "Ben Stanley",
    ))
  ]
  #html.elem("div", attrs: (class: "hero-bio"))[
    I am Associate Professor in the Centre for the Study of Democracy at the SWPS
    University of Social Sciences and Humanities. A political scientist and
    sociologist, I received a PhD in Government from the University of Essex and
    have previously worked at the Institute for Public Affairs in Bratislava, the
    Cardinal Stefan Wyszyński University in Warsaw, and the University of Sussex
    in Brighton, United Kingdom. I am currently conducting research into the
    politics of populism, illiberalism and authoritarianism in Central and
    Eastern Europe.
  ]
]
