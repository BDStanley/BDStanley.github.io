#set document(title: [Pooling the Poles])
#metadata((
  title: "Pooling the Poles",
  description: "Interactive Polish polling tracker",
)) <website-metadata>

// No on-page title (#title omitted) so the page shows only the Shiny app.
// The navbar label still comes from <website-metadata> / calepin.toml, and the
// browser-tab title from #set document(...). Built HTML-only (pdf = false), so
// the raw <iframe> renders directly; .embed-frame sizing lives in themes/site-theme/css/90_site.css.
#html.elem("iframe", attrs: (
  src: "https://ben-stanley.shinyapps.io/polish-polls/",
  class: "embed-frame",
  title: "Pooling the Poles — interactive Polish polling tracker",
  loading: "lazy",
))[]
