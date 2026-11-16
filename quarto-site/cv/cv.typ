#import "@preview/academicv:1.0.0": *

// Or for local development
// #import "../cv.typ": *

// Import your CV data. The publication entries end with their DOIs, which the
// website's Publications page shows; the CV PDF leaves them out, except for the
// datasets, where the DOI is the only reference.
#let cv-data = {
  let data = yaml("cv.yml")
  data.sections = data.sections.map(section => {
    if section.at("layout", default: none) == "numbered-list" and section.key != "datasets" and type(section.at("entries", default: none)) == array {
      section.entries = section.entries.map(e => if type(e) == str { e.replace(regex("\s+doi:\S+"), "") } else { e })
    }
    section
  })
  data
}

// Validate that required fields exist in each section
#for section in cv-data.sections {
  if "key" not in section {
    panic("Missing 'key' in section: " + str(section))
  }
  if "layout" not in section {
    panic("Missing 'layout' in section with key: " + section.key)
  }
  if "title" not in section and section.key != "personal" {
    warn("Missing 'title' in section with key: " + section.key)
  }
}

// Get settings from YAML file
#let settings = cv-data.settings

// Define default settings if not present in YAML
#let default-settings = (
  font-heading: "Libertinus Serif",
  font-body: "Libertinus Serif",
  fontsize: 10pt, // Must be a string with unit
  spacing-section: 12pt, // Space between sections. Must be a string with unit
  spacing-entry: 0.1em, // Space between entries within a section. Must be a string with unit
  spacing-element: 3pt, // Space between elements within an entry. Must be a string with unit
  spacing-line: 5pt, // Space between lines within an element. Must be a string with unit
  color-hyperlink: rgb(128, 128, 128), // Can be either a colour string or a rgb() value
)

// Merge with defaults for any missing settings
#let settings = if settings != none {
  // First add any missing settings from defaults
  for (k, v) in default-settings {
    if k not in settings {
      settings.insert(k, v)
    }
  }

  // Convert length strings to actual length values
  let settings-length = ("fontsize", "spacing-line", "spacing-section", "spacing-entry", "spacing-element")

  // Page settings separately
  for setting in settings-length {
    settings.at(setting) = convert-string-to-length(settings.at(setting))
  }
  if "page" in settings and "margin" in settings.page {
    settings.page.margin = convert-string-to-length(settings.page.margin)
  }

  // Convert color strings to actual colors
  let settings-color = ("color-hyperlink",)
  for setting in settings-color {
    settings.at(setting) = convert-string-to-color(settings.at(setting))
  }

  settings
} else {
  default-settings
}

#let customrules(doc) = {
  // Get page settings from YAML if available
  set page(
    // https://typst.app/docs/reference/layout/page
    paper: if "page" in settings and "paper" in settings.page {
      settings.page.paper
    } else {
      "a4"
    },
    numbering: if "page" in settings and "numbering" in settings.page {
      settings.page.numbering
    } else {
      "1 / 1"
    },
    number-align: if "page" in settings and "number-align" in settings.page {
      // Convert string align values to actual Typst align values
      let align = settings.page.number-align
      if align == "center" { center } else if align == "left" { left } else if align == "right" { right } else {
        center
      } // Default
    } else {
      center
    },
    margin: if "page" in settings and "margin" in settings.page {
      settings.page.margin
    } else {
      3.5cm
    },
  )

  // Set hyperlink styling
  show link: it => {
    text(
      fill: if "color-hyperlink" in settings {
        settings.color-hyperlink
      } else {
        rgb(0, 0, 255) // Default blue
      },
    )[#it]
  }

  // set list(indent: 1em)
  doc
}


// ---- House style (as on benstanley.eu and in the apps; set November 2026) ----
// The name and section headings light and dark red, each section heading over
// a grey hairline; ink text, a grey timeline rule, grey page numbers; links
// dark red (color-hyperlink in cv.yml). Replaces academicv's semibold black
// headings and full-width black rules without forking the package.
#let house-red = rgb("#8B0000")
#let house-ink = rgb("#141414")
#let house-grey = rgb("#767676")
#let house-rule = rgb("#D5D5D5")

#let house-showrules(doc) = {
  set text(fill: house-ink)
  set grid.vline(stroke: 0.6pt + house-rule)
  // Section headings: light dark red over a grey hairline.
  show heading.where(level: 2): it => block(width: 100%, breakable: false)[
    #v(settings.spacing-section)
    #set align(left)
    #text(font: settings.font-heading, size: 1.45em, weight: "light", fill: house-red, it.body)
    #v(-0.55em) #line(length: 100%, stroke: 0.6pt + house-rule)
  ]
  // The name: large, light, dark red.
  show heading.where(level: 1): it => block(width: 100%)[
    #text(font: settings.font-heading, size: 2.6em, weight: "light", fill: house-red, it.body)
    #v(2pt)
  ]
  doc
}

// Page numbers in grey (the margins and paper stay as set in cv.yml).
#let house-page(doc) = {
  set page(footer: context align(center, text(size: 8pt, fill: house-grey,
    counter(page).display("1 / 1", both: true))))
  doc
}

#let cvinit(doc) = {
  doc = setrules(settings, doc)
  doc = house-showrules(doc)
  doc = customrules(doc)
  doc = house-page(doc)
  doc
}

#show: doc => cvinit(doc)

// Process CV sections dynamically based on the YAML configuration
#if "sections" in cv-data {
  for section in cv-data.sections {
    if section.at("show", default: true) == true {
      if section.key == "personal" {
        // Special case for personal/heading section
        layout-header(cv-data, settings)
      } else {
        // Standard sections
        let layout = section.layout
        let key = section.key
        let title = section.title

        // Get the data for this section
        let section-data = get-section-data(section, cv-data)

        // Create a temporary dictionary with just this section's data
        let temp-data = (
          personal: cv-data.personal, // Keep personal for reference
          (key): section-data.entries, // Add this section's entries
        )

        // Add layout configuration if present
        if "primary-element" in section-data {
          temp-data.insert("primary-element", section-data.primary-element)
        }
        if "secondary-element" in section-data {
          temp-data.insert("secondary-element", section-data.secondary-element)
        }
        if "tertiary-element" in section-data {
          temp-data.insert("tertiary-element", section-data.tertiary-element)
        }

        // Call cvsection with the appropriate data
        cvsection(temp-data, layout: layout, section: key, settings: settings, title: title)
      }
    }
  }
}
