# HTML for the Policy reach page (pages/policy-reach.qmd): the Quarto twin of
# the markup that calepin-site/pages/policy-reach.typ builds. Every number,
# colour, bar length, hover text and list entry comes from
# data/policy-reach.json (written by calepin-site/scripts/update-policy-reach.py,
# which copies it here) and the map shapes from data/world-map.json. The
# generic pieces (el, esc, swatch, bars, tables, reach_html) come from
# R/reach-helpers.R, as the page reuses the Citation reach page's .reach-*
# styles; the lists use the .policy-* block in assets/styles/styles.css.

source(file.path(getOption("cvsite_root", Sys.getenv("QUARTO_PROJECT_DIR", ".")), "R", "reach-helpers.R"))
pr <- jsonlite::read_json(file.path(reach_root, "data", "policy-reach.json"))

policy_latest <- function() max(unlist(pr$as_of))

policy_kpis <- function() {
  t <- pr$totals
  items <- list(c(t$documents, "policy documents"), c(t$works, "research works"),
                c(t$organisations, "organisations"), c(t$countries, "countries"))
  el("div", class = "reach-kpis", body = vapply(items, function(i) {
    el("div", class = "reach-kpi", body = c(
      el("span", class = "reach-kpi-value", body = reach_num(as.integer(i[1]))),
      el("span", class = "reach-kpi-label", body = i[2])))
  }, ""))
}

policy_map <- function() {
  on_map <- Filter(function(c) isTRUE(c$on_map), pr$countries)
  by_code <- setNames(on_map, vapply(on_map, `[[`, "", "code"))
  paths <- vapply(world$countries, function(c) {
    row <- if (is.null(c$code)) NULL else by_code[[c$code]]
    el("path", d = c$d, fill = if (is.null(row)) pr$no_records_colour else row$colour,
       body = el("title", body = esc(if (is.null(row)) paste0(c$name, ": none") else row$tip)))
  }, "")
  dots <- unlist(lapply(world$points, function(p) {
    row <- by_code[[p$code]]
    if (is.null(row)) return(NULL)
    el("circle", class = "reach-map-dot", cx = p$x, cy = p$y, r = "4", fill = row$colour,
       body = el("title", body = esc(row$tip)))
  }))
  legend <- c(el("span", class = "reach-legend-title", body = "Documents and works"),
              vapply(pr$bins, function(b) swatch(b$colour, b$label), ""),
              swatch(pr$no_records_colour, "None"))
  el("figure", class = "reach-map", body = c(
    el("svg", viewBox = world$viewBox, class = "reach-map-svg", role = "img",
       `aria-label` = paste("World map shading each country by the number of policy documents and",
                            "research works from organisations there that cite my work"),
       body = c(paths, dots)),
    el("figcaption", class = "reach-legend", body = legend)))
}

policy_type_legend <- function() {
  el("div", class = "reach-legend", body = vapply(pr$types, function(r) swatch(r$colour, r$name), ""))
}

policy_years <- function() {
  y <- pr$years
  rows <- y$rows
  ticks <- vapply(y$ticks, function(tk) {
    el("div", class = "reach-tick", style = paste0("bottom: ", tk$pos * 100, "%"), body = el("span", body = tk$label))
  }, "")
  cols <- vapply(rows, function(r) {
    segs <- vapply(r$segments, function(s) {
      el("span", class = "reach-seg", title = s$tip,
         style = paste0("flex-grow: ", s$count, "; background: ", s$colour))
    }, "")
    el("div", class = "reach-col", title = r$tip,
       body = el("div", class = "reach-stack", style = paste0("height: ", r$h * 100, "%"), body = paste0(segs, collapse = "")))
  }, "")
  axis <- vapply(rows, function(r) {
    el("span", class = if (isTRUE(r$minor)) "reach-year minor" else "reach-year", body = r$year)
  }, "")
  c(el("div", class = "reach-legend", body = vapply(y$legend, function(l) swatch(l$colour, l$name), "")),
    el("div", class = "reach-years", role = "img",
       `aria-label` = sprintf(paste("Bar chart of policy documents and research works per year from %s to %s,",
                                    "each bar divided by type of organisation"),
                              rows[[1]]$year, rows[[length(rows)]]$year),
       body = c(el("div", class = "reach-years-plot", body = c(ticks, el("div", class = "reach-cols", body = cols))),
                el("div", class = "reach-years-axis", body = axis))))
}

policy_year_table <- function() {
  names <- vapply(pr$years$legend, `[[`, "", "name")
  rows <- lapply(pr$years$rows, function(r) {
    by <- setNames(lapply(r$segments, `[[`, "count"), vapply(r$segments, `[[`, "", "name"))
    c(list(r$label, r$count), lapply(names, function(n) if (is.null(by[[n]])) 0L else by[[n]]))
  })
  reach_table("As a table", c("Year", "Documents and works", names), rows)
}

# The documents and works citing one publication, newest first, collapsed.
# summary is HTML (it may hold an <em> title).
policy_items <- function(summary, rows) {
  lis <- vapply(rows, function(it) {
    meta <- Filter(nzchar, c(it$when, it$orgs, it$kind))
    el("li", body = c(
      el("span", class = "policy-item-title", body = el("a", href = it$url, body = esc(it$title))),
      el("span", class = "policy-item-meta", body = esc(paste(meta, collapse = " · ")))))
  }, "")
  el("details", class = "reach-table policy-details", body = c(
    el("summary", body = summary),
    el("ul", class = "policy-items", body = paste0(lis, collapse = ""))))
}

# Each key idea: a heading, the sentence naming its publication, and its list.
policy_ideas <- function() {
  for (g in pr$ideas) {
    cat("### ", g$idea, "\n\n", sep = "")
    work <- sprintf("Set out in <em>%s</em> (%s)", esc(g$title), g$year)
    if (length(g$items) == 0) {
      reach_html(paste0("<p>", work, ". No policy documents or research works from policy organisations cite it yet.</p>"))
    } else {
      reach_html(paste0("<p>", work, ", which is cited in ", esc(g$summary), ".</p>",
                        policy_items("The documents and works", g$items)))
    }
  }
}

policy_others <- function() {
  reach_html(paste0(vapply(pr$other_publications, function(g) {
    policy_items(sprintf("<em>%s</em> (%s): %s", esc(g$title), g$year, esc(g$summary)), g$items)
  }, ""), collapse = ""))
}
