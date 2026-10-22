# HTML for the Media reach page (pages/media-reach.qmd): the Quarto twin of the
# markup that calepin-site/pages/media-reach.typ builds. Every number, colour,
# bar length, hover text and list entry comes from data/media-reach.json
# (written by calepin-site/scripts/update-media-reach.py, which copies it here)
# and the map shapes from data/world-map.json. The generic pieces (el, esc,
# swatch, bars, tables, reach_html) come from R/reach-helpers.R; the lists use
# the .policy-* block in assets/styles/styles.css.

source(file.path(getOption("cvsite_root", Sys.getenv("QUARTO_PROJECT_DIR", ".")), "R", "reach-helpers.R"))
md <- jsonlite::read_json(file.path(reach_root, "data", "media-reach.json"))

media_kpis <- function() {
  t <- md$totals
  items <- list(c(t$articles, "articles"), c(t$outlets, "outlets"),
                c(t$countries, "countries"), c(t$languages, "languages"))
  el("div", class = "reach-kpis", body = vapply(items, function(i) {
    el("div", class = "reach-kpi", body = c(
      el("span", class = "reach-kpi-value", body = reach_num(as.integer(i[1]))),
      el("span", class = "reach-kpi-label", body = i[2])))
  }, ""))
}

media_map <- function() {
  on_map <- Filter(function(c) isTRUE(c$on_map), md$countries)
  by_code <- setNames(on_map, vapply(on_map, `[[`, "", "code"))
  paths <- vapply(world$countries, function(c) {
    row <- if (is.null(c$code)) NULL else by_code[[c$code]]
    el("path", d = c$d, fill = if (is.null(row)) md$no_articles_colour else row$colour,
       body = el("title", body = esc(if (is.null(row)) paste0(c$name, ": none") else row$tip)))
  }, "")
  dots <- unlist(lapply(world$points, function(p) {
    row <- by_code[[p$code]]
    if (is.null(row)) return(NULL)
    el("circle", class = "reach-map-dot", cx = p$x, cy = p$y, r = "4", fill = row$colour,
       body = el("title", body = esc(row$tip)))
  }))
  legend <- c(el("span", class = "reach-legend-title", body = "Articles"),
              vapply(md$bins, function(b) swatch(b$colour, b$label), ""),
              swatch(md$no_articles_colour, "None"))
  el("figure", class = "reach-map", body = c(
    el("svg", viewBox = world$viewBox, class = "reach-map-svg", role = "img",
       `aria-label` = "World map shading each country by the number of articles in outlets there that quote, interview or cite me",
       body = c(paths, dots)),
    el("figcaption", class = "reach-legend", body = legend)))
}

media_years <- function() {
  y <- md$years
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
       `aria-label` = sprintf("Bar chart of articles per year from %s to %s, each bar divided by kind of article",
                              rows[[1]]$year, rows[[length(rows)]]$year),
       body = c(el("div", class = "reach-years-plot", body = c(ticks, el("div", class = "reach-cols", body = cols))),
                el("div", class = "reach-years-axis", body = axis))))
}

media_year_table <- function() {
  names <- vapply(md$years$legend, `[[`, "", "name")
  rows <- lapply(md$years$rows, function(r) {
    by <- setNames(lapply(r$segments, `[[`, "count"), vapply(r$segments, `[[`, "", "name"))
    c(list(r$label, r$count), lapply(names, function(n) if (is.null(by[[n]])) 0L else by[[n]]))
  })
  reach_table("As a table", c("Year", "Articles", names), rows)
}

# Articles, newest first, collapsed: title, then date, outlet and author.
media_items <- function(summary, rows) {
  lis <- vapply(rows, function(it) {
    meta <- paste0(it$meta, if (isTRUE(it$checked)) "" else " · not checked")
    el("li", body = c(
      el("span", class = "policy-item-title", body = el("a", href = it$url, body = esc(it$title))),
      el("span", class = "policy-item-meta", body = esc(meta))))
  }, "")
  el("details", class = "reach-table policy-details", body = c(
    el("summary", body = esc(summary)),
    el("ul", class = "policy-items", body = paste0(lis, collapse = ""))))
}

media_ideas <- function() {
  for (g in md$ideas) {
    cat("### ", g$idea, "\n\n", sep = "")
    if (length(g$items) == 0) {
      reach_html("<p>No articles found yet.</p>")
    } else {
      reach_html(media_items(g$summary, g$items))
    }
  }
}

media_commentary <- function() {
  reach_html(paste0(vapply(md$commentary, function(g) media_items(g$summary, g$items), ""), collapse = ""))
}

media_research <- function() {
  r <- md$research
  reach_html(if (length(r$items) == 0) "<p>No articles found yet.</p>" else media_items(r$summary, r$items))
}
