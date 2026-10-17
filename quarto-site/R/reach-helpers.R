# HTML for the Citation reach page (pages/citation-reach.qmd): the Quarto twin
# of the markup that calepin-site/pages/citation-reach.typ builds. Every number,
# colour, bar length and hover text comes from data/reach.json (written by
# calepin-site/scripts/update-reach.py, which copies it here) and the map shapes
# from data/world-map.json (calepin-site/scripts/make-world-map.py); these
# functions only lay them out. Each returns a string of HTML; reach_html() wraps
# one in a raw-HTML block so Pandoc leaves it as it is. Styles: the .reach-*
# block in assets/styles/styles.css.

reach_root <- getOption("cvsite_root", Sys.getenv("QUARTO_PROJECT_DIR", "."))
reach <- jsonlite::read_json(file.path(reach_root, "data", "reach.json"))
world <- jsonlite::read_json(file.path(reach_root, "data", "world-map.json"))

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# 4044 -> "4,044"
reach_num <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)

# month.name, not format(), so the month stays English in any locale
reach_month <- function(ym) paste(month.name[as.integer(substr(ym, 6, 7))], substr(ym, 1, 4))

# <name attr="value" ...>body</name>, with the attribute values escaped.
el <- function(name, ..., body = "") {
  attrs <- list(...)
  a <- if (length(attrs)) {
    paste0(" ", names(attrs), "=\"", vapply(attrs, function(v) esc(as.character(v)), ""), "\"", collapse = "")
  } else ""
  paste0("<", name, a, ">", paste0(body, collapse = ""), "</", name, ">")
}

reach_html <- function(x) cat("\n```{=html}\n", x, "\n```\n\n", sep = "")

swatch <- function(colour, name) {
  el("span", class = "reach-legend-item",
     body = c(el("span", class = "reach-swatch", style = paste0("background: ", colour)), esc(name)))
}

reach_kpis <- function() {
  t <- reach$totals
  items <- list(c(t$citations, "citations"), c(t$citing_works, "citing works"),
                c(t$scholars, "citing scholars"), c(t$countries, "countries"))
  el("div", class = "reach-kpis", body = vapply(items, function(i) {
    el("div", class = "reach-kpi", body = c(
      el("span", class = "reach-kpi-value", body = reach_num(as.integer(i[1]))),
      el("span", class = "reach-kpi-label", body = i[2])))
  }, ""))
}

reach_map <- function() {
  by_code <- setNames(reach$countries, vapply(reach$countries, `[[`, "", "code"))
  paths <- vapply(world$countries, function(c) {
    row <- if (is.null(c$code)) NULL else by_code[[c$code]]
    el("path", d = c$d, fill = if (is.null(row)) reach$no_scholars_colour else row$colour,
       body = el("title", body = esc(if (is.null(row)) paste0(c$name, ": no citing scholars") else row$tip)))
  }, "")
  dots <- unlist(lapply(world$points, function(p) {
    row <- by_code[[p$code]]
    if (is.null(row)) return(NULL)
    el("circle", class = "reach-map-dot", cx = p$x, cy = p$y, r = "4", fill = row$colour,
       body = el("title", body = esc(row$tip)))
  }))
  legend <- c(el("span", class = "reach-legend-title", body = "Citing scholars"),
              vapply(reach$bins, function(b) swatch(b$colour, b$label), ""),
              swatch(reach$no_scholars_colour, "None"))
  el("figure", class = "reach-map", body = c(
    el("svg", viewBox = world$viewBox, class = "reach-map-svg", role = "img",
       `aria-label` = "World map shading each country by the number of scholars there who cite my work",
       body = c(paths, dots)),
    el("figcaption", class = "reach-legend", body = legend)))
}

# One horizontal bar per row: label, then a bar of length w (a share of the
# longest) in the row's colour, with the value at its tip.
reach_bars <- function(rows, label, value) {
  el("div", class = "reach-bars", body = vapply(rows, function(r) {
    el("div", class = "reach-bar-row", title = r$tip, body = c(
      el("span", class = "reach-bar-label", body = esc(r[[label]])),
      el("span", class = "reach-bar-area", body = c(
        el("span", class = "reach-bar", style = paste0("--w: ", r$w, "; background: ", r$colour)),
        el("span", class = "reach-bar-value", body = reach_num(r[[value]]))))))
  }, ""))
}

# A collapsed table under each chart, so every value can be read without hovering.
reach_table <- function(summary, head, rows) {
  cell <- function(tagname, v, i) {
    numeric <- i > 1 && is.numeric(v)
    el(tagname, class = if (numeric || (tagname == "th" && i > 1)) "num" else "",
       body = if (is.numeric(v)) reach_num(v) else esc(v))
  }
  th <- vapply(seq_along(head), function(i) cell("th", head[[i]], i), "")
  trs <- vapply(rows, function(r) el("tr", body = vapply(seq_along(r), function(i) cell("td", r[[i]], i), "")), "")
  el("details", class = "reach-table", body = c(
    el("summary", body = esc(summary)),
    el("div", class = "reach-table-scroll", body = el("table", body = c(
      el("thead", body = el("tr", body = th)), el("tbody", body = trs))))))
}

reach_years <- function() {
  y <- reach$years
  rows <- y$rows
  ticks <- vapply(y$ticks, function(tk) {
    el("div", class = "reach-tick", style = paste0("bottom: ", tk$pos * 100, "%"), body = el("span", body = tk$label))
  }, "")
  cols <- vapply(rows, function(r) {
    segs <- vapply(r$segments, function(s) {
      el("span", class = "reach-seg", title = s$tip,
         style = paste0("flex-grow: ", s$citations, "; background: ", s$colour))
    }, "")
    el("div", class = "reach-col", title = r$tip,
       body = el("div", class = "reach-stack", style = paste0("height: ", r$h * 100, "%"), body = segs))
  }, "")
  axis <- vapply(rows, function(r) {
    el("span", class = if (isTRUE(r$minor)) "reach-year minor" else "reach-year", body = r$year)
  }, "")
  c(el("div", class = "reach-legend", body = vapply(reach$legend, function(l) swatch(l$colour, l$name), "")),
    el("div", class = "reach-years", role = "img",
       `aria-label` = sprintf("Bar chart of citations per year from %s to %s, each bar divided by subdiscipline",
                              rows[[1]]$year, rows[[length(rows)]]$year),
       body = c(el("div", class = "reach-years-plot", body = c(ticks, el("div", class = "reach-cols", body = cols))),
                el("div", class = "reach-years-axis", body = axis))))
}

# The table rows each chart's table shows, as on the Calepin page.
reach_year_table <- function() {
  names <- vapply(reach$legend, `[[`, "", "name")
  rows <- lapply(reach$years$rows, function(r) {
    by <- setNames(lapply(r$segments, `[[`, "citations"), vapply(r$segments, `[[`, "", "name"))
    c(list(r$label, r$citations), lapply(names, function(n) if (is.null(by[[n]])) 0L else by[[n]]))
  })
  reach_table("As a table", c("Year", "Citations", names), rows)
}
