// Shared by the Polish political analyses apps (assets/*/index.html) for their PNG
// downloads. Each app exports a chart by serialising its SVG into an <img> and
// drawing that on a canvas; but the image sees neither the page's stylesheet nor
// its web fonts, so the chart's labels fell back to a serif face. clone() returns
// a self-contained copy of the SVG: every text element carries its computed font
// and fill, and Jost is embedded as data URIs in a <style>. fonts() loads the
// faces the canvas draws the title and credits in, and TITLE etc. are those
// fonts, so that every export is set like the site's headings.
//
// The suggested citation of each analysis lives here too, so that the pages, the
// PNGs and the CSVs all cite it the same way: citeHtml() for a page, drawCite()
// for a PNG's footer and csvCite() for a CSV's first line (CSV cannot set italics
// or bold). The year and the series name are CITE_YEAR and SERIES.
//
// Exports are drawn at scale(W, H) canvas pixels per point: 4, or less where a
// large chart would pass the 16.7 million pixels Safari allows a canvas. The SVG
// images are given SVG_SCALE times their size (setSize), because Safari rasterises
// an SVG <img> at its own size before drawing it, which blurs it when scaled up.
(function () {
  "use strict";
  const NS = "http://www.w3.org/2000/svg";
  const JOST = "https://fonts.googleapis.com/css2?family=Jost:ital,wght@0,300..700;1,300..700&display=swap";
  const PROPS = ["font-family", "font-size", "font-weight", "font-style", "letter-spacing", "fill", "fill-opacity", "visibility", "display"];
  let jostCss;

  // The Google Fonts CSS for Jost with its font files inlined, or "" offline.
  function jost() {
    jostCss = jostCss || fetch(JOST).then(r => r.text()).then(async css => {
      const urls = [...new Set(css.match(/https:\/\/[^)'"\s]+/g) || [])];
      const data = await Promise.all(urls.map(u => fetch(u).then(r => r.blob()).then(b => new Promise((res, rej) => {
        const fr = new FileReader(); fr.onload = () => res(fr.result); fr.onerror = rej; fr.readAsDataURL(b);
      }))));
      urls.forEach((u, i) => { css = css.split(u).join(data[i]); });
      return css;
    }).catch(() => "");
    return jostCss;
  }

  // Copy before removing anything from the clone, so its text nodes still pair
  // up one to one with the original's.
  async function clone(svg) {
    const c = svg.cloneNode(true), src = svg.querySelectorAll("text, tspan"), dst = c.querySelectorAll("text, tspan");
    src.forEach((n, i) => {
      const cs = getComputedStyle(n);
      dst[i].setAttribute("style", PROPS.map(p => `${p}:${cs.getPropertyValue(p)}`).join(";"));
    });
    const css = await jost();
    if (css) { const st = document.createElementNS(NS, "style"); st.textContent = css; c.insertBefore(st, c.firstChild); }
    return c;
  }

  function fonts() {
    return Promise.all(["300 22px Jost", "400 13px Jost", "500 13px Jost", "italic 400 12px Jost", "600 12px Jost"].map(f => document.fonts.load(f))).catch(() => {});
  }

  const SVG_SCALE = 4;
  const scale = (W, H) => Math.max(2, Math.min(4, Math.sqrt(16e6 / (W * H))));
  function setSize(svg, w, h) {
    svg.setAttribute("width", w * SVG_SCALE); svg.setAttribute("height", h * SVG_SCALE);
  }

  const CITE_YEAR = 2026, SERIES = "Polish Political Analyses", BY = `Ben Stanley, ${CITE_YEAR}. `;
  const esc = s => String(s).replace(/[&<>"]/g, c => ({"&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;"})[c]);
  const citeText = page => `${BY}${page}, ${SERIES}, benstanley.eu.`;
  const citeHtml = page => `${BY}<i>${esc(page)}</i>, <b>${SERIES}</b>, benstanley.eu.`;
  const csvCite = page => `# ${citeText(page)}\n`;
  // The citation on one line from (x, y) in the canvas's current fill, in CREDIT's size, or
  // smaller where it would run past maxW
  function drawCite(g, page, x, y, maxW) {
    const runs = [[BY, ""], [page, "italic 400 "], [", ", ""], [SERIES, "600 "], [", benstanley.eu.", ""]];
    const font = (pre, px) => `${pre}${px}px Jost, sans-serif`;
    let px = 12;
    const width = () => runs.reduce((s, [t, pre]) => { g.font = font(pre, px); return s + g.measureText(t).width; }, 0);
    if (maxW && width() > maxW) px = Math.max(8, Math.floor(px * maxW / width() * 10) / 10);
    runs.forEach(([t, pre]) => { g.font = font(pre, px); g.fillText(t, x, y); x += g.measureText(t).width; });
  }

  window.ChartExport = {
    clone, fonts, scale, setSize, citeText, citeHtml, csvCite, drawCite,
    TITLE: "300 22px Jost, sans-serif",   // dark red, like the site's h2
    SUBTITLE: "13px Jost, sans-serif",    // #4B4B4B
    CREDIT: "12px Jost, sans-serif",      // #767676
  };
})();
