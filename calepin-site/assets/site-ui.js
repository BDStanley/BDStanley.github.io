// site-ui.js — small pieces of the house style shared by the analysis pages (Polish political analyses).
// Load it before a page's own app.js:  <script src="../site-ui.js"></script>
//
//   SiteUI.endLabels(items, x, opts)   SVG text for line-end labels: dot in the series colour, the name in ink and the
//                                      number in the series colour (house red if it has none), pushed apart so they
//                                      never overlap
//   SiteUI.inDoubt(share, cut, p)      whether a threshold is in play for a party: a share within 3 points of it, or a
//                                      chance of clearing it between 2% and 98%
//   SiteUI.inkOn(fill)                 white or ink, whichever reads on a fill colour
//   SiteUI.circleLabel(...)            a cartogram circle's number: sized to the circle, white or ink by the fill,
//                                      regular weight, semibold when selected
//   SiteUI.snapRow(row)                scroll the box that holds a table so that a row sits in its middle (the page
//                                      itself does not move)
//   Sticky table headings are automatic: every table inside a box that scrolls vertically keeps its header row in
//   view (a MutationObserver marks such boxes as pages render them).
const SiteUI = (() => {
  const RED = "#8B0000", INK = "#141414";
  const esc = (s) => String(s == null ? "" : s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));

  // items: [{y, label, value, colour}] (value already formatted, e.g. "33.2%"); x: the labels' left edge.
  // opts: size (11.5), gap (12), top and bottom (keep the labels inside), dot (true), weight of the name.
  function endLabels(items, x, opts) {
    opts = opts || {};
    const size = opts.size || 11.5, gap = opts.gap || Math.round(size + 0.5);
    const lab = items.filter((d) => d && isFinite(d.y)).map((d) => Object.assign({}, d, { yy: d.y })).sort((a, b) => a.yy - b.yy);
    for (let i = 1; i < lab.length; i++) if (lab[i].yy - lab[i - 1].yy < gap) lab[i].yy = lab[i - 1].yy + gap;
    if (opts.bottom != null && lab.length && lab[lab.length - 1].yy > opts.bottom) {
      lab[lab.length - 1].yy = opts.bottom;
      for (let i = lab.length - 2; i >= 0; i--) if (lab[i + 1].yy - lab[i].yy < gap) lab[i].yy = lab[i + 1].yy - gap;
    }
    if (opts.top != null && lab.length && lab[0].yy < opts.top) {
      lab[0].yy = opts.top;
      for (let i = 1; i < lab.length; i++) if (lab[i].yy - lab[i - 1].yy < gap) lab[i].yy = lab[i - 1].yy + gap;
    }
    return lab.map((l) => `<text x="${x}" y="${(l.yy + size * 0.35).toFixed(1)}" font-size="${size}" fill="${INK}">` +
      (opts.dot === false ? "" : `<tspan fill="${l.colour || INK}">●</tspan> `) +
      `${esc(l.label)}${l.value != null && l.value !== "" ? ` <tspan fill="${l.colour || RED}">${esc(l.value)}</tspan>` : ""}</text>`).join("");
  }

  const NEAR = 0.03, P_LO = 0.02, P_HI = 0.98;
  function inDoubt(share, cut, p) {
    return (share != null && Math.abs(share - cut) <= NEAR) || (p != null && p > P_LO && p < P_HI);
  }
  const DOUBT_NOTE = "shown where it is in doubt: a share within 3 points of the threshold, or a chance between 2% and 98%";

  function rgb(c) {
    if (!c) return [255, 255, 255];
    if (c[0] === "#") { const h = c.length === 4 ? c.slice(1).split("").map((x) => x + x).join("") : c.slice(1, 7); return [0, 2, 4].map((i) => parseInt(h.slice(i, i + 2), 16)); }
    const m = c.match(/\d+(\.\d+)?/g); return m ? m.slice(0, 3).map(Number) : [255, 255, 255];
  }
  function inkOn(fill) {
    const [r, g, b] = rgb(fill).map((v) => v / 255);
    return 0.2126 * r + 0.7152 * g + 0.0722 * b < 0.5 ? "#FFFFFF" : INK;
  }

  // opts: min and max font size in the SVG's units (defaults 16 and 34 suit circles of radius 30-70), k (0.5 of r)
  function circleLabel(x, y, r, text, fill, selected, opts) {
    opts = opts || {};
    const fs = Math.max(opts.min || 16, Math.min(opts.max || 34, r * (opts.k || 0.5)));
    return `<text x="${x}" y="${y}" dy=".35em" text-anchor="middle" style="font-size:${fs.toFixed(1)}px;font-weight:${selected ? 600 : 400};fill:${inkOn(fill)};pointer-events:none">${esc(text)}</text>`;
  }

  function scrollBox(el) {
    for (let p = el && el.parentElement; p && p !== document.body; p = p.parentElement) {
      const o = getComputedStyle(p).overflowY;
      if ((o === "auto" || o === "scroll") && p.scrollHeight > p.clientHeight) return p;
    }
    return null;
  }
  function snapRow(row) {
    const box = scrollBox(row); if (!box) return;
    const head = box.querySelector("thead, tr:first-child");
    const hh = head && head.contains(row) ? 0 : (box.querySelector("th") ? box.querySelector("th").offsetHeight : 0);
    box.scrollTop += row.getBoundingClientRect().top - box.getBoundingClientRect().top - hh - (box.clientHeight - hh - row.offsetHeight) / 2;
  }

  // Sticky headings: any table in a vertically scrolling box
  function markSticky(root) {
    (root || document).querySelectorAll("table").forEach((t) => {
      for (let p = t.parentElement; p && p !== document.body; p = p.parentElement) {
        const o = getComputedStyle(p).overflowY;
        if (o === "auto" || o === "scroll") { p.classList.add("ui-sticky"); break; }
      }
    });
  }
  if (typeof document !== "undefined") {
    const css = document.createElement("style");
    css.textContent = ".ui-sticky thead th,.ui-sticky table>tr:first-child>th,.ui-sticky tbody:first-child>tr:first-child>th{position:sticky;top:0;z-index:1;background:var(--bg,var(--paper,#fff))}";
    document.head.appendChild(css);
    let pending = false;
    const run = () => { pending = false; markSticky(document); };
    const go = () => { run(); new MutationObserver(() => { if (!pending) { pending = true; requestAnimationFrame(run); } }).observe(document.body, { childList: true, subtree: true }); };
    if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", go); else go();
  }

  return { RED, INK, endLabels, inDoubt, DOUBT_NOTE, inkOn, circleLabel, snapRow, markSticky };
})();
if (typeof module !== "undefined") module.exports = SiteUI;
