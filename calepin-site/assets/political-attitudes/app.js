// Political attitudes: long-run trends in Polish public opinion.
// Reads attitudes.json (the index) and one <slug>.json per attitude, both written by
// "Political attitudes/web/build_web.R" in the Polish political analyses repo. Each attitude's
// file holds a monthly latent trend (median and bands), the same for every group of each
// breakdown, the survey readings behind it and a few events. No libraries.
"use strict";

const $ = id => document.getElementById(id);
const NS = "http://www.w3.org/2000/svg";
const ACCENT = "#8B0000";
const BREAKS = [["all", "Everyone"], ["party", "Party"], ["age", "Age"], ["sex", "Gender"], ["age_sex", "Age and gender"], ["educ", "Education"], ["residence", "Place of residence"]];
const PERIODS = [["all", "All years"], ["2004", "Since 2004"], ["10y", "Last 10 years"]];
// Colours for the survey readings in the national view, by source family
const FAMILY_COLOURS = {
  "CBOS": "#8B0000", "Eurobarometer": "#2C6E8F", "Central and Eastern Eurobarometer": "#5B8DB8", "Candidate Countries Eurobarometer": "#7FA7CF",
  "European Social Survey": "#B0416E", "Polish National Election Study (PGSW)": "#D0A020",
  "Polish General Social Survey (PGSS)": "#5E8C31", "European Election Study": "#3B2A5A", "Commercial polls": "#8F8F8F",
  "Consolidation of Democracy in CEE": "#9C7A3C", "SWPS online panels": "#6F6F6F"
};
const MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
const MON = MONTHS.map(m => m.slice(0, 3));

let IDX = null, D = null, CACHE = {};
const S = { a: null, b: "all", p: "all", pts: true, ev: true, hide: new Set() };
let GEO = null; // geometry of the last drawn chart, for hover

// ---- helpers ------------------------------------------------------------------------------
const clamp = (x, a, b) => Math.max(a, Math.min(b, x));
const fmt = v => v == null ? "–" : (Math.round(v * 10) / 10).toFixed(1);
const fmt0 = v => v == null ? "–" : String(Math.round(v));
function debounce(f, ms) { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => f(...a), ms); }; }
function el(tag, attrs = {}, parent = null) {
  const n = document.createElementNS(NS, tag);
  for (const k in attrs) n.setAttribute(k, attrs[k]);
  if (parent) parent.appendChild(n);
  return n;
}
function html(tag, attrs = {}, text = "") {
  const n = document.createElement(tag);
  for (const k in attrs) n.setAttribute(k, attrs[k]);
  if (text) n.textContent = text;
  return n;
}
function toast(msg) { const t = $("toast"); t.textContent = msg; t.classList.add("on"); setTimeout(() => t.classList.remove("on"), 1800); }
const esc = s => String(s).replace(/[&<>"]/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));

// Month i (0-based) of the calendar starting at D.meta.start, as a fractional year
let Y0 = 1990, M0 = 9;
const monthYear = i => Y0 + (M0 + i) / 12;
const monthLabel = i => { const m = (M0 + i) % 12, y = Y0 + Math.floor((M0 + i) / 12); return `${MONTHS[m]} ${y}`; };
const monthShort = i => { const m = (M0 + i) % 12, y = Y0 + Math.floor((M0 + i) / 12); return `${MON[m]} ${y}`; };
function dateToYear(s) { const [y, m, d] = s.split("-").map(Number); return y + (m - 1) / 12 + (d - 1) / 365; }
function dateLabel(s) { const [y, m, d] = s.split("-").map(Number); return `${d} ${MON[m - 1]} ${y}`; }
function yearToMonth(y) { return Math.round((y - Y0) * 12 - M0); }

// ---- data ---------------------------------------------------------------------------------
async function getJSON(f) { const r = await fetch(f, { cache: "no-cache" }); if (!r.ok) throw new Error(f + ": " + r.status); return r.json(); }
async function loadAttitude(slug) {
  if (!CACHE[slug]) {
    const meta = IDX.attitudes.find(a => a.slug === slug);
    const d = await getJSON(meta.file);
    const cols = d.readings.cols;
    d.rd = d.readings.rows.map(r => Object.fromEntries(cols.map((c, j) => [c, r[j]])));
    d.rd.forEach(r => { r.x = dateToYear(r.d); });
    d.itemById = Object.fromEntries(d.items.map(it => [it.id, it]));
    CACHE[slug] = d;
  }
  D = CACHE[slug];
  const [y, m] = D.meta.start.split("-").map(Number); Y0 = y; M0 = m - 1;
}

// Series shown for the current breakdown: [{name, colour, median, lo80, hi80, lo95?, hi95?}]
function seriesNow() {
  const n = D.national;
  if (S.b === "all") return [{ name: "Everyone", colour: ACCENT, median: n.median, lo80: n.lo80, hi80: n.hi80, lo95: n.lo95, hi95: n.hi95, national: true }];
  const g = D.groupings.find(x => x.id === S.b);
  return g ? g.groups.filter(x => !S.hide.has(x.name)) : [];
}
function groupingNow() { return D.groupings.find(x => x.id === S.b); }

function periodRange() {
  const last = D.meta.months - 1, endY = monthYear(last) + 0.25;
  if (S.p === "2004") return [2004, endY];
  if (S.p === "10y") return [endY - 10.25, endY];
  return [monthYear(0) - 0.15, endY];
}

// ---- UI -----------------------------------------------------------------------------------
function buildSeg(id, opts, cur, onPick, disabled = new Set()) {
  const box = $(id); box.innerHTML = "";
  opts.forEach(([v, lab]) => {
    const b = html("button", { type: "button", "aria-pressed": String(v === cur) }, lab);
    if (disabled.has(v)) b.disabled = true;
    b.onclick = () => onPick(v);
    box.appendChild(b);
  });
}
function renderControls() {
  buildSeg("attitudes", IDX.attitudes.map(a => [a.slug, a.short]), S.a, async v => { if (v === S.a) return; S.a = v; S.hide.clear(); await loadAttitude(v); if (!D.groupings.some(g => g.id === S.b)) S.b = "all"; render(); });
  const have = new Set(["all", ...D.groupings.map(g => g.id)]);
  buildSeg("breaks", BREAKS, S.b, v => { S.b = v; S.hide.clear(); render(); }, new Set(BREAKS.map(b => b[0]).filter(b => !have.has(b))));
  buildSeg("periods", PERIODS, S.p, v => { S.p = v; render(); });
  $("pts").checked = S.pts; $("evs").checked = S.ev;
  $("title").textContent = D.meta.title;
  $("measure").textContent = `${D.meta.measure}. Monthly estimates pooled from ${D.meta.n_readings} national readings` +
    (D.meta.n_subgroup_readings ? ` and ${D.meta.n_subgroup_readings} subgroup readings` : "") +
    `, ${D.meta.start.slice(0, 4)}–${D.meta.last_data.slice(0, 4)}; latest reading ${dateLabel(D.meta.last_data)}.`;
  // chips
  const chips = $("chips"); chips.innerHTML = "";
  const g = groupingNow();
  if (S.b !== "all" && g) {
    g.groups.forEach(x => {
      const c = html("button", { type: "button", class: "chip", "aria-pressed": String(!S.hide.has(x.name)), title: `${x.n_readings} readings` });
      const dt = html("span", { class: "dot" }); dt.style.background = x.colour; c.appendChild(dt);
      c.appendChild(document.createTextNode(x.name));
      c.onclick = () => { S.hide.has(x.name) ? S.hide.delete(x.name) : S.hide.add(x.name); render(); };
      chips.appendChild(c);
    });
    const all = html("button", { type: "button", class: "linkbtn" }, S.hide.size ? "Show all" : "Hide all");
    all.onclick = () => { if (S.hide.size) S.hide.clear(); else g.groups.forEach(x => S.hide.add(x.name)); render(); };
    chips.appendChild(all);
  }
}

// ---- chart --------------------------------------------------------------------------------
function niceStep(span) { return span > 30 ? 5 : span > 12 ? 2 : 1; }

function draw() {
  const svg = $("chart");
  const W = Math.max(320, svg.parentNode.clientWidth - 12);
  const narrow = W < 640;
  const H = Math.round(clamp(W * 0.48, 300, 520));
  const m = { l: 40, r: narrow ? 16 : 128, t: S.ev ? 30 : 14, b: 30 };
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  svg.innerHTML = "";
  const [x0, x1] = periodRange();
  const series = seriesNow();
  const i0 = clamp(yearToMonth(x0) - 1, 0, D.meta.months - 1), i1 = D.meta.months - 1;
  const readings = !S.pts ? [] : D.rd.filter(r => r.x >= x0 && r.x <= x1 && (S.b === "all" ? r.g === "all" : r.g === S.b && !hiddenReading(r)));
  // y domain from what is visible
  let lo = 100, hi = 0;
  series.forEach(s => { for (let i = i0; i <= i1; i++) { const a = s.lo80[i], b = s.hi80[i]; if (a != null) lo = Math.min(lo, a); if (b != null) hi = Math.max(hi, b); } });
  if (S.b !== "all") for (let i = i0; i <= i1; i++) { const v = D.national.median[i]; if (v != null) { lo = Math.min(lo, v); hi = Math.max(hi, v); } }
  readings.forEach(r => { lo = Math.min(lo, r.adj); hi = Math.max(hi, r.adj); });
  if (lo > hi) { lo = 0; hi = 100; }
  const yStep = hi - lo > 40 ? 10 : 5;
  lo = Math.max(0, Math.floor((lo - 2) / yStep) * yStep); hi = Math.min(100, Math.ceil((hi + 1) / yStep) * yStep);
  const X = x => m.l + (x - x0) / (x1 - x0) * (W - m.l - m.r);
  const Y = y => m.t + (hi - y) / (hi - lo) * (H - m.t - m.b);
  GEO = { W, H, m, x0, x1, X, Y, lo, hi, series, readings, i0, i1 };

  const gGrid = el("g", {}, svg);
  for (let v = lo; v <= hi + 1e-9; v += yStep) {
    el("line", { x1: m.l, x2: W - m.r, y1: Y(v), y2: Y(v), stroke: v === lo ? "#D5D5D5" : "#ECECEC", "stroke-width": 1 }, gGrid);
    const t = el("text", { x: m.l - 6, y: Y(v) + 4, "text-anchor": "end", "font-size": 11, fill: "#767676" }, gGrid); t.textContent = v + (v === hi ? "%" : "");
  }
  const span = x1 - x0, step = niceStep(span) * (narrow && span > 12 ? 2 : 1);
  for (let y = Math.ceil(x0 / step) * step; y <= x1; y += step) {
    el("line", { x1: X(y), x2: X(y), y1: H - m.b, y2: H - m.b + 4, stroke: "#D5D5D5" }, gGrid);
    const t = el("text", { x: X(y), y: H - m.b + 17, "text-anchor": "middle", "font-size": 11, fill: "#767676" }, gGrid); t.textContent = y;
  }
  // events
  if (S.ev) {
    const gE = el("g", {}, svg);
    D.events.forEach((e, k) => {
      const x = dateToYear(e.d); if (x < x0 || x > x1) return;
      el("line", { x1: X(x), x2: X(x), y1: m.t - 6, y2: H - m.b, stroke: "#C9C9C9", "stroke-dasharray": "2 3" }, gE);
      el("circle", { cx: X(x), cy: m.t - 13, r: 8, fill: "#fff", stroke: "#B5B5B5" }, gE);
      const t = el("text", { x: X(x), y: m.t - 9.5, "text-anchor": "middle", "font-size": 10, "font-weight": 500, fill: "#4B4B4B" }, gE); t.textContent = k + 1;
    });
  }
  const clip = el("clipPath", { id: "plotclip" }, el("defs", {}, svg));
  el("rect", { x: m.l, y: 0, width: W - m.l - m.r, height: H }, clip);
  const gP = el("g", { "clip-path": "url(#plotclip)" }, svg);
  const path = (arr, lo2 = null) => {
    // line (or band when lo2 given) through non-null months, split at gaps
    let d = "", seg = [];
    const flush = () => {
      if (!seg.length) return;
      if (lo2) {
        d += "M" + seg.map(i => `${X(monthYear(i)).toFixed(1)},${Y(arr[i]).toFixed(1)}`).join("L");
        d += "L" + seg.slice().reverse().map(i => `${X(monthYear(i)).toFixed(1)},${Y(lo2[i]).toFixed(1)}`).join("L") + "Z";
      } else d += "M" + seg.map(i => `${X(monthYear(i)).toFixed(1)},${Y(arr[i]).toFixed(1)}`).join("L");
      seg = [];
    };
    for (let i = Math.max(0, i0 - 1); i <= i1; i++) { if (arr[i] == null || (lo2 && lo2[i] == null)) flush(); else seg.push(i); }
    flush();
    return d;
  };
  // national reference line in breakdown views
  if (S.b !== "all") el("path", { d: path(D.national.median), fill: "none", stroke: "#9A9A9A", "stroke-width": 1.4, "stroke-dasharray": "5 4" }, gP);
  series.forEach(s => {
    if (s.lo95) el("path", { d: path(s.hi95, s.lo95), fill: s.colour, "fill-opacity": 0.07 }, gP);
    el("path", { d: path(s.hi80, s.lo80), fill: s.colour, "fill-opacity": S.b === "all" ? 0.14 : 0.10 }, gP);
  });
  // readings
  const gR = el("g", {}, gP);
  readings.forEach(r => {
    const c = readingColour(r);
    el("circle", { cx: X(r.x), cy: Y(r.adj), r: S.b === "all" ? 2.8 : 2.4, fill: c.fill, stroke: c.stroke, "stroke-width": c.fill === "#fff" ? 1.2 : 0, "fill-opacity": 0.75 }, gR);
  });
  series.forEach(s => el("path", { d: path(s.median), fill: "none", stroke: s.colour, "stroke-width": S.b === "all" ? 2.4 : 2, "stroke-linejoin": "round" }, gP));
  // labels: lines reaching the end of the chart are labelled in the right margin (spread apart
  // so they do not overlap); lines that stop earlier (parties that dissolved) at their last month
  if (!narrow) {
    const ends = series.map(s => { let i = i1; while (i >= 0 && s.median[i] == null) i--; return i < i0 ? null : { s, i, y: Y(s.median[i]) }; }).filter(Boolean);
    const right = ends.filter(l => l.i >= i1 - 12), early = ends.filter(l => l.i < i1 - 12);
    right.sort((a, b) => a.y - b.y);
    for (let k = 1; k < right.length; k++) if (right[k].y - right[k - 1].y < 14) right[k].y = right[k - 1].y + 14;
    const over = right.length ? right[right.length - 1].y - (H - m.b) : 0;
    if (over > 0) right.forEach(l => { l.y -= over; });
    right.forEach(l => {
      const t = el("text", { x: X(monthYear(i1)) + 6, y: l.y + 4, "font-size": 12, fill: l.s.colour, "font-weight": 500 }, svg);
      t.textContent = `${l.s.name} ${fmt0(l.s.median[l.i])}%`;
    });
    early.forEach(l => {
      const t = el("text", { x: X(monthYear(l.i)) + 5, y: l.y + 4, "font-size": 11.5, fill: l.s.colour, "font-weight": 500, "paint-order": "stroke", stroke: "#fff", "stroke-width": 3 }, svg);
      t.textContent = l.s.name;
    });
  }
  // hover layer
  el("line", { id: "xhair", x1: 0, x2: 0, y1: m.t, y2: H - m.b, stroke: "#4B4B4B", "stroke-width": 1, visibility: "hidden" }, svg);
  el("g", { id: "hoverdots" }, svg);
  const hit = el("rect", { id: "hit", x: m.l, y: m.t - 22, width: W - m.l - m.r, height: H - m.t - m.b + 22, fill: "transparent" }, svg);
  hit.addEventListener("mousemove", hover);
  hit.addEventListener("mouseleave", unhover);
  hit.addEventListener("touchstart", e => { hover(e.touches[0]); }, { passive: true });
  hit.addEventListener("touchmove", e => { hover(e.touches[0]); }, { passive: true });
}

function hiddenReading(r) {
  // a subgroup reading is hidden only if every group it covers is hidden
  const g = groupingNow(); if (!g) return false;
  if (g.groups.some(x => x.name === r.k)) return S.hide.has(r.k);
  return false;
}
function readingColour(r) {
  if (S.b === "all") { const it = D.itemById[r.i]; return { fill: FAMILY_COLOURS[it ? it.family : ""] || "#8F8F8F", stroke: "none" }; }
  const g = groupingNow(), x = g && g.groups.find(q => q.name === r.k);
  return x ? { fill: x.colour, stroke: "none" } : { fill: "#fff", stroke: "#8F8F8F" };
}

function hover(ev) {
  if (!GEO) return;
  const svg = $("chart"), pt = svg.createSVGPoint(), box = svg.getBoundingClientRect();
  const sc = GEO.W / box.width;
  const px = (ev.clientX - box.left) * sc, py = (ev.clientY - box.top) * sc;
  const { X, Y, m, H, W } = GEO;
  const tip = $("tip");
  // events near the top
  if (S.ev && py < m.t) {
    const e = D.events.map((e, k) => ({ e, k, x: dateToYear(e.d) })).filter(o => o.x >= GEO.x0 && o.x <= GEO.x1)
      .find(o => Math.abs(X(o.x) - px) < 10);
    if (e) { showTip(px, py, `<b>${esc(e.e.label)}</b><div class="s">${dateLabel(e.e.d)}</div>`, sc); hideHair(); return; }
  }
  // nearest reading
  let best = null, bd = 64;
  GEO.readings.forEach(r => { const dx = X(r.x) - px, dy = Y(r.adj) - py, d2 = dx * dx + dy * dy; if (d2 < bd) { bd = d2; best = r; } });
  if (best) { showTip(px, py, readingTip(best), sc); hideHair(); return; }
  // month crosshair
  const yr = GEO.x0 + (px - m.l) / (W - m.l - m.r) * (GEO.x1 - GEO.x0);
  const i = clamp(yearToMonth(yr), GEO.i0, GEO.i1);
  const xh = $("xhair"); xh.setAttribute("x1", X(monthYear(i))); xh.setAttribute("x2", X(monthYear(i))); xh.setAttribute("visibility", "visible");
  const dots = $("hoverdots"); dots.innerHTML = "";
  const rows = GEO.series.filter(s => s.median[i] != null).sort((a, b) => b.median[i] - a.median[i]);
  rows.forEach(s => el("circle", { cx: X(monthYear(i)), cy: Y(s.median[i]), r: 3.5, fill: s.colour, stroke: "#fff", "stroke-width": 1.5 }, dots));
  let h = `<b>${monthLabel(i)}</b>`;
  rows.forEach(s => {
    h += `<div class="r"><span><span class="dot" style="background:${s.colour}"></span> ${esc(s.name)}</span><span><b>${fmt0(s.median[i])}%</b> <span class="s">${fmt0(s.lo80[i])}–${fmt0(s.hi80[i])}</span></span></div>`;
  });
  if (S.b !== "all" && D.national.median[i] != null) h += `<div class="r s"><span>Everyone</span><span>${fmt0(D.national.median[i])}%</span></div>`;
  if (!rows.length) h += `<div class="s">No estimate shown for this month</div>`;
  else h += `<div class="s">Median, with 80% credible interval</div>`;
  showTip(px, py, h, sc);
}
function readingTip(r) {
  const it = D.itemById[r.i] || {};
  const who = r.g === "all" ? "" : ` · ${esc(r.k)}`;
  return `<b>${esc(it.family || r.i)}</b>${who}<div class="s">${esc(r.s)} · ${dateLabel(r.d)}</div>` +
    `<div style="margin:4px 0">${esc(it.wording || "")}</div>` +
    `<div class="r"><span>${esc(it.pro || "Pro")} among those taking a side</span><span><b>${fmt(r.raw)}%</b></span></div>` +
    `<div class="r"><span>On the trend's scale</span><span>${fmt(r.adj)}%</span></div>` +
    `<div class="s">Effective sample ${r.n}</div>`;
}
function showTip(px, py, h, sc) {
  const tip = $("tip"); tip.innerHTML = h; tip.style.display = "block";
  const box = $("chart").getBoundingClientRect(), x = px / sc, y = py / sc;
  const tw = tip.offsetWidth, th = tip.offsetHeight;
  let left = x + 16; if (left + tw > box.width) left = x - tw - 16;
  let top = y - th / 2; top = clamp(top, 0, Math.max(0, box.height - th));
  tip.style.left = (left + 6) + "px"; tip.style.top = (top + 6) + "px";
}
function hideHair() { const xh = $("xhair"); if (xh) xh.setAttribute("visibility", "hidden"); const d = $("hoverdots"); if (d) d.innerHTML = ""; }
function unhover() { $("tip").style.display = "none"; hideHair(); }

// ---- legend, notes, tables ----------------------------------------------------------------
function renderLegend() {
  const lg = $("legend"); lg.innerHTML = "";
  if (S.b === "all") {
    lg.innerHTML = `<span><span class="swatch" style="background:${ACCENT}"></span>Trend, with 80% and 95% credible intervals</span>`;
    if (S.pts) {
      const fams = [...new Set(D.rd.filter(r => r.g === "all").map(r => (D.itemById[r.i] || {}).family))].filter(Boolean);
      fams.forEach(f => { lg.innerHTML += `<span><span class="dot" style="background:${FAMILY_COLOURS[f] || "#8F8F8F"}"></span>${esc(f)}</span>`; });
    }
  } else {
    lg.innerHTML = `<span><svg width="22" height="8"><line x1="0" x2="22" y1="4" y2="4" stroke="#9A9A9A" stroke-width="1.5" stroke-dasharray="5 4"/></svg>Everyone</span>`;
    if (S.pts) lg.innerHTML += `<span><span class="dot" style="background:#fff;border:1.2px solid #8F8F8F"></span>Reading covering more than one group</span>`;
  }
  if (S.ev) {
    const [x0, x1] = periodRange();
    D.events.forEach((e, k) => { const x = dateToYear(e.d); if (x >= x0 && x <= x1) lg.innerHTML += `<span class="muted">${k + 1}. ${esc(e.label)}</span>`; });
  }
  const notes = {
    all: "Each dot is one survey's reading, placed on the trend's scale after allowing for how that question's wording, organisation and mode differ from CBOS's question. Hover over the chart for the month's estimate, or over a dot for the survey.",
    party: "Party preference is the party respondents intend to vote for or voted for at the last Sejm election, followed across elections by party lineage (SLD to Lewica, PO to KO and so on). Lines are shown while a party existed and its voters were surveyed.",
    age: "Age at the time of the survey.",
    sex: "",
    age_sex: "Age at the time of the survey, for men and women separately.",
    educ: "Highest completed education: primary (including lower secondary), secondary (including basic vocational), higher.",
    residence: "Size of the respondent's town or city. Sources that ask people to describe where they live (Eurobarometer, ESS) count as a mix of the two town groups."
  };
  const base = S.b === "all" ? "" : "Between surveys that report this breakdown, each group's line follows the national trend at its last known distance from it, and its interval widens. ";
  $("note").textContent = (base + (notes[S.b] || "")).trim();
}

function renderLatest() {
  const series = seriesNow(), box = $("latest"); box.innerHTML = "";
  if (!series.length) { box.innerHTML = `<p class="help">No group selected.</p>`; return; }
  const last = D.meta.months - 1;
  const at = (s, i) => (i >= 0 && s.median[i] != null) ? s.median[i] : null;
  if (S.b === "all") {
    const s = series[0], i = last;
    const a = at(s, i - 12), b5 = at(s, i - 60);
    $("latesthead").textContent = `Latest estimate · ${monthLabel(i)}`;
    box.innerHTML = `<div class="big">${fmt0(s.median[i])}%</div>
      <p class="help">80% credible interval ${fmt0(s.lo80[i])}–${fmt0(s.hi80[i])}%. A year earlier ${fmt0(a)}%; five years earlier ${fmt0(b5)}%.</p>
      <table class="t"><tr><th>Period</th><th>Low</th><th>High</th></tr>${periodsTable(s)}</table>`;
    return;
  }
  $("latesthead").textContent = `Latest estimates · ${monthLabel(last)}`;
  const t = html("table", { class: "t" });
  t.innerHTML = `<tr><th>Group</th><th>Latest</th><th>80% interval</th><th>Change over a year</th></tr>`;
  series.forEach(s => {
    let i = last; while (i >= 0 && s.median[i] == null) i--;
    if (i < 0) return;
    const prev = at(s, i - 12), ch = prev == null ? null : s.median[i] - prev;
    const when = i === last ? "" : `<br><small>${monthShort(i)}</small>`;
    t.innerHTML += `<tr><td class="l"><span class="dot" style="background:${s.colour}"></span> ${esc(s.name)}${when}</td><td><b>${fmt0(s.median[i])}%</b></td><td>${fmt0(s.lo80[i])}–${fmt0(s.hi80[i])}</td>` +
      `<td class="${ch == null ? "" : ch > 0.5 ? "pos" : ch < -0.5 ? "neg" : ""}">${ch == null ? "–" : (ch > 0 ? "+" : "") + fmt(ch)}</td></tr>`;
  });
  box.appendChild(t);
  box.appendChild(html("p", { class: "help" }, "Groups whose lines end earlier show their last month with an estimate."));
}
function periodsTable(s) {
  // the lowest and highest month in a few eras, from the attitude's meta.eras (an open end runs to
  // the latest month); exports without them get the EU membership eras
  const dateToMonth = s => { const [y, m] = s.split("-").map(Number); return (y - Y0) * 12 + m - 1 - M0; };
  const eras = D.meta.eras
    ? D.meta.eras.map(e => [e.label, dateToMonth(e.from), e.to ? dateToMonth(e.to) : D.meta.months - 1])
    : [["Before accession (to April 2004)", 0, yearToMonth(2004.25) - 1], ["2004–2015", yearToMonth(2004.34), yearToMonth(2015.9)], ["Since November 2015", yearToMonth(2015.9) + 1, D.meta.months - 1]];
  return eras.map(([lab, a, b]) => {
    a = clamp(a, 0, D.meta.months - 1); b = clamp(b, 0, D.meta.months - 1);
    let lo = null, hi = null, il = a, ih = a;
    for (let i = a; i <= b; i++) { const v = s.median[i]; if (v == null) continue; if (lo == null || v < lo) { lo = v; il = i; } if (hi == null || v > hi) { hi = v; ih = i; } }
    return `<tr><td class="l">${lab}</td><td>${fmt0(lo)}% <small>${monthShort(il)}</small></td><td>${fmt0(hi)}% <small>${monthShort(ih)}</small></td></tr>`;
  }).join("");
}
function renderItems() {
  const t = $("items");
  t.innerHTML = `<tr><th>Source and question</th><th>Readings</th><th>Years</th></tr>`;
  const items = D.items.slice().sort((a, b) => (b.n + b.n_sub) - (a.n + a.n_sub));
  items.forEach(it => {
    const yrs = it.first.slice(0, 4) === it.last.slice(0, 4) ? it.first.slice(0, 4) : `${it.first.slice(0, 4)}–${it.last.slice(0, 4)}`;
    t.innerHTML += `<tr><td class="l"><b style="font-weight:500">${esc(it.family)}</b>${it.role === "anchor" ? " <small>(anchor)</small>" : ""}<br><small>${esc(it.wording)}</small></td>` +
      `<td>${it.n}${it.n_sub ? `<br><small>+${it.n_sub} by group</small>` : ""}</td><td>${yrs}</td></tr>`;
  });
  $("srcnote").textContent = `${D.items.length} questions from ${D.meta.sources.length} kinds of source. Each question has its own level and sensitivity, estimated from where its readings overlap with others; the trend is read on the scale of the anchor question.`;
}

// ---- share, downloads ---------------------------------------------------------------------
function hashTarget() { try { if (window.top !== window && window.top.location.host === location.host) return window.top; } catch (e) { /* cross-origin */ } return window; }
function stateString() {
  const q = new URLSearchParams();
  if (S.a !== IDX.attitudes[0].slug) q.set("a", S.a);
  if (S.b !== "all") q.set("b", S.b);
  if (S.p !== "all") q.set("p", S.p);
  if (!S.pts) q.set("r", "0");
  if (!S.ev) q.set("e", "0");
  if (S.hide.size) q.set("h", [...S.hide].join(";"));
  return q.toString();
}
const saveHash = debounce(() => {
  const t = hashTarget(), s = stateString();
  try { t.history.replaceState(null, "", t.location.pathname + t.location.search + (s ? "#" + s : "")); } catch (e) { /* ignore */ }
}, 250);
function readHash() {
  let h = ""; try { h = hashTarget().location.hash.slice(1); } catch (e) { h = location.hash.slice(1); }
  const q = new URLSearchParams(h);
  if (q.get("a") && IDX.attitudes.some(a => a.slug === q.get("a"))) S.a = q.get("a");
  if (BREAKS.some(b => b[0] === q.get("b"))) S.b = q.get("b");
  if (PERIODS.some(p => p[0] === q.get("p"))) S.p = q.get("p");
  S.pts = q.get("r") !== "0"; S.ev = q.get("e") !== "0";
  if (q.get("h")) q.get("h").split(";").forEach(x => S.hide.add(x));
}
function shareUrl() { const t = hashTarget(), s = stateString(); return t.location.origin + t.location.pathname + (s ? "#" + s : ""); }
function download(name, blob) {
  const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = name;
  document.body.appendChild(a); a.click(); setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 500);
}
function csv() {
  const rows = [["attitude", "breakdown", "group", "month", "median", "lower80", "upper80"]];
  const [x0] = periodRange(), i0 = clamp(yearToMonth(x0), 0, D.meta.months - 1);
  seriesNow().forEach(s => { for (let i = i0; i < D.meta.months; i++) if (s.median[i] != null) rows.push([D.meta.slug, S.b, s.name, monthShort(i), s.median[i], s.lo80[i], s.hi80[i]]); });
  const q = v => typeof v === "string" && /[",]/.test(v) ? `"${v.replace(/"/g, '""')}"` : v;
  download(`${D.meta.slug}-${S.b}.csv`, new Blob([rows.map(r => r.map(q).join(",")).join("\n") + "\n"], { type: "text/csv" }));
}
async function png() {
  const svg = $("chart"), vb = svg.viewBox.baseVal, w = vb.width, h = vb.height;
  const clone = await ChartExport.clone(svg);   // ../chart-export.js: labels stay in Jost
  clone.setAttribute("xmlns", NS); clone.setAttribute("width", w); clone.setAttribute("height", h);
  clone.querySelectorAll("#hit,#xhair,#hoverdots").forEach(n => n.remove());
  const img = await new Promise(res => { const im = new Image(); im.onload = () => res(im); im.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(clone)); });
  await ChartExport.fonts();
  const pad = 24, head = 64, foot = 34, W = w + 2 * pad, H = h + head + foot;
  const cv = document.createElement("canvas"), sc = 2; cv.width = W * sc; cv.height = H * sc;
  const g = cv.getContext("2d"); g.scale(sc, sc); g.fillStyle = "#fff"; g.fillRect(0, 0, W, H);
  g.fillStyle = ACCENT; g.font = ChartExport.TITLE;
  const bl = BREAKS.find(b => b[0] === S.b)[1];
  g.fillText(`${D.meta.title}${S.b === "all" ? "" : " by " + bl.toLowerCase()}`, pad, 32);
  g.fillStyle = "#4B4B4B"; g.font = ChartExport.SUBTITLE; g.fillText(`${D.meta.measure}. Latent trend pooled from several surveys, to ${monthLabel(D.meta.months - 1)}.`, pad, 54);
  g.drawImage(img, pad, head, w, h);
  g.fillStyle = "#767676"; g.font = ChartExport.CREDIT; g.fillText("Ben Stanley, SWPS University · benstanley.eu", pad, H - 12);
  cv.toBlob(b => download(`${D.meta.slug}-${S.b}.png`, b), "image/png");
}

// ---- main ---------------------------------------------------------------------------------
function render() {
  renderControls(); draw(); renderLegend(); renderLatest(); renderItems(); unhover(); saveHash();
}
async function init() {
  try {
    IDX = await getJSON("attitudes.json");
    if (!IDX.attitudes || !IDX.attitudes.length) throw new Error("no attitudes");
    S.a = IDX.attitudes[0].slug;
    readHash();
    await loadAttitude(S.a);
    if (!D.groupings.some(g => g.id === S.b)) S.b = "all";
  } catch (e) {
    $("loading").textContent = "The data could not be loaded. Please try again later."; console.error(e); return;
  }
  $("loading").hidden = true; $("ui").hidden = false;
  $("pts").onchange = e => { S.pts = e.target.checked; render(); };
  $("evs").onchange = e => { S.ev = e.target.checked; render(); };
  $("dlcsv").onclick = csv; $("dlpng").onclick = png;
  $("share").onclick = async () => { try { await navigator.clipboard.writeText(shareUrl()); toast("Link copied"); } catch (e) { toast(shareUrl()); } };
  render();
  new ResizeObserver(debounce(() => { draw(); }, 80)).observe($("chart").parentNode);
}
init();
