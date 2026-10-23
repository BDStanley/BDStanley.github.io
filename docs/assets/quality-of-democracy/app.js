// Quality of democracy: the state of Polish democracy since 1990 in V-Dem's expert-coded
// indicators, against the other EU member states and the United Kingdom.
// Reads democracy.json (manifest, measures, regions, Poland, regional statistics, the regime
// classification and the Sejm's roll-call polarisation), political-context.json (Sejm elections,
// cabinets, presidents, events) and, once a comparison needs them, democracy-countries.json, all
// written by "Quality of democracy/analysis/export_web.R" in the Polish political analyses repo.
// Values are V-Dem's own: only the Polarisation view reverses the measures marked
// reversedInPolarisation (scale maximum minus value), so that up means more polarised there.
// Each value is V-Dem's day-weighted mean over a calendar year, so it is drawn at the middle of the
// year and the selected year is shaded whole. Regional lines are the median and middle half of
// the other countries in a set, never including Poland. No libraries.
"use strict";

const $ = id => document.getElementById(id);
const NS = "http://www.w3.org/2000/svg";
const ACCENT = "#8B0000";
const PL = "#A11D1D";
// Comparison countries take these in the order they are added, and keep theirs while selected
// (checked for colour-blind separation, every pair, with Poland's red)
const CMP = ["#6A3FA0", "#E0A000", "#5E8C2A", "#D45DA4", "#4C9BE8"];
const MAXC = CMP.length;
const MED = "#4B4B4B";
const SITE = "../../pages/polish-political-analyses/";
const VIEWS = ["overview", "explain", "context", "polarisation", "methods"];
const LENSES = [
  ["elections", "Elections and pluralism", "Whether elections are clean and free, and whether people can speak, organise and hear more than one side: the parts of electoral democracy, with vertical accountability, how far governments answer to voters. Intimidation and other irregularities are coded at each national election and carried forward to the next, so they move in steps."],
  ["checks", "Courts, law and checks", "The liberal component: how far courts and parliament constrain the executive, whether it respects the constitution, and whether the law applies equally. Liberal democracy combines it with electoral democracy, as .25 × EDI^1.585 + .25 × LC + .5 × EDI^1.585 × LC. For Poland, V-Dem's high court is the Constitutional Tribunal."],
  ["media", "Media and information", "Whether the media are free of censorship and harassment, cover the opposition fairly and carry a range of views, and whether the government spreads false information. Diagonal accountability is how far the media and civil society hold the government to account."],
  ["participation", "Civil society and participation", "How freely civil society organisations form and act, whether the government consults or represses them, and how far people take part. The two mobilisation measures count protests and rallies for democracy and for autocracy: higher means more events, not better ones."],
  ["deliberation", "Deliberation and equality", "Whether elites justify decisions by reasons and the common good, respect counterarguments and consult widely, and how equally rights and power are shared. On power by sexual orientation, 3 is equal power."],
  ["rights", "Rights and the state", "Civil liberties, academic freedom and the conduct of the state. On the two corruption indices, higher means more corruption."]
];
const REGIONS = [["pl", "Poland only"], ["cee", "Central and Eastern Europe"], ["we", "Western Europe"], ["both", "Both"]];
const REG_SHORT = {cee: "CEE", we: "Western Europe"};
const OVERVIEW = ["v2x_polyarchy", "v2x_libdem", "v2x_partipdem", "v2x_delibdem", "v2x_egaldem"];
const POLAR_MAIN = ["v2cacamps", "v2smpolsoc", "v2smpolhate"];
const POLAR_MORE = ["v2dlcountr", "v2smpardom", "v2caviol"];
const REV_UP = {v2smpolsoc: "more polarised", v2smpolhate: "more hate speech"};
const MON = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

let D = null, CX = null, C = null, CLOAD = null, Y0 = 1990, Y1 = 2025;
const M = new Map(), MS = new Map();
const SLOT = new Map();
const GEO = {};
const S = {view: "overview", m: "libdem", l: "elections", from: 1990, to: 2025, y: 2025, r: "pl", c: [], u: true, ctx: true, ev: null, cm: "region", so: "level"};

// ---- helpers --------------------------------------------------------------------------------
const clamp = (x, a, b) => Math.max(a, Math.min(b, x));
function debounce(f, ms) { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => f(...a), ms); }; }
function el(tag, attrs = {}, parent = null) {
  const n = document.createElementNS(NS, tag);
  for (const k in attrs) if (attrs[k] != null) n.setAttribute(k, attrs[k]);
  if (parent) parent.appendChild(n);
  return n;
}
function txt(parent, x, y, s, attrs = {}) { const t = el("text", {x, y, ...attrs}, parent); t.textContent = s; return t; }
function toast(msg) { const t = $("toast"); t.textContent = msg; t.classList.add("on"); setTimeout(() => t.classList.remove("on"), 2000); }
const esc = s => String(s).replace(/[&<>"]/g, c => ({"&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;"}[c]));
const f2 = v => v == null ? "–" : v.toFixed(2);
const pct = v => v == null ? "–" : Math.round(v * 100) + "%";
const sgn = v => { if (v == null || isNaN(v)) return "–"; v = Math.round(v * 100) / 100; return v === 0 ? "0.00" : (v > 0 ? "+" : "−") + Math.abs(v).toFixed(2); };
function dec(s) { const [y, m, d] = s.split("-").map(Number); const a = Date.UTC(y, m - 1, d), b = Date.UTC(y, 0, 1), c = Date.UTC(y + 1, 0, 1); return y + (a - b) / (c - b); }
function dateLabel(s) { const [y, m, d] = s.split("-").map(Number); return `${d} ${MON[m - 1]} ${y}`; }
const MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
function monthLabel(s) { const [y, m] = String(s).split("-").map(Number); return m ? `${MONTHS[m - 1]} ${y}` : String(s); }
async function getJSON(f) { const r = await fetch(f, {cache: "no-cache"}); if (!r.ok) throw new Error(f + ": " + r.status); return r.json(); }
function download(name, blob) {
  const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = name;
  document.body.appendChild(a); a.click(); setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 500);
}
function ink(hex) {
  const n = parseInt(hex.slice(1), 16), c = [n >> 16, (n >> 8) & 255, n & 255].map(v => { v /= 255; return v <= .03928 ? v / 12.92 : ((v + .055) / 1.055) ** 2.4; });
  return .2126 * c[0] + .7152 * c[1] + .0722 * c[2] > .22 ? "#141414" : "#FFFFFF";
}
const link = (href, label, top = true) => `<a href="${esc(href)}"${top ? ' target="_top"' : ' target="_blank" rel="noopener"'}>${esc(label)}</a>`;

// ---- data -----------------------------------------------------------------------------------
const yi = y => y - Y0;
const cinfo = iso => D.countries.find(c => c.id === iso);
const nameOf = iso => iso === "POL" ? "Poland" : (cinfo(iso) || {}).name || iso;
const regionOf = iso => iso === "POL" ? "cee" : (cinfo(iso) || {}).region;
function seriesOf(iso, m) { return iso === "POL" ? D.poland[m.id] : (C && C.byId.has(iso) ? C.byId.get(iso).series[m.id] : null); }
const colourOf = iso => iso === "POL" ? PL : CMP[SLOT.get(iso) ?? 0];
const regionsShown = () => S.r === "both" ? ["cee", "we"] : S.r === "pl" ? [] : [S.r];
function vals(s, m, rev) {
  if (!s) return null;
  const f = v => v == null ? null : rev ? m.scale[1] - v : v;
  return {est: s.est.map(f), lo: s.lo ? (rev ? s.hi : s.lo).map(f) : null, hi: s.hi ? (rev ? s.lo : s.hi).map(f) : null};
}
function regVals(r, m, rev) {
  const s = D.regional[r] && D.regional[r][m.id]; if (!s) return null;
  const f = v => v == null ? null : rev ? m.scale[1] - v : v;
  return {med: s.med.map(f), q25: (rev ? s.q75 : s.q25).map(f), q75: (rev ? s.q25 : s.q75).map(f), n: s.n};
}
function addCountry(iso) {
  if (iso === "POL" || S.c.includes(iso) || !cinfo(iso)) return false;
  if (S.c.length >= MAXC) { toast(`Compare up to ${MAXC} countries: remove one first`); return false; }
  const used = new Set(S.c.map(c => SLOT.get(c))); let k = 0; while (used.has(k)) k++;
  SLOT.set(iso, k); S.c.push(iso); return true;
}
function removeCountry(iso) { S.c = S.c.filter(x => x !== iso); SLOT.delete(iso); }
function toggleCountry(iso) { if (S.c.includes(iso)) removeCountry(iso); else addCountry(iso); render(); }
const needCountries = () => S.c.length > 0 || S.view === "context";
function loadCountries() {
  if (C) return Promise.resolve();
  if (!CLOAD) CLOAD = getJSON("democracy-countries.json").then(j => { C = j; C.byId = new Map(j.countries.map(c => [c.id, c])); })
    .catch(e => { console.error(e); toast("The comparison countries could not be loaded"); CLOAD = null; });
  return CLOAD;
}

// ---- panels ---------------------------------------------------------------------------------
function ticksFor(m) {
  if (m.kind === "index") return [0, .25, .5, .75, 1];
  const t = []; for (let v = m.scale[0]; v <= m.scale[1]; v++) t.push(v); return t;
}
function vdemPanel(m, opt = {}) {
  // opt.rev asks for reversal; only the measures marked reversedInPolarisation are reversed
  const {rev: wantRev, ...o} = opt;
  const rev = !!wantRev && m.reversedInPolarisation;
  const up = rev ? REV_UP[m.id] : m.up;
  return Object.assign({
    kind: "vdem", m, rev,
    title: m.label + (rev ? " (reversed)" : ""),
    sub: m.shown + (up && up !== "more democratic" ? ` · ↑ ${up}` : ""),
    y0: m.scale[0], y1: m.scale[1], yt: ticksFor(m),
    anchors: m.anchors ? (rev ? [m.anchors[1], m.anchors[0]] : m.anchors) : null,
    x0: S.from, x1: S.to + 1, step: m.electionSpecific
  }, o);
}
function rollPanel(which) {
  const r = D.rollcall, v = r[which].value;
  let lo = 0, hi = 1, yt = [0, .25, .5, .75, 1];
  if (which === "distance") {
    lo = Math.floor(Math.min(...v.filter(x => x != null)) * 4) / 4; hi = Math.ceil(Math.max(...v.filter(x => x != null)) * 4) / 4;
    yt = []; for (let t = lo; t <= hi + 1e-9; t += .25) yt.push(+t.toFixed(2));
  }
  return {kind: "roll", which, title: which === "distance" ? "Distance between PiS and PO/KO MPs" : "Contested votes on which the PiS and PO/KO lines opposed each other",
    sub: which === "distance" ? "Sejm roll calls, dynamic IRT · pooled SD units" : "Sejm roll calls · share of contested votes",
    y0: lo, y1: hi, yt, x0: Math.max(S.from, 2005), x1: S.to === Y1 ? Y1 + 2 : S.to + 1};
}
// Half-year midpoints of the roll-call series
const rollT = () => D.rollcall.periods.map(p => +p.slice(0, 4) + (p.slice(5) === "1" ? .25 : .75));

function pathYears(v, X, Y, step, i0, i1) {
  let d = "", pen = false;
  for (let i = i0; i <= i1; i++) {
    if (v[i] == null) { pen = false; continue; }
    const y = Y0 + i;
    d += step ? `${pen ? "L" : "M"}${X(y).toFixed(1)},${Y(v[i]).toFixed(1)}L${X(y + 1).toFixed(1)},${Y(v[i]).toFixed(1)}` : `${pen ? "L" : "M"}${X(y + .5).toFixed(1)},${Y(v[i]).toFixed(1)}`;
    pen = true;
  }
  return d;
}
function bandYears(lo, hi, X, Y, step, i0, i1) {
  let d = "", run = [];
  const pts = (i, arr, rev) => step ? (rev ? [[Y0 + i + 1, arr[i]], [Y0 + i, arr[i]]] : [[Y0 + i, arr[i]], [Y0 + i + 1, arr[i]]]) : [[Y0 + i + .5, arr[i]]];
  const flush = () => {
    if (run.length > (step ? 0 : 1)) {
      const top = run.flatMap(i => pts(i, hi, false)), bot = run.slice().reverse().flatMap(i => pts(i, lo, true));
      d += "M" + top.concat(bot).map(([x, y]) => `${X(x).toFixed(1)},${Y(y).toFixed(1)}`).join("L") + "Z";
    }
    run = [];
  };
  for (let i = i0; i <= i1; i++) { if (lo[i] == null || hi[i] == null) flush(); else run.push(i); }
  flush();
  return d;
}

// One figure: a grid of panels in a single SVG (so a PNG holds all of it), with the context strip
function drawFigure(key, items, opt = {}) {
  const svg = $("fig-" + key);
  const W = Math.max(300, svg.parentNode.clientWidth - 24);
  const cols = opt.cols ? Math.min(opt.cols, W >= 620 ? opt.cols : 1) : (W >= 1000 ? 3 : W >= 620 ? 2 : 1);
  const gap = 22, pw = (W - gap * (cols - 1)) / cols, ph = opt.ph || 168, head = 44;
  svg.innerHTML = "";
  const geo = {W, panels: [], key};
  let y = 0, col = 0;
  const newRow = () => { if (col) { y += head + ph + 18; col = 0; } };
  for (const p of items) {
    if (p.kind === "heading") { newRow(); txt(svg, 0, y + 16, p.text, {"font-size": 14, "font-weight": 500, fill: ACCENT}); y += 30; continue; }
    if (p.kind === "regime") { newRow(); drawRegime(svg, 0, y, W); y += 36; continue; }
    geo.panels.push(drawPanel(svg, p, col * (pw + gap), y, pw, head + ph, head));
    col++; if (col === cols) { col = 0; y += head + ph + 18; }
  }
  newRow();
  if (opt.strip && S.ctx) { geo.strip = drawStrip(svg, 0, y, W, opt.stripR); y += 112; }
  svg.setAttribute("viewBox", `0 0 ${W} ${Math.max(40, y)}`);
  GEO[key] = geo;
  wire(key);
}

function drawPanel(svg, p, ox, oy, w, h, head) {
  const g = el("g", {transform: `translate(${ox},${oy})`}, svg);
  const maxc = Math.floor(w / 7.2);
  const words = p.title.split(" "), lines = [""];
  for (const wd of words) { const cur = lines[lines.length - 1]; if ((cur + " " + wd).trim().length > maxc && cur) lines.push(wd); else lines[lines.length - 1] = (cur + " " + wd).trim(); }
  lines.slice(0, 2).forEach((l, k) => txt(g, 0, 13 + k * 14, k === 1 && lines.length > 2 ? l + "…" : l, {"font-size": 13, "font-weight": 500, fill: "#141414"}));
  txt(g, 0, lines.length > 1 ? 41 : 28, p.sub, {"font-size": 11, fill: "#767676"});
  const m = {l: 34, r: p.rlab ? 86 : 8, t: head + 6, b: 20};
  const X = v => m.l + (v - p.x0) / (p.x1 - p.x0) * (w - m.l - m.r);
  const Y = v => m.t + (p.y1 - v) / (p.y1 - p.y0) * (h - m.t - m.b);
  const fmtT = p.kind === "roll" ? (p.which === "opposed" ? pct : (v => v.toFixed(2))) : (p.m && p.m.kind === "index" ? (v => v === 0 ? "0" : v === 1 ? "1" : String(v).replace(/^0/, "")) : String);
  p.yt.forEach((v, i) => {
    el("line", {x1: m.l, x2: w - m.r, y1: Y(v), y2: Y(v), stroke: i === 0 ? "#D5D5D5" : "#ECECEC", "stroke-width": 1}, g);
    txt(g, m.l - 5, Y(v) + 3.5, fmtT(v), {"font-size": 10.5, fill: "#767676", "text-anchor": "end"});
  });
  const span = p.x1 - p.x0, step = span > 24 ? 10 : span > 10 ? 5 : span > 4 ? 2 : 1;
  for (let t = Math.ceil(p.x0 / step) * step; t < p.x1; t += step) txt(g, X(t), h - 5, String(t), {"font-size": 10.5, fill: "#767676", "text-anchor": "middle"});
  // the selected year, shaded whole
  if (S.y >= p.x0 && S.y < p.x1) el("rect", {x: X(S.y), y: m.t, width: Math.max(1, X(S.y + 1) - X(S.y)), height: h - m.t - m.b, fill: ACCENT, "fill-opacity": .07}, g);
  const clipId = `clip-${svg.id}-${Math.round(ox)}-${Math.round(oy)}`;
  el("rect", {x: m.l, y: m.t - 3, width: w - m.l - m.r, height: h - m.t - m.b + 6}, el("clipPath", {id: clipId}, el("defs", {}, g)));
  const body = el("g", {"clip-path": `url(#${clipId})`}, g);
  const q = {p, ox, oy, w, h, m, X, Y, g, labels: []};
  if (p.kind === "vdem") drawVdem(q, body); else drawRoll(q, body);
  if (p.anchors) {
    const halo = {stroke: "#fff", "stroke-width": 3, "paint-order": "stroke", "font-size": 10, fill: "#4B4B4B"};
    txt(g, m.l + 4, m.t + 9, p.anchors[1], halo); txt(g, m.l + 4, h - m.b - 4, p.anchors[0], halo);
  }
  if (p.rlab) placeLabels(q);
  q.hov = el("g", {class: "hov"}, g);
  return q;
}

function drawVdem(q, body) {
  const {p, X, Y} = q, m = p.m, i0 = yi(Math.max(S.from, m.from)), i1 = yi(S.to);
  const regs = p.regions || regionsShown();
  const iqr = p.iqr ?? regs.length === 1;
  regs.forEach((r, k) => {
    const rv = regVals(r, m, p.rev); if (!rv) return;
    if (iqr) el("path", {d: bandYears(rv.q25, rv.q75, X, Y, p.step, i0, i1), fill: "#000", "fill-opacity": .07}, body);
    el("path", {d: pathYears(rv.med, X, Y, p.step, i0, i1), fill: "none", stroke: MED, "stroke-width": 1.6, "stroke-dasharray": regs.length === 2 && k === 1 ? "1.5 3" : "5 3", "stroke-linecap": "round"}, body);
    if (p.rlab) { const i = lastIdx(rv.med, i1); if (i >= 0) q.labels.push({y: Y(rv.med[i]), text: "Median", colour: MED, x: X(Y0 + i + (p.step ? 1 : .5))}); }
  });
  q.ghosts = [];
  (p.ghosts || []).forEach(iso => {
    const s = vals(seriesOf(iso, m), m, p.rev); if (!s) return;
    const path = el("path", {d: pathYears(s.est, X, Y, p.step, i0, i1), fill: "none", stroke: "#000", "stroke-opacity": .16, "stroke-width": 1}, body);
    q.ghosts.push({iso, s, path});
  });
  const sel = (p.countries || S.c).filter(iso => !p.regionOnly || regionOf(iso) === p.regionOnly);
  sel.forEach(iso => {
    const s = vals(seriesOf(iso, m), m, p.rev); if (!s) return;
    if (p.cbands && S.u && s.lo) el("path", {d: bandYears(s.lo, s.hi, X, Y, p.step, i0, i1), fill: colourOf(iso), "fill-opacity": .10}, body);
    el("path", {d: pathYears(s.est, X, Y, p.step, i0, i1), fill: "none", stroke: colourOf(iso), "stroke-width": 2, "stroke-linejoin": "round"}, body);
    if (p.rlab) { const i = lastIdx(s.est, i1); if (i >= 0) q.labels.push({y: Y(s.est[i]), text: nameOf(iso), colour: "#141414", x: X(Y0 + i + (p.step ? 1 : .5))}); }
  });
  const P = vals(D.poland[m.id], m, p.rev);
  if (S.u && P.lo) el("path", {d: bandYears(P.lo, P.hi, X, Y, p.step, i0, i1), fill: PL, "fill-opacity": .16}, body);
  el("path", {d: pathYears(P.est, X, Y, p.step, i0, i1), fill: "none", stroke: PL, "stroke-width": 2.6, "stroke-linejoin": "round"}, body);
  if (p.rlab) { const i = lastIdx(P.est, i1); if (i >= 0) q.labels.unshift({y: Y(P.est[i]), text: "Poland", colour: PL, x: X(Y0 + i + (p.step ? 1 : .5))}); }
}
function lastIdx(v, i1) { for (let i = i1; i >= 0; i--) if (v[i] != null) return i; return -1; }

function drawRoll(q, body) {
  const {p, X, Y} = q, r = D.rollcall[p.which], t = rollT();
  const keep = t.map((x, i) => x >= p.x0 && x <= p.x1 && r.value[i] != null);
  if (S.u) {
    let d = "", run = [];
    const flush = () => { if (run.length > 1) d += "M" + run.map(i => `${X(t[i]).toFixed(1)},${Y(r.hi[i]).toFixed(1)}`).join("L") + "L" + run.slice().reverse().map(i => `${X(t[i]).toFixed(1)},${Y(r.lo[i]).toFixed(1)}`).join("L") + "Z"; run = []; };
    t.forEach((_, i) => { if (keep[i] && r.lo[i] != null && r.hi[i] != null) run.push(i); else flush(); }); flush();
    el("path", {d, fill: "#141414", "fill-opacity": .10}, body);
  }
  let d = "", pen = false;
  t.forEach((x, i) => { if (!keep[i]) { pen = false; return; } d += `${pen ? "L" : "M"}${X(x).toFixed(1)},${Y(r.value[i]).toFixed(1)}`; pen = true; });
  el("path", {d, fill: "none", stroke: "#141414", "stroke-width": 1.8, "stroke-linejoin": "round"}, body);
  t.forEach((x, i) => { if (keep[i]) el("circle", {cx: X(x), cy: Y(r.value[i]), r: 1.8, fill: "#141414"}, body); });
}

function placeLabels(q) {
  const ls = q.labels.sort((a, b) => a.y - b.y);
  for (let k = 1; k < ls.length; k++) if (ls[k].y - ls[k - 1].y < 13) ls[k].y = ls[k - 1].y + 13;
  const over = ls.length ? ls[ls.length - 1].y - (q.h - q.m.b) : 0;
  if (over > 0) ls.forEach(l => { l.y -= over; });
  ls.forEach(l => txt(q.g, q.w - q.m.r + 6, l.y + 4, l.text, {"font-size": 11.5, "font-weight": 500, fill: l.colour, stroke: "#fff", "stroke-width": 3, "paint-order": "stroke"}));
}

function drawRegime(svg, ox, oy, W) {
  const g = el("g", {transform: `translate(${ox},${oy})`}, svg);
  const ml = 64, X = v => ml + (v - S.from) / (S.to + 1 - S.from) * (W - ml - 8);
  txt(g, ml - 6, 18, "Regime", {"font-size": 10, fill: "#767676", "text-anchor": "end", "letter-spacing": ".06em"});
  const fills = ["#EFEAE8", "#D8CFCC", "#B5A9A5", "#7E7572"], row = D.regime.row, labels = D.regime.row_labels;
  let i = yi(S.from);
  while (i <= yi(S.to)) {
    let j = i; while (j + 1 <= yi(S.to) && row[j + 1] === row[i]) j++;
    const a = X(Y0 + i), b = X(Y0 + j + 1), c = row[i];
    if (c != null) {
      const r = el("rect", {x: a, y: 4, width: Math.max(1, b - a - 1), height: 20, rx: 3, fill: fills[c]}, g);
      el("title", {}, r).textContent = `${labels[c]}, ${Y0 + i}–${Y0 + j} (Regimes of the World)`;
      const lab = labels[c] + (b - a > 190 ? `, ${Y0 + i}${j > i ? "–" + (Y0 + j) : ""}` : "");
      if (b - a > lab.length * 6.2 + 10) txt(g, a + 6, 18, lab, {"font-size": 11, fill: ink(fills[c]), "pointer-events": "none"});
    }
    i = j + 1;
  }
}

// ---- the context strip ------------------------------------------------------------------------
function drawStrip(svg, ox, oy, W, mr) {
  const g = el("g", {transform: `translate(${ox},${oy})`}, svg);
  const ml = 64, r = mr ?? 8, x0 = S.from, x1 = S.to + 1;
  const X = v => ml + (v - x0) / (x1 - x0) * (W - ml - r);
  const rows = [26, 46, 66, 86];
  el("rect", {x: ml - .5, y: 12, width: W - ml - r + 1, height: 88, rx: 6, fill: "none", stroke: "#ECECEC"}, g);
  txt(g, W - r, 8, "Context (not V-Dem data): select an item", {"font-size": 10, fill: "#767676", "text-anchor": "end", "letter-spacing": ".04em"});
  ["Sejm", "Cabinet", "President", "Events"].forEach((s, i) => txt(g, ml - 6, rows[i] + 3.5, s, {"font-size": 10, fill: "#767676", "text-anchor": "end", "letter-spacing": ".06em"}));
  if (S.y >= x0 && S.y < x1) el("rect", {x: X(S.y), y: 13, width: X(S.y + 1) - X(S.y), height: 86, fill: ACCENT, "fill-opacity": .07}, g);
  const clipId = `clip-${svg.id}-strip`;
  el("rect", {x: ml, y: 12, width: W - ml - r, height: 90}, el("clipPath", {id: clipId}, el("defs", {}, g)));
  const body = el("g", {"clip-path": `url(#${clipId})`}, g);
  const sel = id => S.ev === id ? {stroke: "#141414", "stroke-width": 2} : {};
  const item = (node, id, label) => { node.setAttribute("data-ev", id); node.setAttribute("tabindex", "0"); node.setAttribute("role", "button"); node.setAttribute("aria-label", label); node.style.cursor = "pointer"; el("title", {}, node).textContent = label; };
  CX.elections.forEach(e => {
    const x = X(dec(e.date)); if (x < ml || x > W - r) return;
    const gg = el("g", {}, body);
    el("line", {x1: x, x2: x, y1: rows[0] - 7, y2: rows[0] + 7, stroke: S.ev === e.id ? "#141414" : "#4B4B4B", "stroke-width": S.ev === e.id ? 2.5 : 1.3}, gg);
    txt(gg, x + 3, rows[0] + 4, "’" + String(e.year).slice(2), {"font-size": 10.5, fill: "#4B4B4B"});
    el("rect", {x: x - 5, y: rows[0] - 9, width: 26, height: 18, fill: "transparent"}, gg);
    item(gg, e.id, `Sejm election, ${dateLabel(e.date)}`);
  });
  const bar = (row, a, b, fill, id, label, short, stroke) => {
    const xa = Math.max(X(a), ml), xb = Math.min(X(b), W - r); if (xb <= ml || xa >= W - r) return;
    const gg = el("g", {}, body);
    el("rect", {x: xa, y: rows[row] - 7, width: Math.max(1.5, xb - xa - 1), height: 14, rx: 2, fill, stroke: stroke || "none", ...sel(id)}, gg);
    if (xb - xa > short.length * 6 + 8) txt(gg, xa + 4, rows[row] + 4, short, {"font-size": 10.5, fill: ink(fill), "font-weight": 500, "pointer-events": "none"});
    item(gg, id, label);
  };
  CX.cabinets.forEach(c => bar(1, dec(c.start), c.end ? dec(c.end) : x1 + 1, c.colour, c.id, `${c.pm} cabinet (${c.pmParty}), ${dateLabel(c.start)} – ${c.end ? dateLabel(c.end) : "in office"}`, c.pm.split(" ").pop()));
  CX.presidents.forEach(p => bar(2, dec(p.start), p.end ? dec(p.end) : x1 + 1, "#F3F3F1", p.id, `President ${p.name}${p.acting ? " (acting)" : ""}, ${dateLabel(p.start)} – ${p.end ? dateLabel(p.end) : "in office"}`, p.short, "#D5D5D5"));
  CX.events.forEach(e => {
    const x = X(dec(e.date)); if (x < ml || x > W - r) return;
    const gg = el("g", {}, body);
    el("path", {d: `M${x},${rows[3] - 6}L${x + 6},${rows[3]}L${x},${rows[3] + 6}L${x - 6},${rows[3]}Z`, fill: S.ev === e.id ? "#141414" : "#fff", stroke: "#4B4B4B", "stroke-width": 1.3}, gg);
    item(gg, e.id, `${e.label}, ${dateLabel(e.date)}`);
  });
  return {ox, oy, X, ml, W, r};
}
function evItem(id) {
  for (const [k, list] of [["election", CX.elections], ["cabinet", CX.cabinets], ["president", CX.presidents], ["event", CX.events]]) {
    const x = list.find(o => o.id === id); if (x) return {k, x};
  }
  return null;
}
function cardHTML() {
  const after = `<span class="s">After ${Y1}: ${link(SITE + "polling-tracker.html", "where the parties stand now, in the Polling tracker →")}</span>`;
  const it = S.ev && evItem(S.ev);
  if (!it || !S.ctx) return S.ctx ? `<p class="help">Select an election, a cabinet, a president or an event in the context strip for its dates, its source and links to the other analyses. ${after}</p>` : "";
  const x = it.x, close = `<button type="button" class="x" data-close="1">Close ✕</button>`;
  if (it.k === "election") {
    const l = x.inPartySystemIndicators ? link(SITE + `party-system-indicators.html#e=${x.year}`, `Party system indicators at the ${x.year} election →`) : `<span class="s">Party system indicators begins with the 1991 election.</span>`;
    return `<div class="evcard">${close}<b>Sejm election, ${dateLabel(x.date)}</b><div class="links">${l}</div><span class="s">Context, not V-Dem data. V-Dem's ${x.year} values average the whole year, before and after the election.</span></div>`;
  }
  if (it.k === "cabinet") {
    const links = [];
    if (x.election && x.election >= 1991) links.push(link(SITE + `party-system-indicators.html#e=${x.election}&s=government`, `Governments after the ${x.election} election →`));
    links.push(link(SITE + `party-genealogy.html#pt=${encodeURIComponent(x.pmPartyId)}`, `${x.pmParty} in the party genealogy →`));
    links.push(link(x.source, "Source", false));
    return `<div class="evcard">${close}<b>${esc(x.name)}</b><span>Prime minister ${esc(x.pm)} (${esc(x.pmParty)}), ${dateLabel(x.start)} – ${x.end ? dateLabel(x.end) : "in office"}. Parties: ${esc(x.parties.join(", "))}.</span><div class="links">${links.join("")}</div><span class="s">From Party tracker. Context, not V-Dem data.</span></div>`;
  }
  if (it.k === "president") return `<div class="evcard">${close}<b>President ${esc(x.name)}${x.acting ? " (acting)" : ""}</b><span>${dateLabel(x.start)} – ${x.end ? dateLabel(x.end) : "in office"}${x.note ? ". " + esc(x.note) : ""}.</span><div class="links">${link(x.source, "Source", false)}</div></div>`;
  return `<div class="evcard">${close}<b>${esc(x.label)}</b><span>${dateLabel(x.date)}${x.note ? ". " + esc(x.note) : ""}.</span><div class="links">${link(x.source, "Source", false)}</div><span class="s">Shown for context. Its place beside a score does not make it the cause of any change.</span></div>`;
}

// ---- hover, click, keys -----------------------------------------------------------------------
function point(svg, e) { const b = svg.getBoundingClientRect(), vb = svg.viewBox.baseVal; const sc = vb.width / b.width; return {x: (e.clientX - b.left) * sc, y: (e.clientY - b.top) * sc, sc}; }
function panelAt(geo, pt) { return geo.panels.find(q => pt.x >= q.ox + q.m.l && pt.x <= q.ox + q.w - q.m.r && pt.y >= q.oy + q.m.t - 6 && pt.y <= q.oy + q.h - q.m.b + 6); }
function yearAt(q, pt) { const x = pt.x - q.ox; return Math.floor(q.p.x0 + (x - q.m.l) / (q.w - q.m.l - q.m.r) * (q.p.x1 - q.p.x0)); }
function nearestGhost(q, yr, pt) {
  if (!q.ghosts || !q.ghosts.length) return null;
  const i = yi(yr); let best = null, bd = 9;
  q.ghosts.forEach(gh => { const v = gh.s.est[i]; if (v == null) return; const d = Math.abs(q.oy + q.Y(v) - pt.y); if (d < bd) { bd = d; best = gh; } });
  return best;
}
function hover(key, e) {
  const geo = GEO[key], svg = $("fig-" + key), tip = $("tip-" + key); if (!geo) return;
  const pt = point(svg, e), q = panelAt(geo, pt);
  geo.panels.forEach(o => { o.hov.innerHTML = ""; });
  if (geo.hot) { geo.hot.path.setAttribute("stroke-opacity", .16); geo.hot.path.setAttribute("stroke-width", 1); geo.hot = null; }
  if (!q) { tip.hidden = true; return; }
  let h = "";
  if (q.p.kind === "roll") {
    const t = rollT(), x = q.p.x0 + (pt.x - q.ox - q.m.l) / (q.w - q.m.l - q.m.r) * (q.p.x1 - q.p.x0);
    let i = -1, bd = 1; t.forEach((v, k) => { if (Math.abs(v - x) < bd) { bd = Math.abs(v - x); i = k; } });
    if (i < 0 || D.rollcall[q.p.which].value[i] == null) { tip.hidden = true; return; }
    const r = D.rollcall[q.p.which], f = q.p.which === "opposed" ? pct : f2;
    el("circle", {cx: q.X(t[i]), cy: q.Y(r.value[i]), r: 4, fill: "#141414", stroke: "#fff", "stroke-width": 1.5}, q.hov);
    h = `<b>${D.rollcall.periods[i].replace("H", ", half ")}</b><div class="r"><span>${q.p.which === "opposed" ? "Opposed lines" : "Distance"}</span><span><b>${f(r.value[i])}</b> <span class="s">${f(r.lo[i])}–${f(r.hi[i])}</span></span></div>` +
      (q.p.which === "opposed" ? `<div class="s">${r.n[i]} contested roll calls</div>` : "") + `<div class="s">Bootstrap interval, a lower bound</div>`;
  } else {
    const yr = clamp(yearAt(q, pt), q.p.x0, q.p.x1 - 1), i = yi(yr), m = q.p.m;
    el("rect", {x: q.X(yr), y: q.m.t, width: Math.max(1, q.X(yr + 1) - q.X(yr)), height: q.h - q.m.t - q.m.b, fill: "#4B4B4B", "fill-opacity": .07}, q.hov);
    const P = vals(D.poland[m.id], m, q.p.rev);
    h = `<b>${esc(q.p.title)}, ${yr}</b>`;
    const row = (name, colour, s, dash) => {
      if (!s || s.est[i] == null) return "";
      el("circle", {cx: q.X(yr + (q.p.step ? .5 : .5)), cy: q.Y(s.est[i]), r: 3.6, fill: colour, stroke: "#fff", "stroke-width": 1.5}, q.hov);
      return `<div class="r"><span><span class="dot" style="background:${colour}"></span> ${esc(name)}</span><span><b>${f2(s.est[i])}</b>${s.lo && S.u ? ` <span class="s">${f2(s.lo[i])}–${f2(s.hi[i])}</span>` : ""}</span></div>`;
    };
    h += row("Poland", PL, P);
    (q.p.regions || regionsShown()).forEach(r => { const rv = regVals(r, m, q.p.rev); if (rv && rv.med[i] != null) h += `<div class="r"><span>Median, ${REG_SHORT[r]} <span class="s">(${rv.n[i]})</span></span><span>${f2(rv.med[i])} <span class="s">${f2(rv.q25[i])}–${f2(rv.q75[i])}</span></span></div>`; });
    (q.p.countries || S.c).filter(iso => !q.p.regionOnly || regionOf(iso) === q.p.regionOnly).forEach(iso => { h += row(nameOf(iso), colourOf(iso), vals(seriesOf(iso, m), m, q.p.rev)); });
    const gh = nearestGhost(q, yr, pt);
    if (gh) { geo.hot = gh; gh.path.setAttribute("stroke-opacity", .7); gh.path.setAttribute("stroke-width", 1.8); h += `<div class="r"><span>${esc(nameOf(gh.iso))}</span><span>${f2(gh.s.est[i])}</span></div><div class="s">Click to ${S.c.includes(gh.iso) ? "remove it from" : "add it to"} the comparison</div>`; }
    else h += `<div class="s">${S.u && P.lo ? "68% intervals. " : ""}Click to select ${yr}</div>`;
  }
  tip.innerHTML = h; tip.hidden = false;
  const box = svg.getBoundingClientRect(), x = pt.x / pt.sc, y = pt.y / pt.sc, tw = tip.offsetWidth, th = tip.offsetHeight;
  let left = x + 18; if (left + tw > box.width) left = x - tw - 14;
  tip.style.left = (Math.max(0, left) + 12) + "px"; tip.style.top = (clamp(y - th / 2, 0, Math.max(0, box.height - th)) + 10) + "px";
}
function unhover(key) {
  const geo = GEO[key]; if (geo) { geo.panels.forEach(o => { o.hov.innerHTML = ""; }); if (geo.hot) { geo.hot.path.setAttribute("stroke-opacity", .16); geo.hot.path.setAttribute("stroke-width", 1); geo.hot = null; } }
  const tip = $("tip-" + key); if (tip) tip.hidden = true;
}
function wire(key) {
  const svg = $("fig-" + key);
  svg.setAttribute("tabindex", "0");
  svg.onpointermove = e => { if (e.pointerType === "mouse") hover(key, e); };
  svg.onpointerleave = () => unhover(key);
  svg.onclick = e => {
    const it = e.target.closest("[data-ev]");
    if (it) { S.ev = S.ev === it.dataset.ev ? null : it.dataset.ev; render(); return; }
    const geo = GEO[key], pt = point(svg, e), q = panelAt(geo, pt); if (!q) return;
    if (q.p.kind === "roll") { hover(key, e); return; }
    const yr = clamp(yearAt(q, pt), q.p.x0, q.p.x1 - 1), gh = nearestGhost(q, yr, pt);
    if (gh) { toggleCountry(gh.iso); return; }
    S.y = clamp(yr, S.from, S.to); render();
    if (e.pointerType !== "mouse") hover(key, e);
  };
  svg.onkeydown = e => {
    const it = e.target.closest && e.target.closest("[data-ev]");
    if (it && (e.key === "Enter" || e.key === " ")) { e.preventDefault(); S.ev = it.dataset.ev; render(); return; }
    const k = {ArrowLeft: -1, ArrowRight: 1}[e.key];
    if (k) { e.preventDefault(); S.y = clamp(S.y + k, S.from, S.to); render(); }
  };
}

// ---- views -------------------------------------------------------------------------------------
function legendHTML(opt = {}) {
  const parts = [`<span><i class="sw line" style="border-color:${PL}"></i>Poland</span>`];
  if (S.u) parts.push(`<span><i class="sw band"></i>Poland's 68% interval</span>`);
  const regs = opt.regions || regionsShown();
  if (regs.length === 1) parts.push(`<span><i class="sw dash"></i>Median of the other ${regs[0] === "cee" ? "CEE members" : "Western European countries"}</span>`, `<span><i class="sw iqr"></i>Their middle half</span>`);
  if (regs.length === 2 && !opt.split) parts.push(`<span><i class="sw dash"></i>Median, other CEE members</span>`, `<span><i class="sw dots"></i>Median, Western Europe</span>`);
  if (regs.length === 2 && opt.split) parts.push(`<span><i class="sw dash"></i>Median of the other countries</span>`, `<span><i class="sw iqr"></i>Their middle half</span>`);
  if (opt.ghosts) parts.push(`<span><i class="sw ghost"></i>Each other country (click to compare)</span>`);
  S.c.forEach(iso => parts.push(`<span><i class="sw line" style="border-color:${colourOf(iso)}"></i>${esc(nameOf(iso))}</span>`));
  if (opt.roll) parts.push(`<span><i class="sw line" style="border-color:#141414"></i>Sejm roll calls, by half-year</span>`);
  return parts.join("");
}
function readoutHTML(panels) {
  const y = S.y, i = yi(y), b = yi(S.from), regs = regionsShown();
  let h = `<table class="t"><thead><tr><th>Measure</th><th>Poland, ${y} <span class="s">(68% interval)</span></th><th>Change since ${S.from}</th>${regs.map(r => `<th>Median, ${REG_SHORT[r]}</th>`).join("")}${S.c.map(c => `<th>${esc(nameOf(c))}</th>`).join("")}</tr></thead><tbody>`;
  panels.filter(p => p.kind === "vdem").forEach(p => {
    const m = p.m, P = vals(D.poland[m.id], m, p.rev), bi = Math.max(b, yi(m.from));
    const ch = P.est[i] != null && P.est[bi] != null ? P.est[i] - P.est[bi] : null;
    const overlap = P.lo && ch != null && !(P.lo[i] > P.hi[bi] || P.hi[i] < P.lo[bi]);
    h += `<tr><td>${esc(p.title)} <span class="id">${esc(m.shown)}</span></td><td>${f2(P.est[i])}${P.lo ? ` <span class="s">${f2(P.lo[i])}–${f2(P.hi[i])}</span>` : ""}</td>` +
      `<td>${y === Y0 + bi ? "–" : sgn(ch)}${overlap ? ` <span class="within">intervals overlap</span>` : ""}${bi > b ? ` <span class="s">since ${Y0 + bi}</span>` : ""}</td>` +
      regs.map(r => { const rv = regVals(r, m, p.rev); return `<td>${rv ? f2(rv.med[i]) : "–"}</td>`; }).join("") +
      S.c.map(c => { const s = vals(seriesOf(c, m), m, p.rev); return `<td>${s ? f2(s.est[i]) : "…"}</td>`; }).join("") + `</tr>`;
  });
  return h + `</tbody></table><p class="help" style="margin-top:6px">"Intervals overlap" marks a change smaller than the uncertainty around the two years' scores: read it as no clear change. Select a year on a chart, or focus a chart and use the arrow keys.</p>`;
}

function renderOverview() {
  const L = M.get("v2x_libdem"), P = D.poland[L.id], i0 = yi(S.from), i1 = yi(S.to);
  let lo = i0, hi = i0; for (let i = i0; i <= i1; i++) { if (P.est[i] < P.est[lo]) lo = i; if (P.est[i] > P.est[hi]) hi = i; }
  $("ovlead").textContent = `Poland's liberal democracy score was ${f2(P.est[i1])} in ${S.to} (68% interval ${f2(P.lo[i1])}–${f2(P.hi[i1])}); between ${S.from} and ${S.to} its highest was ${f2(P.est[hi])}, in ${Y0 + hi}, and its lowest ${f2(P.est[lo])}, in ${Y0 + lo}. Regimes of the World classes Poland as a${/^[AEIOU]/.test(D.regime.row_labels[D.regime.row[i1]]) ? "n" : ""} ${D.regime.row_labels[D.regime.row[i1]].toLowerCase()} in ${S.to}.`;
  const panels = OVERVIEW.map(id => vdemPanel(M.get(id)));
  drawFigure("overview", [{kind: "regime"}, ...panels], {strip: true});
  $("leg-overview").innerHTML = legendHTML();
  $("card-overview").innerHTML = cardHTML();
  $("read-overview").innerHTML = readoutHTML(panels);
}
function renderExplain() {
  const lens = LENSES.find(l => l[0] === S.l);
  $("lenses").innerHTML = LENSES.map(l => `<button type="button" role="tab" data-l="${l[0]}" aria-pressed="${l[0] === S.l}" aria-selected="${l[0] === S.l}">${l[1]}</button>`).join("");
  $("lenslead").textContent = lens[2];
  const panels = D.measures.filter(m => m.lenses.includes(S.l) && m.kind !== "category").map(m => vdemPanel(m));
  drawFigure("explain", panels, {strip: true});
  $("leg-explain").innerHTML = legendHTML();
  $("card-explain").innerHTML = cardHTML();
  $("read-explain").innerHTML = readoutHTML(panels);
}
function renderContext() {
  const m = MS.get(S.m) && MS.get(S.m).kind !== "category" ? MS.get(S.m) : M.get("v2x_libdem");
  const sel = $("measure");
  if (!sel.options.length) {
    const groups = [["overall", "Democracy indices"], ...LENSES.map(l => [l[0], l[1]]), ["polarisation", "Polarisation"]];
    sel.innerHTML = groups.map(([g, lab]) => `<optgroup label="${esc(lab)}">` + D.measures.filter(x => x.lenses[0] === g && x.kind !== "category").map(x => `<option value="${x.short}">${esc(x.label)}</option>`).join("") + `</optgroup>`).join("");
  }
  sel.value = m.short;
  document.querySelectorAll("#cmode button").forEach(b => b.setAttribute("aria-pressed", b.dataset.cm === S.cm));
  document.querySelectorAll("#csort button").forEach(b => b.setAttribute("aria-pressed", b.dataset.so === S.so));
  const sets = S.r === "cee" ? ["cee"] : S.r === "we" ? ["we"] : ["cee", "we"];
  let items, help;
  if (S.cm === "countries") {
    items = [vdemPanel(m, {regions: [], countries: S.c, cbands: true, rlab: true, title: m.label})];
    help = S.c.length ? `Poland and the countries you have chosen, each with its 68% interval. Overlapping bands mean the difference in that year is within V-Dem's uncertainty.` : `Add up to ${MAXC} countries from the list above, or from the table below, to compare them with Poland.`;
  } else {
    items = sets.map(r => vdemPanel(m, {regions: [r], ghosts: D.regions.sets[r].members.map(x => x.id).filter(x => x !== "POL"), regionOnly: r, iqr: true, rlab: true,
      title: `${D.regions.sets[r].label}`, sub: `${m.label} · ${m.shown}${m.up !== "more democratic" ? " · ↑ " + m.up : ""}`}));
    help = `The dashed line is the median of the other countries in each set and the grey band their middle half; Poland is never part of its own benchmark. ${S.r === "pl" ? "Choose a comparison above to show one set alone. " : ""}Hover over a faint line to see which country it is, and click it to add it to the comparison.`;
  }
  $("cxhelp").textContent = help;
  drawFigure("context", items, {strip: true, cols: items.length, ph: 300, stripR: 86});
  $("leg-context").innerHTML = legendHTML({regions: S.cm === "countries" ? [] : sets, split: sets.length === 2, ghosts: S.cm !== "countries"});
  $("card-context").innerHTML = cardHTML();
  renderCards(m);
  renderRanks(m);
}
function renderCards(m) {
  const i = yi(S.y), t = yi(S.to), b = Math.max(yi(S.from), yi(m.from)), P = D.poland[m.id];
  $("ccards").innerHTML = S.c.map(iso => {
    const s = seriesOf(iso, m); if (!s) return `<div class="card"><h3><span class="dot" style="background:${colourOf(iso)}"></span>${esc(nameOf(iso))}</h3><span class="help">Loading…</span></div>`;
    const gap = s.est[i] != null && P.est[i] != null ? s.est[i] - P.est[i] : null;
    const overlap = s.lo && P.lo && gap != null && !(s.lo[i] > P.hi[i] || s.hi[i] < P.lo[i]);
    return `<div class="card"><h3><span class="dot" style="background:${colourOf(iso)}"></span>${esc(nameOf(iso))} <button type="button" class="btn2" style="height:24px;padding:0 8px;margin-left:auto;font-size:12px" data-rm="${iso}">Remove</button></h3><div class="kv">` +
      `<span>${S.to}</span><span>${f2(s.est[t])}</span><span>Change since ${Y0 + b}</span><span>${sgn(s.est[t] - s.est[b])}</span>` +
      `<span>Gap to Poland, ${S.y}</span><span>${sgn(gap)}</span></div>${overlap ? `<span class="within">In ${S.y} the two intervals overlap.</span>` : ""}</div>`;
  }).join("");
}
function renderRanks(m) {
  const members = r => D.regions.sets[r].members.map(x => x.id);
  const scope = S.r === "cee" ? members("cee") : S.r === "we" ? ["POL", ...members("we")] : ["POL", ...D.countries.map(c => c.id)];
  const i = yi(S.y), b = Math.max(yi(S.from), yi(m.from));
  const rows = scope.map(iso => { const s = seriesOf(iso, m); return s ? {iso, v: s.est[i], lo: s.lo ? s.lo[i] : null, hi: s.hi ? s.hi[i] : null, ch: s.est[i] != null && s.est[b] != null ? s.est[i] - s.est[b] : null} : {iso, v: null}; });
  const key = S.so === "change" ? "ch" : "v";
  const ranked = rows.filter(r => r[key] != null).sort((a, c) => c[key] - a[key]);
  const P = rows.find(r => r.iso === "POL"), pr = ranked.findIndex(r => r.iso === "POL") + 1;
  const line = (r, k) => `<tr data-c="${r.iso}" class="${r.iso === "POL" ? "pl" : ""}${S.c.includes(r.iso) ? " on" : ""}"><td>${k}. ${S.c.includes(r.iso) ? `<span class="dot" style="background:${colourOf(r.iso)}"></span> ` : ""}${esc(nameOf(r.iso))}</td><td>${r.iso === "POL" ? "CEE" : REG_SHORT[regionOf(r.iso)]}</td>` +
    `<td>${f2(r.v)}${r.lo != null ? ` <span class="s">${f2(r.lo)}–${f2(r.hi)}</span>` : ""}</td><td>${sgn(r.ch)}</td><td>${r.iso === "POL" ? "–" : sgn(r.v != null && P.v != null ? r.v - P.v : null)}</td></tr>`;
  const scopeLabel = S.r === "cee" ? "Central and Eastern Europe" : S.r === "we" ? "Poland and Western Europe" : "all 28 countries";
  $("ranks").innerHTML = C ? `<table class="t"><thead><tr><th>${esc(m.label)}, ${scopeLabel} <span class="s">(Poland first)</span></th><th>Region</th><th>${S.y} <span class="s">(68% interval)</span></th><th>Change since ${Y0 + b}</th><th>Gap to Poland</th></tr></thead><tbody>` +
    (pr ? line(P, pr) : "") + ranked.filter(r => r.iso !== "POL").map(r => line(r, ranked.indexOf(r) + 1)).join("") + `</tbody></table><p class="help" style="margin-top:6px">↑ ${esc(m.up)}. Ranks are by the point estimate; neighbouring countries' intervals often overlap. A dash for change means no score in the first year: Croatia enters V-Dem in 1991 and Slovakia in 1993.</p>` : `<p class="help">Loading the comparison countries…</p>`;
}
function renderPolarisation() {
  $("pzlead").textContent = "V-Dem's experts' judgement of polarisation in Poland, against the other countries. Two of the three measures run the other way in V-Dem, with 0 the most polarised; here they are reversed and marked so, so that up means more polarised on all three. Poland's political polarisation has been at or near the top of its scale since 2016, so the flat line from 2021 cannot show a further rise. Below them are three measures of elite conduct, each in its own direction, and, from my own data, the Sejm's roll calls.";
  const items = [{kind: "heading", text: "Polarisation in V-Dem"}, ...POLAR_MAIN.map(id => vdemPanel(M.get(id), {rev: true})),
    {kind: "heading", text: "Elite conduct and violence in V-Dem"}, ...POLAR_MORE.map(id => vdemPanel(M.get(id)))];
  if (S.to >= 2006) items.push({kind: "heading", text: "From this site's own data: PiS and PO/KO in the Sejm's roll calls"}, rollPanel("distance"), rollPanel("opposed"));
  drawFigure("polarisation", items, {strip: true});
  $("leg-polarisation").innerHTML = legendHTML({roll: true});
  $("card-polarisation").innerHTML = cardHTML();
  const scope = S.r === "cee" ? "cee" : S.r === "we" ? "eu" : "cee";
  $("pzlinks").innerHTML = `The roll-call measures are the distance between the two blocs' MPs on the first dimension of a dynamic IRT model linking every non-lopsided vote since 2005, and the share of contested votes on which their club lines opposed each other; their bootstrap intervals are lower bounds. Polarisation of party positions: ${link(SITE + "party-system-indicators.html#s=polarisation&p=galtan", "Party system indicators →")}, and across Europe, ${link(SITE + `party-ideology.html#view=systems&s=${scope}&m=galtan`, "Party ideology's Party systems view →")}`;
  $("read-polarisation").innerHTML = readoutHTML(items);
}
function renderMethods() {
  const mt = D.meta, sets = D.regions.sets;
  const lensName = id => (LENSES.find(l => l[0] === id) || [id, id === "overall" ? "Democracy indices" : id === "polarisation" ? "Polarisation" : id])[1];
  const rows = D.measures.map(m => `<tr><td>${esc(m.label)}</td><td><span class="id">${esc(m.shown)}</span></td><td>${esc(m.lenses.map(lensName).join("; "))}</td><td>${m.kind === "category" ? "categories 0–3" : `${m.kind} ${m.scale[0]}–${m.scale[1]}`}</td><td>${esc(m.up)}</td><td>${m.from}</td></tr>`).join("");
  $("methods").innerHTML = `
    <h3>Source</h3>
    <p>${esc(mt.source)} (released ${esc(monthLabel(mt.released))}), from the ${esc(mt.package)} package, downloaded on ${esc(dateLabel(mt.downloaded).replace(/ (\w{3}) /, (_, mo) => " " + MONTHS[MON.indexOf(mo)] + " "))}. Annual data from ${mt.years[0]} to ${mt.lastYear}: nothing later is shown. ${link(mt.url, "V-Dem", false)} revises past estimates in every release, so this page uses one edition throughout and never mixes them.</p>
    <h3>How to read the scores</h3>
    <ul>
      <li>V-Dem's country experts rate each country, year by year; a statistical measurement model combines their ratings, adjusting for how differently experts use the scales, and gives each score its uncertainty. Each band is the ${esc(mt.interval)}. Two scores whose intervals overlap may not differ.</li>
      <li>${esc(mt.aggregation)}. So 2015 is mostly months of the PO–PSL government (the Szydło cabinet took office on 16 November), and 2023 mostly months of PiS government (Tusk's cabinet took office on 13 December).</li>
      <li>Indices run from 0 to 1. Indicators are shown on V-Dem's original response scale, with the answers at its two ends on the chart; most run 0–4, a few 0–2, 0–3 or 0–5. An axis never mixes the two.</li>
      <li>Regional lines: ${esc(mt.regional.charAt(0).toLowerCase() + mt.regional.slice(1))}. ${esc(sets.cee.label)}: ${esc(sets.cee.members.map(x => x.name).join(", "))}. ${esc(sets.we.label)}: ${esc(sets.we.members.map(x => x.name).join(", "))}. Croatia enters V-Dem in 1991 and Slovakia in 1993. The median is a description of the countries, not an estimate of a regional level of democracy.</li>
      <li>In the Polarisation view, polarisation of society and parties' hate speech are reversed (the scale's top minus V-Dem's value) and marked "reversed"; downloads keep V-Dem's values.</li>
      <li>The context strip comes from my Polish party tracker (elections, cabinets) and short lists of presidents and institutional events, each with its source. It places events beside the scores; it does not show that any event caused any change.</li>
    </ul>
    <h3>Downloads and links</h3>
    <p>CSV downloads hold exactly what the current view shows, with V-Dem's variable names; PNG downloads the current chart. Copy link gives the address of this page with every setting, which other pages also use to link here.</p>
    <h3>Licence and citation</h3>
    <p>V-Dem's data are ${link(mt.licenceUrl, mt.licence, false)}, and so are the values shown and downloaded here. Please cite: ${esc(mt.citation.dataset)} And: ${esc(mt.citation.model)}</p>
    <h3>Measures</h3>
    <div class="tscroll"><table class="t left"><thead><tr><th>Measure</th><th>V-Dem variable</th><th>Lens</th><th>Scale</th><th>Higher means</th><th>From</th></tr></thead><tbody>${rows}</tbody></table></div>
    <p class="help">Data file ${esc(mt.file)}, SHA-256 ${esc(mt.sha256.slice(0, 12))}…, extracted ${esc(mt.extracted.slice(0, 10))}.</p>`;
}

// ---- controls ----------------------------------------------------------------------------------
function renderControls() {
  document.querySelectorAll("#views button").forEach(b => { const on = b.dataset.v === S.view; b.setAttribute("aria-pressed", on); b.setAttribute("aria-selected", on); });
  VIEWS.forEach(v => { $("v-" + v).hidden = v !== S.view; });
  $("from").innerHTML = D.years.slice(0, -1).map(y => `<option${y === S.from ? " selected" : ""}>${y}</option>`).join("");
  $("to").innerHTML = D.years.filter(y => y > S.from).map(y => `<option${y === S.to ? " selected" : ""}>${y}</option>`).join("");
  $("regions").innerHTML = REGIONS.map(([k, l]) => `<button type="button" data-r="${k}" aria-pressed="${k === S.r}">${l}</button>`).join("");
  const opts = r => D.countries.filter(c => c.region === r && !S.c.includes(c.id)).map(c => `<option value="${c.id}">${esc(c.name)}</option>`).join("");
  $("addc").innerHTML = `<option value="">${S.c.length >= MAXC ? "Five countries chosen" : "Add a country…"}</option><optgroup label="Central and Eastern Europe">${opts("cee")}</optgroup><optgroup label="Western Europe">${opts("we")}</optgroup>`;
  $("addc").disabled = S.c.length >= MAXC;
  $("chips").innerHTML = S.c.map(iso => `<span class="chip"><span class="dot" style="background:${colourOf(iso)}"></span>${esc(nameOf(iso))}<button type="button" data-rm="${iso}" aria-label="Remove ${esc(nameOf(iso))}">×</button></span>`).join("");
  $("u").checked = S.u; $("ctx").checked = S.ctx;
  $("dlpng").disabled = $("dlcsv").disabled = S.view === "methods";
}
function render() {
  if (needCountries() && !C) loadCountries().then(() => { if (C) render(); });
  renderControls();
  ({overview: renderOverview, explain: renderExplain, context: renderContext, polarisation: renderPolarisation, methods: renderMethods})[S.view]();
  saveHash();
}
const renderSoon = debounce(() => { if (S.view !== "methods") render(); }, 120);

// ---- links -------------------------------------------------------------------------------------
function hashTarget() { try { if (window.top !== window && window.top.location.host === location.host) return window.top; } catch (e) { /* cross-origin */ } return window; }
function stateString() {
  const q = new URLSearchParams();
  if (S.view !== "overview") q.set("view", S.view);
  if (S.m !== "libdem") q.set("m", S.m);
  if (S.l !== "elections") q.set("l", S.l);
  if (S.from !== Y0) q.set("from", S.from);
  if (S.to !== Y1) q.set("to", S.to);
  if (S.y !== S.to) q.set("y", S.y);
  if (S.r !== "pl") q.set("r", S.r);
  if (S.c.length) q.set("c", S.c.join(","));
  if (!S.u) q.set("u", "0");
  if (!S.ctx) q.set("ctx", "0");
  if (S.ev) q.set("ev", S.ev);
  if (S.cm !== "region") q.set("cm", S.cm);
  if (S.so !== "level") q.set("so", S.so);
  return q.toString().replace(/%2C/g, ",");
}
const saveHash = debounce(() => {
  const t = hashTarget(), s = stateString();
  try { t.history.replaceState(null, "", t.location.pathname + t.location.search + (s ? "#" + s : "")); } catch (e) { /* ignore */ }
}, 250);
function restoreHash() {
  let h = "";
  try { h = hashTarget().location.hash.slice(1); } catch (e) { h = location.hash.slice(1); }
  const q = new URLSearchParams(h), int = k => parseInt(q.get(k), 10);
  Object.assign(S, {view: "overview", m: "libdem", l: "elections", from: Y0, to: Y1, r: "pl", u: true, ctx: true, ev: null, cm: "region", so: "level"});
  S.c = []; SLOT.clear();
  if (VIEWS.includes(q.get("view"))) S.view = q.get("view");
  if (MS.has(q.get("m"))) S.m = q.get("m");
  if (LENSES.some(l => l[0] === q.get("l"))) S.l = q.get("l");
  if (int("from") >= Y0 && int("from") < Y1) S.from = int("from");
  if (int("to") > S.from && int("to") <= Y1) S.to = int("to");
  S.y = int("y") >= S.from && int("y") <= S.to ? int("y") : S.to;
  if (REGIONS.some(r => r[0] === q.get("r"))) S.r = q.get("r");
  (q.get("c") || "").split(",").map(s => s.trim().toUpperCase()).filter(Boolean).forEach(iso => addCountry(iso));
  S.u = q.get("u") !== "0"; S.ctx = q.get("ctx") !== "0";
  if (q.get("ev") && evItem(q.get("ev"))) S.ev = q.get("ev");
  if (q.get("cm") === "countries") S.cm = "countries";
  if (q.get("so") === "change") S.so = "change";
  const ev = S.ev && evItem(S.ev);
  if (ev && !q.get("y")) { const d = ev.x.date || ev.x.start; const yr = +d.slice(0, 4); if (yr >= S.from && yr <= S.to) S.y = yr; }
}
function shareUrl() { const t = hashTarget(), s = stateString(); return t.location.origin + t.location.pathname + (s ? "#" + s : ""); }

// ---- downloads ---------------------------------------------------------------------------------
function viewPanels() {
  if (S.view === "overview") return OVERVIEW.map(id => vdemPanel(M.get(id)));
  if (S.view === "explain") return D.measures.filter(m => m.lenses.includes(S.l) && m.kind !== "category").map(m => vdemPanel(m));
  if (S.view === "polarisation") return [...POLAR_MAIN.map(id => vdemPanel(M.get(id), {rev: true})), ...POLAR_MORE.map(id => vdemPanel(M.get(id)))];
  return [vdemPanel(MS.get(S.m) || M.get("v2x_libdem"))];
}
function csv() {
  const rows = [["view", "measure_id", "shown", "measure", "kind", "scale_min", "scale_max", "drawn_reversed", "series_id", "series", "period", "value", "codelow", "codehigh", "n_countries", "source"]];
  const src = `V-Dem ${D.meta.version}`;
  const isos = S.view === "context" ? ["POL", ...D.countries.filter(c => S.cm === "countries" ? S.c.includes(c.id) : S.r === "pl" || S.r === "both" || c.region === S.r).map(c => c.id)] : ["POL", ...S.c];
  const regs = S.view === "context" ? (S.cm === "countries" ? [] : S.r === "cee" ? ["cee"] : S.r === "we" ? ["we"] : ["cee", "we"]) : regionsShown();
  viewPanels().forEach(p => {
    const m = p.m, base = [S.view, m.id, m.shown, m.label, m.kind, m.scale[0], m.scale[1], p.rev ? "TRUE" : "FALSE"];
    for (let y = Math.max(S.from, m.from); y <= S.to; y++) {
      const i = yi(y);
      isos.forEach(iso => { const s = seriesOf(iso, m); if (s && s.est[i] != null) rows.push([...base, iso, nameOf(iso), y, s.est[i], s.lo ? s.lo[i] : "", s.hi ? s.hi[i] : "", "", src]); });
      regs.forEach(r => { const s = D.regional[r][m.id]; if (s && s.med[i] != null) [["q25", s.q25], ["median", s.med], ["q75", s.q75]].forEach(([k, a]) => rows.push([...base, `${r}_${k}`, `${D.regions.sets[r].label}, ${k === "median" ? "median" : k === "q25" ? "25th percentile" : "75th percentile"} of the other countries`, y, a[i], "", "", s.n[i], src])); });
    }
  });
  if (S.view === "polarisation") {
    const r = D.rollcall;
    r.periods.forEach((per, i) => {
      rows.push([S.view, "sejm_rollcall_distance", "x_std", "PiS–PO/KO distance in Sejm roll calls (pooled SD units)", "other", "", "", "FALSE", "POL", "Poland", per, r.distance.value[i], r.distance.lo[i], r.distance.hi[i], "", "Sejm analysis"]);
      rows.push([S.view, "sejm_opposed_line_rate", "", "Share of contested roll calls with opposed PiS and PO/KO lines", "other", 0, 1, "FALSE", "POL", "Poland", per, r.opposed.value[i], r.opposed.lo[i], r.opposed.hi[i], "", "Sejm analysis"]);
    });
  }
  const qv = v => typeof v === "string" && /[",\n]/.test(v) ? `"${v.replace(/"/g, '""')}"` : v ?? "";
  const tag = S.view === "explain" ? S.l : S.view === "context" ? S.m : "";
  download(`quality-of-democracy-${S.view}${tag ? "-" + tag : ""}.csv`, new Blob([rows.map(r => r.map(qv).join(",")).join("\n") + "\n"], {type: "text/csv"}));
}
async function png() {
  const svg = $("fig-" + S.view); if (!svg) return;
  const vb = svg.viewBox.baseVal, w = vb.width, h = vb.height;
  const clone = await ChartExport.clone(svg);   // ../chart-export.js: labels stay in Jost
  clone.setAttribute("xmlns", NS); clone.setAttribute("width", w); clone.setAttribute("height", h);
  clone.querySelectorAll(".hov").forEach(n => n.remove());
  const img = await new Promise(res => { const im = new Image(); im.onload = () => res(im); im.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(clone)); });
  await ChartExport.fonts();
  const pad = 24, head = 64, foot = 34, W = w + 2 * pad, H = h + head + foot;
  const cv = document.createElement("canvas"), sc = 2; cv.width = W * sc; cv.height = H * sc;
  const g = cv.getContext("2d"); g.scale(sc, sc); g.fillStyle = "#fff"; g.fillRect(0, 0, W, H);
  const lens = LENSES.find(l => l[0] === S.l), m = MS.get(S.m);
  const title = {overview: "Polish democracy in V-Dem", explain: `${lens[1]} in Poland`, context: `${m ? m.label : "Liberal democracy"}: Poland and the other countries`, polarisation: "Polarisation in Poland"}[S.view];
  const cmp = S.view === "context" ? (S.cm === "countries" ? "Poland and selected countries" : "Medians of the other countries in each set") : S.r === "pl" ? "Poland" : `Poland against the median of ${S.r === "both" ? "the other CEE members and of Western Europe" : S.r === "cee" ? "the other CEE members" : "Western Europe"}`;
  g.fillStyle = ACCENT; g.font = ChartExport.TITLE; g.fillText(`${title}, ${S.from}–${S.to}`, pad, 32);
  g.fillStyle = "#4B4B4B"; g.font = ChartExport.SUBTITLE; g.fillText(`V-Dem ${D.meta.version}. ${cmp}.${S.u ? " Bands are 68% intervals." : ""}`, pad, 54);
  g.drawImage(img, pad, head, w, h);
  g.fillStyle = "#767676"; g.font = ChartExport.CREDIT; g.fillText(D.meta.credit, pad, H - 12);
  cv.toBlob(b => download(`quality-of-democracy-${S.view}.png`, b), "image/png");
}

// ---- main --------------------------------------------------------------------------------------
async function init() {
  try {
    [D, CX] = await Promise.all([getJSON("democracy.json"), getJSON("political-context.json")]);
    if (!D.measures || !D.poland || !CX.cabinets) throw new Error("incomplete data");
  } catch (e) {
    $("asof").textContent = "The data could not be loaded. Please try again later."; console.error(e); return;
  }
  Y0 = D.years[0]; Y1 = D.years[D.years.length - 1];
  D.measures.forEach(m => { M.set(m.id, m); MS.set(m.short, m); });
  $("asof").innerHTML = `V-Dem ${esc(D.meta.version)} (released ${esc(monthLabel(D.meta.released))}), annual to ${Y1}: ${D.measures.length} measures for Poland and ${D.countries.length} other countries, the EU members and the United Kingdom. Bands are 68% intervals. ${link(D.meta.url, "V-Dem", false)}, ${esc(D.meta.licence)}.`;
  restoreHash();
  $("ui").hidden = false;
  $("views").onclick = e => { const b = e.target.closest("button"); if (b) { S.view = b.dataset.v; render(); } };
  $("regions").onclick = e => { const b = e.target.closest("button"); if (b) { S.r = b.dataset.r; render(); } };
  $("lenses").onclick = e => { const b = e.target.closest("button"); if (b) { S.l = b.dataset.l; render(); } };
  $("cmode").onclick = e => { const b = e.target.closest("button"); if (b) { S.cm = b.dataset.cm; render(); } };
  $("csort").onclick = e => { const b = e.target.closest("button"); if (b) { S.so = b.dataset.so; render(); } };
  $("measure").onchange = e => { S.m = e.target.value; render(); };
  $("from").onchange = e => { S.from = +e.target.value; if (S.to <= S.from) S.to = Math.min(Y1, S.from + 1); S.y = clamp(S.y, S.from, S.to); render(); };
  $("to").onchange = e => { S.to = +e.target.value; S.y = clamp(S.y, S.from, S.to); render(); };
  $("addc").onchange = e => { if (e.target.value && addCountry(e.target.value)) render(); else e.target.value = ""; };
  $("u").onchange = e => { S.u = e.target.checked; render(); };
  $("ctx").onchange = e => { S.ctx = e.target.checked; if (!S.ctx) S.ev = null; render(); };
  document.addEventListener("click", e => {
    const rm = e.target.closest("[data-rm]"); if (rm) { removeCountry(rm.dataset.rm); render(); return; }
    if (e.target.closest("[data-close]")) { S.ev = null; render(); return; }
    const tr = e.target.closest("#ranks tr[data-c]"); if (tr && tr.dataset.c !== "POL") { toggleCountry(tr.dataset.c); return; }
  });
  $("dlcsv").onclick = csv; $("dlpng").onclick = png;
  $("share").onclick = () => { const u = shareUrl(); (navigator.clipboard ? navigator.clipboard.writeText(u) : Promise.reject()).then(() => toast("Link copied"), () => window.prompt("Copy this link:", u)); };
  try { hashTarget().addEventListener("hashchange", () => { restoreHash(); render(); }); } catch (e) { /* ignore */ }
  render();
  // Redraw when the width changes, not the height, which the charts themselves set
  let lastW = $("stage").clientWidth;
  new ResizeObserver(() => { const w = $("stage").clientWidth; if (Math.abs(w - lastW) > 4) { lastW = w; renderSoon(); } }).observe($("stage"));
}
init();
