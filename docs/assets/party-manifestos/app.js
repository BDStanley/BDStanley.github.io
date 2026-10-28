/* Party manifestos — what Polish parties' election manifestos stressed, 1991–2019.
 *
 * Reads manifestos.json: the Manifesto Project's coding of each manifesto (shares by domain and
 * category; positions and saliences on the dimensions defined in meta.dimensions, built from the
 * coded categories, with bootstrap intervals over quasi-sentences; and the CHES ratings of the
 * same party at the nearest wave). Written by "Party manifestos/analysis/30_export.R" in the
 * Polish political analyses repo. Views: Manifesto (one party's manifesto at one election) and
 * Over time (one measure for every party across elections) and Against the experts (a
 * dimension against the CHES rating of the same party). State goes into the parent page's URL
 * hash: #p=<manifesto id>&v=time&m=<measure> or &v=ches&d=<dimension>&c=<CHES variable>.
 */
"use strict";

const $ = id => document.getElementById(id);
const esc = s => String(s == null ? "" : s).replace(/[&<>"']/g, c => ({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}[c]));
const isNum = v => typeof v === "number" && isFinite(v);
const fmt = (v, dp = 1) => isNum(v) ? v.toFixed(dp).replace("-", "−") : "–";
const SVGNS = "http://www.w3.org/2000/svg";
const YEAR = id => id.slice(-4);
const GREY = "#9a9a9a";
const CHES_NAME = {lrecon: "economic left–right", galtan: "GAL–TAN", lrgen: "left–right", eu_position: "EU position", eu_salience: "EU salience", nationalism: "nationalism",
  multiculturalism: "multiculturalism", sociallifestyle: "social lifestyle", religious_principles: "religious principles", civlib_laworder: "civil liberties or law and order",
  deregulation: "deregulation", protectionism: "protectionism", spendvtax: "public services or tax cuts", redistribution: "redistribution", environment: "environment or growth",
  regions: "decentralisation", corrupt_salience: "corruption salience", urban_rural: "urban or rural interests", ethnic_minorities: "ethnic minorities"};
const chesName = c => CHES_NAME[c] || c.replace(/_/g, " ");

let D, S = {v: "profile", p: null, m: "econ", d: "econ", c: null, e: null, hm: "main", hs: "abs", a: null, b: null};
const byId = {}, dimByKey = {};

function init(d) {
  D = d;
  D.programmes.forEach(p => { byId[p.id] = p; });
  D.meta.dimensions.forEach(x => { dimByKey[x.key] = x; });
  D.cat = Object.fromEntries(D.categories.map(c => [c.code, c.label]));
  readHash();
  if (!byId[S.p]) S.p = D.elections[D.elections.length - 1].programmes[0];
  if (!S.e) S.e = byId[S.p].election;
  if (!S.a) S.a = S.p;
  if (!S.b) S.b = otherDefault(S.a);
  const opts = D.elections.map(e => `<optgroup label="${YEAR(e.id)}">` + e.programmes.map(i => `<option value="${i}">${esc(byId[i].name)} ${YEAR(e.id)}</option>`).join("") + "</optgroup>").join("");
  $("ca").innerHTML = opts; $("cb").innerHTML = opts;
  $("ca").addEventListener("change", e => { S.a = e.target.value; render(); });
  $("cb").addEventListener("change", e => { S.b = e.target.value; render(); });
  $("cswap").addEventListener("click", () => { [S.a, S.b] = [S.b, S.a]; render(); });
  let rz; window.addEventListener("resize", () => { clearTimeout(rz); rz = setTimeout(() => { if (S.v === "time") drawTime(); }, 150); });
  $("hmode").querySelectorAll("button").forEach(b => b.addEventListener("click", () => { S.hm = b.dataset.m; render(); }));
  $("hscale").querySelectorAll("button").forEach(b => b.addEventListener("click", () => { S.hs = b.dataset.s; render(); }));
  $("asof").innerHTML = "Manifesto Project dataset 2026a and corpus 2026-1; " + esc(D.meta.coverage) +
    ". Shares are percentages of a manifesto's coded sentences; intervals come from resampling those sentences, so they show sampling noise only, not coding judgement.";
  $("views").querySelectorAll("button").forEach(b => b.addEventListener("click", () => { S.v = b.dataset.v; render(); }));
  $("measure").innerHTML = measureOptions();
  $("measure").addEventListener("change", e => { S.m = e.target.value; render(); });
  $("cdim").innerHTML = D.meta.dimensions.flatMap(x => x.ches.map(c => `<option value="${x.key}|${c}">${esc(x.label)}${x.ches.length > 1 ? " (against " + esc(chesName(c)) + ")" : ""}</option>`)).join("");
  $("cdim").addEventListener("change", e => { const [k, c] = e.target.value.split("|"); S.d = k; S.c = c; render(); });
  try { hashTarget().addEventListener("hashchange", () => { readHash(); render(); }); } catch (e) { window.addEventListener("hashchange", () => { readHash(); render(); }); }
  render();
}

// The page's address lives on the parent page when this is embedded (as in the Polling tracker), so links
// from other pages (#p=<id>) reach it there
function hashTarget() { try { if (window.top !== window && window.top.location.host === location.host) return window.top; } catch (e) { /* cross-origin */ } return window; }
function readHash() {
  let raw; try { raw = hashTarget().location.hash; } catch (e) { raw = location.hash; }
  const h = new URLSearchParams(raw.replace(/^#/, ""));
  const p = h.get("p"); if (p && byId[p]) S.p = p;
  if (["time", "ches", "heat", "compare"].includes(h.get("v"))) S.v = h.get("v");
  const e = h.get("e"); if (e && D.elections.some(x => x.id === e)) S.e = e;
  if (["main", "all", "domain"].includes(h.get("hm"))) S.hm = h.get("hm");
  if (["abs", "rel"].includes(h.get("hs"))) S.hs = h.get("hs");
  if (byId[h.get("a")]) S.a = h.get("a");
  if (byId[h.get("b")]) S.b = h.get("b");
  const m = h.get("m"); if (m) S.m = m;
  const d = h.get("d"); if (d && dimByKey[d]) S.d = d;
  const c = h.get("c"); if (c) S.c = c;
}
function writeHash() {
  const h = new URLSearchParams();
  h.set("p", S.p);
  if (S.v === "time") { h.set("v", "time"); h.set("m", S.m); }
  if (S.v === "ches") { h.set("v", "ches"); h.set("d", S.d); if (S.c) h.set("c", S.c); }
  if (S.v === "heat") { h.set("v", "heat"); h.set("e", S.e); h.set("hm", S.hm); h.set("hs", S.hs); }
  if (S.v === "compare") { h.set("v", "compare"); h.set("a", S.a); h.set("b", S.b); }
  try { const t = hashTarget(); t.history.replaceState(null, "", "#" + h.toString()); } catch (e) {}
}

function measureOptions() {
  const dims = (kind, lab) => [lab, D.meta.dimensions.filter(x => x.kind === kind).map(x => [x.key, x.label])];
  const o = [dims("position", "Positions"), dims("salience", "Saliences"),
    ["Domains", D.domains.map(d => ["domain_" + d.id, d.label])],
    ["Categories", D.categories.filter(c => c.code !== "000").map(c => ["cat_" + c.code, c.code + " " + c.label])]];
  return o.map(([g, items]) => `<optgroup label="${esc(g)}">` + items.map(([k, l]) => `<option value="${k}">${esc(l)}</option>`).join("") + "</optgroup>").join("");
}

const ZERO = [0, 0, 0];
// A sensible second manifesto: the same party's next (or previous) one, else the largest party in the same election
function otherDefault(id) {
  const p = byId[id], same = D.programmes.filter(q => q.id !== id && q.palette && q.palette === p.palette).sort((x, y) => Math.abs(+YEAR(x.election) - +YEAR(p.election)) - Math.abs(+YEAR(y.election) - +YEAR(p.election)));
  if (same.length) return same[0].id;
  const el = D.elections.find(e => e.id === p.election).programmes.filter(i => i !== id);
  return el.length ? el[0] : id;
}
function value(p, key) {
  if (key.startsWith("domain_")) return p.domains[key.slice(7)] || ZERO;
  if (key.startsWith("cat_")) return p.categories[key.slice(4)] || ZERO;
  return p.dims[key] || ZERO;
}
const isLog = key => !key.includes("_") && dimByKey[key] && dimByKey[key].kind === "position" && key !== "rile";
const col = p => (p.palette && D.meta.colours[p.palette]) || GREY;
const name = p => p.palette || p.abbrev || p.name;
const dp = key => (isLog(key) ? 2 : 1);

function el(tag, attrs, parent) {
  const e = document.createElementNS(SVGNS, tag);
  for (const k in attrs) e.setAttribute(k, attrs[k]);
  if (parent) parent.appendChild(e);
  return e;
}
function text(parent, x, y, s, attrs = {}) { const t = el("text", Object.assign({x, y, "font-size": 12, fill: "#4B4B4B"}, attrs), parent); t.textContent = s; return t; }
function niceStep(raw) {
  const e = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / e;
  return (f <= 1 ? 1 : f <= 2 ? 2 : f <= 5 ? 5 : 10) * e;
}

function render() {
  $("views").querySelectorAll("button").forEach(b => b.setAttribute("aria-pressed", b.dataset.v === S.v));
  ["profile", "time", "ches", "heat", "compare"].forEach(v => { $("v-" + v).hidden = S.v !== v; });
  $("measure").value = S.m;
  const dd = dimByKey[S.d]; if (!S.c || !dd.ches.includes(S.c)) S.c = dd.ches[0];
  $("cdim").value = S.d + "|" + S.c;
  writeHash();
  ({profile: drawProfile, time: drawTime, ches: drawChes, heat: drawHeat, compare: drawCompare})[S.v]();
}

// ---- Manifesto ----------------------------------------------------------------------------------
function drawProfile() {
  const p = byId[S.p], el_ = D.elections.find(e => e.id === p.election);
  $("elections").innerHTML = D.elections.map(e => `<button type="button" data-e="${e.id}" aria-pressed="${e.id === p.election}">${YEAR(e.id)}</button>`).join("");
  $("elections").querySelectorAll("button").forEach(b => b.addEventListener("click", () => {
    const e = D.elections.find(x => x.id === b.dataset.e);
    const same = e.programmes.map(i => byId[i]).find(q => p.palette && q.palette === p.palette) || byId[e.programmes[0]];
    S.p = same.id; render();
  }));
  $("parties").innerHTML = el_.programmes.map(i => byId[i]).map(q =>
    `<button type="button" class="chip" data-p="${q.id}" aria-pressed="${q.id === p.id}"><span class="dot" style="background:${col(q)}"></span>${esc(name(q))}</button>`).join("");
  $("parties").querySelectorAll("button").forEach(b => b.addEventListener("click", () => { S.p = b.dataset.p; render(); }));

  const notes = [];
  if (p.flags.small) notes.push(`This manifesto has only ${p.sentences} coded sentences, so its shares and positions are very uncertain.`);
  if (p.flags.estimate) notes.push("The Manifesto Project estimated this manifesto.");
  if (p.flags.bloc) notes.push("A coalition's manifesto, not a single party's.");
  if (p.flags.englishSummary) notes.push("The coded document is an English summary, not the full manifesto.");
  if (p.progtype === 6) notes.push("A general manifesto, not one written for this election.");
  $("pnotice").hidden = !notes.length; $("pnotice").textContent = notes.join(" ");

  const links = [];
  if (p.partyId) links.push(`<a href="party-genealogy.html#pt=${esc(p.partyId)}" target="_top">Party in the Sejm</a>`);
  if (p.committeeId) links.push(`<a href="election-results.html#e=${esc(p.election.replace("_", "-"))}&it=${esc(p.committeeId)}" target="_top">Election result</a>`);
  links.push(`<a href="#" data-go="compare">Compare with another manifesto</a>`);
  links.push(`<a href="#" data-go="heat">This election's heat map</a>`);
  const cw = D.ches.find(c => c.programme === p.id);
  if (p.ches && cw) links.push(`<a href="party-ideology.html#view=profile&party=${p.ches}&w=${cw.wave}" target="_top">The experts' ratings in Party ideology</a>`);
  $("card").innerHTML = `<h3><span class="dot" style="background:${col(p)}"></span> ${esc(p.name)}, ${YEAR(p.election)}</h3>` +
    `<div class="kv"><span class="k">Document</span><span>${esc(p.title || "title not recorded")}</span>` +
    `<span class="k">Length</span><span>${p.sentences.toLocaleString("en-GB")} coded sentences${p.flags.small ? ' <span class="flag">short</span>' : ""}</span>` +
    `<span class="k">Vote</span><span>${isNum(p.voteShare) ? fmt(p.voteShare, 1) + "%" : "no separate figure"}${isNum(p.seats) ? ", " + p.seats + " seats" : ""}</span>` +
    `<span class="k">Text</span><span>${p.sentenceLevel ? "sentence-level coding available" : "dataset shares only"}</span></div>` +
    (p.note ? `<div class="help">${esc(p.note)}</div>` : "") + `<div class="links">${links.join("")}</div>`;
  $("card").querySelectorAll("a[data-go]").forEach(l => l.addEventListener("click", ev => {
    ev.preventDefault();
    if (l.dataset.go === "compare") { S.a = p.id; if (S.b === p.id) S.b = otherDefault(p.id); S.v = "compare"; }
    else { S.e = p.election; S.v = "heat"; }
    render();
  }));

  drawDomains(p); drawTop(p); drawDims(p);
}

function bars(svg, rows, L, W, rowH, fillCol, labelFn) {
  svg.innerHTML = "";
  const R = 60, H = rows.length * rowH + 14;
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  const mx = Math.max(5, ...rows.map(r => r[1][2])) * 1.05, X = v => L + (W - L - R) * v / mx;
  rows.forEach(([k, v], i) => {
    const y = 6 + i * rowH, lab = labelFn(k);
    const t = text(svg, L - 8, y + rowH / 2 + 4, lab.length > 36 ? lab.slice(0, 35) + "…" : lab, {"text-anchor": "end"});
    const ti = document.createElementNS(SVGNS, "title"); ti.textContent = lab; t.appendChild(ti);
    el("rect", {x: L, y: y + 4, width: Math.max(0, X(v[0]) - L), height: rowH - 12, fill: fillCol, opacity: .85}, svg);
    el("line", {x1: X(v[1]), x2: X(v[2]), y1: y + rowH / 2 - 2, y2: y + rowH / 2 - 2, stroke: "#141414", "stroke-width": 1.2}, svg);
    text(svg, X(v[2]) + 6, y + rowH / 2 + 2, fmt(v[0], 1) + "%");
  });
}
function drawDomains(p) {
  bars($("domains"), D.domains.map(d => [String(d.id), p.domains[d.id] || ZERO]), 190, 560, 30, col(p), k => D.domains.find(d => String(d.id) === k).label);
}
function drawTop(p) {
  const rows = Object.entries(p.categories).filter(([k]) => k !== "000").sort((a, b) => b[1][0] - a[1][0]).slice(0, 10);
  bars($("top"), rows, 250, 560, 28, col(p), k => D.cat[k] || k);
}

function drawDims(p) {
  const ches = D.ches.find(c => c.programme === p.id);
  const rows = D.meta.dimensions.map(d => {
    const v = p.dims[d.key]; if (!v) return "";
    const g = isLog(d.key) ? 2 : 1;
    const cv = ches ? d.ches.map(c => (ches.values[c] != null ? `${chesName(c)} ${fmt(ches.values[c], 1)}` : null)).filter(Boolean).join("; ") : "";
    const pole = d.kind === "salience" ? "" : `<br><span class="muted">${esc(d.lowMeans)} ← → ${esc(d.highMeans)}</span>`;
    return `<tr><td>${esc(d.label)}${pole}</td><td>${fmt(v[0], g)}${d.kind === "salience" ? "%" : ""}</td><td>${fmt(v[1], g)} to ${fmt(v[2], g)}</td><td>${cv || "–"}</td></tr>`;
  }).join("");
  $("positions").innerHTML = `<h3>Dimensions built from the coded categories</h3>` +
    `<table class="t"><tr><th>Dimension</th><th>Manifesto</th><th>95% interval</th><th>CHES${ches ? " " + ches.wave : ""}</th></tr>${rows}</table>` +
    `<div class="help">Positions are log-ratios of how much a manifesto stresses one pole over the other (rile is in percentage points); saliences are percentages of sentences. CHES runs 0–10 (the EU position 1–7). The scales differ, so compare the order of parties, not the numbers. ` +
    (ches ? `CHES's party is ${esc(ches.chesParty)}${p.flags.bloc ? ", the main member of this bloc" : ""}, rated in ${ches.wave}.` : "CHES has no matching rating at the nearest wave.") + "</div>";
}

// ---- Charts in the house style of the Social and political trends page --------------------------------
// Light grid, a darker baseline, a darker line at zero, tick labels in grey, and "↑ / ↓" labels at the
// plot's corners saying what up and down mean; series named at their last point, intervals as bands.
const INK = "#141414", SOFT = "#4B4B4B", MUTED = "#767676", GRID = "#ECECEC", BASE = "#D5D5D5", MID = "#A6A6A6";
const HALO = {stroke: "#fff", "stroke-width": 3, "paint-order": "stroke"};
const tipAt = (tip, svg, ev, html) => {
  const r = svg.parentNode.getBoundingClientRect();
  tip.hidden = false; tip.style.left = Math.min(ev.clientX - r.left + 12, r.width - 260) + "px"; tip.style.top = ev.clientY - r.top + 12 + "px"; tip.innerHTML = html;
};
const trunc = (s, n) => s.length > n ? s.slice(0, n - 1) + "…" : s;
const yearOf = p => { const d = new Date(p.date); return d.getUTCFullYear() + d.getUTCMonth() / 12; };
const labelOf = p => p.palette || (p.abbrev && p.abbrev.length > 1 ? p.abbrev : trunc(p.name, 16));
function niceTicks(lo, hi, n) {
  const raw = (hi - lo) / n, e = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / e;
  const step = (f <= 1 ? 1 : f <= 2 ? 2 : f <= 5 ? 5 : 10) * e;
  return {step, lo: Math.floor(lo / step + 1e-9) * step, hi: Math.ceil(hi / step - 1e-9) * step};
}
function poles(key) {
  const d = dimByKey[key];
  if (d && d.kind === "position") return [d.highMeans, d.lowMeans];
  if (d) return [d.highMeans, "less emphasis"];
  return ["larger share of the manifesto", "smaller share"];
}

// ---- Over time: one panel per party family, all on the same scale ------------------------------------
function drawTime() {
  const svg = $("time"); svg.innerHTML = "";
  const key = S.m, log = isLog(key), g = log ? 2 : 1, tip = $("tip");
  const sel = $("measure").selectedOptions[0];
  const label = sel ? sel.textContent : key;
  const dim = dimByKey[key];
  $("timehelp").textContent = label + (dim && dim.kind === "position" ? (log ? " (log-ratio)" : " (points)") : ", % of a manifesto's coded sentences") +
    ". One panel for each group of party families (Party tracker's classification of a list's lead party, with the two left families grouped as Left, the national-conservative, libertarian, radical-nationalist and Solidarity families as Right, and the populist, communist-era allied and other families as Other), all on the same scale. Each point is a manifesto, the line joins a party's manifestos across elections and the band is the 95% interval. Hollow points are short manifestos (under " + D.meta.smallBelow + " sentences). Click a point to open it.";
  const all = D.programmes.map(p => value(p, key));
  const tk = niceTicks(Math.min(0, ...all.map(v => v[1])), Math.max(0, ...all.map(v => v[2])), 8);
  const lo = tk.lo, hi = tk.hi, step = tk.step;
  const W = Math.max(320, svg.parentNode.clientWidth - 12), cols = W >= 900 ? 3 : W >= 560 ? 2 : 1, pw = W / cols, ph = Math.max(200, Math.min(250, pw * 0.74));
  const groups = D.meta.facets.map(f => ({f, ps: D.programmes.filter(p => p.facet === f.key)})).filter(x => x.ps.length);
  const rows = Math.ceil(groups.length / cols);
  svg.setAttribute("viewBox", `0 0 ${W} ${rows * ph}`);
  const [up, dn] = poles(key);
  const x0 = 1990, x1 = 2021;
  groups.forEach((grp, gi) => {
    const o = el("g", {transform: `translate(${(gi % cols) * pw},${Math.floor(gi / cols) * ph})`}, svg);
    const m = {l: 40, r: 14, t: 30, b: 26}, X = yr => m.l + (yr - x0) / (x1 - x0) * (pw - m.l - m.r), Y = v => m.t + (hi - v) / (hi - lo) * (ph - m.t - m.b);
    text(o, m.l, 14, `${grp.f.label}`, {"font-size": 12.5, "font-weight": 500, fill: INK});
    text(o, pw - m.r, 14, `${grp.ps.length} manifesto${grp.ps.length > 1 ? "s" : ""}`, {"text-anchor": "end", "font-size": 11, fill: MUTED});
    for (let v = lo; v <= hi + 1e-9; v += step) {
      el("line", {x1: m.l, x2: pw - m.r, y1: Y(v), y2: Y(v), stroke: v === lo ? BASE : GRID, "stroke-width": 1}, o);
      text(o, m.l - 6, Y(v) + 4, fmt(Math.abs(v) < 1e-9 ? 0 : v, step < 1 ? 1 : 0), {"text-anchor": "end", "font-size": 11, fill: MUTED});
    }
    if (lo < 0 && hi > 0 && dim && dim.kind === "position") el("line", {x1: m.l, x2: pw - m.r, y1: Y(0), y2: Y(0), stroke: MID, "stroke-width": 1}, o);
    for (let yr = 1990; yr <= 2020; yr += 10) {
      el("line", {x1: X(yr), x2: X(yr), y1: ph - m.b, y2: ph - m.b + 4, stroke: BASE}, o);
      text(o, X(yr), ph - m.b + 17, yr, {"text-anchor": "middle", "font-size": 11, fill: MUTED});
    }
    const nch = Math.floor((pw - m.l - m.r - 12) / 5.4) - 2;
    text(o, m.l + 6, m.t + 12, "↑ " + trunc(up, nch), Object.assign({"font-size": 11, fill: SOFT}, HALO));
    text(o, m.l + 6, ph - m.b - 6, "↓ " + trunc(dn, nch), Object.assign({"font-size": 11, fill: SOFT}, HALO));
    const series = {};
    grp.ps.forEach(p => { (series[p.series] = series[p.series] || []).push(p); });
    const keys = Object.keys(series), labels = [];
    const clip = el("clipPath", {id: "pc" + gi}, el("defs", {}, o)); el("rect", {x: m.l, y: m.t - 6, width: pw - m.l - m.r + 6, height: ph - m.t - m.b + 12}, clip);
    const gp = el("g", {"clip-path": `url(#pc${gi})`}, o);
    keys.forEach((k, ki) => {
      const colour = D.meta.colours[k] || GREY;
      const ps = series[k].slice().sort((a, b) => a.date.localeCompare(b.date));
      const dx = (ki - (keys.length - 1) / 2) * 3;
      const pts = ps.map(p => ({p, x: X(yearOf(p)) + dx, v: value(p, key)}));
      if (pts.length > 1) {
        el("path", {d: "M" + pts.map(q => `${q.x.toFixed(1)},${Y(q.v[2]).toFixed(1)}`).join("L") + "L" + pts.slice().reverse().map(q => `${q.x.toFixed(1)},${Y(q.v[1]).toFixed(1)}`).join("L") + "Z", fill: colour, "fill-opacity": .12}, gp);
        el("path", {d: "M" + pts.map(q => `${q.x.toFixed(1)},${Y(q.v[0]).toFixed(1)}`).join("L"), fill: "none", stroke: colour, "stroke-width": 2, "stroke-linejoin": "round", opacity: D.meta.colours[k] ? 1 : .6}, gp);
      }
      pts.forEach(q => {
        if (pts.length === 1) el("line", {x1: q.x, x2: q.x, y1: Y(q.v[1]), y2: Y(q.v[2]), stroke: colour, "stroke-width": 1.4, opacity: .5}, gp);
        const c = el("circle", {cx: q.x, cy: Y(q.v[0]), r: 3.8, fill: q.p.flags.small ? "#fff" : colour, "fill-opacity": .9, stroke: q.p.flags.small ? colour : "#fff", "stroke-width": q.p.flags.small ? 1.6 : 1, style: "cursor:pointer"}, gp);
        c.addEventListener("mousemove", ev => tipAt(tip, svg, ev, `<b>${esc(q.p.name)}, ${YEAR(q.p.election)}</b><br>${fmt(q.v[0], g)} (${fmt(q.v[1], g)} to ${fmt(q.v[2], g)})<br><span class="muted">${q.p.sentences} sentences</span>`));
        c.addEventListener("mouseleave", () => { tip.hidden = true; });
        c.addEventListener("click", () => { S.p = q.p.id; S.v = "profile"; render(); });
      });
      const last = pts[pts.length - 1];
      labels.push({txt: labelOf(last.p), colour, x: last.x, y: Y(last.v[0]), left: last.x > pw - m.r - 70});
    });
    // name each series at its last point, spread apart so the names do not overlap
    labels.sort((a, b) => a.y - b.y);
    for (let k = 1; k < labels.length; k++) if (labels[k].y - labels[k - 1].y < 11) labels[k].y = labels[k - 1].y + 11;
    labels.forEach(l => text(o, l.left ? l.x - 7 : l.x + 7, l.y + 3.5, l.txt, Object.assign({"text-anchor": l.left ? "end" : "start", "font-size": 11, "font-weight": 500, fill: l.colour === GREY ? MUTED : l.colour}, HALO)));
  });
  $("legend").innerHTML = "";
}

// ---- Heat map -------------------------------------------------------------------------------------
function heatColour(t) {
  // white to the site's dark red; square root so small shares still show
  const u = Math.sqrt(Math.max(0, Math.min(1, t)));
  const r = Math.round(255 + (139 - 255) * u), g = Math.round(255 + (0 - 255) * u), b = Math.round(255 + (0 - 255) * u);
  return {bg: `rgb(${r},${g},${b})`, fg: u > .55 ? "#fff" : "#141414"};
}
function drawHeat() {
  const el_ = D.elections.find(e => e.id === S.e), ps = el_.programmes.map(i => byId[i]);
  $("helections").innerHTML = D.elections.map(e => `<button type="button" data-e="${e.id}" aria-pressed="${e.id === S.e}">${YEAR(e.id)}</button>`).join("");
  $("helections").querySelectorAll("button").forEach(b => b.addEventListener("click", () => { S.e = b.dataset.e; render(); }));
  $("hmode").querySelectorAll("button").forEach(b => b.setAttribute("aria-pressed", b.dataset.m === S.hm));
  $("hscale").querySelectorAll("button").forEach(b => b.setAttribute("aria-pressed", b.dataset.s === S.hs));
  let rows;
  if (S.hm === "domain") rows = D.domains.map(d => ({key: String(d.id), label: d.label, get: p => (p.domains[d.id] || ZERO)}));
  else {
    const cats = D.categories.filter(c => c.code !== "000").map(c => ({key: c.code, label: c.label, dom: c.domain, get: p => (p.categories[c.code] || ZERO),
      max: Math.max(...ps.map(p => (p.categories[c.code] || ZERO)[0]))})).filter(c => c.max > 0);
    rows = S.hm === "main" ? cats.slice().sort((a, b) => b.max - a.max).slice(0, 20).sort((a, b) => a.dom - b.dom || b.max - a.max) : cats;
  }
  const rowMax = rows.map(r => Math.max(...ps.map(p => r.get(p)[0]), 0.0001));
  const gmax = Math.max(...rowMax);
  let h = `<table class="heat"><thead><tr><th class="cat"></th>` + ps.map(p =>
    `<th class="${p.flags.small ? "small" : ""}" title="${esc(p.name)}${p.flags.small ? ": short, so uncertain" : ""}"><button type="button" data-p="${p.id}"><span class="dot" style="background:${col(p)}"></span><br>${esc(name(p))}</button><small>${p.sentences.toLocaleString("en-GB")}${p.flags.small ? " ·short" : ""}</small></th>`).join("") + `</tr></thead><tbody>`;
  let lastDom = null;
  rows.forEach((r, i) => {
    if (S.hm !== "domain" && r.dom !== lastDom) {
      lastDom = r.dom;
      h += `<tr class="dom"><td class="cat">${esc((D.domains.find(d => d.id === r.dom) || {label: ""}).label)}</td>${ps.map(() => "<td></td>").join("")}</tr>`;
    }
    h += `<tr><td class="cat" title="${esc(r.label)}">${S.hm === "domain" ? "" : `<b>${r.key}</b>`}${esc(r.label)}</td>` + ps.map(p => {
      const v = r.get(p), t = S.hs === "rel" ? v[0] / rowMax[i] : v[0] / gmax, c = heatColour(v[0] > 0 ? t : 0);
      return `<td class="v" style="background:${c.bg};color:${c.fg}" title="${esc(p.name)}, ${r.label}: ${fmt(v[0], 1)}% (95% interval ${fmt(v[1], 1)} to ${fmt(v[2], 1)})">${v[0] > 0 ? fmt(v[0], 1) : ""}</td>`;
    }).join("") + `</tr>`;
  });
  h += `</tbody></table>`;
  $("heat").innerHTML = h;
  $("heat").querySelectorAll("th button").forEach(b => b.addEventListener("click", () => { S.p = b.dataset.p; S.v = "profile"; render(); }));
  $("hhelp").textContent = `${ps.length} manifestos at the ${YEAR(S.e)} election; each cell is the percentage of the manifesto's sentences in the category` +
    (S.hs === "rel" ? ", coloured relative to the party highest in that row" : ", coloured on one scale across the table") +
    (S.hm === "main" ? ". The twenty categories with the highest share in any manifesto are shown, grouped by domain" : "") +
    ". The figure under each party is its length in coded sentences. Click a party to open its manifesto.";
}

// ---- Compare ----------------------------------------------------------------------------------------
function compareNotes(A, B) {
  const n = [];
  [A, B].forEach((p, i) => {
    if (p.flags.small) n.push(`${i ? "B" : "A"} has only ${p.sentences} sentences.`);
    if (p.flags.bloc) n.push(`${i ? "B" : "A"} is a coalition's manifesto.`);
    if (p.flags.englishSummary) n.push(`${i ? "B" : "A"} is an English summary.`);
  });
  if (Math.max(A.sentences, B.sentences) > 5 * Math.min(A.sentences, B.sentences)) n.push("One manifesto is more than five times as long as the other, so its shares rest on many more sentences.");
  return n;
}
function drawCompare() {
  $("ca").value = S.a; $("cb").value = S.b;
  const A = byId[S.a], B = byId[S.b];
  const notes = compareNotes(A, B);
  $("cnotice").hidden = !notes.length; $("cnotice").textContent = notes.join(" ");
  $("ccards").innerHTML = [A, B].map((p, i) => `<div class="card"><h3><span class="dot" style="background:${col(p)}"></span> ${i ? "B" : "A"}: ${esc(p.name)}, ${YEAR(p.election)}</h3>` +
    `<div class="kv"><span class="k">Document</span><span>${esc(p.title || "title not recorded")}</span><span class="k">Length</span><span>${p.sentences.toLocaleString("en-GB")} coded sentences</span>` +
    `<span class="k">Vote</span><span>${isNum(p.voteShare) ? fmt(p.voteShare, 1) + "%" : "no separate figure"}${isNum(p.seats) ? ", " + p.seats + " seats" : ""}</span></div></div>`).join("");

  // dimensions: difference B - A, with an interval that combines the two bootstrap intervals
  const se = v => (v[2] - v[1]) / 3.92;
  let rows = "";
  D.meta.dimensions.forEach(d => {
    const a = A.dims[d.key], b = B.dims[d.key]; if (!a || !b) return;
    const g = isLog(d.key) ? 2 : 1, diff = b[0] - a[0], s = Math.sqrt(se(a) ** 2 + se(b) ** 2), sig = s > 0 && Math.abs(diff) > 1.96 * s;
    rows += `<tr><td>${esc(d.label)}</td><td>${fmt(a[0], g)}</td><td>${fmt(b[0], g)}</td><td class="${sig ? "sig" : ""}">${diff > 0 ? "+" : ""}${fmt(diff, g)}${s > 0 ? ` <span class="muted">(${fmt(diff - 1.96 * s, g)} to ${fmt(diff + 1.96 * s, g)})</span>` : ""}</td></tr>`;
  });
  $("cdims").innerHTML = `<h3>Dimensions</h3><table class="t"><tr><th>Dimension</th><th>A</th><th>B</th><th>B − A (95% interval)</th></tr>${rows}</table>` +
    `<div class="help">Bold differences have an interval that excludes zero. The interval combines the two manifestos' sentence-resampling intervals, so it is approximate. Positions are log-ratios, saliences percentages, rile points.</div>`;

  // categories: share difference with a two-proportion test on sentence counts
  const NA = A.sentences, NB = B.sentences, gaps = [];
  D.categories.filter(c => c.code !== "000").forEach(c => {
    const sa = (A.categories[c.code] || ZERO)[0], sb = (B.categories[c.code] || ZERO)[0];
    if (!sa && !sb) return;
    const ca = sa / 100 * NA, cb = sb / 100 * NB, p = (ca + cb) / (NA + NB);
    const z = p > 0 && p < 1 ? (sb / 100 - sa / 100) / Math.sqrt(p * (1 - p) * (1 / NA + 1 / NB)) : 0;
    gaps.push({code: c.code, label: c.label, sa, sb, diff: sb - sa, sig: Math.abs(z) > 1.96});
  });
  const top = gaps.sort((x, y) => Math.abs(y.diff) - Math.abs(x.diff)).slice(0, 15);
  $("cgaptitle").textContent = "Where they differ most: share of the manifesto, B minus A (points)";
  const svg = $("cgaps"); svg.innerHTML = "";
  const W = 600, rh = 26, L = 250, R = 56, H = top.length * rh + 40;
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  const x0 = L + 46, x1 = W - R - 20, mx = Math.max(2, ...top.map(t => Math.abs(t.diff))) * 1.05, cx = (x0 + x1) / 2, X = v => cx + (x1 - x0) / 2 * v / mx;
  text(svg, x0, 12, "← more in A", {fill: "#767676"}); text(svg, x1, 12, "more in B →", {"text-anchor": "end", fill: "#767676"});
  el("line", {x1: cx, x2: cx, y1: 20, y2: H - 6, stroke: "#B8B8B8"}, svg);
  top.forEach((t, i) => {
    const y = 26 + i * rh, colour = t.diff >= 0 ? col(B) : col(A);
    const lab = `${t.code} ${t.label}`;
    const tt = text(svg, L - 8, y + 14, lab.length > 38 ? lab.slice(0, 37) + "…" : lab, {"text-anchor": "end"});
    const ti = document.createElementNS(SVGNS, "title"); ti.textContent = `${lab}: A ${fmt(t.sa, 1)}%, B ${fmt(t.sb, 1)}%`; tt.appendChild(ti);
    el("rect", {x: Math.min(cx, X(t.diff)), y: y + 3, width: Math.abs(X(t.diff) - cx), height: rh - 10, fill: colour, opacity: t.sig ? .9 : .35}, svg);
    text(svg, t.diff >= 0 ? X(t.diff) + 5 : X(t.diff) - 5, y + 15, (t.diff > 0 ? "+" : "") + fmt(t.diff, 1), {"text-anchor": t.diff >= 0 ? "start" : "end", "font-weight": t.sig ? 600 : 400});
  });
  const nsig = top.filter(t => t.sig).length;
  $("chelp2").textContent = `Solid bars are differences a two-proportion test on sentence counts flags at the 5% level (${nsig} of these ${top.length}); with 56 categories compared, a few such flags are expected by chance. The test treats sentences as independent draws, which they are not quite, so read it as a guide.`;
}

// ---- Against the experts --------------------------------------------------------------------------
// What the low and high ends of each CHES scale mean (from its codebook)
const CHES_POLES = {
  lrecon: ["economic left", "economic right"], galtan: ["libertarian, post-materialist", "traditional, authoritarian"], lrgen: ["left", "right"],
  eu_position: ["strongly opposed to the EU", "strongly in favour of the EU"], nationalism: ["cosmopolitan", "nationalist"],
  multiculturalism: ["favours multiculturalism", "favours assimilation"], sociallifestyle: ["supports liberal policies", "opposes liberal policies"],
  religious_principles: ["opposes religious principles in politics", "supports religious principles in politics"],
  civlib_laworder: ["promotes civil liberties", "tough measures against crime"], deregulation: ["opposes deregulation", "supports deregulation"],
  protectionism: ["favours trade liberalisation", "favours protecting domestic producers"], spendvtax: ["improve public services", "reduce taxes"],
  redistribution: ["favours redistribution", "opposes redistribution"], environment: ["environmental protection first", "economic growth first"],
  regions: ["favours decentralisation", "opposes decentralisation"], corrupt_salience: ["corruption unimportant", "corruption extremely important"],
  urban_rural: ["urban interests", "rural interests"], eu_salience: ["EU unimportant", "EU extremely important"],
  ethnic_minorities: ["more rights for ethnic minorities", "opposes more rights"]
};
function drawChes() {
  const svg = $("scat"); svg.innerHTML = "";
  const dim = dimByKey[S.d], cv = S.c, log = isLog(S.d), g = log ? 2 : 1, tip = $("ctip");
  const pts = D.ches.map(c => ({c, p: byId[c.programme]})).filter(o => o.p && !o.p.flags.small && o.c.values[cv] != null && o.p.dims[S.d])
    .map(o => ({p: o.p, wave: o.c.wave, x: o.p.dims[S.d][0], y: o.c.values[cv]}));
  const n = pts.length, mx = pts.reduce((s, q) => s + q.x, 0) / n, my = pts.reduce((s, q) => s + q.y, 0) / n;
  const sxy = pts.reduce((s, q) => s + (q.x - mx) * (q.y - my), 0), sxx = pts.reduce((s, q) => s + (q.x - mx) ** 2, 0), syy = pts.reduce((s, q) => s + (q.y - my) ** 2, 0);
  const r = sxy / Math.sqrt(sxx * syy), b1 = sxy / sxx, b0 = my - b1 * mx, expect = dim.expectedSign;
  const ym = cv === "eu_position" ? [1, 7] : [0, 10];
  $("chelp").innerHTML = `Each manifesto of 100 sentences or more: its position on <b>${esc(dim.label.toLowerCase())}</b> against the experts' <b>${esc(chesName(cv))}</b> rating of the same party at the nearest Chapel Hill wave (0–10${cv === "eu_position" ? ", here 1–7" : ""}). ` +
    `${n} manifestos; r = ${fmt(r, 2)}${expect < 0 ? ", where a negative correlation is expected" : ""}. ` +
    (dim.note ? esc(dim.note) + " " : "") + "The scales differ, so read the order of parties, not the numbers. Click a point to open its manifesto.";
  const W = Math.max(320, svg.parentNode.clientWidth - 12), H = Math.round(Math.max(320, Math.min(520, W * 0.5)));
  const m = {l: 40, r: 16, t: 14, b: 44};
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  const xt = niceTicks(Math.min(...pts.map(q => q.x)), Math.max(...pts.map(q => q.x)), 6), xlo = xt.lo, xhi = xt.hi;
  const X = v => m.l + (v - xlo) / (xhi - xlo) * (W - m.l - m.r), Y = v => m.t + (ym[1] - v) / (ym[1] - ym[0]) * (H - m.t - m.b);
  for (let v = ym[0]; v <= ym[1]; v += 2) {
    el("line", {x1: m.l, x2: W - m.r, y1: Y(v), y2: Y(v), stroke: v === ym[0] ? BASE : GRID}, svg);
    text(svg, m.l - 6, Y(v) + 4, v, {"text-anchor": "end", "font-size": 11, fill: MUTED});
  }
  if (xlo < 0 && xhi > 0 && dim.kind === "position") el("line", {x1: X(0), x2: X(0), y1: m.t, y2: H - m.b, stroke: MID}, svg);
  for (let v = xlo; v <= xhi + xt.step / 2; v += xt.step) {
    el("line", {x1: X(v), x2: X(v), y1: H - m.b, y2: H - m.b + 4, stroke: BASE}, svg);
    text(svg, X(v), H - m.b + 17, fmt(Math.abs(v) < 1e-9 ? 0 : v, xt.step < 1 ? 1 : 0), {"text-anchor": "middle", "font-size": 11, fill: MUTED});
  }
  const [cl, ch] = CHES_POLES[cv] || ["low", "high"];
  text(svg, m.l + 6, m.t + 12, "↑ " + ch, Object.assign({"font-size": 11.5, fill: SOFT}, HALO));
  text(svg, m.l + 6, H - m.b - 6, "↓ " + cl, Object.assign({"font-size": 11.5, fill: SOFT}, HALO));
  const lowM = dim.lowMeans && dim.lowMeans !== "null" ? dim.lowMeans : "less emphasis";
  text(svg, m.l, H - 8, "← " + lowM, {"font-size": 11.5, fill: SOFT});
  text(svg, W - m.r, H - 8, dim.highMeans + " →", {"text-anchor": "end", "font-size": 11.5, fill: SOFT});
  text(svg, (m.l + W - m.r) / 2, H - 8, "Manifesto (" + (log ? "log-ratio" : dim.kind === "salience" ? "% of sentences" : "points") + ")", {"text-anchor": "middle", "font-size": 11, fill: MUTED});
  const cp = v => Math.max(ym[0], Math.min(ym[1], v));
  el("line", {x1: X(xlo), y1: Y(cp(b0 + b1 * xlo)), x2: X(xhi), y2: Y(cp(b0 + b1 * xhi)), stroke: "#9A9A9A", "stroke-width": 1.4, "stroke-dasharray": "5 4"}, svg);
  const names = new Map();
  pts.slice().sort((a, b) => (a.p.id === S.p) - (b.p.id === S.p)).forEach(q => {
    const on = q.p.id === S.p, colour = col(q.p); names.set(name(q.p), colour);
    if (on) el("circle", {cx: X(q.x), cy: Y(q.y), r: 11, fill: "none", stroke: "#8B0000", "stroke-width": 1.6}, svg);
    const c = el("circle", {cx: X(q.x), cy: Y(q.y), r: 4.6, fill: colour, "fill-opacity": .9, stroke: "#fff", "stroke-width": 1, style: "cursor:pointer"}, svg);
    c.addEventListener("mousemove", ev => tipAt(tip, svg, ev, `<b>${esc(q.p.name)}, ${YEAR(q.p.election)}</b><br>Manifesto ${fmt(q.x, g)}; experts ${fmt(q.y, 1)} (${q.wave})`));
    c.addEventListener("mouseleave", () => { tip.hidden = true; });
    c.addEventListener("click", () => { S.p = q.p.id; S.v = "profile"; render(); });
  });
  $("clegend").innerHTML = [...names.entries()].map(([k, c]) => `<span><span class="dot" style="background:${c}"></span>${esc(k)}</span>`).join("");
}

fetch("manifestos.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).then(init)
  .catch(e => { $("asof").innerHTML = '<div class="loading">The manifesto data could not be loaded (' + esc(e.message) + ').</div>'; });
