// Voting behaviour: who votes for whom in Poland's Sejm elections, 1997-2023, and who stays at home.
// Reads voting.json (the surveys' turnout and list shares by election and group, the comparison with the
// official results, the average marginal effects, the checks) and, for the calculator, calc-<year>.json (200
// posterior draws of the model's coefficients for one election, the calibration shifts, the support tables and the
// rules), all written by "Voting behaviour/analysis/70_export_web.R" in the Polish political analyses repo.
// Probabilities are computed here from the draws: the page applies the formulas listed in each calc file's
// design.formulas. Shares are weighted survey figures; effects are in percentage points with 90% credible
// intervals. No libraries.
"use strict";

const $ = id => document.getElementById(id);
const NS = "http://www.w3.org/2000/svg";
const PAGE = "Voting behaviour";   // the page's name in its suggested citation (../chart-export.js)
const ACCENT = "#8B0000";
const MUTED = "#767676";
// Group levels take these in order (checked for colour-blind separation)
const LEVEL_COL = ["#6A3FA0", "#E0A000", "#5E8C2A", "#D45DA4", "#4C9BE8"];
const VIEWS = ["who", "effects", "calc", "pol", "about"];
// The names the site uses for lists elsewhere (the Election results and Polling tracker pages); a list's name at one
// election comes from the export (ballot), so the 2023 PSL list is Trzecia Droga there
const NAMES = {PSL: "PSL / Trzecia Droga", "Liberal centre": "Liberal centre (UW)", Nowoczesna: "Liberal centre (Nowoczesna)", "No vote": "Did not vote", turnout: "Turnout"};
const SITE = "../../pages/polish-political-analyses/";   // the other analysis pages, from this app's place in the site
const LEGEND = {};
let CALC_ROWS = null;   // the calculator's bars as last drawn, for the PNG
const SPEC_NAMES = {
  main_constant: "Main effects constant across elections", main_varying: "Main effects drifting (the reference)",
  interactions_constant: "Main effects drifting, six constant interactions (the model on this page)",
  interactions_age_education: "As above, with age and education drifting", interactions_gender_age: "As above, with gender and age drifting"
};
const SHIFT_NAMES = {cc: "Complete cases", shift0: "Imputed", "shift0.5": "Imputed, refusers 0.5 points left", shift1: "Imputed, refusers 1 point left", "shift1.5": "Imputed, refusers 1.5 points left"};

let D = null;
let YEARS = [];
const CALC = {};
const CAT = {};
const GR = new Map();
const COMP = new Map();
const S = {
  view: "who", show: "all", split: 0, cel: 2023, by: 1, ct: "educ_higher", el: 2023, out: "No vote", cal: false,
  k: {el: 2023, age: 45, sex: "f", ed: 2, res: 3, rel: 4, ses: 3, cal: false},
  pv: "aff", pe: 2023, pa: "PiS", pb: "KO"
};
let POL = null;

// ---- helpers ----------------------------------------------------------------------------------
const clamp = (x, a, b) => Math.max(a, Math.min(b, x));
function debounce(f, ms) { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => f(...a), ms); }; }
function el(tag, attrs = {}, parent = null) {
  const n = document.createElementNS(NS, tag);
  for (const k in attrs) n.setAttribute(k, attrs[k]);
  if (parent) parent.appendChild(n);
  return n;
}
function txt(parent, x, y, s, attrs = {}) { const t = el("text", {x, y, fill: MUTED, "font-size": 11, ...attrs}, parent); t.textContent = s; return t; }
const esc = s => String(s).replace(/[&<>"]/g, c => ({"&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;"}[c]));
const MINUS = "−";
const pct = (x, d = 1) => x == null ? "–" : (100 * x).toFixed(d) + "%";
const pp = (x, d = 1) => x == null ? "–" : (x < 0 ? MINUS : x > 0 ? "+" : "") + Math.abs(x).toFixed(d);
const name = id => NAMES[id] || id;
const colour = id => CAT[id] || "#7f7f7f";
function electionOf(year) { return D.elections.find(e => e.year === year); }
// The lists on an election's ballot, largest share of voters first, Other last (not voting is left out)
function listsByShare(year) {
  const bs = electionOf(year).ballot.filter(b => b.id !== "No vote");
  const share = b => b.id === "Other" ? -1 : (whoValue(year, 0, 0, b.id) || 0);
  return bs.slice().sort((a, b) => share(b) - share(a));
}
function ballotName(year, id) {
  const e = electionOf(year), b = e && e.ballot.find(x => x.id === id);
  return b ? b.name : name(id);
}
function toast(msg) {
  const t = document.createElement("div"); t.className = "toast"; t.textContent = msg; document.body.appendChild(t);
  setTimeout(() => t.remove(), 1600);
}
function option(sel, value, label) { const o = document.createElement("option"); o.value = value; o.textContent = label; sel.appendChild(o); }
function fillSelect(sel, items, current) { sel.innerHTML = ""; items.forEach(([v, l]) => option(sel, v, l)); sel.value = String(current); }
function quantile(sorted, p) {
  const i = (sorted.length - 1) * p, lo = Math.floor(i), hi = Math.ceil(i);
  return sorted[lo] + (sorted[hi] - sorted[lo]) * (i - lo);
}

// A symmetric axis about zero whose ticks include zero: the step is 5, 10 or 20 points and the limit a multiple of it
function niceAxis(maxAbs) {
  const step = maxAbs <= 10 ? 5 : maxAbs <= 30 ? 10 : 20;
  return {step, lim: Math.max(step, Math.ceil(maxAbs / step) * step)};
}

// The title and subtitle inside a figure, so that a PNG holds them (the other pages draw their panels this way)
function subLines(sub, W) {
  const max = Math.floor((W - 8) / 5.4), lines = [];
  (sub || "").split(" · ").forEach(part => { const last = lines.length - 1; if (last >= 0 && (lines[last] + " · " + part).length <= max) lines[last] += " · " + part; else lines.push(part); });
  return lines;
}
function figureHead(svg, title, sub, W) {
  txt(svg, 0, 13, title, {"font-size": 13, "font-weight": 500, fill: "#141414"});
  const lines = subLines(sub, W);
  lines.forEach((l, i) => txt(svg, 0, 28 + 14 * i, l, {"font-size": 11, fill: "#767676"}));
  return lines.length;
}

// ---- links to the other pages: keys are those of their addresses (#e=, #it=, #pt=, #a=)
const aLink = (href, label) => `<a href="${esc(href)}" target="_top">${esc(label)}</a>`;
function listLinks(year, id) {
  const L = D.links && D.links[year] && D.links[year][id];
  if (!L) return [];
  const out = [aLink(`${SITE}election-results.html#e=sejm-${year}&m=share&it=${L.er}`, "Results map")];
  if (L.pt) out.push(aLink(`${SITE}party-genealogy.html#pt=${L.pt}`, "Party history"));
  if (L.ide) out.push(aLink(`${SITE}party-ideology.html#pt=${L.ide}`, "Ideology"));
  return out;
}
const electionLinks = year => [aLink(`${SITE}election-results.html#e=sejm-${year}`, `${year} election results`), aLink(`${SITE}party-system-indicators.html#e=${year}`, `${year} party system indicators`)];
const trendLinks = () => [aLink(`${SITE}political-attitudes.html#a=left-right`, "Left–right trends"), aLink(`${SITE}political-attitudes.html#a=religious-practice`, "Religious practice trends")];

// ---- line chart ---------------------------------------------------------------------------------
// o: {xs, ymin, ymax, ystep, fmt, series: [{name, color, vals, lo, hi, dash, rings}], tip(i) -> html, h, zero}
function lineChart(svgId, tipId, o) {
  const svg = $(svgId), box = svg.parentNode;
  const W = Math.max(320, box.clientWidth - 16), H = o.h || (o.title ? 300 : 270);
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.innerHTML = "";
  const L = 44, R = W - 12, B = H - 24, n = o.xs.length;
  const T = o.title ? 30 + 14 * figureHead(svg, o.title, o.sub, W) : 10;
  const X = i => L + (R - L) * (n === 1 ? 0.5 : i / (n - 1));
  const Y = v => B - (B - T) * (v - o.ymin) / (o.ymax - o.ymin);
  for (let g = o.ymin; g <= o.ymax + 1e-9; g += o.ystep) {
    el("line", {x1: L, x2: R, y1: Y(g), y2: Y(g), stroke: "#ECECEC", "stroke-width": 1}, svg);
    txt(svg, L - 8, Y(g) + 4, o.fmt(g), {"text-anchor": "end"});
  }
  if (o.ymin < 0 && o.ymax > 0) el("line", {x1: L, x2: R, y1: Y(0), y2: Y(0), stroke: "#141414", "stroke-width": 1.8}, svg);   // the zero line: increases above, decreases below
  o.xs.forEach((x, i) => txt(svg, X(i), H - 7, x, {"text-anchor": "middle"}));
  o.series.forEach(s => {
    const idx = s.vals.map((v, i) => v == null ? null : i).filter(i => i != null);
    if (s.lo && idx.length > 1) {
      const up = idx.map(i => `${X(i)} ${Y(s.hi[i])}`), dn = idx.slice().reverse().map(i => `${X(i)} ${Y(s.lo[i])}`);
      el("path", {d: `M${up.join("L")}L${dn.join("L")}Z`, fill: s.color, opacity: 0.16}, svg);
    }
    if (!s.rings) {
      let d = "", pen = false;
      s.vals.forEach((v, i) => { if (v == null) { pen = false; return; } d += `${pen ? "L" : "M"}${X(i)} ${Y(v)}`; pen = true; });
      el("path", {d, fill: "none", stroke: s.color, "stroke-width": 2.4, "stroke-linejoin": "round", "stroke-dasharray": s.dash ? "6 4" : null}, svg);
    }
    s.vals.forEach((v, i) => {
      if (v == null) return;
      if (s.rings) el("circle", {cx: X(i), cy: Y(v), r: 4.5, fill: "#fff", stroke: s.color, "stroke-width": 2}, svg);
      else el("circle", {cx: X(i), cy: Y(v), r: 3, fill: s.color}, svg);
    });
  });
  const guide = el("line", {y1: T, y2: B, stroke: "#4B4B4B", "stroke-width": 1, "stroke-dasharray": "3 3", visibility: "hidden"}, svg);
  const tip = tipId ? $(tipId) : null;
  const move = ev => {
    const r = svg.getBoundingClientRect(), cx = (ev.touches ? ev.touches[0].clientX : ev.clientX) - r.left;
    const x = cx * W / r.width, i = clamp(Math.round((x - L) / (R - L) * (n - 1)), 0, n - 1);
    guide.setAttribute("x1", X(i)); guide.setAttribute("x2", X(i)); guide.setAttribute("visibility", "visible");
    if (!tip) return;
    const html = o.tip ? o.tip(i) : "";
    if (!html) { tip.hidden = true; return; }
    tip.innerHTML = html; tip.hidden = false;
    const bw = box.clientWidth, tw = tip.offsetWidth, px = X(i) * r.width / W + 12;
    tip.style.left = clamp(px + tw > bw - 8 ? px - tw - 24 : px, 4, bw - tw - 4) + "px"; tip.style.top = "26px";
  };
  const leave = () => { guide.setAttribute("visibility", "hidden"); if (tip) tip.hidden = true; };
  svg.onmousemove = move; svg.onmouseleave = leave; svg.ontouchstart = move; svg.ontouchmove = move; svg.ontouchend = leave;
}
function legend(id, items) {
  LEGEND[id] = items;
  $(id).innerHTML = items.map(([label, color, kind]) =>
    `<span><i class="sw ${kind === "ring" ? "ring" : kind === "band" ? "band" : ""}" style="${kind === "ring" ? "border-color:" + color : kind === "band" ? "background:" + color : "border-color:" + color + (kind === "dash" ? ";border-top-style:dashed" : "")}"></i>${esc(label)}</span>`).join("");
}
const tipRow = (color, label, value, sub) =>
  `<div class="r"><span><i class="dot" style="background:${color}"></i> ${esc(label)}</span><span>${value}</span></div>${sub ? `<div class="s">${sub}</div>` : ""}`;

// ---- Who votes ------------------------------------------------------------------------------------
function listCats() {
  const seen = [];
  D.elections.forEach(e => e.ballot.forEach(b => { if (b.id !== "No vote" && b.id !== "Other" && !seen.includes(b.id)) seen.push(b.id); }));
  return seen;
}
function official(year, cat) {
  if (cat === "turnout") return electionOf(year).turnout_official;
  const r = D.vs_official.find(x => x.year === year && x.category === cat);
  return r ? r.official : null;
}
function whoValue(year, g, l, cat) {
  const r = GR.get(`${year}|${g}|${l}`);
  if (!r) return null;
  if (cat === "turnout") return r.turnout;
  const v = r.shares[cat];
  return v == null ? null : v;
}
function drawWho() {
  const sp = S.split, isAll = S.show === "all" && sp === 0;
  if (sp > 0 && S.show === "all") S.show = "turnout";
  const series = [], leg = [];
  const lists = isAll ? listCats() : [S.show];
  if (sp === 0) {
    lists.forEach(cat => {
      const c = cat === "turnout" ? ACCENT : colour(cat);
      series.push({name: name(cat), color: c, vals: YEARS.map(y => whoValue(y, 0, 0, cat))});
      series.push({name: name(cat) + " (official)", color: c, rings: true, official: true, vals: YEARS.map(y => official(y, cat))});
      leg.push([name(cat), c]);
    });
    leg.push(["Official result", MUTED, "ring"]);
  } else {
    const levels = D.groupings[sp].levels;
    levels.forEach((lv, i) => {
      series.push({name: lv, color: LEVEL_COL[i % 5], vals: YEARS.map(y => whoValue(y, sp, i, S.show))});
      leg.push([lv, LEVEL_COL[i % 5]]);
    });
    series.push({name: "Everyone", color: "#4B4B4B", dash: true, vals: YEARS.map(y => whoValue(y, 0, 0, S.show))});
    leg.push(["Everyone", "#4B4B4B", "dash"]);
  }
  const turnout = S.show === "turnout";
  const top = Math.max(...series.flatMap(s => s.vals.filter(v => v != null)));
  const ymax = turnout ? 1 : Math.max(0.1, Math.ceil(top * 10 + 0.001) / 10), ystep = turnout ? 0.2 : ymax > 0.5 ? 0.1 : 0.05;
  const gid = D.groupings[sp].id.toLowerCase();
  const wtitle = (turnout ? "Turnout" : isAll ? "Support for each list among voters" : `${name(S.show)}: share of voters`) + (sp ? ` by ${gid}` : "");
  const wsub = (turnout ? "Share who say they voted" : "Share of the voters who name a list") + ", weighted survey" + (sp === 0 ? " · rings: official result" : "");
  lineChart("fig-who", "tip-who", {
    xs: YEARS, ymin: 0, ymax, ystep, fmt: v => Math.round(v * 100) + "%", series, title: wtitle, sub: wsub,
    tip: i => {
      const y = YEARS[i], rows = [];
      series.filter(s => !s.official).forEach(s => {
        if (s.vals[i] == null) return;
        const off = sp === 0 ? series.find(o => o.official && o.name === s.name + " (official)") : null;
        rows.push(tipRow(s.color, s.name, pct(s.vals[i]), off && off.vals[i] != null ? "official " + pct(off.vals[i]) : ""));
      });
      if (!rows.length) return "";
      const g = GR.get(`${y}|${sp}|0`), e = electionOf(y);
      return `<b>${y}</b>${rows.join("")}<div class="s">${e.in_model.toLocaleString("en-GB")} respondents in the model${g && sp > 0 ? "" : ""}</div>`;
    }
  });
  legend("leg-who", leg);
  const what = S.show === "turnout" ? "the share who say they voted" : S.show === "all" ? "each list's share of the voters who name a list" : `${name(S.show)}'s share of the voters who name a list`;
  $("whohelp").textContent = `Lines: ${what}, weighted survey figures. ` + (sp === 0 ? "Rings: the official results of the National Electoral Commission; the gap is the survey's overstatement of turnout and of the winners." : `Each line is a group of respondents; the dashed line is everyone. Groups are small in places: the number behind each point is in the table below the chart when you hover.`) + " Other lists are not drawn as a line because they mean different lists at different elections.";
}
function drawComposition() {
  const yr = S.cel, by = S.by, levels = D.groupings[by].levels, ballot = listsByShare(yr);
  const every = Object.fromEntries(ballot.map(b => [b.id, whoValue(yr, 0, 0, b.id) || 0]));
  const rows = ballot.map(b => ({id: b.id, name: b.name, vals: levels.map((_, l) => (COMP.get(`${yr}|${b.id}|${by}|${l}`) || {share: null}).share)}));
  const all = levels.map((_, l) => ballot.reduce((a, b) => a + every[b.id] * ((COMP.get(`${yr}|${b.id}|${by}|${l}`) || {share: 0}).share || 0), 0));
  const cell = v => v == null ? "<td>–</td>" : `<td><span class="cell" style="background:rgba(139,0,0,${(0.04 + 0.6 * v).toFixed(2)});${v > 0.45 ? "color:#fff" : ""}">${(100 * v).toFixed(0)}%</span></td>`;
  $("comp").innerHTML = `<table class="t"><thead><tr><th>${yr}</th>${levels.map(l => `<th>${esc(l)}</th>`).join("")}</tr></thead><tbody>` +
    `<tr><td><b>All voters</b><div class="al">${electionLinks(yr).join(" · ")}</div></td>${all.map(cell).join("")}</tr>` +
    rows.map(r => `<tr><td><i class="dot" style="background:${colour(r.id)}"></i> ${esc(r.name)}<div class="al">${listLinks(yr, r.id).join(" · ")}</div></td>${r.vals.map(cell).join("")}</tr>`).join("") + "</tbody></table>";
}
function renderWho() {
  $("wholead").textContent = "Turnout and support for each list in the eight Sejm elections from 1997 to 2023, as the Polish General Election Studies recorded them, with the official results alongside. Split the survey by age, gender, education, residence, religious attendance or socio-economic position to see who differs.";
  drawWho(); drawComposition();
}

// ---- What changes the vote ----------------------------------------------------------------------------
function effectRows(year, ct) { return D.effects.rows.filter(r => r.year === year && D.effects.contrasts[r.effect].id === ct); }
function effLabel(ct) { return D.effects.contrasts.find(c => c.id === ct).label; }
function drawEffects() {
  const rows = effectRows(S.el, S.ct), ver = S.cal ? "cal" : "uncal", other = S.cal ? "uncal" : "cal";
  const order = [electionOf(S.el).ballot.find(b => b.id === "No vote"), ...listsByShare(S.el)];
  const items = order.map(b => ({b, r: rows.find(x => x.category === b.id)})).filter(x => x.r);
  const svg = $("fig-eff"), W = Math.max(320, svg.parentNode.clientWidth - 16), rowH = 26, sub = `Change in the probability of each choice, percentage points · ${S.el} · 90% interval · ${S.cal ? "adjusted to the official results" : "unadjusted"}`, T = 38 + 14 * subLines(sub, W).length, Lm = Math.min(220, W * 0.34), Rm = W - 44, H = T + rowH * items.length + 46;
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.innerHTML = "";
  figureHead(svg, effLabel(S.ct), sub, W);
  const {lim, step} = niceAxis(Math.max(...items.flatMap(x => [Math.abs(x.r[ver][1]), Math.abs(x.r[ver][2])])));
  const X = v => Lm + (Rm - Lm) * (v + lim) / (2 * lim);
  for (let g = -lim; g <= lim + 1e-9; g += step) {
    el("line", {x1: X(g), x2: X(g), y1: T - 6, y2: T + rowH * items.length, stroke: "#ECECEC", "stroke-width": 1}, svg);
    txt(svg, X(g), T + rowH * items.length + 16, (g > 0 ? "+" : g < 0 ? MINUS : "") + Math.abs(g), {"text-anchor": "middle"});
  }
  el("line", {x1: X(0), x2: X(0), y1: T - 6, y2: T + rowH * items.length, stroke: "#141414", "stroke-width": 1.8}, svg);   // the zero line
  txt(svg, Rm, T + rowH * items.length + 34, "percentage points", {"text-anchor": "end"});
  items.forEach((x, i) => {
    const cy = T + rowH * i + rowH / 2, [m, lo, hi] = x.r[ver], c = colour(x.b.id), clear = lo > 0 || hi < 0;
    txt(svg, Lm - 10, cy + 4, x.b.name.length > 36 ? x.b.name.slice(0, 34) + "…" : x.b.name, {"text-anchor": "end", fill: "#141414", "font-size": 12.5});
    el("line", {x1: X(lo), x2: X(hi), y1: cy, y2: cy, stroke: c, "stroke-width": 3, "stroke-linecap": "round", opacity: clear ? 1 : 0.45}, svg);
    el("circle", {cx: X(m), cy, r: 4.5, fill: clear ? c : "#fff", stroke: c, "stroke-width": 2}, svg);
    txt(svg, X(hi) + 8, cy + 4, pp(m), {fill: "#141414", "font-size": 12});
    const hit = el("rect", {x: 0, y: cy - rowH / 2, width: W, height: rowH, fill: "transparent"}, svg);
    const o = x.r[other];
    el("title", {}, hit).textContent = `${x.b.name}: ${pp(m)} (${pp(lo)} to ${pp(hi)}); ${S.cal ? "unadjusted" : "adjusted to the official results"}: ${pp(o[0])} (${pp(o[1])} to ${pp(o[2])})`;
  });
}
function drawEffects2() {
  const out = S.out, ver = S.cal ? "cal" : "uncal";
  const vals = YEARS.map(y => { const r = effectRows(y, S.ct).find(x => x.category === out); return r ? r[ver] : null; });
  const nz = vals.filter(Boolean), {lim, step} = niceAxis(Math.max(...nz.flatMap(v => [Math.abs(v[1]), Math.abs(v[2])])));
  const c = out === "No vote" ? "#9aa5a9" : colour(out);
  lineChart("fig-eff2", "tip-eff2", {
    xs: YEARS, ymin: -lim, ymax: lim, ystep: step, fmt: v => (v > 0 ? "+" : v < 0 ? MINUS : "") + Math.abs(v), h: 260,
    title: `${effLabel(S.ct)}: ${name(out)} across elections`, sub: `Change in the probability, percentage points · band: 90% interval · ${S.cal ? "adjusted to the official results" : "unadjusted"}`,
    series: [{name: name(out), color: c, vals: vals.map(v => v && v[0]), lo: vals.map(v => v && v[1]), hi: vals.map(v => v && v[2])}],
    tip: i => vals[i] ? `<b>${YEARS[i]}</b>${tipRow(c, name(out), pp(vals[i][0]) + " points", `90% interval ${pp(vals[i][1])} to ${pp(vals[i][2])}`)}` : ""
  });
}
function renderEffects() {
  $("efflead").textContent = `${effLabel(S.ct)}: how much more or less likely each choice is for a person like the average respondent of the election, other characteristics held as observed.`;
  drawEffects(); drawEffects2();
  $("effhelp").textContent = "Each estimate is the change in the probability of the choice, in percentage points, when everyone is set from the first level to the second, averaged over the election's respondents. Bars are 90% credible intervals; a filled dot means the interval excludes zero. " +
    (S.cal ? "Adjusted: each list's probability is shifted so that the survey reproduces the official turnout and list shares, which removes the survey's overstatement of turnout and of the winners." : "Unadjusted: as the survey records them, including its overstatement of turnout and of the winners. Switch on the adjustment to see the effects once the survey is brought into line with the official results.") +
    " Hover a row to see the other version.";
  const also = [...electionLinks(S.el), ...(S.ct.startsWith("relig") ? trendLinks().slice(1) : [])];
  $("eff-also").innerHTML = "See also: " + also.join(" · ");
}

// ---- Hypothetical voter ------------------------------------------------------------------------------------------
function loadCalc(year) {
  if (CALC[year]) return Promise.resolve(CALC[year]);
  return fetch(`calc-${year}.json`).then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).then(j => (CALC[year] = j));
}
function design(cd, p) {
  const d = cd.design, c = d.centres, ab = d.age_basis.values[clamp(p.age, 18, 98) - 18];
  const relig = (p.rel - d.relig_centre) / d.relig_scale, ses = d.ses_z_by_quintile[p.ses - 1], female = p.sex === "f" ? 1 : 0;
  const raw = [...ab, female, p.ed === 2 ? 1 : 0, p.ed === 3 ? 1 : 0, p.res === 2 ? 1 : 0, p.res === 3 ? 1 : 0, p.res === 4 ? 1 : 0, p.res === 5 ? 1 : 0, relig, relig * relig, ses, ses * ses];
  const X = raw.map((v, i) => v - c[i]), ageC = (p.age - d.age_mean) / d.age_sd, bigC = (p.res >= 4 ? 1 : 0) - d.bigcity_mean;
  return {X, XI: [ageC * X[5], ageC * X[6], ageC * X[11], bigC * X[5], bigC * X[6], X[4] * ageC]};
}
// Draws x outcomes (the reference first) of probabilities; cal adds the calibration shifts
function probabilities(cd, p, cal) {
  const {X, XI} = design(cd, p), P = X.length, K = cd.ballot.length, out = [];
  for (let d = 0; d < cd.n_draws; d++) {
    const eta = [0];
    for (let k = 0; k < K; k++) {
      const cf = cd.coef[d][k];
      let e = cf[0];
      for (let j = 0; j < P; j++) e += X[j] * cf[1 + j];
      for (let q = 0; q < XI.length; q++) e += XI[q] * cf[1 + P + q];
      eta.push(e + (cal ? cd.shifts[d][k] : 0));
    }
    const m = Math.max(...eta), ex = eta.map(v => Math.exp(v - m)), s = ex.reduce((a, b) => a + b, 0);
    out.push(ex.map(v => v / s));
  }
  return out;
}
// Attitudes: for each position, the probability of 0..10 in each draw (cumulative logit), summarised over draws
const ATT_STYLE = {lr: ["#141414", true], econ: ["#D32F2F", false], soc: ["#2F7FC1", false]};
function attitudes(cd, p) {
  const {X, XI} = design(cd, p), x = [...X, ...XI];
  return cd.attitudes.positions.map(pos => {
    const draws = pos.beta.map((b, d) => {
      let eta = 0; for (let j = 0; j < x.length; j++) eta += x[j] * b[j];
      const cum = pos.cut[d].map(c => 1 / (1 + Math.exp(-(c - eta)))), out = [];
      for (let k = 0; k <= 10; k++) out.push((k < 10 ? cum[k] : 1) - (k > 0 ? cum[k - 1] : 0));
      return out;
    });
    const s = Array.from({length: 11}, (_, k) => { const v = draws.map(d => d[k]).sort((a, b) => a - b); return {m: quantile(v, 0.5), lo: quantile(v, 0.1), hi: quantile(v, 0.9)}; });
    return {pos, s};
  });
}
function drawAtt(cd, p) {
  const res = attitudes(cd, p), top = Math.max(...res.flatMap(r => r.s.map(v => v.hi))), step = top > 0.4 ? 0.1 : top > 0.2 ? 0.05 : 0.025, ymax = Math.ceil(top / step) * step;
  const series = res.map(r => ({name: r.pos.label, color: ATT_STYLE[r.pos.id][0], dash: ATT_STYLE[r.pos.id][1], vals: r.s.map(v => v.m), lo: r.s.map(v => v.lo), hi: r.s.map(v => v.hi), id: r.pos.id}));
  lineChart("fig-att", "tip-att", {
    xs: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10], ymin: 0, ymax, ystep: step, fmt: v => Math.round(v * 1000) / 10 + "%", h: 300, series,
    title: "Where this voter is likely to stand", sub: `Probability of each position, 0 (left, progressive) to 10 (right, conservative) · median and 80% interval · ${S.k.el}`,
    tip: i => `<div class="h">Position ${i}</div>` + series.map(s => tipRow(s.color, s.name, Math.round(s.vals[i] * 100) + "%", `${Math.round(s.lo[i] * 100)}–${Math.round(s.hi[i] * 100)}%`)).join("")
  });
  legend("leg-att", [...series.map(s => [s.name, s.color, s.dash ? "dash" : null]), ["80% interval", "#9a9a9a", "band"]]);
  const med = res.map(r => { let c = 0, k = 0; for (; k <= 10; k++) { c += r.s[k].m; if (c >= 0.5) break; } return `${r.pos.label.toLowerCase()} ${k}`; });
  $("att-note").textContent = `Median positions: ${med.join(", ")}. The economic and socio-cultural positions are the mean of ${cd.attitudes.positions[1].items.length} economic and ${cd.attitudes.positions[2].items.length} socio-cultural survey items in ${S.k.el}, which differ between elections.`;
}
function summarise(draws) {
  const K = draws[0].length;
  return Array.from({length: K}, (_, k) => {
    const v = draws.map(d => d[k]).sort((a, b) => a - b);
    return {m: quantile(v, 0.5), lo: quantile(v, 0.05), hi: quantile(v, 0.95)};
  });
}
function supportOf(cd, p) {
  const s = cd.support, [a0, , step] = s.age_grid, i = clamp(Math.round((p.age - a0) / step), 0, s.age_education.length - 1), female = p.sex === "f" ? 1 : 0;
  const pairs = [
    ["age and education", s.age_education[i][p.ed - 1]], ["age and religious attendance", s.age_religiosity[i][p.rel - 1]],
    ["age and gender", s.age_gender[i][female]], ["residence and education", s.residence_education[p.res - 1][p.ed - 1]]
  ];
  return pairs.reduce((a, b) => (b[1] < a[1] ? b : a));
}
// {level: "stop" | "warn" | "ok", message}
function judge(cd, p) {
  const r = cd.rules;
  if (p.age < r.min_age) return {level: "stop", message: "Voters are at least 18."};
  if (p.age > r.max_age) return {level: "stop", message: `The oldest respondent in ${cd.year} was ${r.max_age}, so the survey says nothing about older people.`};
  if (p.ed === 3 && p.age <= 20) return {level: "stop", message: r.impossible[0].message};
  const [pair, n] = supportOf(cd, p);
  if (n < r.support_stop_below) return {level: "stop", message: `${r.support_messages.stop} Only about ${n.toFixed(0)} respondents are like this person in age and ${pair.split(" and ")[1]}.`};
  const warns = [];
  if (p.ed === 3 && p.age <= 23) warns.push(r.warn[0].message);
  if (n < r.support_warn_below) warns.push(`${r.support_messages.warn} About ${n.toFixed(0)} respondents are like this person in ${pair}.`);
  if (warns.length) return {level: "warn", message: warns.join(" ")};
  return {level: "ok", message: `Well supported: at least ${Math.floor(n)} respondents in ${cd.year} are like this person in every pair of characteristics the model combines.`};
}
function profileText() {
  const k = S.k, sel = id => $(id).selectedOptions[0].textContent.toLowerCase();
  return `${k.age}-year-old ${k.sex === "f" ? "woman" : "man"} · ${sel("k-ed")} education · ${sel("k-res")} · attends ${sel("k-rel")} · ${sel("k-ses")} socio-economic position`;
}
function profile() {
  const k = S.k;
  return {age: k.age, sex: k.sex, ed: k.ed, res: k.res, rel: k.rel, ses: k.ses};
}
function renderCalc() {
  const k = S.k;
  loadCalc(k.el).then(cd => {
    const sl = $("k-age");
    sl.max = Math.min(90, cd.rules.max_age); if (k.age > +sl.max) k.age = +sl.max; sl.value = k.age; $("k-age-o").textContent = k.age;
    document.querySelectorAll("#k-sex button").forEach(b => b.setAttribute("aria-pressed", b.dataset.s === k.sex));
    const p = profile(), j = judge(cd, p), note = $("k-note"), bars = $("k-bars");
    note.className = "notice " + (j.level === "stop" ? "stop" : j.level === "warn" ? "" : "ok"); note.textContent = j.message;
    bars.innerHTML = ""; CALC_ROWS = null;
    if (j.level !== "stop") {
      const sm = summarise(probabilities(cd, p, k.cal)), names = ["Did not vote", ...cd.ballot.map(b => b.name)], ids = ["No vote", ...cd.ballot.map(b => b.id)];
      CALC_ROWS = sm.map((s, i) => ({s, id: ids[i], nm: i === 0 ? "Did not vote" : cd.ballot[i - 1].name})).sort((a, b) => b.s.m - a.s.m);
      CALC_ROWS.forEach(r => {
        const row = document.createElement("div"); row.className = "bar";
        row.innerHTML = `<span>${esc(r.nm)}</span><span class="track"><span class="fill" style="width:${(100 * r.s.m).toFixed(1)}%;background:${r.id === "No vote" ? "#9aa5a9" : colour(r.id)}"></span><span class="ci" style="left:${(100 * r.s.lo).toFixed(1)}%;width:${(100 * (r.s.hi - r.s.lo)).toFixed(1)}%"></span></span><span class="num">${Math.round(100 * r.s.m)}% <i>(${Math.round(100 * r.s.lo)}–${Math.round(100 * r.s.hi)})</i></span>`;
        bars.appendChild(row);
      });
    }
    $("khelp").textContent = `Each bar is the median probability across the model's 200 posterior draws; the line is the 90% interval. ${k.cal ? "Adjusted to the official results: each list's chance is shifted so that the survey reproduces the official turnout and list shares." : "Unadjusted: as the survey records votes, so turnout and the winning list are somewhat too high."} Other lists means different lists at different elections.`;
    const ab = $("att-box");
    if (j.level !== "stop" && cd.attitudes) { ab.hidden = false; $("leg-att").hidden = false; drawAtt(cd, p); } else { ab.hidden = true; $("leg-att").hidden = true; $("att-note").textContent = ""; }
    $("calc-also").innerHTML = "See also: " + [...electionLinks(k.el), ...(k.rel >= 4 ? trendLinks().slice(1) : [])].join(" · ");
    drawAll();
  }).catch(() => { $("k-note").className = "notice stop"; $("k-note").textContent = "The calculator's data for this election could not be loaded."; });
}
function drawAll() {
  const k = S.k, p = profile();
  Promise.all(YEARS.map(loadCalc)).then(cds => {
    const cats = ["No vote", ...listCats(), "Other"], series = [], info = YEARS.map(() => ({}));
    const res = cds.map((cd, yi) => {
      const j = judge(cd, p);
      if (j.level === "stop") return {j, ids: null};
      const sm = summarise(probabilities(cd, p, k.cal)), ids = ["No vote", ...cd.ballot.map(b => b.id)];
      return {j, ids, sm};
    });
    cats.forEach(cat => {
      const vals = res.map(r => { if (!r.ids) return null; const i = r.ids.indexOf(cat); return i < 0 ? null : r.sm[i].m; });
      if (vals.some(v => v != null)) series.push({name: name(cat), color: cat === "No vote" ? "#9aa5a9" : colour(cat), vals, cat});
    });
    const note = res.map((r, i) => r.ids ? null : YEARS[i]).filter(Boolean);
    $("k-allnote").textContent = note.length ? `No estimate for ${note.join(", ")}: the profile is outside what the survey supports in those elections.` : "";
    lineChart("fig-all", "tip-all", {
      xs: YEARS, ymin: 0, ymax: 1, ystep: 0.2, fmt: v => Math.round(v * 100) + "%", h: 290, series,
      title: "The same person at every election", sub: `${profileText()} · median probability${k.cal ? " · adjusted to the official results" : ""}`,
      tip: i => {
        const r = res[i];
        if (!r.ids) return `<b>${YEARS[i]}</b><div class="s">${esc(r.j.message)}</div>`;
        return `<b>${YEARS[i]}</b>` + r.ids.map((id, q) => ({id, q, s: r.sm[q]})).sort((a, b) => b.s.m - a.s.m).slice(0, 7).map(x => tipRow(x.id === "No vote" ? "#9aa5a9" : colour(x.id), ballotName(YEARS[i], x.id), pct(x.s.m, 0), `90% interval ${pct(x.s.lo, 0)}–${pct(x.s.hi, 0)}`)).join("");
      }
    });
    legend("leg-all", series.map(s => [s.name, s.color]));
  });
}

// ---- Model and data ------------------------------------------------------------------------------------------
function renderAbout() {
  const m = D.meta, el_ = D.elections;
  const sample = `<table class="t"><thead><tr><th>Election</th><th>Interviewed</th><th>In the model</th><th>Turnout, survey</th><th>Turnout, official</th><th>Gap (points)</th></tr></thead><tbody>` +
    el_.map(e => `<tr><td>${e.year}</td><td>${e.interviewed.toLocaleString("en-GB")}</td><td>${e.in_model.toLocaleString("en-GB")}</td><td>${pct(e.turnout_survey)}</td><td>${pct(e.turnout_official)}</td><td>${pp(100 * (e.turnout_survey - e.turnout_official))}</td></tr>`).join("") + "</tbody></table>";
  const cats = listCats().concat("Other");
  const shareTable = `<table class="t"><thead><tr><th>List</th>${YEARS.map(y => `<th>${y}</th>`).join("")}</tr></thead><tbody>` +
    cats.map(c => `<tr><td><i class="dot" style="background:${colour(c)}"></i> ${esc(name(c))}</td>${YEARS.map(y => {
      const r = D.vs_official.find(x => x.year === y && x.category === c);
      return r ? `<td title="survey ${pct(r.survey)}, official ${pct(r.official)}">${pp(100 * (r.survey - r.official))}</td>` : "<td></td>";
    }).join("")}</tr>`).join("") + "</tbody></table>";
  const checks = m.fit_checks.loeo_gain_over_drifting_main_effects.map(c => `<tr><td>${esc(SPEC_NAMES[c.spec] || c.spec)}</td><td>${(c.difference >= 0 ? "+" : MINUS) + Math.abs(c.difference).toFixed(4)}</td><td>${c.se.toFixed(4)}</td></tr>`).join("");
  const sens = m.sensitivity.imputation.map(r => `<tr><td>${esc(SHIFT_NAMES[r.spec] || r.spec)}</td><td>${r.cells}</td><td>${r.mean_abs_difference.toFixed(2)}</td><td>${r.max_abs_difference.toFixed(1)}</td><td>${pct(r.share_outside_cc_interval)}</td><td>${r.conclusions_changed}</td><td>${r.conclusions_flipped}</td></tr>`).join("");
  const cite = window.ChartExport ? ChartExport.citeHtml(PAGE) : "Ben Stanley, 2026. <i>Voting behaviour</i>, <b>Polish Political Analyses</b>, benstanley.eu.";
  $("about").innerHTML = `
    <h3>The model</h3><p>${esc(m.model.description)} It uses ${m.model.respondents.toLocaleString("en-GB")} of the ${m.model.interviewed.toLocaleString("en-GB")} people interviewed (${m.model.draws.toLocaleString("en-GB")} posterior draws; no divergences, largest R-hat ${m.model.max_rhat}). Respondents with a missing answer to any characteristic, and voters who give no list, are left out; the checks below show that bringing them back changes none of the conclusions.</p>
    <h3>Who is in the sample</h3><div class="tscroll">${sample}</div>
    <p class="help">Self-reported turnout is overstated in every survey. People also tend to recall voting for the winner.</p>
    <h3>How the survey compares with the official results</h3><p>Each cell is the survey's share of the voters who name a list minus the official share, in percentage points (hover for both). Positive means the survey overstates the list.</p><div class="tscroll">${shareTable}</div>
    <h3>How the model was chosen</h3><p>Each specification was fitted without one election at a time and asked to predict who chose what in the election it had not seen, with that election's overall levels taken from its own results. The table gives the gain in held-out accuracy (the mean log probability given to what each respondent did) over the reference, with a standard error that treats respondents as independent and so is optimistic.</p>
    <div class="tscroll"><table class="t"><thead><tr><th>Specification</th><th>Gain over the reference</th><th>Standard error</th></tr></thead><tbody>${checks}</tbody></table></div>
    <h3>Voters who give no list</h3><p>${esc(m.notes.refusers)} The table compares the average marginal effects with those from complete cases. The imputed rows fill in each missing list from the person's characteristics, left–right placement and the party they feel close to, and then push the imputed voters further left.</p>
    <div class="tscroll"><table class="t"><thead><tr><th>Version</th><th>Estimates compared</th><th>Mean difference (points)</th><th>Largest (points)</th><th>Outside the complete-case interval</th><th>Conclusions changed</th><th>Signs flipped</th></tr></thead><tbody>${sens}</tbody></table></div>
    <h3>Left-right, economic and socio-cultural positions</h3><p>In Hypothetical voter, the chart of positions shows how likely a person like the one you chose is to stand at each point from 0 to 10. Left-right is the survey's self-placement. The economic and socio-cultural positions are the mean of the survey's questions on those issues (tax, welfare, privatisation and similar; abortion, the Church, crime and similar), each placed on 0 to 10 with 10 the right or conservative end, for respondents who answered at least two. Each election and position has its own ordinal model on the same six characteristics as the vote model, with 80% intervals from 200 draws. The questions differ between elections and change format in 2011, so the economic and socio-cultural positions can be compared between people in one election but not between elections.</p>
    <h3>Polarisation</h3><p>Affective polarisation uses the survey's like–dislike scales (0 to 10) for the lists it asks about; not every list is asked about in every election, and the lists rated are named under the chart. For the voters of each list, the gap is their mean like for their own list minus their mean like for the other rated lists, weighted by those lists' official vote shares. Reiljan's index (2020) is the average of these gaps across lists, weighted by vote share, with a 90% bootstrap interval. Reiljan uses reported partisans; this page uses recalled voters, so the figures are not strictly comparable with his. Ideological polarisation shows the weighted share of each list's voters at each position from 0 to 10 on the left-right, economic and socio-cultural measures described above, for the two lists chosen.</p>
    <h3>Reading the page</h3><ul>${["ses", "residence", "calibration", "other"].map(k => `<li>${esc(m.notes[k])}</li>`).join("")}<li>These are associations at eight elections, not causes, and the page says nothing about the current term.</li></ul>
    <h3>Related pages</h3><p>${[aLink(SITE + "election-results.html", "Election results"), aLink(SITE + "party-genealogy.html", "Parties in the Sejm"), aLink(SITE + "party-ideology.html", "Party ideology"), aLink(SITE + "party-system-indicators.html", "Party system indicators"), aLink(SITE + "political-attitudes.html#a=left-right", "Social and political trends")].join(" · ")}</p>
    <p class="cite">${cite}</p>`;
}

// ---- links, downloads ---------------------------------------------------------------------------------------------
function hashTarget() { try { if (window.top !== window && window.top.location.host === location.host) return window.top; } catch (e) { /* cross-origin */ } return window; }
function stateString() {
  const q = new URLSearchParams(), k = S.k;
  if (S.view !== "who") q.set("view", S.view);
  if (S.show !== "all") q.set("show", S.show);
  if (S.split) q.set("sp", S.split);
  if (S.cel !== 2023) q.set("cel", S.cel);
  if (S.by !== 1) q.set("by", S.by);
  if (S.ct !== "educ_higher") q.set("ct", S.ct);
  if (S.el !== 2023) q.set("el", S.el);
  if (S.out !== "No vote") q.set("out", S.out);
  if (S.cal) q.set("cal", "1");
  if (k.el !== 2023) q.set("kel", k.el);
  if (k.age !== 45) q.set("age", k.age);
  if (k.sex !== "f") q.set("sex", k.sex);
  if (k.ed !== 2) q.set("ed", k.ed);
  if (k.res !== 3) q.set("res", k.res);
  if (k.rel !== 4) q.set("rel", k.rel);
  if (k.ses !== 3) q.set("ses", k.ses);
  if (k.cal) q.set("kcal", "1");
  if (S.pv !== "aff") q.set("pv", S.pv);
  if (S.pe !== 2023) q.set("pe", S.pe);
  if (S.pa !== "PiS") q.set("pa", S.pa);
  if (S.pb !== "KO") q.set("pb", S.pb);
  return q.toString().replace(/%2C/g, ",");
}
const saveHash = debounce(() => {
  const t = hashTarget(), s = stateString();
  try { t.history.replaceState(null, "", t.location.pathname + t.location.search + (s ? "#" + s : "")); } catch (e) { /* ignore */ }
}, 250);
function restoreHash() {
  let h = "";
  try { h = hashTarget().location.hash.slice(1); } catch (e) { h = location.hash.slice(1); }
  const q = new URLSearchParams(h), int = (key, lo, hi, def) => { const v = parseInt(q.get(key), 10); return v >= lo && v <= hi ? v : def; };
  const lists = ["all", "turnout", ...listCats()], cts = D.effects.contrasts.map(c => c.id), outs = ["No vote", ...listCats(), "Other"];
  S.view = VIEWS.includes(q.get("view")) ? q.get("view") : "who";
  S.show = lists.includes(q.get("show")) ? q.get("show") : "all";
  S.split = int("sp", 0, D.groupings.length - 1, 0);
  if (S.split > 0 && S.show === "all") S.show = "turnout";
  S.cel = YEARS.includes(+q.get("cel")) ? +q.get("cel") : 2023;
  S.by = int("by", 1, D.groupings.length - 1, 1);
  S.ct = cts.includes(q.get("ct")) ? q.get("ct") : "educ_higher";
  S.el = YEARS.includes(+q.get("el")) ? +q.get("el") : 2023;
  S.out = outs.includes(q.get("out")) ? q.get("out") : "No vote";
  S.cal = q.get("cal") === "1";
  S.pv = q.get("pv") === "ideo" ? "ideo" : "aff";
  S.pe = YEARS.includes(+q.get("pe")) ? +q.get("pe") : 2023;
  S.pa = q.get("pa") || "PiS"; S.pb = q.get("pb") || "KO";
  Object.assign(S.k, {el: YEARS.includes(+q.get("kel")) ? +q.get("kel") : 2023, age: int("age", 18, 98, 45), sex: q.get("sex") === "m" ? "m" : "f", ed: int("ed", 1, 3, 2),
    res: int("res", 1, 5, 3), rel: int("rel", 1, 6, 4), ses: int("ses", 1, 5, 3), cal: q.get("kcal") === "1"});
}
function shareUrl() { const t = hashTarget(), s = stateString(); return t.location.origin + t.location.pathname + (s ? "#" + s : ""); }
// ---- PNG: the view's figures stacked on one canvas, with their legends and the citation -----------------
function barsSvg() {
  const k = S.k, W = 560, rowH = 24, sub = `${profileText().split(" · ").slice(1).join(" · ")} · median and 90% interval${k.cal ? " · adjusted to the official results" : ""}`, T = 30 + 14 * subLines(sub, 560).length, H = T + rowH * CALC_ROWS.length + 8, Lm = 130, Rm = W - 100;
  const svg = document.createElementNS(NS, "svg"); svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.setAttribute("xmlns", NS);
  figureHead(svg, `A ${k.age}-year-old ${k.sex === "f" ? "woman" : "man"} at the ${k.el} election`, sub, W);
  CALC_ROWS.forEach((r, i) => {
    const y = T + i * rowH, c = r.id === "No vote" ? "#9aa5a9" : colour(r.id), x = v => Lm + v * (Rm - Lm);
    txt(svg, Lm - 8, y + 15, r.nm, {"font-size": 12, fill: "#141414", "text-anchor": "end"});
    svg.insertAdjacentHTML("beforeend", `<rect x="${Lm}" y="${y + 4}" width="${Rm - Lm}" height="14" fill="#f1f1ef"/><rect x="${Lm}" y="${y + 4}" width="${(x(r.s.m) - Lm).toFixed(1)}" height="14" fill="${c}"/><line x1="${x(r.s.lo).toFixed(1)}" x2="${x(r.s.hi).toFixed(1)}" y1="${y + 11}" y2="${y + 11}" stroke="#141414" stroke-width="2"/>`);
    txt(svg, Rm + 8, y + 15, `${Math.round(100 * r.s.m)}% (${Math.round(100 * r.s.lo)}–${Math.round(100 * r.s.hi)})`, {"font-size": 12, fill: "#141414"});
  });
  return svg;
}
function pngParts() {
  const P = (svg, leg) => ({svg, leg});
  if (S.view === "who") return {parts: [P($("fig-who"), "leg-who")], name: "who-votes", head: "Who votes", sub: "Share of voters who name each list, by group; weighted survey with the official results"};
  if (S.view === "effects") return {parts: [P($("fig-eff")), P($("fig-eff2"), "leg-eff2")], name: "what-changes-the-vote", head: "What changes the vote", sub: "Change in the probability of each choice, percentage points; 90% credible intervals"};
  if (S.view === "calc") {
    const parts = []; if (CALC_ROWS) parts.push(P(barsSvg()), P($("fig-all"), "leg-all"));
    if (!$("att-box").hidden) parts.push(P($("fig-att"), "leg-att"));
    return {parts, name: "hypothetical-voter", head: "Hypothetical voter", sub: "Probability of each choice for one kind of voter; median and 90% credible interval"};
  }
  if (S.view === "pol") {
    const head = "Polarisation";
    if (S.pv === "aff") return {parts: [P($("fig-heat")), P($("fig-api"))], name: "affective-polarisation", head: "Affective polarisation", sub: `Feelings towards the lists, ${S.pe}, and Reiljan's index over time`};
    return {parts: [P($("fig-i-lr")), P($("fig-i-econ"), null), P($("fig-i-soc"), "leg-ideo")], name: "ideological-polarisation", head: "Ideological polarisation", sub: `Positions of the voters of two lists, ${S.pe}`};
  }
  return {parts: []};
}
async function png() {
  const {parts, name: fname, head: headT, sub: headS} = pngParts(); if (!parts.length) return;
  const pad = 24, gap = 16, foot = 50, head = 62, items = [];
  const meas = document.createElement("canvas").getContext("2d");
  for (const {svg, leg} of parts) {
    const vb = svg.viewBox.baseVal, w = vb.width, h = vb.height;
    const temp = !svg.isConnected;
    if (temp) { svg.style.cssText = "position:absolute;left:-9999px;top:0;width:" + w + "px"; document.body.appendChild(svg); }
    const clone = await ChartExport.clone(svg);
    if (temp) { svg.remove(); clone.removeAttribute("style"); }
    clone.setAttribute("xmlns", NS); ChartExport.setSize(clone, w, h);
    clone.querySelectorAll(".hov").forEach(n => n.remove());
    const img = await new Promise(res => { const im = new Image(); im.onload = () => res(im); im.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(clone)); });
    meas.font = "12px Jost, sans-serif";
    const rows = [[]]; let x = 0;
    (leg ? LEGEND[leg] || [] : []).forEach(it => { const iw = 24 + meas.measureText(it[0]).width + 14; if (x + iw > w && rows[rows.length - 1].length) { rows.push([]); x = 0; } rows[rows.length - 1].push([it, x]); x += iw; });
    items.push({img, w, h, rows, legH: rows[0].length ? rows.length * 18 + 8 : 0});
  }
  await ChartExport.fonts();
  const W = Math.max(760, Math.max(...items.map(i => i.w)) + 2 * pad);
  const H = head + items.reduce((s, i) => s + i.h + i.legH, 0) + gap * (items.length - 1) + foot;
  const cv = document.createElement("canvas"), sc = ChartExport.scale(W, H); cv.width = W * sc; cv.height = H * sc;
  const g = cv.getContext("2d"); g.scale(sc, sc); g.fillStyle = "#fff"; g.fillRect(0, 0, W, H);
  g.fillStyle = "#8B0000"; g.font = ChartExport.TITLE; g.fillText(`Voting behaviour · ${headT}`, pad, 32);
  g.fillStyle = "#4B4B4B"; g.font = ChartExport.SUBTITLE; g.fillText(`${headS}.`, pad, 52);
  let y = head;
  items.forEach(i => {
    g.drawImage(i.img, pad, y, i.w, i.h); y += i.h + 4;
    i.rows.forEach((r, ri) => r.forEach(([[label, color, kind], lx]) => {
      const cy = y + ri * 18 + 9, sx = pad + lx;
      g.strokeStyle = color; g.fillStyle = color; g.lineWidth = 2.2; g.setLineDash(kind === "dash" ? [4, 3] : []);
      if (kind === "ring") { g.beginPath(); g.arc(sx + 9, cy, 4, 0, 7); g.stroke(); }
      else if (kind === "band") { g.globalAlpha = 0.35; g.fillRect(sx, cy - 5, 18, 10); g.globalAlpha = 1; }
      else { g.beginPath(); g.moveTo(sx, cy); g.lineTo(sx + 18, cy); g.stroke(); }
      g.setLineDash([]); g.fillStyle = "#4B4B4B"; g.font = "12px Jost, sans-serif"; g.fillText(label, sx + 24, cy + 4);
    }));
    y += i.legH - 4 + gap;
  });
  g.fillStyle = "#767676"; g.font = ChartExport.CREDIT; g.fillText("Data: Polish National Election Study (PGSW), 1997–2023; model estimates.", pad, H - 28);
  ChartExport.drawCite(g, PAGE, pad, H - 12, W - 2 * pad);
  cv.toBlob(b => download(`voting-behaviour-${fname}.png`, b), "image/png");
}
function download(fileName, blob) {
  const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = fileName; document.body.appendChild(a); a.click();
  setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 500);
}
function csv() {
  const rows = [];
  const cite = window.ChartExport ? ChartExport.csvCite(PAGE) : "";
  if (S.view === "who") {
    rows.push(["election", "split", "group", "measure", "survey", "official"]);
    const lists = S.show === "all" ? listCats() : [S.show];
    D.groupings[S.split].levels.forEach((lv, l) => lists.forEach(cat => YEARS.forEach(y => {
      const v = whoValue(y, S.split, l, cat); if (v != null) rows.push([y, D.groupings[S.split].id, lv, name(cat), (100 * v).toFixed(2), S.split === 0 && official(y, cat) != null ? (100 * official(y, cat)).toFixed(2) : ""]);
    })));
  } else if (S.view === "effects") {
    rows.push(["election", "characteristic", "outcome", "adjusted_to_official", "median_points", "lower_90", "upper_90"]);
    D.effects.rows.filter(r => D.effects.contrasts[r.effect].id === S.ct).forEach(r => ["uncal", "cal"].forEach(v => rows.push([r.year, effLabel(S.ct), name(r.category), v === "cal" ? "yes" : "no", ...r[v].map(x => x.toFixed(3))])));
  } else if (S.view === "pol" && POL) {
    if (S.pv === "aff") {
      rows.push(["election", "voters_of", "rated_list", "mean_like_0_10", "own_list", "own_minus_others"]);
      const E = POL.affective.elections[S.pe];
      E.electorates.forEach((id, i) => E.rated.forEach((r, j) => rows.push([S.pe, id, r.id, E.matrix[i][j], id === r.id ? "yes" : "no", E.gap[i]])));
    } else {
      rows.push(["election", "list", "position", "measure_index", "share_of_voters", "mean", "se", "n"]);
      POL.ideological.elections[S.pe].forEach(p => POL.ideological.dims.forEach(d => { const x = p.dims[d.id]; if (x) x.p.forEach((v, k) => rows.push([S.pe, p.id, d.id, k, v, x.mean, x.se, x.n])); }));
    }
  } else if (S.view === "calc") {
    const cd = CALC[S.k.el]; if (!cd) return;
    const p = profile(), j = judge(cd, p); if (j.level === "stop") return;
    rows.push([`# election ${cd.year}; age ${p.age}; ${p.sex === "f" ? "woman" : "man"}; education ${cd.design.education_levels[p.ed - 1]}; residence ${cd.design.residence_levels[p.res - 1]}; attendance ${cd.design.attendance_levels[p.rel - 1]}; SES quintile ${p.ses}; ${S.k.cal ? "adjusted to the official results" : "unadjusted"}`]);
    rows.push(["outcome", "median", "lower_90", "upper_90"]);
    const sm = summarise(probabilities(cd, p, S.k.cal)); sm.forEach((s, i) => rows.push([i === 0 ? "Did not vote" : cd.ballot[i - 1].name, s.m.toFixed(4), s.lo.toFixed(4), s.hi.toFixed(4)]));
  } else {
    rows.push(["election", "list", "survey_share", "official_share"]);
    D.vs_official.forEach(r => rows.push([r.year, name(r.category), r.survey, r.official]));
  }
  const qv = v => /[",\n]/.test(String(v)) ? `"${String(v).replace(/"/g, '""')}"` : String(v);
  download(`voting-behaviour-${S.view}.csv`, new Blob([cite + rows.map(r => r.map(qv).join(",")).join("\n") + "\n"], {type: "text/csv"}));
}

// ---- wiring --------------------------------------------------------------------------------------------------------
// ---- Polarisation -------------------------------------------------------------------------------------
function ensurePol() {
  if (POL) return Promise.resolve(POL);
  return fetch("polarisation.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).then(j => (POL = j));
}
const POL_HEAT = [[247, 244, 240], [0, 121, 107]];
const heatColour = v => { const t = clamp(v / 10, 0, 1), c = POL_HEAT[0].map((a, i) => Math.round(a + (POL_HEAT[1][i] - a) * t)); return `rgb(${c.join(",")})`; };
function polParties(year) {   // lists with an ideological distribution, largest first
  const sh = {}; (POL.affective.elections[year] ? POL.affective.elections[year].rated : []).forEach(r => { sh[r.id] = r.share; });
  return POL.ideological.elections[year].slice().sort((a, b) => (sh[b.id] || 0) - (sh[a.id] || 0));
}
function renderPol() {
  $("pol-err").textContent = "";
  document.querySelectorAll("#p-sub button").forEach(b => b.setAttribute("aria-pressed", b.dataset.p === S.pv));
  $("pol-aff").hidden = S.pv !== "aff"; $("pol-ideo").hidden = S.pv !== "ideo";
  $("p-pick").hidden = S.pv !== "ideo";
  $("pollead").textContent = S.pv === "aff"
    ? "How warmly the voters of each list rate every list, and how large the gap is between feelings about one's own list and the others. Reiljan's index summarises it for the whole party system."
    : "Where the voters of two lists stand on left-right, economic and socio-cultural positions, 0 to 10, with 10 the right or conservative end. The more the shapes overlap, the less the electorates differ.";
  if (S.pv === "aff") drawAffective(); else drawIdeological();
}
function drawAffective() {
  const E = POL.affective.elections[S.pe], svg = $("fig-heat"), box = svg.parentNode;
  const order = E.rated.map((r, i) => i).sort((a, b) => E.rated[b].share - E.rated[a].share);
  const cols = order.map(i => E.rated[i]);
  const rows = E.electorates.map((id, i) => ({id, i, share: (E.rated.find(r => r.id === id) || {}).share || 0})).sort((a, b) => b.share - a.share);
  const nm = id => (E.rated.find(r => r.id === id) || {}).name || id;
  const W = Math.max(320, box.clientWidth - 16), Lm = Math.min(150, W * 0.3), Gm = 70, cw = Math.min(84, (W - Lm - Gm - 6) / cols.length), ch = 34, hh = 66;
  const sub = "Mean like–dislike score, 0 (dislike) to 10 (like) · rows: voters of each list · outlined: own list · right: own list minus the others";
  const nsub = subLines(sub, W).length, T = 30 + 14 * nsub + hh, H = T + ch * rows.length + 14;
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.innerHTML = "";
  figureHead(svg, `How each list's voters rate every list, ${S.pe}`, sub, W);
  cols.forEach((c, j) => {
    const x = Lm + j * cw + cw / 2, tx = txt(svg, x, T - 8, nm(c.id), {"font-size": 11, fill: "#141414", "text-anchor": "start"});
    tx.setAttribute("transform", `rotate(-38 ${x} ${T - 8})`);
  });
  txt(svg, Lm + cols.length * cw + 10, T - 8, "Own − others", {"font-size": 11, fill: "#767676"});
  const gmax = Math.max(...rows.map(r => E.gap[r.i]), 1);
  rows.forEach((r, k) => {
    const y = T + k * ch, own = r.id;
    el("circle", {cx: 8, cy: y + ch / 2, r: 5, fill: colour(own)}, svg);
    txt(svg, 20, y + ch / 2 + 4, nm(own), {"font-size": 12, fill: "#141414"});
    txt(svg, Lm - 6, y + ch / 2 + 4, `n=${E.n[r.i]}`, {"font-size": 9.5, fill: "#767676", "text-anchor": "end"});
    cols.forEach((c, j) => {
      const v = E.matrix[r.i][E.rated.findIndex(x => x.id === c.id)];
      const g = el("g", {}, svg);
      el("rect", {x: Lm + j * cw + 1, y: y + 1, width: cw - 2, height: ch - 2, rx: 3, fill: heatColour(v), stroke: c.id === own ? "#141414" : "none", "stroke-width": c.id === own ? 1.6 : 0}, g);
      txt(g, Lm + j * cw + cw / 2, y + ch / 2 + 4, v.toFixed(1), {"font-size": 11, fill: v / 10 > 0.55 ? "#fff" : "#141414", "text-anchor": "middle"});
      g.insertAdjacentHTML("beforeend", `<title>${esc(nm(own))} voters rate ${esc(nm(c.id))}: ${v.toFixed(2)}</title>`);
    });
    const gv = E.gap[r.i], gx = Lm + cols.length * cw + 10;
    el("rect", {x: gx, y: y + 8, width: Math.max(1, 38 * gv / gmax), height: ch - 16, fill: "#8B0000", "fill-opacity": 0.75}, svg);
    txt(svg, gx + 38 * gv / gmax + 4, y + ch / 2 + 4, gv.toFixed(1), {"font-size": 11, fill: "#141414"});
  });
  // The index across elections
  const I = POL.affective.index, cur = I.find(r => r.year === S.pe);
  lineChart("fig-api", "tip-api", {
    xs: YEARS, ymin: 0, ymax: 8, ystep: 2, fmt: v => v, h: 300,
    title: "Reiljan's index of affective polarisation", sub: "Average gap between feelings about one's own list and the others, 0–10, weighted by vote shares · 90% bootstrap interval",
    series: [
      {name: "Index", color: "#8B0000", vals: YEARS.map(y => (I.find(r => r.year === y) || {}).value), lo: YEARS.map(y => (I.find(r => r.year === y) || {}).lo), hi: YEARS.map(y => (I.find(r => r.year === y) || {}).hi)},
      {name: "Selected", color: "#8B0000", rings: true, vals: YEARS.map(y => y === S.pe ? cur.value : null)}],
    tip: i => { const r = I.find(x => x.year === YEARS[i]); return r ? `<div class="h">${r.year}</div>${tipRow("#8B0000", "Index", r.value.toFixed(2), `${r.lo.toFixed(2)}–${r.hi.toFixed(2)} · ${r.electorates} electorates, ${r.rated} lists rated`)}` : ""; }
  });
  const missing = POL.ideological.elections[S.pe].map(p => p.id).filter(id => !E.rated.some(r => r.id === id) && id !== "Other");
  $("aff-note").innerHTML = `<b>${S.pe}: ${cur.value.toFixed(2)}</b> (${cur.lo.toFixed(2)}–${cur.hi.toFixed(2)}). Lists rated that year: ${E.rated.map(r => esc(r.name)).join(", ")}.` +
    (missing.length ? ` Not rated in the survey, so left out: ${missing.map(m => esc(name(m))).join(", ")}.` : "") +
    " The index is the vote-weighted average, over the lists whose voters rated their own list, of the gap between their mean like for their own list and their mean like for the other rated lists, those weighted by official vote shares.";
}
function drawIdeological() {
  const pl = polParties(S.pe), ids = pl.map(p => p.id);
  fillSelect($("p-a"), pl.map(p => [p.id, p.name]), ids.includes(S.pa) ? S.pa : ids[0]);
  if (!ids.includes(S.pa)) S.pa = ids[0];
  const others = pl.filter(p => p.id !== S.pa);
  if (!ids.includes(S.pb) || S.pb === S.pa) S.pb = (others.find(p => p.id === "KO") || others[0]).id;
  fillSelect($("p-b"), pl.map(p => [p.id, p.name]), S.pb);
  const A = pl.find(p => p.id === S.pa), Bp = pl.find(p => p.id === S.pb), dims = POL.ideological.dims;
  const ends = {lr: ["Left", "Right"], econ: ["Left", "Right"], soc: ["Progressive", "Conservative"]};
  dims.forEach(d => densityChart("fig-i-" + d.id, d, A, Bp, ends[d.id]));
  const colA = colour(A.id), colB = colour(Bp.id);
  $("leg-ideo").innerHTML = [[A.name, colA], [Bp.name, colB]].map(([n, c]) => `<span><i class="sw band" style="background:${c};opacity:.6"></i>${esc(n)}</span>`).join("") + `<span><i class="sw" style="border-color:#4B4B4B;border-top-style:dashed"></i>Mean</span>`;
  LEGEND["leg-ideo"] = [[A.name, colA, "band"], [Bp.name, colB, "band"], ["Mean", "#4B4B4B", "dash"]];
  const rows = dims.map(d => {
    const a = A.dims[d.id], b = Bp.dims[d.id];
    if (!a || !b) return `<tr><td>${esc(d.label)}</td><td colspan="5">Too few respondents</td></tr>`;
    const gap = b.mean - a.mean, se = Math.sqrt(a.se * a.se + b.se * b.se), ov = a.p.reduce((s, v, i) => s + Math.min(v, b.p[i]), 0);
    return `<tr><td>${esc(d.label)}</td><td>${a.mean.toFixed(1)}</td><td>${b.mean.toFixed(1)}</td><td>${(gap < 0 ? MINUS : "+") + Math.abs(gap).toFixed(1)} <i>(${(gap - 1.645 * se).toFixed(1).replace("-", MINUS)} to ${(gap + 1.645 * se).toFixed(1).replace("-", MINUS)})</i></td><td>${Math.round(100 * ov)}%</td><td>${a.n} / ${b.n}</td></tr>`;
  }).join("");
  $("ideo-tab").innerHTML = `<table class="t"><thead><tr><th>Position, ${S.pe}</th><th>${esc(A.name)}</th><th>${esc(Bp.name)}</th><th>${esc(Bp.name)} minus ${esc(A.name)}</th><th>Overlap</th><th>Voters</th></tr></thead><tbody>${rows}</tbody></table>`;
}
function densityChart(svgId, dim, A, Bp, ends) {
  const svg = $(svgId), box = svg.parentNode, W = Math.max(260, box.clientWidth - 16), H = 280, L = 40, R = W - 10, B = H - 40;
  const a = A.dims[dim.id], b = Bp.dims[dim.id];
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.innerHTML = "";
  const sub = "Share of each list's voters at each position, 0–10";
  const T = 30 + 14 * subLines(sub, W).length + 8;
  figureHead(svg, dim.label, sub, W);
  if (!a || !b) { txt(svg, W / 2, H / 2, "Too few respondents", {"text-anchor": "middle"}); return; }
  const top = Math.max(...a.p, ...b.p), step = top > 0.4 ? 0.1 : top > 0.2 ? 0.05 : 0.025, ymax = Math.ceil(top / step) * step;
  const X = i => L + (R - L) * i / 10, Y = v => B - (B - T) * v / ymax;
  for (let g = 0; g <= ymax + 1e-9; g += step) { el("line", {x1: L, x2: R, y1: Y(g), y2: Y(g), stroke: "#ECECEC"}, svg); txt(svg, L - 6, Y(g) + 4, Math.round(g * 1000) / 10 + "%", {"text-anchor": "end"}); }
  for (let i = 0; i <= 10; i++) txt(svg, X(i), B + 16, i, {"text-anchor": "middle"});
  txt(svg, L, B + 33, `0 ${ends[0]}`, {"font-size": 11, fill: "#767676"}); txt(svg, R, B + 33, `10 ${ends[1]}`, {"font-size": 11, fill: "#767676", "text-anchor": "end"});
  [[a, A], [b, Bp]].forEach(([d, p]) => {
    const c = colour(p.id), pts = d.p.map((v, i) => `${X(i)} ${Y(v)}`);
    el("path", {d: `M${X(0)} ${Y(0)}L${pts.join("L")}L${X(10)} ${Y(0)}Z`, fill: c, "fill-opacity": 0.42}, svg);
    el("path", {d: `M${pts.join("L")}`, fill: "none", stroke: c, "stroke-width": 2, "stroke-linejoin": "round"}, svg);
    el("line", {x1: X(d.mean), x2: X(d.mean), y1: T, y2: B, stroke: c, "stroke-width": 1.6, "stroke-dasharray": "5 4"}, svg);
  });
  const guide = el("line", {y1: T, y2: B, stroke: "#4B4B4B", "stroke-width": 1, "stroke-dasharray": "3 3", visibility: "hidden"}, svg), tip = $("tip-" + svgId.slice(4));
  const move = ev => {
    const r = svg.getBoundingClientRect(), cx = (ev.touches ? ev.touches[0].clientX : ev.clientX) - r.left, i = clamp(Math.round(((cx * W / r.width) - L) / (R - L) * 10), 0, 10);
    guide.setAttribute("x1", X(i)); guide.setAttribute("x2", X(i)); guide.setAttribute("visibility", "visible");
    tip.innerHTML = `<div class="h">Position ${i}</div>` + tipRow(colour(A.id), A.name, Math.round(a.p[i] * 100) + "%") + tipRow(colour(Bp.id), Bp.name, Math.round(b.p[i] * 100) + "%");
    tip.hidden = false; const bw = box.clientWidth, tw = tip.offsetWidth, px = X(i) * r.width / W + 12;
    tip.style.left = clamp(px + tw > bw - 8 ? px - tw - 24 : px, 4, bw - tw - 4) + "px"; tip.style.top = "26px";
  };
  const leave = () => { guide.setAttribute("visibility", "hidden"); tip.hidden = true; };
  svg.onmousemove = move; svg.onmouseleave = leave; svg.ontouchstart = move; svg.ontouchmove = move; svg.ontouchend = leave;
}
function render() {
  document.querySelectorAll("#views button").forEach(b => b.setAttribute("aria-selected", b.dataset.v === S.view));
  VIEWS.forEach(v => { $("v-" + v).hidden = v !== S.view; });
  $("dlcsv").disabled = false; $("dlpng").hidden = S.view === "about";
  if (S.view === "who") renderWho();
  else if (S.view === "effects") renderEffects();
  else if (S.view === "calc") renderCalc();
  else if (S.view === "pol") ensurePol().then(renderPol, e => { $("pol-err").textContent = "The polarisation data could not be loaded (" + e.message + ")."; });
  else renderAbout();
  saveHash();
}
function syncControls() {
  fillSelect($("p-el"), YEARS.slice().reverse().map(y => [y, y]), S.pe);
  const lists = listCats();
  fillSelect($("w-show"), [["all", "All lists"], ["turnout", "Turnout"], ...lists.map(c => [c, name(c)])], S.show);
  fillSelect($("w-split"), D.groupings.map((g, i) => [i, i === 0 ? "Everyone" : g.id]), S.split);
  fillSelect($("c-el"), YEARS.map(y => [y, y]), S.cel);
  fillSelect($("c-by"), D.groupings.slice(1).map((g, i) => [i + 1, g.id]), S.by);
  fillSelect($("e-ct"), D.effects.contrasts.map(c => [c.id, c.label]), S.ct);
  fillSelect($("e-el"), YEARS.slice().reverse().map(y => [y, y]), S.el);
  fillSelect($("e-out"), [["No vote", "Did not vote"], ...lists.map(c => [c, name(c)]), ["Other", "Other"]], S.out);
  $("e-cal").checked = S.cal;
  fillSelect($("k-el"), YEARS.slice().reverse().map(y => [y, y]), S.k.el);
  const cd0 = D.elections[0];
  fillSelect($("k-ed"), [[1, "Primary or less"], [2, "Secondary"], [3, "Higher"]], S.k.ed);
  fillSelect($("k-res"), [[1, "Village"], [2, "Town under 20,000"], [3, "Town 20,000–99,999"], [4, "City 100,000–499,999"], [5, "City of 500,000 or more"]], S.k.res);
  fillSelect($("k-rel"), [[1, "Never"], [2, "Once a year or less"], [3, "Several times a year"], [4, "Once a month"], [5, "Two or three times a month"], [6, "Weekly or more"]], S.k.rel);
  fillSelect($("k-ses"), [[1, "Low"], [2, "Medium-low"], [3, "Medium"], [4, "Medium-high"], [5, "High"]], S.k.ses);
  $("k-cal").checked = S.k.cal; $("k-age").value = S.k.age; $("k-age-o").textContent = S.k.age;
  void cd0;
}
function wire() {
  document.querySelectorAll("#views button").forEach(b => { b.onclick = () => { S.view = b.dataset.v; render(); }; });
  $("w-show").onchange = e => { S.show = e.target.value; render(); };
  $("w-split").onchange = e => { S.split = +e.target.value; if (S.split > 0 && S.show === "all") { S.show = "turnout"; } syncControls(); render(); };
  $("c-el").onchange = e => { S.cel = +e.target.value; render(); };
  $("c-by").onchange = e => { S.by = +e.target.value; render(); };
  $("e-ct").onchange = e => { S.ct = e.target.value; render(); };
  $("e-el").onchange = e => { S.el = +e.target.value; render(); };
  $("e-out").onchange = e => { S.out = e.target.value; render(); };
  $("e-cal").onchange = e => { S.cal = e.target.checked; render(); };
  $("k-el").onchange = e => { S.k.el = +e.target.value; render(); };
  $("k-age").oninput = e => { S.k.age = +e.target.value; $("k-age-o").textContent = S.k.age; render(); };
  document.querySelectorAll("#k-sex button").forEach(b => { b.onclick = () => { S.k.sex = b.dataset.s; render(); }; });
  $("k-ed").onchange = e => { S.k.ed = +e.target.value; render(); };
  $("k-res").onchange = e => { S.k.res = +e.target.value; render(); };
  $("k-rel").onchange = e => { S.k.rel = +e.target.value; render(); };
  $("k-ses").onchange = e => { S.k.ses = +e.target.value; render(); };
  $("k-cal").onchange = e => { S.k.cal = e.target.checked; render(); };
  document.querySelectorAll("#p-sub button").forEach(b => { b.onclick = () => { S.pv = b.dataset.p; render(); }; });
  $("p-el").onchange = e => { S.pe = +e.target.value; render(); };
  $("p-a").onchange = e => { S.pa = e.target.value; render(); };
  $("p-b").onchange = e => { S.pb = e.target.value; render(); };
  $("dlcsv").onclick = csv;
  $("dlpng").onclick = png;
  $("share").onclick = () => { const u = shareUrl(); (navigator.clipboard ? navigator.clipboard.writeText(u) : Promise.reject()).then(() => toast("Link copied"), () => window.prompt("Copy this link:", u)); };
  try { hashTarget().addEventListener("hashchange", () => { restoreHash(); syncControls(); render(); }); } catch (e) { /* ignore */ }
  window.addEventListener("resize", debounce(() => { if (S.view === "who" || S.view === "effects" || S.view === "calc" || S.view === "pol") render(); }, 200));
}

fetch("voting.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).then(d => {
  D = d; YEARS = d.elections.map(e => e.year);
  d.categories.forEach(c => { CAT[c.id] = c.colour; });
  d.groups.forEach(r => GR.set(`${r.year}|${r.grouping}|${r.level}`, r));
  d.composition.forEach(r => COMP.set(`${r.year}|${r.category}|${r.grouping}|${r.level}`, r));
  $("asof").textContent = `Polish General Election Studies 1997–2023 · ${d.meta.model.respondents.toLocaleString("en-GB")} respondents in the model · eight Sejm elections · estimates with 90% credible intervals`;
  $("ui").hidden = false;
  restoreHash(); syncControls(); wire(); render();
}).catch(err => { $("asof").textContent = "The data could not be loaded (" + err.message + ")."; });
