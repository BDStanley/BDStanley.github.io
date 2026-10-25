/* Party ideology — Polish parties in European ideological space.
 *
 * Reads ideology.json: Chapel Hill Expert Survey (CHES) party means for every
 * wave since 1999, the spread of expert ratings in 2024, each party's PopuList
 * 4.0 classification at every wave, Party tracker records for Polish parties,
 * and polarisation and alignment by country and wave. Written by
 * "Party ideology/analysis/Party ideology.R" in the Polish political analyses
 * repo. Also reads ../polling-tracker/polls.json, if it is there, for each
 * Polish party's current polling.
 *
 * Views: Map (any two dimensions), Profile (one party on every dimension
 * against a comparison group), Populism (anti-elite rhetoric against PopuList),
 * Over time (Polish parties at each wave), Parties and voters (each Polish
 * party beside its own voters, from the Social and political trends in
 * ideology.json's voters) and Party systems (polarisation and alignment by
 * country). The panel follows the selected party. State goes into
 * the parent page's URL hash, as in the Polling tracker; #pt=<Party tracker id>
 * selects a Polish party from the genealogy page.
 */
"use strict";

// ---- utilities ------------------------------------------------------------------------------
const $ = id => document.getElementById(id);
const esc = s => String(s == null ? "" : s).replace(/[&<>"']/g, c => ({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}[c]));
const MON = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
const dstr = d => { const t = new Date(d * 864e5); return t.getUTCDate() + " " + MON[t.getUTCMonth()] + " " + t.getUTCFullYear(); };
const isNum = v => typeof v === "number" && isFinite(v);
const fmt = (v, dp = 1) => isNum(v) ? v.toFixed(dp) : "–";
const signed = (v, dp = 1) => { const r = +v.toFixed(dp); return (r > 0 ? "+" : r < 0 ? "−" : "±") + Math.abs(r).toFixed(dp); };
const clamp = (v, a, b) => Math.max(a, Math.min(b, v));
function quant(sorted, p) {
  const n = sorted.length; if (!n) return NaN;
  const h = (n - 1) * p, lo = Math.floor(h);
  return lo + 1 < n ? sorted[lo] + (h - lo) * (sorted[lo + 1] - sorted[lo]) : sorted[lo];
}
function debounce(fn, ms) { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => fn(...a), ms); }; }
function toast(msg) { const t = $("toast"); t.textContent = msg; t.classList.add("on"); clearTimeout(toast.t); toast.t = setTimeout(() => t.classList.remove("on"), 1800); }
const lc = s => s.replace(/^[A-Z][a-z]/, m => m === "Ch" ? m : m.toLowerCase());

// ---- categories and colours -----------------------------------------------------------------
// PopuList categories in the colours of right and left: blue for the far right and red for the far
// left, darker when the party is also populist, purple for populist parties on neither flank; grey
// recedes for "none". Checked for colour-blind separation between every pair.
const POP_CATS = [
  {k: "pfr", label: "Populist and far right", col: "#3644A0"},
  {k: "fr", label: "Far right", col: "#4BAEED"},
  {k: "pop", label: "Populist", col: "#865FC1"},
  {k: "pfl", label: "Populist and far left", col: "#9E122F"},
  {k: "fl", label: "Far left", col: "#D86353"},
  {k: "none", label: "None of these", col: "#B8B8B8"},
];
const POPC = Object.fromEntries(POP_CATS.map(c => [c.k, c]));
// CHES families grouped into seven, in the colours families usually wear (blue right, red left,
// gold liberals, green greens; the radical right indigo and the radical left plum, so that they
// stay apart from their neighbours), checked for colour-blind separation between every pair
const FAM_GROUPS = [
  {k: "rr", label: "Radical right", fams: [1], col: "#4E59B1"},
  {k: "right", label: "Conservative, Christian democratic", fams: [2, 4, 10], col: "#4299DC"},
  {k: "centre", label: "Liberal, agrarian and centre", fams: [3, 11], col: "#E2A520"},
  {k: "green", label: "Green", fams: [7], col: "#2E9848"},
  {k: "left", label: "Socialist", fams: [5], col: "#B3260E"},
  {k: "rl", label: "Radical left", fams: [6], col: "#88296F"},
  {k: "other", label: "Regionalist and other", fams: [8, 9], col: "#B8B8B8"},
];
const FAMG = {}; FAM_GROUPS.forEach(g => g.fams.forEach(f => { FAMG[f] = g; }));
const FAM_OTHER = FAM_GROUPS.find(g => g.k === "other");
const BITS = {populist: 1, farright: 4, farleft: 16, eurosceptic: 64};
const CAT_LABEL = {populist: "populist", farright: "far right", farleft: "far left", eurosceptic: "Eurosceptic"};
const CAT_TITLE = {populist: "Populist", farright: "Far right", farleft: "Far left", eurosceptic: "Eurosceptic"};
const st = (code, k) => code & BITS[k] ? 2 : code & (2 * BITS[k]) ? 1 : 0;
function popCat(code) {
  const p = st(code, "populist"), r = st(code, "farright"), l = st(code, "farleft");
  const k = p && r ? "pfr" : r ? "fr" : p && l ? "pfl" : l ? "fl" : p ? "pop" : "none";
  const act = [p, r, l].filter(x => x > 0);
  return {k, bl: act.length > 0 && act.every(x => x === 1)};
}
function popText(code) {
  const ks = ["populist", "farright", "farleft", "eurosceptic"];
  const full = ks.filter(k => st(code, k) === 2).map(k => CAT_LABEL[k]);
  const bl = ks.filter(k => st(code, k) === 1).map(k => CAT_LABEL[k]);
  if (!full.length && !bl.length) return "not populist, far right, far left or Eurosceptic";
  const list = a => a.length < 2 ? a.join("") : a.slice(0, -1).join(", ") + " and " + a[a.length - 1];
  return (full.length ? list(full) : "") + (bl.length ? (full.length ? "; borderline " : "borderline ") + list(bl) : "");
}
const ACCENT = "#8B0000", INK = "#141414", SOFT = "#4B4B4B", MUTED = "#767676", FAINT = "#D5D5D5", RULE = "#ECECEC";
const PAGE = "Party ideology";   // the page's name in its suggested citation (../chart-export.js)
const POLAND = 26;

// ---- data -----------------------------------------------------------------------------------
let D, WAVES, POLISH;
const P = new Map(), DIM = new Map(), CTRY = new Map(), BYW = new Map(), ROWS = [];
let POLL = null;

function prep() {
  D.countries.forEach(c => CTRY.set(c.id, c));
  D.dims.forEach(d => DIM.set(d.id, d));
  D.parties.forEach(p => { p.ctry = CTRY.get(p.c); p.byW = new Map(); p.pl = p.c === POLAND; P.set(p.id, p); });
  const R = D.rows, n = R.p.length;
  for (let i = 0; i < n; i++) {
    const p = P.get(R.p[i]), v = {};
    for (const d of D.dims) v[d.id] = R.v[d.id][i];
    const r = {i, p, w: R.w[i], vote: R.vote[i], fam: R.fam[i], gov: R.gov[i], pop: R.pop[i] || 0,
      list: R.list[i], listV: R.listV[i], ref: R.ref[i], v, sd: null, n: null};
    ROWS.push(r); p.byW.set(r.w, r);
    if (!BYW.has(r.w)) BYW.set(r.w, []);
    BYW.get(r.w).push(r);
  }
  if (R.experts) R.experts.rows.forEach((ri, j) => {
    const r = ROWS[ri]; r.n = R.experts.n[j]; r.sd = {};
    for (const k in R.experts.sd) r.sd[k] = R.experts.sd[k][j];
  });
  WAVES = D.meta.waves.filter(w => (BYW.get(w) || []).some(r => r.p.pl));
  POLISH = D.parties.filter(p => p.pl);
  POLISH.forEach(p => { p.search = [p.abbr, p.label, p.name, p.en, "Poland", ...(p.pt || [])].join(" ").toLowerCase(); });
  D.parties.filter(p => !p.pl).forEach(p => { p.search = [p.abbr, p.name, p.en, p.ctry.name, p.ctry.iso].join(" ").toLowerCase(); });
}
const asked = (dim, w) => DIM.get(dim).waves.includes(w);
const dimsIn = w => D.dims.filter(d => d.waves.includes(w));
// A Polish party that CHES rated under different names keeps the name it had in each wave (SLD until
// 2019, Nowa Lewica in 2024; Unia Wolności in 2002, then PD); the party's own label is its latest name
function waveEntry(p, w) {
  const m = p.ptByWave, ids = m ? [...new Set(Object.values(m))] : [];
  if (ids.length < 2 || !m[w] || m[w] === ids[ids.length - 1]) return null;
  return D.ptNames.find(t => t.id === m[w]) || null;
}
const waveName = (p, w) => { const x = waveEntry(p, w); return x ? x.abbr : null; };
// the party's full names as they stood in the wave
const fullNames = (p, w = S.wave) => { const x = waveEntry(p, w); return (x ? [x.name] : [p.name, p.en]).filter(Boolean).filter((v, i, a) => a.indexOf(v) === i); };
const nameOf = (p, w = S.wave) => p.pl ? (waveName(p, w) || p.label || p.abbr) : p.abbr;
// The party to show when none is chosen: the Polish party with the largest vote at the election the wave's ratings follow
function defaultSel(w) {
  let best = null;
  for (const r of BYW.get(w) || []) if (r.p.pl && isNum(r.listV) && (!best || r.listV > best.listV)) best = r;
  return best ? best.p.id : 2605;
}
function inScope(p, scope = S.scope) {
  return scope === "eu" ? true : scope === "cee" ? (p.ctry.east || p.pl) : p.pl;
}
function partyColour(p, r) {
  if (p.pl) return p.col;
  if (S.colour === "family") return (FAMG[r.fam] || FAM_OTHER).col;
  if (S.colour === "none") return "#B8B8B8";
  return POPC[popCat(r.pop).k].col;
}
// the 0–10 equivalent of a value, for distances (European integration runs 1–7)
const norm10 = (d, v) => (v - d.min) / (d.max - d.min) * 10;
const NB_DIMS = ["lrecon", "galtan", "eu_position", "antielite_salience"];
function neighbours(r, pool, k) {
  const ds = NB_DIMS.filter(d => asked(d, r.w) && isNum(r.v[d]));
  const out = [];
  for (const q of pool) {
    if (q === r) continue;
    let s = 0, m = 0;
    for (const d of ds) if (isNum(q.v[d])) { const dm = DIM.get(d); s += (norm10(dm, r.v[d]) - norm10(dm, q.v[d])) ** 2; m++; }
    if (m >= Math.min(3, ds.length)) out.push({q, d: Math.sqrt(s / m)});
  }
  return out.sort((a, b) => a.d - b.d).slice(0, k);
}

// ---- state ----------------------------------------------------------------------------------
const S = {view: "map", wave: 2024, scope: "pl", sd: false, x: "lrecon", y: "galtan", colour: "family", trails: false, voters: false, fit: false, selUser: false,
  sel: 2605, group: "scope", prev: true, popY: "galtan", measure: "galtan", vdim: "lrgen"};
const VIEWS = ["map", "profile", "populism", "time", "voters", "systems"];

function hashTarget() { try { if (window.top !== window && window.top.location.host === location.host) return window.top; } catch (e) { /* cross-origin */ } return window; }
function stateString() {
  const q = new URLSearchParams();
  if (S.view !== "map") q.set("view", S.view);
  if (S.selUser) q.set("party", S.sel);
  if (S.wave !== WAVES[WAVES.length - 1]) q.set("w", S.wave);
  if (S.scope !== "pl") q.set("s", S.scope);
  if (S.sd) q.set("sd", "1");
  if (S.x !== "lrecon") q.set("x", S.x);
  if (S.y !== "galtan") q.set("y", S.y);
  if (S.colour !== "family") q.set("c", S.colour);
  if (S.trails) q.set("t", "1");
  if (S.voters) q.set("vo", "1");
  if (S.fit) q.set("fl", "1");
  if (S.group !== "scope") q.set("g", S.group);
  if (S.popY !== "galtan") q.set("py", S.popY);
  if (S.measure !== "galtan") q.set("m", S.measure);
  if (S.vdim !== "lrgen") q.set("vd", S.vdim);
  return q.toString();
}
const saveHash = debounce(() => {
  const t = hashTarget(), s = stateString();
  try { t.history.replaceState(null, "", t.location.pathname + t.location.search + (s ? "#" + s : "")); } catch (e) { /* ignore */ }
}, 250);
function restoreHash() {
  let h = "";
  try { h = hashTarget().location.hash.slice(1); } catch (e) { h = location.hash.slice(1); }
  const q = new URLSearchParams(h);
  if (VIEWS.includes(q.get("view"))) S.view = q.get("view");
  const pid = parseInt(q.get("party"), 10);
  if (P.has(pid)) { S.sel = pid; S.selUser = true; }
  const pt = q.get("pt");
  if (pt) {
    if (D.ptChes[pt] && P.has(D.ptChes[pt])) { S.sel = D.ptChes[pt]; S.selUser = true; }
    else S.ptMissing = pt;
  }
  const w = parseInt(q.get("w"), 10);
  if (WAVES.includes(w)) { S.wave = w; if (!S.selUser) S.sel = defaultSel(w); }
  else if (P.has(S.sel) && !P.get(S.sel).byW.has(S.wave)) { const ws = [...P.get(S.sel).byW.keys()].filter(x => WAVES.includes(x)); if (ws.length) S.wave = Math.max(...ws); }
  if (["eu", "cee", "pl"].includes(q.get("s"))) S.scope = q.get("s");
  if (q.get("sd") === "1") { S.sd = true; S.scope = "pl"; }
  if (DIM.has(q.get("x"))) S.x = q.get("x");
  if (DIM.has(q.get("y"))) S.y = q.get("y");
  if (["populist", "family", "none"].includes(q.get("c"))) S.colour = q.get("c");
  if (q.get("t") === "1") S.trails = true;
  if (q.get("vo") === "1") S.voters = true;
  if (q.get("fl") === "1") S.fit = true;
  if (["scope", "family", "pop"].includes(q.get("g"))) S.group = q.get("g");
  if (DIM.has(q.get("py"))) S.popY = q.get("py");
  if (["lrgen", "lrecon", "galtan", "align"].includes(q.get("m"))) S.measure = q.get("m");
  if (D.voters && D.voters.dims.some(d => d.id === q.get("vd"))) S.vdim = q.get("vd");
}
function shareUrl() { const t = hashTarget(), s = stateString(); return t.location.origin + t.location.pathname + (s ? "#" + s : ""); }

// ---- tooltips -------------------------------------------------------------------------------
function placeTip(tip, box, e) {
  const b = box.getBoundingClientRect();
  tip.hidden = false;
  const tw = tip.offsetWidth, th = tip.offsetHeight;
  let x = e.clientX - b.left + 14, y = e.clientY - b.top + 14;
  if (x + tw > b.width - 4) x = e.clientX - b.left - tw - 14;
  if (y + th > b.height - 4) y = e.clientY - b.top - th - 10;
  tip.style.left = Math.max(4, x) + "px"; tip.style.top = Math.max(4, y) + "px";
}
function rowTip(r, dims) {
  const p = r.p, c = POPC[popCat(r.pop).k];
  const lines = [`<b>${esc(nameOf(p, r.w))}</b> <span class="s">${esc(p.ctry.name)} · ${r.w}</span>`];
  const nm = fullNames(p, r.w);
  if (nm.length) lines.push(`<div class="s">${esc(nm.join(" · "))}</div>`);
  for (const id of dims) {
    const d = DIM.get(id), v = r.v[id], sd = r.sd && r.sd[id];
    lines.push(`<div class="r"><span>${esc(d.label)}</span><span>${fmt(v)}${isNum(sd) ? ` <span class="s">± ${fmt(sd)}</span>` : ""}</span></div>`);
  }
  const fam = D.families.find(f => f.id === r.fam);
  if (fam) lines.push(`<div class="s">CHES family: ${esc(lc(fam.label))}</div>`);
  lines.push(`<div class="s"><span class="dot" style="background:${c.col};width:8px;height:8px"></span> PopuList: ${esc(popText(r.pop))}</div>`);
  if (p.pl && r.list) lines.push(`<div class="s">Ran on the ${esc(r.list)} list: ${fmt(r.listV)}% in ${r.ref}</div>`);
  else if (isNum(r.vote)) lines.push(`<div class="s">Vote at the last election: ${fmt(r.vote)}%</div>`);
  if (r.n) lines.push(`<div class="s">± is the spread (SD) of ${r.n} experts' ratings</div>`);
  return lines.join("");
}

// ---- controls -------------------------------------------------------------------------------
function dimOptions(sel, current, w, filter) {
  const groups = [...new Set(D.dims.filter(d => !filter || filter(d)).map(d => d.group))];
  sel.innerHTML = groups.map(g => `<optgroup label="${esc(g)}">` + D.dims.filter(d => d.group === g && (!filter || filter(d))).map(d =>
    `<option value="${d.id}"${d.id === current ? " selected" : ""}${d.waves.includes(w) ? "" : " disabled"}>${esc(d.label)}${d.waves.includes(w) ? "" : " (not asked in " + w + ")"}</option>`).join("") + "</optgroup>").join("");
}
function renderControls() {
  document.querySelectorAll("#views button").forEach(b => { const on = b.dataset.v === S.view; b.setAttribute("aria-pressed", on); b.setAttribute("aria-selected", on); });
  $("waves").innerHTML = WAVES.map(w => `<button type="button" data-w="${w}" aria-pressed="${w === S.wave}">${w}</button>`).join("");
  document.querySelectorAll("#scopes button").forEach(b => b.setAttribute("aria-pressed", b.dataset.s === S.scope));
  $("sdbtn").setAttribute("aria-pressed", S.sd); $("sdbtn").disabled = S.wave !== 2024;
  $("sdbtn").title = S.wave !== 2024 ? "The experts' spread (SD) is only published for the 2024 wave" : "Show the spread (one SD) of the experts' ratings on every party";
  document.querySelectorAll("#colourby button").forEach(b => b.setAttribute("aria-pressed", b.dataset.c === S.colour));
  document.querySelectorAll("#groups button").forEach(b => b.setAttribute("aria-pressed", b.dataset.g === S.group));
  document.querySelectorAll("#measures button").forEach(b => b.setAttribute("aria-pressed", b.dataset.m === S.measure));
  if (D.voters) dimOptions($("vdims"), (D.voters.dims.find(d => d.id === S.vdim) || D.voters.dims[0]).id, S.wave, d => voterDims().includes(d.id));
  $("trails").checked = S.trails; $("prev").checked = S.prev; $("fitlines").checked = S.fit; $("popfit").checked = S.fit;
  const vok = mapHasVoters(), vsw = $("mapvoterswitch");
  $("mapvoters").checked = S.voters && vok; $("mapvoters").disabled = !vok;
  vsw.hidden = !D.voters; vsw.classList.toggle("off", !vok);
  vsw.title = vok ? "Each Polish party's own voters, from the Social and political trends" : "Shown when both axes have a voters' measure (the issues under Parties and voters)";
  dimOptions($("xdim"), S.x, S.wave); dimOptions($("ydim"), S.y, S.wave);
  dimOptions($("popy"), S.popY, S.wave, d => d.id !== "antielite_salience");
  VIEWS.forEach(v => { $("v-" + v).hidden = v !== S.view; });
}

// ---- scatter (map and populism) -------------------------------------------------------------
// The map can add each Polish party's own voters where both axes have a voters' measure
// (D.voters, on 0-10; European integration's 1-7 axis takes them rescaled)
const voterDims = () => D.voters ? D.voters.dims.map(d => d.id) : [];
const voterDim = id => D.voters ? D.voters.dims.find(d => d.id === id) : null;
const mapHasVoters = () => S.x !== S.y && voterDims().includes(S.x) && voterDims().includes(S.y);
const onAxis = (d, v) => d.min + v / 10 * (d.max - d.min);
let MAPV = [], MAPVALL = null;
// Smoothed fit of y on x: local linear regression with tricube weights over the nearest span × n
// points (LOESS, as R's loess(degree = 1)), evaluated across the range of x. Under 20 points the
// window is all of them (span 1), as narrower local fits through a handful of parties swing wildly;
// a straight line for three or four points, none for fewer
function smoothFit(pts, span = 0.75, k = 48) {
  const n = pts.length;
  if (n < 3) return null;
  const x0 = Math.min(...pts.map(p => p.x)), x1 = Math.max(...pts.map(p => p.x));
  if (x1 - x0 < 1e-9) return null;
  const wls = (w, g) => {
    let sw = 0, sx = 0, sy = 0, sxx = 0, sxy = 0;
    pts.forEach((p, i) => { sw += w[i]; sx += w[i] * p.x; sy += w[i] * p.y; sxx += w[i] * p.x * p.x; sxy += w[i] * p.x * p.y; });
    const den = sw * sxx - sx * sx;
    if (sw <= 0) return null;
    if (Math.abs(den) < 1e-12) return sy / sw;
    const b = (sw * sxy - sx * sy) / den;
    return (sy - b * sx) / sw + b * g;
  };
  if (n < 5) return [x0, x1].map(g => ({x: g, y: wls(pts.map(() => 1), g)}));
  const q = n < 20 ? n : Math.max(3, Math.floor(span * n));
  const out = [];
  for (let i = 0; i < k; i++) {
    const g = x0 + (x1 - x0) * i / (k - 1);
    const d = pts.map(p => Math.abs(p.x - g)), h = Math.max([...d].sort((a, b) => a - b)[q - 1], 1e-9) * 1.0001;
    const y = wls(d.map(v => v < h ? (1 - (v / h) ** 3) ** 3 : 0), g);
    if (isNum(y)) out.push({x: g, y});
  }
  return out.length > 1 ? out : null;
}
const FIT_PL = INK, FIT_OTHER = "#8F8F8F";
let FITN = {pl: 0, other: 0};
function axisTicks(d) { const t = []; for (let v = d.min; v <= d.max + 1e-9; v += (d.max - d.min > 6 ? 2 : 1)) t.push(v); return t; }
function scatter(svgId, boxId, xd, yd, w, opts) {
  const box = $(boxId), svg = $(svgId);
  const W = Math.max(320, box.clientWidth - 12), H = Math.round(clamp(W * 0.66, 300, 600));
  const m = {l: 40, r: 16, t: 14, b: 50};
  const sx = v => m.l + (v - xd.min) / (xd.max - xd.min) * (W - m.l - m.r);
  const sy = v => H - m.b - (v - yd.min) / (yd.max - yd.min) * (H - m.t - m.b);
  const o = [];
  // grid and axes
  for (const v of axisTicks(xd)) o.push(`<line x1="${sx(v)}" x2="${sx(v)}" y1="${m.t}" y2="${H - m.b}" stroke="${RULE}"></line><text x="${sx(v)}" y="${H - m.b + 15}" text-anchor="middle" font-size="11" fill="${MUTED}">${v}</text>`);
  for (const v of axisTicks(yd)) o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${sy(v)}" y2="${sy(v)}" stroke="${RULE}"></line><text x="${m.l - 8}" y="${sy(v) + 4}" text-anchor="end" font-size="11" fill="${MUTED}">${v}</text>`);
  const xm = (xd.min + xd.max) / 2, ym = (yd.min + yd.max) / 2;
  o.push(`<line x1="${sx(xm)}" x2="${sx(xm)}" y1="${m.t}" y2="${H - m.b}" stroke="${FAINT}" stroke-dasharray="3 3"></line>`);
  o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${sy(ym)}" y2="${sy(ym)}" stroke="${FAINT}" stroke-dasharray="3 3"></line>`);
  o.push(`<text x="${m.l}" y="${H - 10}" font-size="11.5" fill="${SOFT}">← ${esc(xd.lo)}</text>`);
  o.push(`<text x="${W - m.r}" y="${H - 10}" text-anchor="end" font-size="11.5" fill="${SOFT}">${esc(xd.hi)} →</text>`);
  if (W >= 640) o.push(`<text x="${(m.l + W - m.r) / 2}" y="${H - 10}" text-anchor="middle" font-size="12" font-weight="500" fill="${INK}">${esc(xd.label)}</text>`);
  const yl = (H - m.b + m.t) / 2;
  o.push(`<text transform="translate(13 ${yl}) rotate(-90)" text-anchor="middle" font-size="12" font-weight="500" fill="${INK}">${esc(yd.label)}</text>`);
  const halo = `stroke="#fff" stroke-width="3" paint-order="stroke" pointer-events="none"`;
  const yPoles = `<text x="${m.l + 5}" y="${m.t + 12}" font-size="11.5" fill="${SOFT}" ${halo}>↑ ${esc(yd.hi)}</text><text x="${m.l + 5}" y="${H - m.b - 6}" font-size="11.5" fill="${SOFT}" ${halo}>↓ ${esc(yd.lo)}</text>`;

  const ok = r => isNum(r.v[xd.id]) && isNum(r.v[yd.id]);
  const rows = (BYW.get(w) || []).filter(r => ok(r));
  const sel = P.get(S.sel);
  const back = rows.filter(r => !r.p.pl && inScope(r.p) && r.p !== sel);
  const rad = v => 2.6 + Math.sqrt(isNum(v) ? v : 1) * 0.85;
  // backdrop: paler when coloured by family, so a Polish party stands out among its own family
  const fill = S.colour === "family" && svgId === "map" ? 0.35 : 0.62;
  for (const r of back) {
    const c = opts.colour(r), pc = popCat(r.pop), hollow = opts.hollow && pc.bl;
    o.push(`<circle data-k="r:${r.i}" cx="${sx(r.v[xd.id]).toFixed(1)}" cy="${sy(r.v[yd.id]).toFixed(1)}" r="${rad(r.vote).toFixed(1)}" fill="${hollow ? "#fff" : c}" fill-opacity="${hollow ? 1 : fill}" stroke="${hollow ? c : "#fff"}" stroke-width="${hollow ? 1.6 : 0.8}"></circle>`);
  }
  // smoothed fits: one through the Polish parties, one through the other parties compared
  if (opts.fit) FITN = {pl: 0, other: 0};
  if (opts.fit && xd.id !== yd.id) {
    const sets = [["pl", rows.filter(r => r.p.pl), FIT_PL, ""], ["other", S.scope === "pl" ? [] : back.concat(sel && !sel.pl && inScope(sel) ? rows.filter(r => r.p === sel) : []), FIT_OTHER, ' stroke-dasharray="7 4"']];
    for (const [k, set, col, dash] of sets) {
      const f = smoothFit(set.map(r => ({x: r.v[xd.id], y: r.v[yd.id]})));
      if (!f) continue;
      FITN[k] = set.length;
      const d = f.map((p, i) => (i ? "L" : "M") + sx(p.x).toFixed(1) + "," + sy(clamp(p.y, yd.min, yd.max)).toFixed(1)).join("");
      o.push(`<path d="${d}" fill="none" stroke="#fff" stroke-width="5.5" stroke-opacity=".85" stroke-linejoin="round" pointer-events="none"></path>`);
      o.push(`<path d="${d}" fill="none" stroke="${col}" stroke-width="2.4"${dash} stroke-linejoin="round" stroke-linecap="round" pointer-events="none"></path>`);
    }
  }
  // trails of Polish parties: earlier waves joined to this one
  if (opts.trails) {
    for (const p of POLISH) {
      const pts = [...p.byW.values()].filter(r => r.w <= w && ok(r)).sort((a, b) => a.w - b.w);
      if (pts.length < 2 || !p.byW.has(w)) continue;
      const on = p.id === S.sel;
      o.push(`<polyline points="${pts.map(r => sx(r.v[xd.id]).toFixed(1) + "," + sy(r.v[yd.id]).toFixed(1)).join(" ")}" fill="none" stroke="${p.col}" stroke-width="${on ? 2.2 : 1.5}" stroke-opacity="${on ? 0.8 : 0.45}" stroke-linejoin="round"></polyline>`);
      for (const r of pts.slice(0, -1)) o.push(`<circle data-k="r:${r.i}" cx="${sx(r.v[xd.id]).toFixed(1)}" cy="${sy(r.v[yd.id]).toFixed(1)}" r="3.2" fill="#fff" stroke="${p.col}" stroke-width="1.5"></circle><text x="${(sx(r.v[xd.id]) + 5).toFixed(1)}" y="${(sy(r.v[yd.id]) - 4).toFixed(1)}" font-size="9.5" fill="${MUTED}" pointer-events="none">${String(r.w).slice(2)}</text>`);
    }
  }
  // each Polish party's own voters: a diamond joined to the party by a dashed line, all voters in grey
  const vobs = [];
  if (svgId === "map") { MAPV = []; MAPVALL = null; }
  if (opts.voters) {
    const V = D.voters, at = (d, pid) => V.parties.find(q => q.d === d && q.w === w && q.p === pid);
    MAPV = rows.filter(r => r.p.pl && inScope(r.p)).map(r => ({r, a: at(xd.id, r.p.id), b: at(yd.id, r.p.id)})).filter(x => x.a && x.b);
    const ax = V.all.find(q => q.d === xd.id && q.w === w), ay = V.all.find(q => q.d === yd.id && q.w === w);
    MAPVALL = ax && ay ? {a: ax, b: ay} : null;
    const diamond = (x, y, s) => `M${x.toFixed(1)},${(y - s).toFixed(1)}L${(x + s).toFixed(1)},${y.toFixed(1)}L${x.toFixed(1)},${(y + s).toFixed(1)}L${(x - s).toFixed(1)},${y.toFixed(1)}Z`;
    if (MAPVALL) {
      const x = sx(onAxis(xd, MAPVALL.a.m)), y = sy(onAxis(yd, MAPVALL.b.m));
      o.push(`<path data-k="va:0" d="${diamond(x, y, 6.5)}" fill="#8F8F8F" stroke="#fff" stroke-width="1.5"></path>`);
      o.push(`<text x="${(x + 9).toFixed(1)}" y="${(y + 4).toFixed(1)}" font-size="11" fill="${SOFT}" stroke="#fff" stroke-width="3" paint-order="stroke" pointer-events="none">All voters</text>`);
      vobs.push({x1: x - 7, x2: x + 66, y1: y - 9, y2: y + 7});
    }
    MAPV.forEach((v, i) => {
      const r = v.r, px = sx(r.v[xd.id]), py = sy(r.v[yd.id]), x = sx(onAxis(xd, v.a.m)), y = sy(onAxis(yd, v.b.m)), on = r.p === sel;
      o.push(`<line x1="${px.toFixed(1)}" y1="${py.toFixed(1)}" x2="${x.toFixed(1)}" y2="${y.toFixed(1)}" stroke="${r.p.col}" stroke-width="${on ? 1.8 : 1.3}" stroke-opacity="${on ? 0.8 : 0.55}" stroke-dasharray="4 3" pointer-events="none"></line>`);
      if (on) {
        o.push(`<line x1="${sx(onAxis(xd, v.a.lo)).toFixed(1)}" x2="${sx(onAxis(xd, v.a.hi)).toFixed(1)}" y1="${y.toFixed(1)}" y2="${y.toFixed(1)}" stroke="${r.p.col}" stroke-width="1.5" stroke-opacity=".6"></line>`);
        o.push(`<line x1="${x.toFixed(1)}" x2="${x.toFixed(1)}" y1="${sy(onAxis(yd, v.b.lo)).toFixed(1)}" y2="${sy(onAxis(yd, v.b.hi)).toFixed(1)}" stroke="${r.p.col}" stroke-width="1.5" stroke-opacity=".6"></line>`);
      }
      o.push(`<path data-k="v:${i}" d="${diamond(x, y, on ? 7 : 6)}" fill="#fff" stroke="${r.p.col}" stroke-width="2.2" style="cursor:pointer"></path>`);
      vobs.push({x1: x - 7, x2: x + 7, y1: y - 7, y2: y + 7});
    });
  }
  // Polish parties, and the selected party wherever it is from
  const front = rows.filter(r => (r.p.pl && inScope(r.p)) || r.p === sel);
  if (opts.spread && w === 2024 && S.sd) for (const r of front) {
    if (!r.sd) continue;
    const x = sx(r.v[xd.id]), y = sy(r.v[yd.id]), c = r.p.pl ? r.p.col : ACCENT;
    const sdx = r.sd[xd.id], sdy = r.sd[yd.id];
    if (isNum(sdx)) o.push(`<line x1="${sx(clamp(r.v[xd.id] - sdx, xd.min, xd.max))}" x2="${sx(clamp(r.v[xd.id] + sdx, xd.min, xd.max))}" y1="${y}" y2="${y}" stroke="${c}" stroke-opacity=".45" stroke-width="1.5"></line>`);
    if (isNum(sdy)) o.push(`<line x1="${x}" x2="${x}" y1="${sy(clamp(r.v[yd.id] - sdy, yd.min, yd.max))}" y2="${sy(clamp(r.v[yd.id] + sdy, yd.min, yd.max))}" stroke="${c}" stroke-opacity=".45" stroke-width="1.5"></line>`);
  }
  const labels = [];
  for (const r of front) {
    const x = sx(r.v[xd.id]), y = sy(r.v[yd.id]), on = r.p === sel;
    const c = r.p.pl ? r.p.col : opts.colour(r), rr = r.p.pl ? 4.2 + Math.sqrt(isNum(r.listV) ? r.listV : (r.vote || 1)) * 0.8 : rad(r.vote) + 1;
    if (on) o.push(`<circle cx="${x.toFixed(1)}" cy="${y.toFixed(1)}" r="${(rr + 4).toFixed(1)}" fill="none" stroke="${ACCENT}" stroke-width="2"></circle>`);
    o.push(`<circle data-k="r:${r.i}" cx="${x.toFixed(1)}" cy="${y.toFixed(1)}" r="${rr.toFixed(1)}" fill="${c}" stroke="#fff" stroke-width="1.5" style="cursor:pointer"></circle>`);
    labels.push({x, y, r: rr + (on ? 4 : 0), t: r.p.pl ? nameOf(r.p, w) : `${r.p.abbr} (${r.p.ctry.iso})`, on, i: r.i});
  }
  o.push(yPoles);
  o.push(placeLabels(labels, W, H, m, [...labels.map(l => ({x1: l.x - l.r, x2: l.x + l.r, y1: l.y - l.r, y2: l.y + l.r})), ...vobs,
    {x1: m.l, x2: m.l + 10 + yd.hi.length * 6.4, y1: m.t, y2: m.t + 16}, {x1: m.l, x2: m.l + 10 + yd.lo.length * 6.4, y1: H - m.b - 18, y2: H - m.b}]));
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  svg.innerHTML = o.join("");
  return rows;
}
// Direct labels for the front parties, nudged apart greedily
function placeLabels(labels, W, H, m, obstacles = []) {
  const placed = obstacles.slice(), out = [];
  const tw = t => t.length * 6.6 + 4;
  const hit = b => placed.some(q => b.x1 < q.x2 && b.x2 > q.x1 && b.y1 < q.y2 && b.y2 > q.y1);
  labels.sort((a, b) => (b.on - a.on) || a.y - b.y);
  for (const l of labels) {
    const w = tw(l.t);
    const cands = [];
    for (const dy of [0, -12, 12, -24, 24]) {
      cands.push({x: l.x + l.r + 4, y: l.y + 4 + dy, a: "start"});
      cands.push({x: l.x - l.r - 4, y: l.y + 4 + dy, a: "end"});
    }
    let best = null;
    for (const c of cands) {
      const x1 = c.a === "start" ? c.x : c.x - w, b = {x1, x2: x1 + w, y1: c.y - 11, y2: c.y + 2};
      if (b.x1 < m.l || b.x2 > W - m.r + 10 || b.y1 < 0 || b.y2 > H - m.b) continue;
      if (!hit(b)) { best = {c, b}; break; }
    }
    if (!best) continue;
    placed.push(best.b);
    out.push(`<text x="${best.c.x.toFixed(1)}" y="${best.c.y.toFixed(1)}" text-anchor="${best.c.a}" font-size="12" font-weight="${l.on ? 600 : 500}" fill="${l.on ? ACCENT : INK}" stroke="#fff" stroke-width="3" paint-order="stroke" pointer-events="none">${esc(l.t)}</text>`);
  }
  return out.join("");
}
function legendHtml(mode, hollow) {
  if (mode === "family") return FAM_GROUPS.map(g => `<span><span class="dot" style="background:${g.col}"></span>${esc(g.label)}</span>`).join("");
  if (mode === "none") return "";
  return POP_CATS.map(c => `<span><span class="dot" style="background:${c.col}"></span>${esc(c.label)}</span>`).join("")
    + (hollow ? `<span><span class="ring" style="border-color:${MUTED}"></span>Borderline</span>` : "");
}

function renderMap() {
  const xd = DIM.get(S.x), yd = DIM.get(S.y), w = S.wave;
  const miss = [xd, yd].filter(d => !d.waves.includes(w));
  const n = $("mapnotice");
  if (miss.length) {
    n.hidden = false;
    n.innerHTML = miss.map(d => `${esc(d.label)} was not asked in ${w}; it was asked in ${d.waves.join(", ")}.`).join(" ") + " Pick another dimension or wave.";
  } else n.hidden = true;
  const voters = S.voters && mapHasVoters() && !miss.length;
  const rows = scatter("map", "mapbox", xd, yd, w, {colour: r => partyColour(r.p, r), hollow: S.colour === "populist", trails: S.trails, spread: true, voters, fit: S.fit});
  $("maplegend").innerHTML = legendHtml(S.colour, true) + `<span><span class="dot" style="background:${INK}"></span>Polish parties in their own colours</span>`
    + (voters ? `<span><span class="diam" style="border-color:${INK}"></span>Its voters</span><span><span class="diam" style="border-color:#8F8F8F;background:#8F8F8F"></span>All voters</span>` : "")
    + fitLegend();
  const pa = `<a href="../../pages/polish-political-analyses/political-attitudes.html" target="_top">Social and political trends</a>`;
  const shown = rows.filter(r => inScope(r.p)).length;
  $("mapnote").innerHTML = `${shown} parties rated in the ${w} wave (fieldwork ${esc(D.meta.fieldwork[w])})${S.scope !== "pl" ? ", sized by vote" : ""}; Polish parties are sized by the vote for the list they ran on. `
    + (S.sd && w === 2024 ? "The whiskers show the spread (one SD) of the experts' ratings of each party. " : "")
    + (S.trails ? "The trajectory joins each Polish party's positions at earlier waves (labelled by year). " : "")
    + fitNote(xd, yd)
    + (voters ? `Diamonds are each Polish party's own voters at the wave's fieldwork month, from my ${pa}, joined to the party by a dashed line (with 80% intervals for the selected party); grey is all voters. ${MAPV.length ? "" : "No Polish party rated in " + w + " has a voter estimate on both axes. "}The voters' scales are survey measures placed on 0–10${[xd, yd].some(d => d.id === "eu_position" && voterDim(d.id).share) ? " (support for EU membership is a share of those with a view, divided by 10 and set on the 1–7 axis)" : ""}, not the experts' scale: read direction and order, not exact distances. `
      : S.voters && !mapHasVoters() ? `Party voters can be shown when both axes are among the issues under Parties and voters. ` : "")
    + "Hover over a party for its ratings; click to select it.";
}

// The fit lines' legend entries and note, from the last scatter drawn (FITN)
const comparedName = () => S.scope === "cee" ? "other parties in Central and Eastern Europe" : "other parties across Europe";
function fitLegend() {
  if (!S.fit) return "";
  const sw = (c, dash) => `<svg width="24" height="8" aria-hidden="true"><line x1="0" x2="24" y1="4" y2="4" stroke="${c}" stroke-width="2.4"${dash ? ' stroke-dasharray="7 4"' : ""}/></svg>`;
  return (FITN.pl ? `<span>${sw(FIT_PL)}Fit: Polish parties</span>` : "") + (FITN.other ? `<span>${sw(FIT_OTHER, true)}Fit: ${comparedName()}</span>` : "");
}
function fitNote(xd, yd) {
  if (!S.fit || !(FITN.pl || FITN.other)) return S.fit && xd.id !== yd.id ? "Too few parties are rated on both axes for a fit line. " : "";
  const parts = [FITN.pl ? `a solid line through the ${FITN.pl} Polish parties${FITN.pl < 5 ? " (a straight line, as there are fewer than five)" : ""}` : "",
    FITN.other ? `a dashed grey line through the ${FITN.other} ${comparedName()}` : ""].filter(Boolean);
  return `Fit lines show how ${esc(yd.label)} varies with ${esc(xd.label)}: ${parts.join(" and ")}. They are smoothed (LOESS: local linear with tricube weights, over the nearest three-quarters of the parties, or over all of them where fewer than 20 are rated, as in Poland), with each party counting once whatever its vote. `;
}

// ---- profile --------------------------------------------------------------------------------
function compareRows(r) {
  const rows = (BYW.get(r.w) || []).filter(q => q !== r);
  if (S.group === "family") return rows.filter(q => q.fam === r.fam);
  if (S.group === "pop") { const k = popCat(r.pop).k; return rows.filter(q => popCat(q.pop).k === k); }
  return rows.filter(q => inScope(q.p));
}
function groupLabel(r) {
  if (S.group === "family") { const f = D.families.find(x => x.id === r.fam); return `${f ? lc(f.label) : "same-family"} parties across Europe`; }
  if (S.group === "pop") { const c = POPC[popCat(r.pop).k]; return c.k === "none" ? "parties across Europe that PopuList does not classify" : `parties across Europe that PopuList classifies as ${c.label.toLowerCase()}`; }
  return S.scope === "eu" ? "parties across Europe" : S.scope === "cee" ? "parties in Central and Eastern Europe" : "Polish parties";
}
function renderProfile() {
  const p = P.get(S.sel), r = p.byW.get(S.wave), el = $("profile");
  if (!r) {
    const ws = [...p.byW.keys()].filter(w => WAVES.includes(w));
    el.innerHTML = `<p class="help">${esc(nameOf(p))} was not rated in ${S.wave}. ${ws.length ? "It was rated in " + ws.map(w => `<button class="linkbtn" data-goto="${w}">${w}</button>`).join(", ") + "." : ""}</p>`;
    $("profilehelp").textContent = "";
    return;
  }
  const cmp = compareRows(r);
  const prevW = WAVES.filter(w => w < S.wave && p.byW.has(w)).pop();
  const prev = S.prev && prevW ? p.byW.get(prevW) : null;
  $("profilehelp").innerHTML = `${esc(nameOf(p))} in ${S.wave} on every dimension the experts rated, against ${cmp.length} other ${esc(groupLabel(r))}. Grey ticks are those parties, the grey bar their middle half and the dark tick their median. ` +
    (p.pl ? "Small coloured dots are the other Polish parties. " : "") + (prev ? `The hollow circle is ${prevW}. ` : "") + (r.sd ? "Whiskers show one SD of the experts' ratings." : "");
  const W = Math.max(200, el.clientWidth - 32 - 200 - 92 - 20);
  const others = (BYW.get(S.wave) || []).filter(q => q.p.pl && q.p !== p);
  const groups = [...new Set(D.dims.map(d => d.group))];
  const o = [];
  for (const g of groups) {
    const ds = D.dims.filter(d => d.group === g && d.waves.includes(S.wave) && isNum(r.v[d.id]));
    if (!ds.length) continue;
    o.push(`<div class="label grp">${esc(g)}</div>`);
    for (const d of ds) {
      const v = r.v[d.id], sx = x => 8 + (x - d.min) / (d.max - d.min) * (W - 16);
      const vals = cmp.map(q => q.v[d.id]).filter(isNum).sort((a, b) => a - b);
      const s = [`<line x1="8" x2="${W - 8}" y1="20" y2="20" stroke="${FAINT}"></line>`];
      for (const x of vals) s.push(`<line x1="${sx(x).toFixed(1)}" x2="${sx(x).toFixed(1)}" y1="15" y2="25" stroke="#9E9E9E" stroke-opacity=".35"></line>`);
      if (vals.length >= 4) {
        const q1 = quant(vals, .25), q3 = quant(vals, .75), md = quant(vals, .5);
        s.push(`<rect x="${sx(q1).toFixed(1)}" y="17" width="${Math.max(1, sx(q3) - sx(q1)).toFixed(1)}" height="6" rx="3" fill="#C9C9C9" fill-opacity=".7"></rect>`);
        s.push(`<line x1="${sx(md).toFixed(1)}" x2="${sx(md).toFixed(1)}" y1="13" y2="27" stroke="${SOFT}" stroke-width="2"></line>`);
      }
      if (p.pl || S.scope !== "eu") for (const q of others) if (isNum(q.v[d.id])) s.push(`<circle cx="${sx(q.v[d.id]).toFixed(1)}" cy="7" r="3.4" fill="${q.p.col}"><title>${esc(nameOf(q.p))}: ${fmt(q.v[d.id])}</title></circle>`);
      const col = p.pl ? p.col : ACCENT;
      if (prev && isNum(prev.v[d.id])) {
        s.push(`<line x1="${sx(prev.v[d.id]).toFixed(1)}" x2="${sx(v).toFixed(1)}" y1="20" y2="20" stroke="${col}" stroke-width="2" stroke-opacity=".5"></line>`);
        s.push(`<circle cx="${sx(prev.v[d.id]).toFixed(1)}" cy="20" r="4.5" fill="#fff" stroke="${col}" stroke-width="1.6"><title>${prevW}: ${fmt(prev.v[d.id])}</title></circle>`);
      }
      const sd = r.sd && r.sd[d.id];
      if (isNum(sd)) s.push(`<line x1="${sx(clamp(v - sd, d.min, d.max)).toFixed(1)}" x2="${sx(clamp(v + sd, d.min, d.max)).toFixed(1)}" y1="20" y2="20" stroke="${col}" stroke-width="1.5" stroke-opacity=".55"></line>`);
      s.push(`<circle cx="${sx(v).toFixed(1)}" cy="20" r="6" fill="${col}" stroke="#fff" stroke-width="1.5"><title>${esc(nameOf(p))} ${S.wave}: ${fmt(v)}${isNum(sd) ? " ± " + fmt(sd) : ""}</title></circle>`);
      s.push(`<text x="8" y="34" font-size="10" fill="${MUTED}">${esc(d.lo)}</text><text x="${W - 8}" y="34" text-anchor="end" font-size="10" fill="${MUTED}">${esc(d.hi)}</text>`);
      const below = vals.filter(x => x < v).length + 0.5 * vals.filter(x => x === v).length;
      const pctl = vals.length ? Math.round(100 * below / vals.length) : null;
      const delta = prev && isNum(prev.v[d.id]) ? v - prev.v[d.id] : null;
      o.push(`<div class="prow"><div class="nm">${esc(d.label)}<small>${d.min}–${d.max} scale</small></div><svg viewBox="0 0 ${W} 36" aria-label="${esc(d.label)}">${s.join("")}</svg>` +
        `<div class="val">${fmt(v)}${delta != null ? ` <span class="${Math.abs(delta) >= 0.05 ? "" : "muted"}">(${signed(delta)})</span>` : ""}<small>${pctl != null ? `above ${pctl}%` : ""}</small></div></div>`);
    }
  }
  el.innerHTML = o.join("");
}

// ---- populism -------------------------------------------------------------------------------
function auc(pos, neg) {
  if (!pos.length || !neg.length) return NaN;
  let s = 0;
  for (const a of pos) for (const b of neg) s += a > b ? 1 : a === b ? 0.5 : 0;
  return s / (pos.length * neg.length);
}
function renderPopulism() {
  const xd = DIM.get("antielite_salience"), yd = DIM.get(S.popY), w = S.wave;
  const n = $("popnotice");
  const wsOk = xd.waves.filter(x => WAVES.includes(x));
  if (!xd.waves.includes(w)) {
    n.hidden = false;
    n.innerHTML = `Experts rated anti-elite rhetoric only in ${wsOk.join(", ")}. ` + wsOk.map(x => `<button class="linkbtn" data-goto="${x}">Show ${x}</button>`).join(" · ");
    $("pop").innerHTML = ""; $("popcards").innerHTML = ""; $("poplegend").innerHTML = "";
    return;
  }
  n.hidden = !(!yd.waves.includes(w));
  if (!yd.waves.includes(w)) n.innerHTML = `${esc(yd.label)} was not asked in ${w}. Pick another dimension.`;
  scatter("pop", "popbox", xd, yd, w, {colour: r => POPC[popCat(r.pop).k].col, hollow: true, trails: false, spread: true, fit: S.fit});
  $("poplegend").innerHTML = legendHtml("populist", true) + `<span><span class="dot" style="background:${INK}"></span>Polish parties in their own colours</span>` + fitLegend();
  $("popfitnote").innerHTML = fitNote(xd, yd);
  const rows = (BYW.get(w) || []).filter(r => inScope(r.p) && isNum(r.v.antielite_salience));
  const pos = rows.filter(r => st(r.pop, "populist") === 2).map(r => r.v.antielite_salience);
  const neg = rows.filter(r => st(r.pop, "populist") === 0).map(r => r.v.antielite_salience);
  const a = auc(pos, neg);
  const where = S.scope === "eu" ? "across Europe" : S.scope === "cee" ? "in Central and Eastern Europe" : "in Poland";
  const tbl = list => `<table class="t"><tr><th>Party</th><th>Anti-elite</th><th>PopuList</th></tr>` + list.map(r =>
    `<tr data-p="${r.p.id}"><td>${esc(nameOf(r.p))} <span class="muted">${esc(r.p.ctry.iso)}</span></td><td>${fmt(r.v.antielite_salience)}</td><td>${st(r.pop, "populist") === 2 ? "populist" : st(r.pop, "populist") === 1 ? "borderline" : "–"}</td></tr>`).join("") + `</table>`;
  const lowPop = rows.filter(r => st(r.pop, "populist") > 0).sort((x, y) => x.v.antielite_salience - y.v.antielite_salience).slice(0, 6);
  const highNon = rows.filter(r => st(r.pop, "populist") === 0).sort((x, y) => y.v.antielite_salience - x.v.antielite_salience).slice(0, 6);
  $("popcards").innerHTML =
    `<div class="card soft"><span class="label">Agreement</span><div class="big">${isNum(a) ? Math.round(100 * a) + "%" : "–"}</div>` +
    `<p class="help">The chance that a party PopuList classifies as populist has a higher anti-elite score than a party it does not classify as populist, ${where} in ${w} (${pos.length} populist and ${neg.length} other parties; borderline cases left out). 50% would be no better than chance.</p></div>` +
    `<div class="card soft"><span class="label">Populist, little anti-elite rhetoric</span><p class="help">PopuList calls them populist (or borderline), but experts rate anti-elite rhetoric as unimportant to them.</p>${lowPop.length ? tbl(lowPop) : '<p class="help">None.</p>'}</div>` +
    `<div class="card soft"><span class="label">Anti-elite, not populist</span><p class="help">Experts rate anti-elite rhetoric as important to them, but PopuList does not call them populist.</p>${highNon.length ? tbl(highNon) : '<p class="help">None.</p>'}</div>`;
}

// ---- over time ------------------------------------------------------------------------------
const TIME_DIMS = ["lrgen", "lrecon", "galtan", "eu_position", "immigrate_policy", "antielite_salience"];
function renderTime() {
  const grid = $("timegrid");
  if (!grid.dataset.built) {
    grid.innerHTML = TIME_DIMS.map(id => `<div class="chartbox" id="tb-${id}"><div class="charttitle">${esc(DIM.get(id).label)} <span class="muted" style="font-weight:400;font-size:11.5px">↑ ${esc(DIM.get(id).hi)} · ↓ ${esc(DIM.get(id).lo)}</span></div><svg id="ts-${id}" role="img" aria-label="${esc(DIM.get(id).label)} of Polish parties at each wave"></svg><div class="tip" id="tt-${id}" hidden></div></div>`).join("");
    grid.dataset.built = "1";
    TIME_DIMS.forEach(id => {
      const svg = $("ts-" + id), box = $("tb-" + id), tip = $("tt-" + id);
      svg.addEventListener("mousemove", e => {
        const t = e.target.closest("[data-k]");
        if (!t) { tip.hidden = true; return; }
        const [kind, val] = t.dataset.k.split(":");
        if (kind === "r") { const r = ROWS[+val]; tip.innerHTML = rowTip(r, [id]); }
        else { const p = P.get(+val); tip.innerHTML = `<b>${esc(nameOf(p))}</b>`; }
        placeTip(tip, box, e);
      });
      svg.addEventListener("mouseleave", () => { tip.hidden = true; });
      svg.addEventListener("click", e => { const t = e.target.closest("[data-k]"); if (!t) return; const [kind, val] = t.dataset.k.split(":"); select(kind === "r" ? ROWS[+val].p.id : +val); });
    });
  }
  const sel = P.get(S.sel), x0 = WAVES[0], x1 = WAVES[WAVES.length - 1];
  for (const id of TIME_DIMS) {
    const d = DIM.get(id), box = $("tb-" + id), svg = $("ts-" + id);
    const W = Math.max(260, box.clientWidth - 12), H = Math.round(clamp(W * 0.55, 200, 300));
    const m = {l: 30, r: 12, t: 8, b: 24};
    const sx = w => m.l + (w - x0) / (x1 - x0) * (W - m.l - m.r), sy = v => H - m.b - (v - d.min) / (d.max - d.min) * (H - m.t - m.b);
    const o = [];
    for (const v of axisTicks(d)) o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${sy(v)}" y2="${sy(v)}" stroke="${RULE}"></line><text x="${m.l - 6}" y="${sy(v) + 4}" text-anchor="end" font-size="10.5" fill="${MUTED}">${v}</text>`);
    for (const w of WAVES) o.push(`<text x="${sx(w)}" y="${H - 6}" text-anchor="middle" font-size="10.5" fill="${w === S.wave ? ACCENT : MUTED}" font-weight="${w === S.wave ? 600 : 400}">${w}</text>`);
    if (S.scope !== "pl") {
      const ws = WAVES.filter(w => d.waves.includes(w));
      const q = ws.map(w => { const vals = (BYW.get(w) || []).filter(r => !r.p.pl && inScope(r.p) && isNum(r.v[id])).map(r => r.v[id]).sort((a, b) => a - b); return {w, q1: quant(vals, .25), md: quant(vals, .5), q3: quant(vals, .75)}; }).filter(q => isNum(q.md));
      if (q.length > 1) {
        o.push(`<path d="M${q.map(z => sx(z.w) + "," + sy(z.q3)).join("L")}L${q.slice().reverse().map(z => sx(z.w) + "," + sy(z.q1)).join("L")}Z" fill="#9E9E9E" fill-opacity=".14"></path>`);
        o.push(`<polyline points="${q.map(z => sx(z.w) + "," + sy(z.md)).join(" ")}" fill="none" stroke="#9E9E9E" stroke-width="1.5" stroke-dasharray="4 3"></polyline>`);
      }
    }
    const order = POLISH.slice().sort((a, b) => (a.id === S.sel) - (b.id === S.sel));
    for (const p of order) {
      const pts = [...p.byW.values()].filter(r => WAVES.includes(r.w) && isNum(r.v[id])).sort((a, b) => a.w - b.w);
      if (!pts.length) continue;
      const on = p.id === S.sel;
      if (pts.length > 1) {
        const pp = pts.map(r => sx(r.w).toFixed(1) + "," + sy(r.v[id]).toFixed(1)).join(" ");
        o.push(`<polyline points="${pp}" fill="none" stroke="${p.col}" stroke-width="${on ? 3 : 1.8}" stroke-opacity="${on || !sel.pl ? 1 : 0.55}" stroke-linejoin="round"></polyline>`);
        o.push(`<polyline data-k="p:${p.id}" points="${pp}" fill="none" stroke="transparent" stroke-width="10" style="cursor:pointer"></polyline>`);
      }
      for (const r of pts) o.push(`<circle data-k="r:${r.i}" cx="${sx(r.w).toFixed(1)}" cy="${sy(r.v[id]).toFixed(1)}" r="${on ? 4.5 : 3.5}" fill="${p.col}" stroke="#fff" stroke-width="1.2" style="cursor:pointer"></circle>`);
      if (on) { const l = pts[pts.length - 1]; o.push(`<text x="${(sx(l.w) - 6).toFixed(1)}" y="${(sy(l.v[id]) - 8).toFixed(1)}" text-anchor="end" font-size="12" font-weight="600" fill="${ACCENT}" stroke="#fff" stroke-width="3" paint-order="stroke">${esc(nameOf(p))}</text>`); }
    }
    svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
    svg.innerHTML = o.join("");
  }
  $("timelegend").innerHTML = POLISH.slice().sort((a, b) => b.last - a.last || a.label.localeCompare(b.label)).map(p =>
    `<span><button type="button" class="chip" data-p="${p.id}" aria-pressed="${p.id === S.sel}"><span class="dot" style="background:${p.col}"></span>${esc(nameOf(p))}</button></span>`).join("");
}

// ---- parties and voters ---------------------------------------------------------------------
// Each Polish party as CHES experts place it (filled dot) beside its own voters (ring, with an 80%
// interval) and all voters (dashed line, grey band), on one 0–10 scale (every CHES scale but
// European integration is already on it): European integration is rescaled from 1–7, and support
// for EU membership (a share of those with a view, share: true) divided by 10.
let VROWS = [], VALL = null;
function renderVoters() {
  const V = D.voters, box = $("vbox"), svg = $("vt"), notice = $("vnotice");
  VROWS = []; VALL = null;
  if (!V) { notice.hidden = false; notice.textContent = "Voters' positions could not be loaded."; svg.innerHTML = ""; $("vlegend").innerHTML = ""; $("vhelp").textContent = ""; return; }
  const dv = V.dims.find(d => d.id === S.vdim) || V.dims[0], d = DIM.get(dv.id), w = S.wave;
  const party10 = v => norm10(d, v);
  VALL = V.all.find(a => a.d === dv.id && a.w === w) || null;
  VROWS = POLISH.map(p => ({p, r: p.byW.get(w), v: V.parties.find(q => q.d === dv.id && q.w === w && q.p === p.id)}))
    .filter(x => x.r && isNum(x.r.v[dv.id]) && x.v)
    .sort((a, b) => party10(a.r.v[dv.id]) - party10(b.r.v[dv.id]));
  if (!VROWS.length) {
    notice.hidden = false; notice.textContent = `No Polish party rated in ${w} has a voter estimate on this issue.`;
    svg.setAttribute("viewBox", "0 0 10 10"); svg.innerHTML = ""; $("vlegend").innerHTML = ""; $("vhelp").textContent = ""; return;
  }
  notice.hidden = true;
  const W = Math.max(320, box.clientWidth - 12), rowH = 36, m = {l: Math.min(150, W * 0.28), r: 22, t: 30, b: 46};
  const H = m.t + m.b + VROWS.length * rowH;
  const sx = v => m.l + clamp(v, 0, 10) / 10 * (W - m.l - m.r);
  const o = [];
  for (let v = 0; v <= 10; v++) {
    o.push(`<line x1="${sx(v)}" x2="${sx(v)}" y1="${m.t - 8}" y2="${H - m.b}" stroke="${v === 5 ? FAINT : RULE}"${v === 5 ? ' stroke-dasharray="3 3"' : ""}></line>`);
    if (v % 2 === 0) o.push(`<text x="${sx(v)}" y="${H - m.b + 15}" text-anchor="middle" font-size="11" fill="${MUTED}">${v}</text>`);
  }
  o.push(`<text x="${sx(0)}" y="${H - 10}" font-size="11.5" fill="${SOFT}">← ${esc(d.lo)}</text><text x="${sx(10)}" y="${H - 10}" text-anchor="end" font-size="11.5" fill="${SOFT}">${esc(d.hi)} →</text>`);
  if (VALL) {
    o.push(`<rect data-k="a:0" x="${sx(VALL.lo).toFixed(1)}" y="${m.t - 8}" width="${Math.max(1, sx(VALL.hi) - sx(VALL.lo)).toFixed(1)}" height="${H - m.b - m.t + 8}" fill="#9E9E9E" fill-opacity=".14"></rect>`);
    o.push(`<line x1="${sx(VALL.m).toFixed(1)}" x2="${sx(VALL.m).toFixed(1)}" y1="${m.t - 8}" y2="${H - m.b}" stroke="#8F8F8F" stroke-width="1.5" stroke-dasharray="5 4" pointer-events="none"></line>`);
    o.push(`<text x="${sx(VALL.m).toFixed(1)}" y="${m.t - 13}" text-anchor="middle" font-size="11" fill="${SOFT}">All voters</text>`);
  }
  VROWS.forEach((x, i) => {
    const y = m.t + i * rowH + rowH / 2, pv = sx(party10(x.r.v[dv.id])), vv = sx(x.v.m), c = x.p.col, on = x.p.id === S.sel;
    o.push(`<rect data-k="row:${i}" x="0" y="${y - rowH / 2}" width="${W}" height="${rowH}" fill="${on ? "#F6EEEE" : "transparent"}" style="cursor:pointer"></rect>`);
    o.push(`<text x="${m.l - 12}" y="${y + 4}" text-anchor="end" font-size="12.5" font-weight="${on ? 600 : 500}" fill="${on ? ACCENT : INK}" pointer-events="none">${esc(nameOf(x.p))}</text>`);
    o.push(`<line x1="${pv.toFixed(1)}" x2="${vv.toFixed(1)}" y1="${y}" y2="${y}" stroke="${c}" stroke-width="2.5" stroke-opacity=".4" pointer-events="none"></line>`);
    o.push(`<line x1="${sx(x.v.lo).toFixed(1)}" x2="${sx(x.v.hi).toFixed(1)}" y1="${y}" y2="${y}" stroke="${c}" stroke-width="2" stroke-opacity=".75" pointer-events="none"></line>`);
    o.push(`<circle data-k="v:${i}" cx="${vv.toFixed(1)}" cy="${y}" r="6" fill="#fff" stroke="${c}" stroke-width="2.2" style="cursor:pointer"></circle>`);
    o.push(`<circle data-k="p:${i}" cx="${pv.toFixed(1)}" cy="${y}" r="7" fill="${c}" stroke="#fff" stroke-width="1.5" style="cursor:pointer"></circle>`);
  });
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  svg.innerHTML = o.join("");
  $("vlegend").innerHTML = `<span><span class="dot" style="background:${INK}"></span>The party, as experts rate it</span>` +
    `<span><span class="ring" style="border-color:${INK}"></span>Its voters, with 80% interval</span>` +
    `<span><svg width="22" height="8"><line x1="0" x2="22" y1="4" y2="4" stroke="#8F8F8F" stroke-width="1.5" stroke-dasharray="5 4"/></svg>All voters</span>`;
  const month = V.month[w] ? MON[+V.month[w].slice(5, 7) - 1] + " " + V.month[w].slice(0, 4) : String(w);
  const link = `<a href="../../pages/polish-political-analyses/political-attitudes.html" target="_top">Social and political trends</a>`;
  const pooled = (what, issues, ends) => `${what} as CHES experts rated each party in ${w}, beside the average position of the party's own voters on ${issues} (${ends}), from my ${link} for ${month}. The voters' scale pools survey questions placed on 0–10; it is not the experts' scale, so read the order and the gaps, not equal numbers as agreement. `;
  const ends = `0 ${d.lo.toLowerCase()}, 10 ${d.hi.toLowerCase()}`;
  $("vhelp").innerHTML = (dv.share
    ? `European integration as CHES experts rated each party in ${w}, rescaled from 1–7 to 0–10, beside support for EU membership among the party's own voters with a view, divided by 10 (8 means 80% support), from my ${link} for ${month}. The two are different kinds of measure, a position on a scale and a share, so read the order and the gaps, not equal numbers as agreement. `
    : {
      lrgen: `Left–right overall as CHES experts rated each party in ${w}, beside the average left–right self-placement of the party's own voters (0 left, 10 right), from my ${link} for ${month}. Experts place parties and voters place themselves; voters bunch towards the centre, so a party further out than its voters is not necessarily out of step with them. `,
      lrecon: pooled("Economic left–right", "questions about redistribution, state and market, taxes and welfare", "0 left, 10 right"),
      galtan: pooled("GAL–TAN", "questions about abortion, gay and lesbian rights, the Church and the state, immigration, the environment and nationalism", "0 libertarian, 10 traditional")
    }[dv.id] || pooled(d.label + (d.max !== 10 ? `, rescaled from ${d.min}–${d.max} to 0–10,` : ""), `questions on ${dv.attitude.toLowerCase()}`, ends))
    + `A party's voters are those who intended to vote for it, or voted for it at the last Sejm election, followed across elections by lineage (PO with KO, SLD with Nowa Lewica); parties whose voters no survey reports apart are left out. Hover for values; click a party to select it.`;
}
function votersTip(k) {
  const [kind, v] = k.split(":"), dv = D.voters.dims.find(d => d.id === S.vdim) || D.voters.dims[0], d = DIM.get(dv.id);
  if (kind === "a") return VALL ? `<b>All voters</b> <span class="s">${S.wave}</span><div class="r"><span>${esc(dv.attitude)}</span><span>${fmt(VALL.m)}</span></div><div class="s">80% interval ${fmt(VALL.lo)}–${fmt(VALL.hi)}. ${esc(dv.measure)}.</div>` : null;
  const x = VROWS[+v]; if (!x) return null;
  const raw = x.r.v[dv.id], p10 = norm10(d, raw);
  const party = `<div class="r"><span>${esc(d.label)} (experts)</span><span>${fmt(p10)}${d.max !== 10 ? ` <span class="s">(${fmt(raw)} on ${d.min}–${d.max})</span>` : ""}</span></div>`;
  const voters = `<div class="r"><span>${esc(x.v.g)} voters</span><span>${fmt(x.v.m)} <span class="s">(${fmt(x.v.lo)}–${fmt(x.v.hi)})</span></span></div>`;
  const all = VALL ? `<div class="r s"><span>All voters</span><span>${fmt(VALL.m)}</span></div>` : "";
  return `<b>${esc(nameOf(x.p))}</b> <span class="s">${S.wave}</span>${party}${voters}${all}<div class="s">Voters: ${esc(dv.measure)}; 80% interval in brackets.</div>`;
}
// The map's electorates: the party's own voters (or all voters) on both axes, beside the experts' rating
function mapVoterTip(kind, i) {
  const xd = DIM.get(S.x), yd = DIM.get(S.y), dv = id => D.voters.dims.find(d => d.id === id);
  const row = (d, q, party) => `<div class="r"><span>${esc(d.label)}</span><span>${fmt(q.m)} <span class="s">(${fmt(q.lo)}–${fmt(q.hi)})</span>${isNum(party) ? ` <span class="s">· party ${fmt(norm10(d, party))}</span>` : ""}</span></div>`;
  if (kind === "va") return MAPVALL ? `<b>All voters</b> <span class="s">${S.wave}</span>${row(xd, MAPVALL.a)}${row(yd, MAPVALL.b)}<div class="s">On 0–10, 80% interval in brackets. ${esc(dv(xd.id).measure)}; ${esc(dv(yd.id).measure).replace(/^./, c => c.toLowerCase())}.</div>` : null;
  const v = MAPV[i]; if (!v) return null;
  return `<b>${esc(nameOf(v.r.p))} voters</b> <span class="s">${S.wave} · ${esc(v.a.g)}</span>${row(xd, v.a, v.r.v[xd.id])}${row(yd, v.b, v.r.v[yd.id])}` +
    `<div class="s">Voters on 0–10, 80% interval in brackets, beside the party as experts rate it (also on 0–10). ${esc(dv(xd.id).measure)}; ${esc(dv(yd.id).measure).replace(/^./, c => c.toLowerCase())}.</div>`;
}

// ---- party systems --------------------------------------------------------------------------
const MEASURE = {
  lrgen: {label: "Polarisation on left–right overall", short: "Polarisation, left–right", lo: 0},
  lrecon: {label: "Polarisation on economic left–right", short: "Polarisation, economic", lo: 0},
  galtan: {label: "Polarisation on GAL–TAN", short: "Polarisation, GAL–TAN", lo: 0},
  align: {label: "Alignment of economic left–right and GAL–TAN", short: "Alignment", lo: -1},
};
function renderSystems() {
  const mm = MEASURE[S.measure], key = S.measure;
  const scopeC = c => S.scope === "cee" ? (CTRY.get(c).east || c === POLAND) : true;
  const rows = D.system.filter(s => scopeC(s.c) && isNum(s[key]));
  $("syshelp").innerHTML = key === "align"
    ? "Alignment is the vote-weighted correlation, across a country's parties, between their economic left–right and GAL–TAN positions. Near +1, the economic right is also traditional–authoritarian, as in most of Western Europe; below 0, the economic left is the traditional–authoritarian side, a pattern common in Central and Eastern Europe. Poland's figures weight parties by the vote for the lists they ran on, from my Polish party tracker; other countries use CHES's vote shares. The dashed line is the median of the other countries shown; Poland is in red."
    : "Polarisation is Dalton's index (0–10): how far parties sit from the vote-weighted mean position, weighted by their vote. 0 means every party in the same place; 10, the vote split equally between the two ends. Poland's figures weight parties by the vote for the lists they ran on, from my Polish party tracker; other countries use CHES's vote shares. The dashed line is the median of the other countries shown; Poland is in red.";
  const allW = D.meta.waves;
  // over time
  {
    const box = $("systimebox"), svg = $("systime");
    const W = Math.max(280, box.clientWidth - 12), H = Math.round(clamp(W * 0.68, 240, 420));
    const m = {l: 34, r: 14, t: 10, b: 26};
    const ymax = key === "align" ? 1 : Math.ceil(Math.max(...rows.map(s => s[key])) + 0.2), ymin = mm.lo;
    const sx = w => m.l + (w - allW[0]) / (allW[allW.length - 1] - allW[0]) * (W - m.l - m.r);
    const sy = v => H - m.b - (v - ymin) / (ymax - ymin) * (H - m.t - m.b);
    const o = [];
    const step = key === "align" ? 0.5 : (ymax > 6 ? 2 : 1);
    for (let v = ymin; v <= ymax + 1e-9; v += step) o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${sy(v)}" y2="${sy(v)}" stroke="${v === 0 && key === "align" ? FAINT : RULE}"></line><text x="${m.l - 6}" y="${sy(v) + 4}" text-anchor="end" font-size="10.5" fill="${MUTED}">${key === "align" ? v.toFixed(1) : v}</text>`);
    for (const w of allW) o.push(`<text x="${sx(w)}" y="${H - 6}" text-anchor="middle" font-size="10.5" fill="${w === S.wave ? ACCENT : MUTED}" font-weight="${w === S.wave ? 600 : 400}">${w}</text>`);
    const byC = new Map();
    rows.forEach(s => { if (!byC.has(s.c)) byC.set(s.c, []); byC.get(s.c).push(s); });
    const draw = (c, pts, on) => {
      const pp = pts.sort((a, b) => a.w - b.w).map(s => sx(s.w).toFixed(1) + "," + sy(s[key]).toFixed(1)).join(" ");
      o.push(`<polyline points="${pp}" fill="none" stroke="${on ? ACCENT : "#A8A8A8"}" stroke-width="${on ? 2.6 : 1.2}" stroke-opacity="${on ? 1 : 0.55}"></polyline>`);
      if (on) pts.forEach(s => o.push(`<circle cx="${sx(s.w).toFixed(1)}" cy="${sy(s[key]).toFixed(1)}" r="3.5" fill="${ACCENT}" stroke="#fff"></circle>`));
      o.push(`<polyline data-k="c:${c}" points="${pp}" fill="none" stroke="transparent" stroke-width="9"></polyline>`);
    };
    for (const [c, pts] of byC) if (c !== POLAND) draw(c, pts, false);
    const med = allW.map(w => { const v = rows.filter(s => s.w === w && s.c !== POLAND).map(s => s[key]).sort((a, b) => a - b); return {w, v: v.length >= 3 ? quant(v, .5) : NaN}; }).filter(z => isNum(z.v));
    if (med.length > 1) o.push(`<polyline points="${med.map(z => sx(z.w).toFixed(1) + "," + sy(z.v).toFixed(1)).join(" ")}" fill="none" stroke="${INK}" stroke-width="1.6" stroke-dasharray="5 4"></polyline>`);
    if (byC.has(POLAND)) draw(POLAND, byC.get(POLAND), true);
    svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.innerHTML = o.join("");
    $("systimetitle").textContent = `${mm.label}, by wave`;
  }
  // ranking in the wave
  {
    const box = $("sysrankbox"), svg = $("sysrank");
    const list = rows.filter(s => s.w === S.wave).sort((a, b) => b[key] - a[key]);
    const W = Math.max(280, box.clientWidth - 12), rowH = 17, H = 18 + list.length * rowH + 20;
    const m = {l: 116, r: 40};
    const vmax = key === "align" ? 1 : Math.ceil(Math.max(...list.map(s => s[key]), 1)), vmin = mm.lo;
    const sx = v => m.l + (v - vmin) / (vmax - vmin) * (W - m.l - m.r);
    const o = [];
    const z = sx(0);
    o.push(`<line x1="${z}" x2="${z}" y1="10" y2="${H - 18}" stroke="${FAINT}"></line>`);
    list.forEach((s, i) => {
      const y = 18 + i * rowH, on = s.c === POLAND, name = CTRY.get(s.c).name;
      o.push(`<g data-k="s:${s.c}"><rect x="0" y="${y - 11}" width="${W}" height="${rowH}" fill="${on ? "#F6EBEB" : "transparent"}"></rect>`);
      o.push(`<text x="${m.l - 8}" y="${y + 2}" text-anchor="end" font-size="12" fill="${on ? ACCENT : INK}" font-weight="${on ? 600 : 400}">${esc(name)}</text>`);
      o.push(`<line x1="${z}" x2="${sx(s[key]).toFixed(1)}" y1="${y - 2}" y2="${y - 2}" stroke="${on ? ACCENT : "#B8B8B8"}" stroke-width="2"></line>`);
      o.push(`<circle cx="${sx(s[key]).toFixed(1)}" cy="${y - 2}" r="4.5" fill="${on ? ACCENT : "#8E8E8E"}"></circle>`);
      o.push(`<text x="${(sx(s[key]) + (s[key] >= 0 || key !== "align" ? 8 : -8)).toFixed(1)}" y="${y + 2}" text-anchor="${s[key] >= 0 || key !== "align" ? "start" : "end"}" font-size="11" fill="${MUTED}">${s[key].toFixed(2)}</text></g>`);
    });
    if (!list.length) o.push(`<text x="10" y="30" font-size="12.5" fill="${MUTED}">No figures for ${S.wave}.</text>`);
    svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.innerHTML = o.join("");
    $("sysranktitle").textContent = `${mm.short}, ${S.wave}`;
  }
}

// ---- panel ----------------------------------------------------------------------------------
function spellsSvg(p) {
  const sp = p.spells || [];
  const t0 = 1989, t1 = 2026, W = 330, rowH = 18, cats = ["populist", "farright", "farleft", "eurosceptic"];
  const sx = y => 86 + (y - t0) / (t1 - t0) * (W - 96);
  const o = [];
  cats.forEach((k, i) => {
    const y = 6 + i * rowH;
    o.push(`<text x="0" y="${y + 10}" font-size="11.5" fill="${SOFT}">${CAT_TITLE[k]}</text><rect x="${sx(t0)}" y="${y + 2}" width="${sx(t1) - sx(t0)}" height="10" rx="3" fill="#F3F3F1"></rect>`);
    for (const s of sp.filter(s => s.cat === k)) {
      const a = s.from || t0, b = (s.to || t1) + (s.to ? 1 : 0);
      o.push(`<rect x="${sx(a).toFixed(1)}" y="${y + 2}" width="${Math.max(2, sx(Math.min(b, t1)) - sx(a)).toFixed(1)}" height="10" rx="3" fill="url(#hatch)"><title>${CAT_TITLE[k]} (borderline) ${s.from || "from the start"}–${s.to || "now"}</title></rect>`);
      if (!s.bl) {
        const fa = s.fullFrom || a, fb = s.fullTo ? s.fullTo + 1 : b;
        o.push(`<rect x="${sx(fa).toFixed(1)}" y="${y + 2}" width="${Math.max(2, sx(Math.min(fb, t1)) - sx(fa)).toFixed(1)}" height="10" rx="3" fill="${ACCENT}"><title>${CAT_TITLE[k]} ${s.fullFrom || s.from || "from the start"}–${s.fullTo || s.to || "now"}</title></rect>`);
      }
    }
  });
  const yb = 6 + cats.length * rowH + 4;
  for (const y of [1990, 2000, 2010, 2020]) o.push(`<text x="${sx(y)}" y="${yb + 8}" text-anchor="middle" font-size="10" fill="${MUTED}">${y}</text>`);
  return `<svg viewBox="0 0 ${W} ${yb + 12}"><defs><pattern id="hatch" width="5" height="5" patternUnits="userSpaceOnUse" patternTransform="rotate(45)"><rect width="5" height="5" fill="#F3E3E3"></rect><line x1="0" y1="0" x2="0" y2="5" stroke="${ACCENT}" stroke-width="1.6"></line></pattern></defs>${o.join("")}</svg>`;
}
function spellText(p) {
  const sp = p.spells || [];
  if (!sp.length) return "PopuList 4.0 does not classify it as populist, far right, far left or Eurosceptic.";
  const by = {};
  sp.forEach(s => { (by[s.cat] = by[s.cat] || []).push(s); });
  const yrs = s => s.from && s.to ? `${s.from}–${s.to}` : s.from ? `since ${s.from}` : s.to ? `until ${s.to}` : "throughout";
  return Object.keys(CAT_TITLE).filter(k => by[k]).map(k => {
    const s = by[k][0];
    let t = `${CAT_TITLE[k]} ${yrs(s)}`;
    if (s.bl) t += " (borderline)";
    else if (s.fullFrom && s.fullFrom !== s.from) t += ` (borderline until ${s.fullFrom - 1})`;
    return t;
  }).join("; ") + ".";
}
function pollLine(p) {
  if (!p.poll || !POLL) return "";
  const i = POLL.rules.parties.indexOf(p.poll);
  const wk = POLL.weeks[POLL.weeks.length - 1];
  if (i < 0 || !wk || !isNum(wk.vote[i])) return "";
  return `<div class="poll">Polling now: <b>${fmt(wk.vote[i])}%</b> <span class="muted">(80% range ${fmt(wk.voteLo[i])}–${fmt(wk.voteHi[i])}), week of ${dstr(wk.date)}</span>${p.poll === "Konfederacja" ? '<br><span class="muted">Without KKP, which left Konfederacja in 2025 and is polled separately.</span>' : ""}</div>`;
}
function renderPanel() {
  const p = P.get(S.sel), r = p.byW.get(S.wave), w = S.wave, o = [];
  o.push(`<div class="search"><input id="q" type="search" placeholder="Find a party: PiS, Fidesz, Lega…" autocomplete="off" aria-label="Find a party"><div class="results" id="results" hidden></div></div>`);
  const now = POLISH.filter(q => q.byW.has(w)).sort((a, b) => (b.byW.get(w).listV || 0) - (a.byW.get(w).listV || 0));
  o.push(`<div class="chips">${now.map(q => `<button type="button" class="chip" data-p="${q.id}" aria-pressed="${q.id === p.id}"><span class="dot" style="background:${q.col}"></span>${esc(nameOf(q))}</button>`).join("")}</div>`);
  o.push(`<div class="phead"><h2>${esc(nameOf(p))}</h2><span class="ctry">${esc(p.ctry.name)}</span></div>`);
  const nm = fullNames(p);
  if (nm.length) o.push(`<div class="pname">${esc(nm.join(" · "))}</div>`);
  // positions in the wave
  if (r) {
    const prevW = WAVES.filter(x => x < w && p.byW.has(x)).pop(), prev = prevW ? p.byW.get(prevW) : null;
    const keys = ["lrgen", "lrecon", "galtan", "eu_position", "antielite_salience", "immigrate_policy"].filter(k => asked(k, w) && isNum(r.v[k]));
    const fam = D.families.find(f => f.id === r.fam);
    o.push(`<div class="card soft"><span class="label">In ${w}</span><div class="kv">` + keys.map(k => {
      const d = DIM.get(k), dv = prev && isNum(prev.v[k]) ? r.v[k] - prev.v[k] : null, sd = r.sd && r.sd[k];
      return `<span class="k">${esc(d.label)} <span class="muted">(${d.min}–${d.max})</span></span><span class="v">${fmt(r.v[k])}${isNum(sd) ? ` <span class="muted" style="font-weight:400">± ${fmt(sd)}</span>` : ""}</span><span class="d ${dv == null || Math.abs(dv) < 0.05 ? "muted" : ""}">${dv == null ? "" : signed(dv)}</span>`;
    }).join("") + `</div><p class="help">` +
      [fam ? `CHES family: ${esc(lc(fam.label))}.` : "", prev ? `Change since ${prevW} in the right-hand column.` : "",
        r.n ? `± is the spread (SD) of ${r.n} experts' ratings.` : "",
        p.pl && r.list ? `Ran on the ${esc(r.list)} list (${fmt(r.listV)}% in ${r.ref}).` : isNum(r.vote) ? `${fmt(r.vote)}% at the last election.` : ""].filter(Boolean).join(" ") + `</p></div>`);
  } else {
    const ws = [...p.byW.keys()].filter(x => WAVES.includes(x));
    o.push(`<div class="card soft"><p class="help">Not rated in ${w}.${ws.length ? " Rated in " + ws.map(x => `<button class="linkbtn" data-goto="${x}">${x}</button>`).join(", ") + "." : ""}</p></div>`);
  }
  // PopuList
  o.push(`<div class="card soft"><span class="label">PopuList 4.0</span><div class="tl">${spellsSvg(p)}</div><p class="help">${esc(spellText(p))}${(p.spells || []).length ? " Hatched: borderline." : ""}</p></div>`);
  // closest parties
  if (r) {
    const pool = (BYW.get(w) || []);
    const eu = neighbours(r, pool.filter(q => q.p.c !== p.c), 5), pl = p.pl ? neighbours(r, pool.filter(q => q.p.pl), 3) : [];
    const btn = x => `<button type="button" data-p="${x.q.p.id}"><span class="dot" style="background:${x.q.p.pl ? x.q.p.col : POPC[popCat(x.q.pop).k].col}"></span><span>${esc(nameOf(x.q.p))} <small>${esc(x.q.p.ctry.name)}</small></span><small>${x.d.toFixed(1)}</small></button>`;
    o.push(`<div class="card soft"><span class="label">Closest parties in ${w}</span>` +
      (pl.length ? `<div class="muted" style="font-size:12px">In Poland</div><div class="nb">${pl.map(btn).join("")}</div><div class="muted" style="font-size:12px">Elsewhere in Europe</div>` : "") +
      `<div class="nb">${eu.map(btn).join("")}</div><p class="help">By distance on economic left–right, GAL–TAN, European integration${asked("antielite_salience", w) ? " and anti-elite rhetoric" : ""} (each put on a 0–10 scale); the number is the typical gap in scale points.</p></div>`);
  }
  // Polish parties: record and links
  if (p.pl) {
    const evs = [];
    (p.events || []).forEach(g => g.rows.forEach(e => evs.push(e)));
    const seen = new Set(), uniq = evs.filter(e => { const k = e.d + e.t + e.from + e.to; if (seen.has(k)) return false; seen.add(k); return true; }).sort((a, b) => a.d < b.d ? 1 : -1);
    const show = S.allEvents ? uniq : uniq.slice(0, 6);
    if (uniq.length) o.push(`<div class="card soft"><span class="label">Record</span><div class="ev">${show.map(e => `<div><span class="yr">${esc(e.prec === "day" || e.prec === "month" ? e.d.slice(0, 7).split("-").reverse().map((x, j) => j === 0 ? MON[+x - 1] : x).join(" ") : e.d.slice(0, 4))}</span><span>${esc(e.desc)}</span></div>`).join("")}</div>${uniq.length > 6 ? `<button class="linkbtn" id="allev">${S.allEvents ? "Show fewer" : "Show all " + uniq.length}</button>` : ""}<p class="help">From my Polish party tracker.</p></div>`);
    const L = [];
    const base = "../../pages/polish-political-analyses/";
    (p.gen || []).forEach(g => L.push(`<a href="${base}party-genealogy.html#pt=${encodeURIComponent(g)}" target="_top">${esc((D.ptNames.find(x => x.id === g) || {abbr: g}).abbr)} in the party genealogy →</a>`));
    if (p.poll) L.push(`<a href="${base}polling-tracker.html" target="_top">${esc(p.poll)} in the Polling tracker →</a>`);
    if (L.length || p.poll) o.push(`<div class="card soft"><span class="label">Elsewhere on this site</span>${pollLine(p)}<div class="links">${L.join("")}</div></div>`);
  }
  o.push(`<div class="actions"><button type="button" class="btn2 sm" id="copylink">Copy link</button><button type="button" class="btn2 sm" id="csvbtn">CSV</button><button type="button" class="btn2 sm" id="pngbtn">PNG</button></div>`);
  o.push(`<p class="help">Data: ${esc(D.meta.sources.ches)}; ${esc(D.meta.sources.populist)}.</p>`);
  $("panel").innerHTML = o.join("");
  wireSearch();
}
function wireSearch() {
  const q = $("q"), res = $("results");
  let hits = [], on = 0;
  const show = () => {
    const s = q.value.trim().toLowerCase();
    if (!s) { res.hidden = true; return; }
    hits = D.parties.filter(p => p.search.includes(s)).sort((a, b) => (b.pl - a.pl) || (b.last - a.last) || String(a.abbr).localeCompare(String(b.abbr))).slice(0, 14);
    on = 0;
    res.innerHTML = hits.length ? hits.map((p, i) => `<button type="button" data-p="${p.id}" class="${i === on ? "on" : ""}"><span>${esc(nameOf(p))}</span><small>${esc(p.ctry.name)} · ${p.first === p.last ? p.first : p.first + "–" + p.last}</small></button>`).join("") : `<div class="help" style="padding:8px 12px">No party found.</div>`;
    res.hidden = false;
  };
  q.addEventListener("input", show);
  q.addEventListener("keydown", e => {
    if (e.key === "Enter" && hits[on]) { select(hits[on].id); }
    else if (e.key === "ArrowDown" || e.key === "ArrowUp") { e.preventDefault(); on = clamp(on + (e.key === "ArrowDown" ? 1 : -1), 0, hits.length - 1); [...res.querySelectorAll("button")].forEach((b, i) => b.classList.toggle("on", i === on)); }
    else if (e.key === "Escape") { res.hidden = true; }
  });
}

// ---- downloads ------------------------------------------------------------------------------
function download(name, blob) {
  const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = name;
  document.body.appendChild(a); a.click(); setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 500);
}
function csv() {
  const w = S.wave, ds = dimsIn(w).map(d => d.id);
  const q = v => v == null || (typeof v === "number" && !isFinite(v)) ? "" : typeof v === "string" && /[",\n]/.test(v) ? `"${v.replace(/"/g, '""')}"` : v;
  const head = ["wave", "country", "ches_party_id", "party", "ches_family", "vote_pct", "list", "list_vote_pct", "populist", "far_right", "far_left", "eurosceptic", ...ds, ...(w === 2024 ? ds.map(d => d + "_sd") : []), "experts"];
  const rows = (BYW.get(w) || []).filter(r => inScope(r.p)).sort((a, b) => a.p.ctry.name.localeCompare(b.p.ctry.name) || String(a.p.abbr).localeCompare(String(b.p.abbr)));
  const lab = s => ["no", "borderline", "yes"][s];
  const lines = [head, ...rows.map(r => [w, r.p.ctry.name, r.p.id, r.p.abbr, (D.families.find(f => f.id === r.fam) || {}).label, r.vote, r.list, r.listV,
    lab(st(r.pop, "populist")), lab(st(r.pop, "farright")), lab(st(r.pop, "farleft")), lab(st(r.pop, "eurosceptic")),
    ...ds.map(d => r.v[d]), ...(w === 2024 ? ds.map(d => r.sd ? r.sd[d] : null) : []), r.n])];
  download(`party-positions-${w}-${S.scope}.csv`, new Blob(["\ufeff" + ChartExport.csvCite(PAGE) + lines.map(l => l.map(q).join(",")).join("\n")], {type: "text/csv;charset=utf-8"}));
}
async function png() {
  const id = {map: "map", populism: "pop", systems: "systime", time: "ts-" + TIME_DIMS[0], voters: "vt"}[S.view];
  const svg = id && $(id);
  if (!svg || !svg.viewBox.baseVal || !svg.viewBox.baseVal.width) { toast("Nothing to download in this view"); return; }
  const W = svg.viewBox.baseVal.width, H = svg.viewBox.baseVal.height;
  const c = await ChartExport.clone(svg);   // ../chart-export.js: labels stay in Jost
  c.setAttribute("xmlns", "http://www.w3.org/2000/svg"); ChartExport.setSize(c, W, H);
  await ChartExport.fonts();
  const title = {map: `${DIM.get(S.x).label} and ${DIM.get(S.y).label}, ${S.wave}`, populism: `Anti-elite rhetoric and PopuList, ${S.wave}`,
    systems: MEASURE[S.measure].label, time: `Polish parties: ${DIM.get(TIME_DIMS[0]).label}`,
    voters: `Polish parties and their voters: ${DIM.get(S.vdim).label}, ${S.wave}`}[S.view];
  const leg = S.view === "map" && S.colour === "family" ? FAM_GROUPS : (S.view === "map" && S.colour === "none") || ["systems", "time", "voters"].includes(S.view) ? [] : POP_CATS;
  const img = new Image(), top = 44, foot = leg.length ? 58 : 38, sc = ChartExport.scale(W + 20, H + top + foot);
  img.onload = () => {
    const cv = document.createElement("canvas"); cv.width = (W + 20) * sc; cv.height = (H + top + foot) * sc;
    const g = cv.getContext("2d"); g.scale(sc, sc); g.fillStyle = "#fff"; g.fillRect(0, 0, W + 20, H + top + foot);
    g.fillStyle = ACCENT; g.font = ChartExport.TITLE; g.fillText(title, 10, 28);
    g.drawImage(img, 10, top, W, H);
    g.font = "11.5px Jost, Helvetica, Arial, sans-serif";
    let lx = 10;
    for (const c of leg) {
      g.fillStyle = c.col; g.beginPath(); g.arc(lx + 5, H + top + 11, 5, 0, 2 * Math.PI); g.fill();
      g.fillStyle = SOFT; g.fillText(c.label, lx + 14, H + top + 15); lx += 24 + g.measureText(c.label).width;
    }
    g.fillStyle = MUTED; g.font = "11px Jost, Helvetica, Arial, sans-serif";
    const src = S.view === "voters" ? "Parties: Chapel Hill Expert Survey. Voters: Social and political trends, benstanley.eu." : "Chapel Hill Expert Survey 1999–2024; The PopuList 4.0. Polish parties in their own colours.";
    g.fillText(src, 10, H + top + (leg.length ? 35 : 15));
    ChartExport.drawCite(g, PAGE, 10, H + top + (leg.length ? 51 : 31), W);
    cv.toBlob(b => download(`party-ideology-${S.view}-${S.wave}.png`, b), "image/png");
  };
  img.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(c));
}

// ---- rendering and events -------------------------------------------------------------------
function renderView() {
  if (S.view === "map") renderMap();
  else if (S.view === "profile") renderProfile();
  else if (S.view === "populism") renderPopulism();
  else if (S.view === "time") renderTime();
  else if (S.view === "voters") renderVoters();
  else renderSystems();
}
// Until a party has been chosen, the party shown follows the wave
function setWave(w) { S.wave = w; if (!S.selUser) S.sel = defaultSel(w); }
function render() { renderControls(); renderView(); renderPanel(); saveHash(); }
function select(id) {
  if (!P.has(id)) return;
  S.sel = id; S.selUser = true; S.allEvents = false;
  const p = P.get(id);
  if (!p.byW.has(S.wave)) {
    const ws = [...p.byW.keys()].filter(w => WAVES.includes(w));
    if (ws.length) { const near = ws.reduce((a, b) => Math.abs(b - S.wave) < Math.abs(a - S.wave) ? b : a); toast(`${nameOf(p)} was not rated in ${S.wave}; showing ${near}`); S.wave = near; }
  }
  if (!inScope(p) && S.view !== "systems") toast(`${nameOf(p)} is outside the parties compared, so it is drawn on its own`);
  render();
}
function hoverChart(svgId, boxId, tipId, html) {
  const svg = $(svgId), box = $(boxId), tip = $(tipId);
  svg.addEventListener("mousemove", e => {
    const t = e.target.closest("[data-k]");
    const h = t && html(t.dataset.k);
    if (!h) { tip.hidden = true; return; }
    tip.innerHTML = h; placeTip(tip, box, e);
  });
  svg.addEventListener("mouseleave", () => { tip.hidden = true; });
}
function wire() {
  $("views").addEventListener("click", e => { const b = e.target.closest("button"); if (!b) return; S.view = b.dataset.v; render(); });
  $("waves").addEventListener("click", e => { const b = e.target.closest("button"); if (!b) return; setWave(+b.dataset.w); render(); });
  // The comparisons toggle; with none on, only the Polish parties are shown. Whiskers go with the Polish parties alone.
  $("scopes").addEventListener("click", e => { const b = e.target.closest("button"); if (!b) return; S.scope = S.scope === b.dataset.s ? "pl" : b.dataset.s; S.sd = false; render(); });
  $("sdbtn").addEventListener("click", () => { S.sd = !S.sd; if (S.sd) S.scope = "pl"; render(); });
  $("colourby").addEventListener("click", e => { const b = e.target.closest("button"); if (!b) return; S.colour = b.dataset.c; render(); });
  $("groups").addEventListener("click", e => { const b = e.target.closest("button"); if (!b) return; S.group = b.dataset.g; render(); });
  $("measures").addEventListener("click", e => { const b = e.target.closest("button"); if (!b) return; S.measure = b.dataset.m; render(); });
  $("vdims").addEventListener("change", e => { S.vdim = e.target.value; render(); });
  $("xdim").addEventListener("change", e => { S.x = e.target.value; render(); });
  $("ydim").addEventListener("change", e => { S.y = e.target.value; render(); });
  $("popy").addEventListener("change", e => { S.popY = e.target.value; render(); });
  $("trails").addEventListener("change", e => { S.trails = e.target.checked; render(); });
  for (const id of ["fitlines", "popfit"]) $(id).addEventListener("change", e => { S.fit = e.target.checked; render(); });
  $("mapvoters").addEventListener("change", e => { S.voters = e.target.checked; render(); });
  $("prev").addEventListener("change", e => { S.prev = e.target.checked; render(); });
  const rowHtml = dims => k => { const [kind, v] = k.split(":"); return kind === "r" ? rowTip(ROWS[+v], dims()) : kind === "v" || kind === "va" ? mapVoterTip(kind, +v) : null; };
  hoverChart("map", "mapbox", "maptip", rowHtml(() => [S.x, S.y]));
  hoverChart("pop", "popbox", "poptip", rowHtml(() => ["antielite_salience", S.popY]));
  const sysTip = k => {
    const [kind, v] = k.split(":"), c = +v, key = S.measure;
    const pts = D.system.filter(s => s.c === c && isNum(s[key])).sort((a, b) => a.w - b.w);
    return `<b>${esc(CTRY.get(c).name)}</b>` + pts.map(s => `<div class="r"><span${s.w === S.wave ? ' style="color:#8B0000"' : ""}>${s.w}</span><span>${s[key].toFixed(2)}</span></div>`).join("") +
      `<div class="s">${c === POLAND ? "Weighted by list vote shares" : "Weighted by CHES vote shares"}</div>`;
  };
  hoverChart("vt", "vbox", "vtip", votersTip);
  $("vt").addEventListener("click", e => { const t = e.target.closest("[data-k]"); if (!t) return; const x = VROWS[+t.dataset.k.split(":")[1]]; if (x) select(x.p.id); });
  hoverChart("systime", "systimebox", "systimetip", sysTip);
  hoverChart("sysrank", "sysrankbox", "sysranktip", sysTip);
  const clickRow = e => { const t = e.target.closest("[data-k]"); if (!t) return; const [kind, v] = t.dataset.k.split(":"); if (kind === "r") select(ROWS[+v].p.id); else if (kind === "v" && MAPV[+v]) select(MAPV[+v].r.p.id); };
  $("map").addEventListener("click", clickRow);
  $("pop").addEventListener("click", clickRow);
  document.addEventListener("click", e => {
    const g = e.target.closest("[data-goto]");
    if (g) { setWave(+g.dataset.goto); render(); return; }
    const pb = e.target.closest("[data-p]");
    if (pb && (pb.closest("#panel") || pb.closest("#popcards") || pb.closest("#timelegend"))) { select(+pb.dataset.p); return; }
    if (e.target.id === "allev") { S.allEvents = !S.allEvents; renderPanel(); return; }
    if (e.target.id === "copylink") { const u = shareUrl(); (navigator.clipboard ? navigator.clipboard.writeText(u) : Promise.reject()).then(() => toast("Link copied"), () => window.prompt("Copy this link:", u)); return; }
    if (e.target.id === "csvbtn") { csv(); return; }
    if (e.target.id === "pngbtn") { png(); return; }
    if (!e.target.closest(".search")) { const r = $("results"); if (r) r.hidden = true; }
  });
  window.addEventListener("resize", debounce(renderView, 150));
  try { hashTarget().addEventListener("hashchange", () => { restoreHash(); render(); if (S.ptMissing) { toast("That party has no CHES ratings"); S.ptMissing = null; } }); } catch (e) { /* ignore */ }
}

async function init() {
  try {
    const res = await fetch("ideology.json", {cache: "no-cache"});
    if (!res.ok) throw new Error(res.status);
    D = await res.json();
  } catch (e) {
    $("asof").textContent = "The party data could not be loaded.";
    return;
  }
  prep();
  S.wave = WAVES[WAVES.length - 1]; S.sel = defaultSel(S.wave);
  restoreHash();
  $("asof").innerHTML = `Chapel Hill Expert Survey, ${WAVES[0]}–${WAVES[WAVES.length - 1]} (${WAVES.length} waves; ${D.parties.length} parties in ${D.countries.length} countries), and The PopuList 4.0 (elections to ${esc(D.meta.populistTo)}). Every position is the mean rating of the party's leadership by country experts.`;
  wire();
  render();
  if (S.ptMissing) toast("That party has no CHES ratings, so PiS is shown");
  fetch("../polling-tracker/polls.json").then(r => r.ok ? r.json() : null).then(j => { if (j && j.rules && j.weeks) { POLL = j; renderPanel(); } }).catch(() => {});
}
init();
