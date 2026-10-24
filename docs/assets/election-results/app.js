/* Election results — every presidential, Sejm, European Parliament and referendum vote in Poland
 * since 1990: the facts and results of each, and maps down to gmina level.
 *
 * Reads elections.json (the index), geo.json (pre-projected SVG paths: today's gminas, each set of
 * constituencies) and one <election-id>.json per election, loaded when it is
 * chosen; all written by "Election results/analysis/90_export.R" in the Polish political analyses
 * repo. Unit counts are integer arrays aligned with geo.json's order. Lists and candidates under
 * 0.5% nationally with no seat are summed as "other" in the unit counts (but listed in the results
 * table). Gminas that a 1990s unit covered jointly share its counts, and the tooltip says so.
 *
 * State lives in the URL hash of the parent page, as in the other apps. Plain SVG; no libraries.
 */
"use strict";

// ---- utilities ------------------------------------------------------------------------------
const $ = id => document.getElementById(id);
const esc = s => String(s == null ? "" : s).replace(/[&<>"']/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
const MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
const DAYS = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
const dparts = s => { const [y, m, d] = s.split("-").map(Number); return new Date(Date.UTC(y, m - 1, d)); };
const dlong = s => { const t = dparts(s); return t.getUTCDate() + " " + MONTHS[t.getUTCMonth()] + " " + t.getUTCFullYear(); };
const dday = s => DAYS[dparts(s).getUTCDay()] + " " + dlong(s);
const clamp = (v, a, b) => Math.max(a, Math.min(b, v));
const sum = a => a.reduce((x, y) => x + (y || 0), 0);
const isNum = v => v != null && typeof v === "number" && isFinite(v);
const NA = "–";
const n0 = v => isNum(v) ? Math.round(v).toLocaleString("en-GB") : NA;
const pct = (v, dp = 1) => isNum(v) ? (100 * v).toFixed(dp) + "%" : NA;
const signedPts = (v, dp = 1) => { if (!isNum(v)) return NA; const r = +(100 * v).toFixed(dp); return (r > 0 ? "+" : r < 0 ? "−" : "±") + Math.abs(r).toFixed(dp); };
const signedInt = v => !isNum(v) ? NA : (v > 0 ? "+" : v < 0 ? "−" : "±") + Math.abs(v);
function debounce(fn, ms) { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => fn(...a), ms); }; }
function toast(msg) { const t = $("toast"); t.textContent = msg; t.classList.add("on"); clearTimeout(toast.t); toast.t = setTimeout(() => t.classList.remove("on"), 1800); }
function niceStep(range, n) {
  const raw = range / n, p = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / p;
  return (f < 1.5 ? 1 : f < 3 ? 2 : f < 7 ? 5 : 10) * p;
}
function ticks(lo, hi, n = 5) {
  const st = niceStep(hi - lo || 1, n), out = [];
  for (let v = Math.ceil(lo / st - 1e-9) * st; v <= hi + 1e-9; v += st) out.push(+v.toFixed(10));
  return out;
}
function quantile(vals, p) {
  const v = vals.filter(isNum).sort((a, b) => a - b);
  if (!v.length) return null;
  const h = (v.length - 1) * p, lo = Math.floor(h);
  return v[lo] + (h - lo) * ((v[lo + 1] ?? v[lo]) - v[lo]);
}
function hexRgb(hex) { const n = parseInt(hex.slice(1), 16); return [n >> 16, (n >> 8) & 255, n & 255]; }
function rgbHex(c) { return "#" + c.map(x => Math.round(clamp(x, 0, 255)).toString(16).padStart(2, "0")).join(""); }
function mix(hex, t) { // towards white; t = 1 is the full colour
  const c = hexRgb(hex); return rgbHex(c.map(v => 255 - (255 - v) * t));
}
function ramp(stops, t) { // stops: hex colours spaced evenly over 0–1
  t = clamp(t, 0, 1);
  const k = Math.min(stops.length - 2, Math.floor(t * (stops.length - 1))), f = t * (stops.length - 1) - k;
  const a = hexRgb(stops[k]), b = hexRgb(stops[k + 1]);
  return rgbHex(a.map((v, i) => v + f * (b[i] - v)));
}
const ACCENT = "#8B0000", INK = "#141414", MUTED = "#767676", NODATA = "#ECECEC";
const PAGE = "Election results";   // the page's name in its suggested citation (../chart-export.js)
const FONT = "Jost, 'Avenir Next', -apple-system, sans-serif";
// Not party colours: turnout runs through viridis, and referendum answers take the teal and plum
// the repo uses for series that are not parties
const TURNOUT = ["#440154", "#3b528b", "#21918c", "#5ec962", "#fde725"];
const ANSWER = { yes: "#00796b", no: "#9c2d74" };
const TYPES = { sejm: "Sejm", presidential: "Presidential", ep: "European Parliament", referendum: "Referendum", polls: "Polls" };
const TYPE_SHORT = { sejm: "Sejm", presidential: "President", ep: "EP", referendum: "Referendum", polls: "Polls now" };
const PAGES = "../../pages/polish-political-analyses/";
// Change maps: gains in the party's colour, losses in a dark olive that no party uses; turnout
// gains in viridis's teal
const LOSS = "#5f5b0b", TURN_GAIN = "#21918c";
const TYPECOL = { sejm: "#8B0000", presidential: "#495057", ep: "#00796b", referendum: "#9c2d74" };
const ZONES = [["russian", "Russian Empire"], ["german", "German Empire"], ["austrian", "Austria-Hungary"]];

// ---- data -----------------------------------------------------------------------------------
let E, G, CUR = null, EL = null;   // index, geometry, current election file, its index entry
const CACHE = new Map();
const S = { type: "all", e: null, p: 0, m: null, i: 0, j: 1, l: "g", b: "o", v: "m", sel: null,
  cc: null, ck: null, cx: null, z: false, pc: null, cv: "higher_ed", zc: null };   // change: compared contest, mode, item; zabory; profile camp; scatter colour
let ELECTIONS = [], CONTEST = {}, OWN_ALL = {};
// "Polls now": the Polling tracker's latest estimate (../polling-tracker/polls.json), shown as an
// election of its own: national shares and the model's median seats, with each party's share spread
// across today's 41 constituencies by its regional profile and the seats allocated there, as the
// tracker's own constituency map does
let POLLS = null, POLLS_EL = null;

function prep() {
  ELECTIONS = [...E.elections].sort((a, b) => a.date < b.date ? -1 : a.date > b.date ? 1 : (a.type < b.type ? -1 : 1));
  CONTEST = Object.fromEntries(E.contests.map(c => [c.id, c]));
  // Two finalists in the same colour (Wałęsa and Tymiński in 1990 are both grey) each take one of
  // the colours for series that are not parties, throughout that election (Ben's decision): the
  // winner teal, the runner-up plum
  OWN_ALL = {};
  E.contests.forEach(c => {
    if (elOf(c.election).type !== "presidential" || c.part !== 2 || c.item_ids.length < 2) return;
    if (E.labels[c.item_camps[0]] === E.labels[c.item_camps[1]]) OWN_ALL[c.election] = { [c.item_ids[0]]: ANSWER.yes, [c.item_ids[1]]: ANSWER.no };
  });
}
const campCol = (camp, election, itemId) => (OWN_ALL[election] || {})[itemId] || ANSWER[camp] || E.labels[camp] || E.labels.Other;
const elOf = id => id === "polls" ? POLLS_EL : E.elections.find(e => e.id === id);
const isPolls = () => !!EL && EL.type === "polls";
const partOf = () => CUR && !CUR.facts.facts_only ? CUR.facts.parts[S.p] : null;
const resOf = () => CUR.results[S.p].items;
const unitsOf = () => CUR.units[S.p];
const itemById = id => resOf().find(r => r.id === id);
// The items the map can show: those stored separately in the unit counts
const mapItems = () => {
  const U = unitsOf(), its = (U.gmina || U.okreg41).items.filter(id => id !== "other").map(id => itemById(id)).filter(Boolean);
  return EL.type === "referendum" ? its.sort((a, b) => (a.id === "yes" ? -1 : 1) - (b.id === "yes" ? -1 : 1)) : its;
};
// Any other two items drawn against each other in the same colour take the same two colours
function pairCols(a, b) {
  const ca = colOf(a), cb = colOf(b);
  return ca === cb ? [ANSWER.yes, ANSWER.no, true] : [ca, cb, false];
}
function colOf(item) {
  if (!item) return "#9E9E9E";
  if (EL.type === "referendum") return ANSWER[item.id] || "#9E9E9E";
  const own = OWN_ALL[EL.id];
  if (own && own[item.id]) return own[item.id];
  return item.colour || E.labels[item.label] || E.labels.Other;
}
function nameOf(item, long = false) {
  if (!item) return "";
  if (EL.type === "referendum") return item.id === "yes" ? "Yes" : "No";
  if (EL.type === "presidential") return item.short || item.name;
  return long ? item.name : (item.short || item.name);
}
const itemLabel = () => ({ sejm: "List", ep: "List", presidential: "Candidate", referendum: "Answer", polls: "Party" }[EL.type]);
const dayIso = n => new Date(n * 864e5).toISOString().slice(0, 10);
function pollsEntry() {
  const wk = POLLS.weeks[POLLS.weeks.length - 1], d = dayIso(wk.date);
  return { id: "polls", type: "polls", year: +d.slice(0, 4), date: d, name_en: "The polls now", name_pl: "Pooling the Poles: the Polling tracker's latest estimate",
    early: false, facts_only: false, set: "sejm2001", parts: [{ id: "", contest_id: "polls", set: "sejm2001", date: d }], overview: {} };
}
function buildPolls() {
  const D = POLLS, R = D.rules, wk = D.weeks[D.weeks.length - 1], C = D.constituencies, SP = R.seatParties;
  const share = (p, k) => p === "MN" ? R.mnNational2023 / 100 : isNum(wk.vote[k]) ? wk.vote[k] / 100 : 0;
  const coef = (p, c) => p === "MN" ? (c.okreg === R.mnOkreg ? 100 * R.mnOpoleShare / R.mnNational2023 : 0) : c[R.coef[p]];
  const col = p => E.palette[p] || E.palette.Other;
  const items = SP.map((p, k) => ({ id: p, name: p, short: p, label: p, colour: col(p), pct: share(p, k),
    lo: isNum(wk.voteLo[k]) ? wk.voteLo[k] / 100 : null, hi: isNum(wk.voteHi[k]) ? wk.voteHi[k] / 100 : null,
    seats: wk.seats[k] || 0, seats_lo: wk.seatsLo[k], seats_hi: wk.seatsHi[k], p_thr: wk.pThr[k], votes: null }))
    .filter(it => it.pct > 0).sort((a, b) => b.pct - a.pct);
  const set = G.sets.sejm2001, cs = set.okreg.map(o => C.find(c => c.okreg === o));
  const votes = items.map(it => cs.map(c => Math.round(c.validvotes * coef(it.id, c) * it.pct)));
  const valid = cs.map(c => c.validvotes), none = valid.map(() => null);
  const A = { items: items.map(it => it.id), electorate: none, voters: none, valid, invalid: none, votes, set: "sejm2001" };
  // A single D'Hondt allocation at the median shares, with the 5% threshold (the German minority exempt)
  const rows = [];
  cs.forEach((c, u) => {
    const active = items.map((it, k) => it.id === "MN" || it.pct >= R.thresholdParty ? votes[k][u] : 0), got = items.map(() => 0);
    for (let n = 0; n < c.magnitude; n++) {
      let best = -1, bq = 0;
      active.forEach((v, k) => { const q = v / (got[k] + 1); if (v > 0 && q > bq) { bq = q; best = k; } });
      if (best < 0) break;
      got[best]++;
    }
    got.forEach((g, k) => { if (g > 0) rows.push({ item: items[k].id, okreg: c.okreg, seats: g }); });
  });
  return {
    facts: { id: "polls", type: "polls", year: POLLS_EL.year, name_en: "The polls now", name_pl: POLLS_EL.name_pl, date: POLLS_EL.date, early: false, facts_only: false,
      rules: { seats: 460, constituencies: 41, formula: "D'Hondt in each constituency", threshold_list: "5%", threshold_coalition: "8%" },
      sources: [{ label: "Pooling the Poles: the Polling tracker", url: PAGES + "polling-tracker.html" }],
      parts: [{ id: "", contest_id: "polls", date: POLLS_EL.date, totals: {}, voivodeships: null }],
      seats_by_okreg: { set: "sejm2001", rows, national_list: [] },
      polls: { n: D.meta.nPolls, pollsters: D.meta.pollsterCount, run: D.meta.runTime, last: dayIso(D.meta.lastFieldwork) } },
    results: [{ part: "polls", items }],
    units: [{ part: "polls", turnout_basis: null, gmina: null, okreg: A, okreg41: A }]
  };
}

// ---- chart plumbing -------------------------------------------------------------------------
function placeTip(tip, box, ev, html) {
  tip.innerHTML = html; tip.hidden = false;
  const br = box.getBoundingClientRect();
  let lx = ev.clientX - br.left + 14, ly = ev.clientY - br.top + 14;
  const tw = tip.offsetWidth, th = tip.offsetHeight;
  if (lx + tw > br.width - 4) lx = ev.clientX - br.left - tw - 14;
  if (lx < 4) lx = 4;
  if (ly + th > br.height - 4) ly = Math.max(4, ev.clientY - br.top - th - 14);
  tip.style.left = lx + "px"; tip.style.top = ly + "px";
}
const tipRow = (col, name, value, sub) => `<div class="r"><span>${col ? `<span class="dot" style="background:${col}"></span>` : ""}${esc(name)}</span><span>${value}${sub ? ` <span class="sub">${sub}</span>` : ""}</span></div>`;
const boxW = id => Math.max(280, $(id).clientWidth - 12);

// ---- strip, parts, hash ---------------------------------------------------------------------
function renderStrip() {
  const counts = { all: ELECTIONS.length };
  ELECTIONS.forEach(e => { counts[e.type] = (counts[e.type] || 0) + 1; });
  $("types").innerHTML = ["all", "sejm", "presidential", "ep", "referendum"].map(t =>
    `<button type="button" data-t="${t}" aria-pressed="${S.type === t}">${t === "all" ? "All" : `<span class="tdot ${t}"></span>${TYPES[t]}`}</button>`).join("");
  const shown = ELECTIONS.filter(e => S.type === "all" || e.type === S.type);
  $("elections").innerHTML = shown.map(e =>
    `<button type="button" data-e="${e.id}" aria-pressed="${e.id === S.e}" title="${esc(e.name_en)}, ${e.date2 && e.type === "presidential" ? dlong(e.date) + " and " + dlong(e.date2) : dlong(e.date)}${e.early ? " (early)" : ""}"><span class="tdot ${e.type}"></span>${e.year}${S.type === "all" ? ` ${TYPE_SHORT[e.type]}` : ""}</button>`).join("") +
    (POLLS_EL && (S.type === "all" || S.type === "sejm") ? `<button type="button" data-e="polls" class="polls" aria-pressed="${S.e === "polls"}" title="The Polling tracker's estimate for ${dlong(POLLS_EL.date)}">Polls now</button>` : "");
  const b = $("elections").querySelector('[aria-pressed="true"]');
  if (b) { const seg = $("elections"); seg.scrollLeft = Math.max(0, b.offsetLeft - seg.clientWidth / 2); }
  const parts = EL && !EL.facts_only ? EL.parts : [];
  $("parts").hidden = parts.length < 2;
  $("parts").innerHTML = parts.length < 2 ? "" : parts.map((p, k) =>
    `<button type="button" data-p="${k}" aria-pressed="${k === S.p}">${EL.type === "presidential" ? "Round " + (k + 1) : "Question " + (k + 1)}</button>`).join("");
}
function hashTarget() { try { if (window.top !== window && window.top.location.host === location.host) return window.top; } catch (e) { /* cross-origin */ } return window; }
function stateString() {
  const q = new URLSearchParams();
  const last = ELECTIONS[ELECTIONS.length - 1].id;
  if (S.e !== last) q.set("e", S.e);
  if (S.p) q.set("p", S.p + 1);
  if (S.m && S.m !== defaultMeasure()) q.set("m", S.m);
  const its = partOf() ? mapItems() : [];
  if (S.i && its[S.i]) q.set("it", its[S.i].id);
  if (S.m === "h2h" && S.j !== 1 && its[S.j]) q.set("jt", its[S.j].id);
  if (S.l !== "g") q.set("l", S.l);
  if (S.b !== "o") q.set("b", S.b);
  if (S.v !== "m") q.set("v", S.v);
  if (measure() === "chg" && partOf()) {
    if (S.cc && S.cc !== defaultCompare()) q.set("c", S.cc);
    if (S.ck) q.set("k", S.ck);
    if (S.cx) q.set("x", S.cx);
  }
  if (S.z) q.set("z", 1);
  if (S.cv !== "higher_ed") q.set("cv", S.cv);
  if (S.zc) q.set("zc", S.zc);
  if (S.sel && MAPV && S.sel.l === S.l + S.b + S.v) q.set("u", MAPV.L.kind === "g" ? G.gminas.teryt[S.sel.u] : "c" + G.sets[MAPV.L.set].okreg[S.sel.u]);
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
  S.e = q.get("e") === "polls" ? (POLLS_EL ? "polls" : ELECTIONS[ELECTIONS.length - 1].id) : elOf(q.get("e")) ? q.get("e") : ELECTIONS[ELECTIONS.length - 1].id;
  S.p = Math.max(0, (+q.get("p") || 1) - 1);
  S.m = ["share", "leader", "h2h", "turnout", "chg"].includes(q.get("m")) ? q.get("m") : null;
  S.i = Math.max(0, +q.get("i") || 0);
  S.j = q.has("j") ? Math.max(0, +q.get("j")) : 1;
  // links name a list or candidate by id (it, jt) and a gmina (TERYT) or one of today's 41
  // constituencies (c<number>) by u; they are applied once the election has loaded
  S.it = q.get("it"); S.jt = q.get("jt"); S.u = q.get("u");
  if (["g", "c"].includes(q.get("l"))) S.l = q.get("l");
  if (["o", "t"].includes(q.get("b"))) S.b = q.get("b");
  if (["m", "c"].includes(q.get("v"))) S.v = q.get("v");
  S.cc = CONTEST[q.get("c")] ? q.get("c") : null;
  S.ck = ["lineage", "same", "camp"].includes(q.get("k")) ? q.get("k") : null;
  S.cx = q.get("x");
  S.z = q.get("z") === "1";
  S.cv = q.get("cv") || "higher_ed";
  S.zc = ["list", "1914", "1937"].includes(q.get("zc")) ? q.get("zc") : null;
}
function shareUrl() { const t = hashTarget(), s = stateString(); return t.location.origin + t.location.pathname + (s ? "#" + s : ""); }

// ---- loading an election --------------------------------------------------------------------
async function loadElection(id) {
  if (!CACHE.has(id)) {
    const r = await fetch(elOf(id).file);
    if (!r.ok) throw new Error(`${id}: ${r.status}`);
    CACHE.set(id, await r.json());
  }
  return CACHE.get(id);
}
function applyLinks() {
  const its = mapItems();
  if (S.it) { const k = its.findIndex(it => it.id === S.it); if (k >= 0) { S.i = k; if (!S.m || S.m === "leader") S.m = "share"; } }
  if (S.jt) { const k = its.findIndex(it => it.id === S.jt); if (k >= 0) S.j = k; }
  if (S.u) {
    if (/^c\d+$/.test(S.u)) {
      S.l = "c"; if (!isPolls() && unitsOf().okreg.set !== "sejm2001") S.b = "t";
      const k = G.sets.sejm2001.okreg.indexOf(+S.u.slice(1));
      if (k >= 0) S.sel = { u: k, l: S.l + S.b + S.v };
    } else {
      const k = G.gminas.teryt.indexOf(S.u);
      if (k >= 0) { S.l = "g"; S.sel = { u: k, l: S.l + S.b + S.v }; }
    }
  }
  S.it = S.jt = S.u = null;
}
async function setElection(id, keepPart = false) {
  S.e = id; EL = elOf(id);
  if (id === "polls") CUR = CACHE.get("polls") || (CACHE.set("polls", buildPolls()), CACHE.get("polls"));
  else {
    if (!CACHE.has(id)) toast("Loading the election…");
    try { CUR = await loadElection(id); } catch (err) { console.error(err); toast("Could not load that election"); return; }
  }
  if (!keepPart) { S.p = 0; S.m = null; S.i = 0; S.j = 1; S.sel = null; S.cc = null; S.ck = null; S.cx = null; }
  if (isPolls()) { if (!["leader", "share", "h2h"].includes(S.m)) S.m = null; S.l = "c"; }
  if (!CUR.facts.facts_only) {
    S.p = clamp(S.p, 0, CUR.facts.parts.length - 1);
    applyLinks();
    const n = mapItems().length;
    S.i = clamp(S.i, 0, Math.max(0, n - 1)); S.j = clamp(S.j, 0, Math.max(0, n - 1));
    if (S.j === S.i) S.j = S.i === 0 ? 1 : 0;
  }
  renderAll();
}
function defaultMeasure() {
  if (!EL || EL.facts_only) return "leader";
  if (EL.type === "referendum") return "h2h";
  if (EL.type === "presidential" && unitsOf().gmina.items.filter(i => i !== "other").length === 2) return "h2h";
  return "leader";
}
const measure = () => S.m || defaultMeasure();

// ---- at a glance ----------------------------------------------------------------------------
function renderGlance() {
  const f = CUR.facts;
  $("glancetype").textContent = TYPES[EL.type] + (EL.early ? " · early" : "");
  $("h-glance").textContent = isPolls() ? f.name_en : `${f.name_en}, ${f.year}`;
  $("glancesub").textContent = f.name_pl;
  renderFacts();
  const box = $("glancechart");
  $("glancenote").textContent = "";
  if (f.facts_only) { $("glancetitle").textContent = "Seats in the Sejm"; box.innerHTML = seats1989(); return; }
  if (isPolls()) { $("glancetitle").textContent = "Seats in the Sejm: the tracker's projection"; box.innerHTML = hemicycle(); }
  else if (EL.type === "sejm" || EL.type === "ep") { $("glancetitle").textContent = EL.type === "sejm" ? "Seats in the Sejm" : "Seats in the European Parliament"; box.innerHTML = hemicycle(); }
  else if (EL.type === "presidential") { $("glancetitle").textContent = CUR.facts.parts.length > 1 ? "Both rounds" : "First round"; box.innerHTML = presBars(); }
  else { $("glancetitle").textContent = CUR.facts.parts.length > 1 ? "Every question" : "The result"; box.innerHTML = refBars(); }
}

function hemiLayout(n, rows) {
  if (n <= 0) return [];
  const radii = Array.from({ length: rows }, (_, i) => 1 + i / (rows - 1));
  const rs = sum(radii), counts = radii.map(r => Math.floor(n * r / rs));
  let rem = n - sum(counts);
  const order = radii.map((r, i) => i).sort((p, q) => radii[q] - radii[p]);
  for (let k = 0; rem > 0; k++, rem--) counts[order[k % rows]]++;
  const pts = [];
  counts.forEach((nc, ri) => {
    for (let j = 0; j < nc; j++) {
      const ang = nc === 1 ? Math.PI / 2 : Math.PI - j * Math.PI / (nc - 1);
      pts.push({ x: radii[ri] * Math.cos(ang), y: radii[ri] * Math.sin(ang), ang, r: radii[ri] });
    }
  });
  pts.sort((p, q) => q.ang - p.ang || p.r - q.r);
  return pts;
}
// Seats from left to right by the palette's camps (R/party_colours.R), then by votes
function seatOrder(items) {
  const camp = it => E.camp_order.indexOf(E.label_camps[it.label] || E.palette_camps[it.label] || "Other");
  return items.filter(it => it.seats > 0).sort((a, b) => camp(a) - camp(b) || (b.votes || b.pct) - (a.votes || a.pct));
}
function hemicycle() {
  const items = seatOrder(resOf()), total = sum(items.map(i => i.seats));
  const rows = total > 200 ? 11 : total > 80 ? 7 : 5, pts = hemiLayout(total, rows);
  const rad = Math.min(0.42 / (rows - 1), 0.046 * Math.sqrt(460 / Math.max(total, 1)));
  const o = [];
  let k = 0;
  items.forEach(it => { for (let j = 0; j < it.seats; j++, k++) { const p = pts[k]; o.push(`<circle cx="${p.x.toFixed(3)}" cy="${(-p.y).toFixed(3)}" r="${rad.toFixed(3)}" fill="${colOf(it)}" data-id="${esc(it.id)}"/>`); } });
  const maj = Math.floor(((CUR.facts.rules && CUR.facts.rules.seats) || total) / 2) + 1, top = items.slice().sort((a, b) => b.seats - a.seats)[0];
  if (EL.type === "sejm" || isPolls()) o.push(`<line x1="0" x2="0" y1="-0.86" y2="-2.12" stroke="#141414" stroke-width="0.012" stroke-dasharray="0.04 0.04"/><text x="0" y="-2.16" text-anchor="middle" font-size="0.085" fill="#4B4B4B">${maj}</text>`);
  o.push(`<text x="0" y="-0.36" text-anchor="middle" font-size="0.2" fill="#141414">${total} seats</text>`);
  if (top) o.push(`<text x="0" y="-0.14" text-anchor="middle" font-size="0.085" fill="#767676">largest: ${esc(nameOf(top))}, ${top.seats}${EL.type === "sejm" || isPolls() ? (top.seats >= maj ? " (a majority)" : " (no majority)") : ""}</text>`);
  const legend = items.map(it => `<span><span class="dot" style="background:${colOf(it)}"></span>${esc(nameOf(it))} <b>${it.seats}</b></span>`).join("");
  $("glancenote").textContent = isPolls() ? `The model's median seats (${total} in all: medians need not add up to 460), from left to right by camp. Hover over the table below for the 80% ranges.`
    : EL.type === "sejm" ? "Seats from left to right by camp (as in the site's party colours), then by votes." : "Seats from left to right by camp, then by votes.";
  return `<svg id="glancesvg" viewBox="-2.2 -2.3 4.4 2.38" role="img" aria-label="Seats won by each list" font-family="${FONT}">${o.join("")}</svg><div class="legend" style="margin:4px 8px 4px">${legend}</div>`;
}
function seats1989() {
  const rows = CUR.facts.seats || [];
  return `<table class="t"><thead><tr><th class="l">Grouping</th><th>Seats</th></tr></thead><tbody>${rows.map(r => `<tr><td class="name">${esc(r.name)}${r.note ? `<span class="full">${esc(r.note)}</span>` : ""}</td><td>${r.seats ?? NA}</td></tr>`).join("")}</tbody></table>`;
}
function barSvg(rows, W, opts = {}) { // rows: {name, sub, v, col, bold}
  const lw = clamp(W * 0.34, 110, 210), rh = 24, H = rows.length * rh + 6, vw = 54;
  const X = v => lw + v * (W - lw - vw - 8) / (opts.max || 1);
  const o = rows.map((r, k) => {
    const y = 4 + k * rh;
    return `<text x="${lw - 8}" y="${y + 15}" text-anchor="end" font-size="12.5" fill="#141414"${r.bold ? ' font-weight="500"' : ""}>${esc(r.name)}</text>
      <rect x="${lw}" y="${y + 5}" width="${Math.max(0, X(r.v) - lw).toFixed(1)}" height="13" rx="3" fill="${r.col}"/>
      <text x="${(X(r.v) + 6).toFixed(1)}" y="${y + 15}" font-size="12.5" fill="#141414" font-weight="500">${pct(r.v)}</text>`;
  });
  return `<svg viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" font-family="${FONT}">${o.join("")}</svg>`;
}
// Two shares side by side in one bar, with names above (round 2, or yes against no)
function h2hSvg(a, b, W) { // a, b: {name, v, col}
  const x0 = 8, w = W - 16, wa = w * a.v / (a.v + b.v), H = 52;
  return `<svg viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" font-family="${FONT}">
    <text x="${x0}" y="12" font-size="12.5" fill="#141414" font-weight="500">${esc(a.name)}</text>
    <text x="${x0 + w}" y="12" text-anchor="end" font-size="12.5" fill="#141414" font-weight="500">${esc(b.name)}</text>
    <rect x="${x0}" y="18" width="${wa.toFixed(1)}" height="30" fill="${a.col}"/><rect x="${(x0 + wa).toFixed(1)}" y="18" width="${(w - wa).toFixed(1)}" height="30" fill="${b.col}"/>
    <line x1="${x0 + w / 2}" x2="${x0 + w / 2}" y1="15" y2="51" stroke="#141414" stroke-width="1" stroke-dasharray="2 2"/>
    <text x="${x0 + 10}" y="38" font-size="14" fill="#fff" font-weight="600">${pct(a.v / (a.v + b.v), 2)}</text>
    <text x="${x0 + w - 10}" y="38" text-anchor="end" font-size="14" fill="#fff" font-weight="600">${pct(b.v / (a.v + b.v), 2)}</text></svg>`;
}
function presBars() {
  const W = boxW("glancebox"), out = [];
  CUR.facts.parts.forEach((p, k) => {
    const items = CUR.results[k].items.slice().sort((a, b) => b.votes - a.votes);
    if (k === 0) {
      const max = Math.max(...items.map(i => i.pct));
      out.push(`<div class="charttitle" style="margin-top:4px">Round 1, ${dlong(p.date)}</div>` + barSvg(items.map(i => ({ name: nameOf(i), v: i.pct, col: colOf(i), bold: i.rank <= 2 })), W, { max: Math.ceil(max * 10) / 10 }));
    } else {
      const [a, b] = items;
      const [ca, cb] = pairCols(a, b);
      out.push(`<div class="charttitle" style="margin-top:10px">Round 2, ${dlong(p.date)}</div>` +
        h2hSvg({ name: nameOf(a), v: a.votes, col: ca }, { name: nameOf(b), v: b.votes, col: cb }, W) +
        `<p class="chartnote">${n0(a.votes)} against ${n0(b.votes)} votes: a margin of ${n0(a.votes - b.votes)} (${(100 * (a.votes - b.votes) / (a.votes + b.votes)).toFixed(2)} points).${(OWN_ALL[EL.id] || {})[a.id] ? ` No party behind either finalist has a colour of its own, so on this page ${esc(nameOf(a))} is teal and ${esc(nameOf(b))} plum.` : ""}</p>`);
    }
  });
  return out.join("");
}
function refBars() {
  const parts = CUR.facts.parts, W = boxW("glancebox");
  return parts.map((p, k) => {
    const it = CUR.results[k].items, y = it.find(i => i.id === "yes"), n = it.find(i => i.id === "no"), t = p.totals.turnout;
    const binding = /half/.test(CUR.facts.rules.binding_rule || "");
    return `<div style="margin:6px 8px 12px">
      ${parts.length > 1 ? `<div class="charttitle" style="margin:0 0 2px">Question ${k + 1}</div>` : ""}
      <p class="note" style="color:var(--soft);margin-bottom:4px">${esc(p.question ? p.question.en : "")}</p>
      ${h2hSvg({ name: "Yes", v: y.votes, col: ANSWER.yes }, { name: "No", v: n.votes, col: ANSWER.no }, W - 16)}
      <div class="note">Turnout ${pct(t, 2)}${binding ? (t > 0.5 ? ", more than half the electorate: binding" : ", less than half the electorate: not binding") : ""}.</div></div>`;
  }).join("");
}

function pollsFacts() {
  const f = CUR.facts, pl = f.polls;
  $("facts").innerHTML = `<span class="label">Facts</span>
    <div style="font-size:13px">The estimate for the week of ${dlong(f.date)}</div>
    <p class="note">Not an election: Pooling the Poles, my Bayesian average of ${n0(pl.n)} polls by ${pl.pollsters} pollsters since January 2025 (the latest fieldwork ${dlong(pl.last)}). Seats are the model's medians from simulated elections, with 80% ranges.</p>
    <dl class="facts"><dt>Seats</dt><dd>460 in 41 constituencies, by D'Hondt</dd><dt>Thresholds</dt><dd>5% for parties, 8% for coalitions; the German minority exempt</dd></dl>
    <p class="note"><a href="${PAGES}polling-tracker.html" target="_top">The trends, scenarios and coalitions on the Polling tracker →</a></p>`;
}
function renderFacts() {
  if (isPolls()) return pollsFacts();
  const f = CUR.facts, p = partOf(), o = [];
  const dates = f.date2 ? (EL.type === "referendum" ? `${dday(f.date)} and ${dday(f.date2)}` : `${dday(f.date)}; round 2 ${dday(f.date2)}`) : dday(f.date);
  o.push(`<span class="label">Facts</span><div style="font-size:13px">${esc(dates)}${f.early ? '<span class="badge">Early</span>' : ""}</div>`);
  if (f.early && f.early_reason) o.push(`<p class="note">Early: ${esc(f.early_reason)}${f.early_source && f.early_source.url ? ` (<a href="${f.early_source.url}" target="_blank" rel="noopener">${esc(f.early_source.label)}</a>)` : ""}.</p>`);
  if (f.facts_only) {
    o.push(`<p class="note">The semi-free election agreed at the Round Table, held in two rounds. It is not in PKW's election data, so it has no maps or vote counts here: the table gives the seats each grouping won, from my Polish party tracker.</p><dl class="facts"><dt>Seats</dt><dd>${esc(f.rules.seats)}</dd><dt>Rules</dt><dd>${esc(f.rules.formula || "")}</dd></dl>`);
    $("facts").innerHTML = o.join(""); return;
  }
  const t = p.totals;
  o.push(`${CUR.facts.parts.length > 1 ? `<div class="charttitle" style="margin:4px 0 0">${EL.type === "presidential" ? "Round " : "Question "}${S.p + 1}</div>` : ""}
    <div class="kpis">
      <div>Turnout<b>${pct(t.turnout, 2)}</b></div>
      <div>Electorate<b>${n0(t.electorate)}</b></div>
      ${isNum(t.ballots_issued) ? `<div>Ballots issued<b>${n0(t.ballots_issued)}</b></div>` : ""}
      <div>Votes cast<b>${n0(t.total_votes)}</b></div>
      <div>Valid votes<b>${n0(t.valid_votes)}</b></div>
      <div>Invalid votes<b>${n0(t.invalid_votes)} <span class="muted" style="font-size:12px;font-weight:400">${pct(t.invalid_share)}</span></b></div>
    </div>`);
  const proto = [["n_stations", "Polling stations"], ["proxy_votes", "Votes by proxy"], ["certificate_votes", "Votes on a certificate"],
    ["postal_packages_sent", "Postal packages sent"], ["postal_envelopes_received", "Postal envelopes returned"], ["postal_envelopes_in_box", "Postal votes in the box"],
    ["invalid_ballot_papers", "Invalid ballot papers"], ["invalid_multiple", "Invalid: more than one mark"], ["invalid_none", "Invalid: no mark"], ["invalid_struck_only", "Invalid: only struck-off candidates"]]
    .filter(([k]) => isNum(t[k]) && t[k] > 0);
  const abroad = p.abroad, ships = p.ships;
  const ext = [];
  if (abroad) ext.push(`<dt>Abroad</dt><dd>${n0(abroad.voters)} votes cast of ${n0(abroad.electorate)} registered (${pct(abroad.turnout)})${abroad.derived ? "; the constituency totals less the gminas" : ""}</dd>`);
  if (ships) ext.push(`<dt>On ships</dt><dd>${n0(ships.voters)} votes cast of ${n0(ships.electorate)} registered</dd>`);
  if (!abroad && EL.type === "presidential" && EL.year < 2000 && S.p === 1) ext.push(`<dt>Abroad</dt><dd>The second round was counted without votes abroad or on ships</dd>`);
  if (proto.length || ext.length) o.push(`<details><summary>More from the protocols</summary><dl class="facts" style="margin-top:6px">${ext.join("")}${proto.map(([k, l]) => `<dt>${l}</dt><dd>${n0(t[k])}</dd>`).join("")}</dl></details>`);
  if (t.differs_from_files && t.differs_from_files.length) {
    const what = { n_stations: "polling stations", electorate: "electorate", ballots_issued: "ballots issued", total_votes: "votes cast", valid_votes: "valid votes",
      invalid_votes: "invalid votes", invalid_ballot_papers: "invalid ballot papers", lists: "64 lists, which the files lack" };
    o.push(`<p class="note">These are PKW's announced figures. PKW's files, which the maps use, differ slightly from them in: ${esc(t.differs_from_files.map(k => what[k] || k.replace(/_/g, " ")).join(", "))}.</p>`);
  }
  // Rules
  const r = f.rules, rules = [];
  if (r.seats) rules.push(["Seats", `${r.seats}${r.seats_note ? ` (${r.seats_note})` : ""}`]);
  if (r.constituencies) rules.push(["Constituencies", r.constituencies]);
  if (r.formula) rules.push(["Formula", r.formula]);
  if (r.threshold_list) rules.push(["Threshold", r.threshold_list + (r.threshold_coalition ? `; coalitions ${r.threshold_coalition}` : "")]);
  if (r.threshold_national_list) rules.push(["National list", r.threshold_national_list]);
  if (r.minority_exemption) rules.push(["Minorities", r.minority_exemption]);
  if (r.binding_rule) rules.push(["Binding if", r.binding_rule]);
  if (r.called_by) rules.push(["Called by", r.called_by]);
  if (p.question) rules.unshift(["Question", `${p.question.pl}<br><span class="muted">${esc(p.question.en)}</span>`]);
  if (rules.length) o.push(`<details${EL.type === "referendum" ? " open" : ""}><summary>${EL.type === "referendum" ? "The question and the rules" : "The rules"}</summary><dl class="facts" style="margin-top:6px">${rules.map(([k, v]) => `<dt>${k}</dt><dd>${k === "Question" ? v : esc(v)}</dd>`).join("")}</dl></details>`);
  // Outcome
  o.push(outcomeHtml());
  if (f.note) o.push(`<p class="note">${esc(f.note.charAt(0).toUpperCase() + f.note.slice(1))}${/[.!?]$/.test(f.note) ? "" : "."}</p>`);
  $("facts").innerHTML = o.join("");
}
function outcomeHtml() {
  const f = CUR.facts, oc = f.outcome;
  if (!oc) return "";
  if (EL.type === "sejm") {
    const L = itemById(oc.largest) || CUR.results[0].items.find(i => i.id === oc.largest);
    const gov = (oc.governments || []).map(g => `<div style="margin-top:4px;font-size:12.5px"><b>${esc(g.cabinet_name)}</b> <span class="muted">from ${dlong(g.start_date)}${g.majority === "minority" ? ", minority" : ""}</span><br>${esc(g.party_names)}</div>`).join("");
    const sy = f.system;
    return `<details open><summary>Outcome</summary>
      <div style="font-size:12.5px;margin-top:6px">Largest list: <b>${esc(L ? nameOf(L) : oc.largest)}</b>, ${oc.largest_seats} seats${oc.majority ? ", a majority" : ", short of a majority"}.</div>
      ${gov ? `<div class="label" style="margin-top:8px">Governments of the term</div>${gov}` : ""}
      ${sy ? `<div class="label" style="margin-top:8px">The party system</div><dl class="facts" style="margin-top:4px">
        <dt>Effective number of lists</dt><dd>${sy.enp_votes.toFixed(2)} by votes, ${sy.enp_seats.toFixed(2)} by seats</dd>
        <dt>Gallagher index</dt><dd>${(100 * sy.gallagher).toFixed(1)}</dd>
        ${isNum(sy.volatility_total) ? `<dt>Volatility</dt><dd>${(100 * sy.volatility_total).toFixed(1)}</dd>` : ""}
        <dt>Nationalisation</dt><dd>${sy.psns_gmina.toFixed(2)} (gminas)</dd></dl>
        <p class="note"><a href="${PAGES}party-system-indicators.html#e=${f.year}" target="_top">More on Party system indicators</a></p>` : ""}</details>`;
  }
  if (EL.type === "presidential") {
    return `<details open><summary>Outcome</summary><dl class="facts" style="margin-top:6px">
      <dt>Elected</dt><dd><b>${esc(oc.winner_name)}</b>${isNum(oc.margin_pp) ? `, by ${n0(oc.margin_votes)} votes (${oc.margin_pp.toFixed(2)} points) in round 2` : " in round 1"}</dd>
      <dt>Took office</dt><dd>${dlong(oc.took_office)}</dd>
      <dt>Predecessor</dt><dd>${esc(oc.predecessor)}${oc.note ? `<br><span class="muted">${esc(oc.note)}</span>` : ""}</dd>
      <dt>Incumbent</dt><dd>${oc.incumbent_ran ? `${esc(oc.incumbent)} ran` : "did not run"}</dd></dl></details>`;
  }
  if (EL.type === "ep") {
    const L = itemById(oc.largest);
    return `<details open><summary>Outcome</summary><div style="font-size:12.5px;margin-top:6px">Largest list: <b>${esc(L ? nameOf(L) : oc.largest)}</b>, ${oc.largest_seats} of ${f.rules.seats} seats.</div></details>`;
  }
  return "";
}

// ---- results table --------------------------------------------------------------------------
// Each party behind a list or candidate, linked to the genealogy and Party ideology where they have it
function partyLinks(ids) {
  if (!ids) return "";
  const gen = new Set(E.genealogy_ids || []), ideo = new Set(E.ideology_ids || []);
  return ids.split(";").filter(id => E.parties[id]).map(id => {
    const nm = esc(E.parties[id]), full = esc(E.party_names[id] || "");
    const a = gen.has(id) ? `<a href="${PAGES}party-genealogy.html#pt=${encodeURIComponent(id)}" target="_top" title="${full} in the party genealogy">${nm}</a>` : `<span title="${full}">${nm}</span>`;
    return ideo.has(id) ? `${a} <a href="${PAGES}party-ideology.html#pt=${encodeURIComponent(id)}" target="_top" title="${full} in Party ideology" style="font-size:10.5px">ideology</a>` : a;
  }).join(", ");
}
function pollsResults() {
  const items = resOf();
  $("restable").innerHTML = `<thead><tr><th class="l">Party</th><th>Share</th><th>80% range</th><th>Chance of 5%</th><th>Seats</th><th>80% range</th></tr></thead>
    <tbody>${items.map(it => `<tr class="${mapItems()[S.i] && mapItems()[S.i].id === it.id ? "sel" : ""}"><td class="name"><button type="button" class="tbtn" data-id="${esc(it.id)}"><span class="dot" style="background:${colOf(it)};margin-right:6px"></span>${esc(it.name)}</button></td>
      <td>${pct(it.pct)}</td><td>${it.id === "MN" ? '<span class="na">2023 share</span>' : `${pct(it.lo)}–${pct(it.hi)}`}</td><td>${isNum(it.p_thr) ? pct(it.p_thr, 0) : '<span class="na">exempt</span>'}</td>
      <td>${it.seats}</td><td>${isNum(it.seats_lo) ? `${it.seats_lo}–${it.seats_hi}` : ""}</td></tr>`).join("")}</tbody>`;
  $("resultslede").textContent = "The Polling tracker's estimate for the latest week: each party's share with its 80% range, its chance of clearing 5%, and the model's median seats with their 80% range. The German minority enters at its 2023 share. Click a party to map it.";
  renderExtras();
}
function renderResults() {
  if (isPolls()) return pollsResults();
  if (CUR.facts.facts_only) { $("restable").innerHTML = ""; $("resultslede").textContent = "The semi-free election of 1989 is not on the PKW portal; its seats are in the table above."; $("extras").innerHTML = ""; return; }
  const items = resOf().slice().sort((a, b) => b.votes - a.votes), T = partOf().totals, typ = EL.type;
  const cols = [];
  const c = (h, f, cls = "") => cols.push({ h, f, cls });
  c(itemLabel(), it => {
    const links = typ === "referendum" ? "" : partyLinks(it.party_ids);
    const full = typ === "presidential" ? esc(it.backing || "no party") : it.name !== nameOf(it) ? esc(it.name) : "";
    return `<button type="button" class="tbtn" data-id="${esc(it.id)}"><span class="dot" style="background:${colOf(it)};margin-right:6px"></span>${esc(nameOf(it))}</button>${full ? `<span class="full">${full}</span>` : ""}${links ? `<span class="full">Parties: ${links}</span>` : ""}`;
  }, "name");
  c("Votes", it => n0(it.votes));
  c("%", it => pct(it.pct, 2));
  if (typ === "sejm") {
    c("Change, points", it => it.lineage_status && /^new/.test(it.lineage_status) ? '<span class="na">new</span>' : chg(it.pct, it.prev_pct));
    c("Seats", it => it.seats ?? 0);
    c("Change", it => isNum(it.prev_seats) ? cls(signedInt(it.seats - it.prev_seats), it.seats - it.prev_seats) : '<span class="na">–</span>');
    c("% of seats", it => pct(it.seats_pct));
    c("Threshold", it => it.above_threshold === true ? "passed" : it.above_threshold === false ? '<span class="na">no</span>' : '<span class="na">–</span>');
  }
  if (typ === "ep") {
    c("Change, points", it => chg(it.pct, it.prev_pct));
    c("Seats", it => it.seats ?? 0);
  }
  if (typ === "presidential") {
    c("Rank", it => it.rank);
    if (S.p === 0 && CUR.facts.parts.length > 1) c("Round 2", it => { const r2 = CUR.results[1].items.find(x => x.id === it.id); return r2 ? pct(r2.pct, 2) : '<span class="na">–</span>'; });
  }
  if (typ !== "referendum") {
    c("Voivodeships won", it => it.woj_won ?? 0);
    c("Constituencies won", it => it.okreg_won ?? 0);
    c("Gminas won", it => it.gmina_won ?? 0);
    c("Best constituency", it => it.okreg_best_name ? `${esc(it.okreg_best_name)} <span class="muted">${pct(it.okreg_best_share)}</span>` : NA, "l");
    c("Best gmina", it => it.gmina_best_name ? `${esc(it.gmina_best_name)} <span class="muted">${pct(it.gmina_best_share)}</span>` : NA, "l");
    c("Worst gmina", it => it.gmina_worst_name ? `${esc(it.gmina_worst_name)} <span class="muted">${pct(it.gmina_worst_share)}</span>` : NA, "l");
  }
  if (typ === "sejm") c("Nationalisation", it => isNum(it.spns_gmina) ? `<a href="${PAGES}party-system-indicators.html#e=${EL.year}&s=nationalisation&n=g" target="_top" title="Bochsler's standardised nationalisation score, by gmina, on Party system indicators">${it.spns_gmina.toFixed(2)}</a>` : NA);
  const sel = mapItems()[S.i];
  // Long ballots (1991 had 111 lists) show the lists with 0.5% or a seat until asked for all
  const big = it => it.pct >= 0.005 || it.seats > 0 || typ === "referendum";
  const hidden = items.filter(it => !big(it)).length, shown = hidden > 6 && !S.allLists ? items.filter(big) : items;
  $("restable").innerHTML = `<thead><tr>${cols.map(k => `<th class="${k.cls === "name" ? "l" : k.cls}">${k.h}</th>`).join("")}</tr></thead>
    <tbody>${shown.map(it => `<tr class="${sel && sel.id === it.id ? "sel" : ""}">${cols.map(k => `<td class="${k.cls}">${k.f(it)}</td>`).join("")}</tr>`).join("")}
    ${hidden > 6 ? `<tr><td class="name" colspan="${cols.length}"><button type="button" class="linkbtn" id="alllists">${S.allLists ? "Show only the lists with 0.5% or a seat" : `Show all ${items.length} lists (${hidden} more, each under 0.5% with no seat)`}</button></td></tr>` : ""}
    <tr class="tot"><td class="name">Valid votes</td><td>${n0(T.valid_votes)}</td><td>100%</td>${cols.slice(3).map(() => "<td></td>").join("")}</tr></tbody>`;
  const lede = {
    sejm: "Votes and shares are PKW's. Change is from the previous election, by list lineage, as on Party system indicators; seats include the national lists of 1991–97. Units won and best and worst units count the gmina files, without votes abroad. Click a list to map it.",
    ep: "Votes and shares are PKW's; seats from PKW's lists of the candidates elected. Change compares the list with the previous European election's list of the same camp, where both are the only list of their camp. Click a list to map it.",
    presidential: "Votes and shares are PKW's. Units won and best and worst units count the gmina files, without votes abroad. Click a candidate to map them.",
    referendum: "Votes and shares are PKW's, of the valid votes on the question."
  }[typ];
  $("resultslede").textContent = lede;
  renderExtras();
}
const cls = (s, v) => `<span class="${v > 0 ? "up" : v < 0 ? "down" : ""}">${s}</span>`;
const chg = (v, p) => isNum(p) ? cls(signedPts(v - p), v - p) : '<span class="na">–</span>';

function renderExtras() {
  const p = partOf(), wv = p.voivodeships, items = wv ? wv.items.map(id => id === "other" ? { id: "other", short: "Other", colour: "#BDBDBD" } : itemById(id)) : [];
  const out = [];
  if (wv) out.push(`<details><summary>By voivodeship (${wv.era === "49" ? "the 49 voivodeships of 1975–98" : "the 16 voivodeships"})</summary><div class="scroll" style="margin-top:8px"><table class="t">
    <thead><tr><th class="l">Voivodeship</th><th>Turnout</th>${items.map(it => `<th>${esc(it.id === "other" ? "Others" : nameOf(it))}</th>`).join("")}</tr></thead>
    <tbody>${wv.rows.map(r => `<tr><td class="l">${esc(r.name)}</td><td>${pct(r.turnout)}</td>${r.votes.map((v, k) => `<td${wv.items[k] === r.leader ? ' style="font-weight:600"' : ""}>${isNum(v) ? pct(v / r.valid) : '<span class="na">–</span>'}</td>`).join("")}</tr>`).join("")}</tbody></table></div>
    <p class="note" style="margin-top:4px">Added up from the gmina files, without votes abroad and on ships; the leader in each voivodeship is in bold.</p></details>`);
  if (p.abroad && p.abroad.votes) {
    out.push(`<details><summary>Abroad${p.ships ? " and on ships" : ""}</summary><div class="scroll" style="margin-top:8px"><table class="t"><thead><tr><th class="l"></th><th>Votes cast</th>${items.map(it => `<th>${esc(it.id === "other" ? "Others" : nameOf(it))}</th>`).join("")}</tr></thead><tbody>
      ${[["Abroad", p.abroad], ["Ships", p.ships]].filter(x => x[1]).map(([n, a]) => `<tr><td class="l">${n}</td><td>${n0(a.voters)}</td>${a.votes.map(v => `<td>${isNum(v) ? pct(v / a.valid) : '<span class="na">–</span>'}</td>`).join("")}</tr>`).join("")}</tbody></table></div></details>`);
  }
  const sb = CUR.facts.seats_by_okreg;
  if (sb && sb.rows.length) {
    const set = G.sets[sb.set], lists = [...new Set(sb.rows.map(r => r.item))].map(itemById).filter(Boolean).sort((a, b) => b.seats - a.seats);
    const at = {}; sb.rows.forEach(r => { at[r.okreg + "|" + r.item] = r.seats; });
    out.push(`<details><summary>Seats by constituency</summary><div class="scroll" style="margin-top:8px"><table class="t">
      <thead><tr><th class="l">Constituency</th><th>Seats</th>${lists.map(it => `<th><span class="dot" style="background:${colOf(it)}"></span> ${esc(nameOf(it))}</th>`).join("")}</tr></thead>
      <tbody>${set.okreg.map((k, i) => { const v = lists.map(it => at[k + "|" + it.id] || 0); return `<tr><td class="l">${k}. ${esc(set.name[i])}</td><td>${sum(v)}</td>${v.map(x => `<td>${x || '<span class="na">·</span>'}</td>`).join("")}</tr>`; }).join("")}
      ${sb.national_list.length ? `<tr class="tot"><td class="l">National lists</td><td>${sum(sb.national_list.map(r => r.seats))}</td>${lists.map(it => { const r = sb.national_list.find(x => x.item === it.id); return `<td>${r ? r.seats : '<span class="na">·</span>'}</td>`; }).join("")}</tr>` : ""}</tbody></table></div>
      <p class="note" style="margin-top:4px">${isPolls() ? "A single D'Hondt allocation at the median shares, as on the Polling tracker's map, so the totals can differ from the model's medians." : `From PKW's lists of the candidates elected. An allocation under the year's rules reproduces every constituency${EL.year === 1991 ? " from 1993; 1991's files lack 64 lists, so it is not checked" : ""}.`}</p></details>`);
  }
  $("extras").innerHTML = out.join("");
}

// ---- map ------------------------------------------------------------------------------------
function levelData() {
  const U = unitsOf();
  if (isPolls()) { S.l = "c"; return { kind: "c", A: U.okreg41, set: "sejm2001", n: U.okreg41.valid.length }; }
  if (S.l === "g") return { kind: "g", A: U.gmina, set: null, n: G.gminas.teryt.length };
  const A = S.b === "t" || measure() === "chg" ? U.okreg41 : U.okreg;
  return { kind: "c", A, set: A.set, n: A.valid.length };
}
function unitName(kind, set, u) {
  if (kind === "g") {
    const g = G.gminas, k = g.kind ? g.kind[u] : null;
    return { title: g.name[u] + (k === 1 ? " (urban gmina)" : k === 2 ? " (rural gmina)" : k === 3 ? " (urban-rural gmina)" : ""), sub: `${g.powiat[u] === g.name[u] || g.powiat[u] === "Warszawa" ? "city with powiat rights" : "powiat " + g.powiat[u]}, ${g.voivodeship[u]}` };
  }
  const s = G.sets[set];
  return { title: `${s.okreg[u]}. ${s.name[u]}`, sub: s.label };
}
function valuesFor(L) {
  if (measure() === "chg") return changeValues(L);
  const A = L.A, m = measure(), ids = A.items, its = mapItems(), it = its[S.i], it2 = its[S.j];
  const kk = id => ids.indexOf(id);
  const n = L.n, out = new Array(n).fill(null), extra = new Array(n).fill(null);
  for (let u = 0; u < n; u++) {
    const valid = A.valid[u];
    if (!isNum(valid) || valid <= 0) continue;
    if (m === "turnout") { out[u] = isNum(A.voters[u]) && A.electorate[u] > 0 ? A.voters[u] / A.electorate[u] : null; continue; }
    if (m === "share") { const v = A.votes[kk(it.id)][u]; out[u] = isNum(v) ? v / valid : null; continue; }
    if (m === "h2h") { const a = A.votes[kk(it.id)][u], b = A.votes[kk(it2.id)][u]; out[u] = isNum(a) || isNum(b) ? ((a || 0) - (b || 0)) / valid : null; continue; }
    let best = -1, bv = -1, sv = 0;
    ids.forEach((id, k) => { if (id === "other") return; const v = A.votes[k][u]; if (!isNum(v)) return; if (v > bv) { sv = Math.max(sv, bv); bv = v; best = k; } else if (v > sv) sv = v; });
    if (best >= 0) { out[u] = (bv - Math.max(sv, 0)) / valid; extra[u] = ids[best]; }
  }
  return { v: out, lead: extra };
}
function scaleFor(vals) {
  const m = measure(), its = mapItems();
  if (m === "chg") {
    const o = vals.C && vals.C.opt, gc = o ? o.col : TURN_GAIN;
    const hi = Math.max(0.02, Math.ceil((quantile(vals.v.map(x => isNum(x) ? Math.abs(x) : null), 0.98) || 0.02) * 50) / 50);
    return { kind: "div", chg: true, lo: -hi, hi, col: v => v >= 0 ? mix(gc, 0.04 + 0.96 * clamp(v / hi, 0, 1)) : mix(LOSS, 0.04 + 0.96 * clamp(-v / hi, 0, 1)),
      ticks: ticks(-hi, hi, 4), title: o ? `${o.name}: change since ${contestName(vals.C.cmp)}, in points` : "Change" };
  }
  if (m === "turnout") {
    const lo = quantile(vals.v, 0.02), hi = quantile(vals.v, 0.98), tk = ticks(lo, hi, 5);
    const a = Math.floor(lo * 20) / 20, b = Math.ceil(hi * 20) / 20;
    return { kind: "seq", lo: a, hi: b, col: v => ramp(TURNOUT, (v - a) / (b - a || 1)), ticks: ticks(a, b, 5), title: "Turnout" };
  }
  if (m === "share") {
    const it = its[S.i], c = colOf(it), hi = Math.max(0.05, Math.ceil((quantile(vals.v, 0.98) || 0.05) * 20) / 20);
    return { kind: "seq", lo: 0, hi, col: v => mix(c, 0.05 + 0.95 * clamp(v / hi, 0, 1)), ticks: ticks(0, hi, 5), title: `${nameOf(it)}, share of the valid votes` };
  }
  if (m === "h2h") {
    const a = its[S.i], b = its[S.j], [ca, cb] = pairCols(a, b);
    const hi = Math.max(0.05, Math.ceil((quantile(vals.v.map(x => isNum(x) ? Math.abs(x) : null), 0.98) || 0.05) * 20) / 20);
    return { kind: "div", lo: -hi, hi, ca, cb, col: v => v >= 0 ? mix(ca, 0.04 + 0.96 * clamp(v / hi, 0, 1)) : mix(cb, 0.04 + 0.96 * clamp(-v / hi, 0, 1)), ticks: ticks(-hi, hi, 4), title: `${nameOf(a)} against ${nameOf(b)}: lead in points` };
  }
  const hi = Math.max(0.1, Math.ceil((quantile(vals.v, 0.9) || 0.1) * 10) / 10);
  return { kind: "lead", hi, col: (v, id) => mix(colOf(itemById(id)), 0.3 + 0.7 * clamp(v / hi, 0, 1)), title: `Leading ${itemLabel().toLowerCase()}` };
}
let MAPV = null;
function renderMapControls() {
  const its = mapItems(), m = measure(), typ = EL.type;
  const ms = isPolls() ? [["leader", "Leader"], ["share", "Share"], ["h2h", "Head to head"]] : [["leader", "Leader"], ["share", "Share"], ["h2h", "Head to head"], ["turnout", "Turnout"], ["chg", "Change"]];
  $("measures").innerHTML = ms.map(([k, l]) => `<button type="button" data-m="${k}" aria-pressed="${m === k}">${l}</button>`).join("");
  $("itemlab").textContent = itemLabel();
  if (m === "chg") {
    const C = changeSetup();
    $("itemlab").textContent = C.mode === "camp" ? "Camp" : itemLabel();
    $("item").innerHTML = C.options.map(o => `<option value="${esc(o.key)}"${o.key === C.key ? " selected" : ""}>${esc(o.name)}</option>`).join("");
    $("cmp").innerHTML = compareOptions(C.cmp);
    $("modes").innerHTML = C.modes.map(([k, l]) => `<button type="button" data-k="${k}" aria-pressed="${C.mode === k}">${l}</button>`).join("");
  } else {
    const opts = sel => its.map((it, k) => `<option value="${k}"${k === sel ? " selected" : ""}>${esc(nameOf(it))} (${pct(it.pct)})</option>`).join("");
    $("item").innerHTML = opts(S.i); $("item2").innerHTML = opts(S.j);
  }
  $("itemctrl").hidden = !(m === "share" || m === "h2h" || m === "chg");
  $("item2ctrl").hidden = m !== "h2h";
  $("cmpctrl").hidden = m !== "chg"; $("modectrl").hidden = m !== "chg";
  $("levels").innerHTML = [["g", "Gminas"], ["c", "Constituencies"]].map(([k, l]) => `<button type="button" data-l="${k}" aria-pressed="${S.l === k}"${k === "g" && isPolls() ? ' disabled title="The polls are projected by constituency only"' : ""}>${l}</button>`).join("");
  const own = unitsOf().okreg.set;
  $("bndctrl").hidden = S.l !== "c" || m === "chg" || isPolls();
  $("bounds").innerHTML = [["o", own === "sejm2001" && typ === "sejm" ? "PKW's (with abroad)" : G.sets[own].label.replace(/^(Sejm|EP) constituencies, /, "$1 ")], ["t", "Today's 41"]]
    .map(([k, l]) => `<button type="button" data-b="${k}" aria-pressed="${S.b === k}">${esc(l)}</button>`).join("");
  $("shapectrl").hidden = S.l !== "c";
  $("shapes").innerHTML = [["m", "Map"], ["c", "Cartogram"]].map(([k, l]) => `<button type="button" data-v="${k}" aria-pressed="${S.v === k}">${l}</button>`).join("");
  const carto = S.l === "c" && S.v === "c", zb = $("zabory");
  zb.disabled = carto; zb.setAttribute("aria-pressed", String(S.z && !carto));
  zb.title = carto ? "The partition borders are drawn on the map, not on the cartogram" : "Draw the 1914 borders between the Russian, German and Austro-Hungarian empires";
}
// The map's height is capped so that, with the section's heading and controls above it, it fits the
// window below the sticky election strip (--mapfit); where that would leave it under 420px, the map
// alone fits below the strip (--stripfit). The page usually sits in a frame a screen tall.
function fitMap() {
  const strip = document.querySelector(".strip"), sec = $("mapsec"), map = $("map");
  if (!strip || !sec || !map) return;
  const off = map.getBoundingClientRect().top - sec.getBoundingClientRect().top, r = document.documentElement.style;
  r.setProperty("--mapfit", Math.round(strip.offsetHeight + off + 16) + "px");
  r.setProperty("--stripfit", Math.round(strip.offsetHeight + 16) + "px");
}
function renderMap() {
  if (CUR.facts.facts_only) { $("map").innerHTML = ""; $("legend").innerHTML = ""; $("mapnote").textContent = "No maps for 1989: the election is not on the PKW portal."; return; }
  if (measure() === "chg") {
    const el = CONTEST[S.cc && CONTEST[S.cc] && S.cc !== partOf().contest_id ? S.cc : defaultCompare()].election;
    if (!CACHE.has(el)) {
      $("mapnote").textContent = "Loading the election to compare with…";
      loadElection(el).then(() => { renderMap(); renderUnitCard(); renderScatter(); saveHash(); }).catch(() => toast("Could not load that election"));
      return;
    }
  }
  renderMapControls();
  document.querySelectorAll("#mapbox .tip").forEach(t => { t.hidden = true; });
  const L = levelData(), vals = valuesFor(L), sc = scaleFor(vals), svg = $("map");
  MAPV = { L, vals, sc };
  const fill = u => isNum(vals.v[u]) ? (sc.kind === "lead" ? sc.col(vals.v[u], vals.lead[u]) : sc.col(vals.v[u])) : NODATA;
  const [W, H] = G.meta.view;
  let html = "";
  if (L.kind === "g") {
    svg.classList.remove("cons");
    svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
    html = G.gminas.d.map((d, u) => `<path class="u" d="${d}" fill="${fill(u)}" data-u="${u}"/>`).join("");
  } else if (S.v === "c") {
    const s = G.sets[L.set], cv = s.circle_view;
    svg.classList.add("cons");
    svg.setAttribute("viewBox", cv.join(" "));
    html = s.circles.map((c, u) => `<circle class="u" cx="${c[0]}" cy="${c[1]}" r="${Math.max(1, c[2] - 1.2)}" fill="${fill(u)}" stroke="#fff" stroke-width="1.6" data-u="${u}" tabindex="0" role="button" aria-label="${esc(unitName("c", L.set, u).title)}"/>` +
      (c[2] > 22 ? `<text x="${c[0]}" y="${c[1] + 4}" text-anchor="middle" font-size="${clamp(c[2] * 0.42, 9, 16)}" fill="#141414" pointer-events="none">${s.okreg[u]}</text>` : "")).join("");
  } else {
    const s = G.sets[L.set];
    svg.classList.add("cons");
    svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
    html = s.d.map((d, u) => `<path class="u" d="${d}" fill="${fill(u)}" data-u="${u}" tabindex="0" role="button" aria-label="${esc(unitName("c", L.set, u).title)}"/>`).join("");
  }
  // "Widać zabory!": the 1914 borders, heavier than any other line
  if (S.z && !(L.kind === "c" && S.v === "c")) html += G.partitions.lines.map(l => `<path d="${l.d}" fill="none" stroke="#141414" stroke-width="3.4" stroke-linecap="round" stroke-linejoin="round" pointer-events="none"/>`).join("");
  html += `<g id="hl"></g><g id="hov"></g>`;
  svg.setAttribute("font-family", FONT);
  svg.innerHTML = html;
  fitMap();
  renderLegend(sc, vals);
  renderZones(vals);
  const notes = [];
  if (measure() === "chg") notes.push(changeNote(vals.C, L));
  if (isPolls()) notes.push("The Polling tracker's projection: each party's national share spread across today's 41 constituencies by its regional profile from 2023 (R+ takes PiS's; KKP and Razem those of Braun and Zandberg in the 2025 presidential election), as on the tracker.");
  else if (measure() === "chg") { /* the change note covers the level */ }
  else if (L.kind === "g") notes.push("Gminas as they are today (2022 boundaries; Warsaw as one). Earlier results are mapped onto them: where a 1990s gmina later split, its result covers all of its successors, and the tooltip says so.");
  else if (S.b === "t") notes.push("Every election's gmina results added up by today's 41 Sejm constituencies, without votes abroad and on ships, so that any two elections compare like for like.");
  else if (EL.type === "sejm" || EL.type === "ep") notes.push("PKW's constituency results, which count votes abroad and on ships in the constituencies PKW assigns them to (Warsaw, and the seaports for ships).");
  else notes.push(`${G.sets[L.set].label}, in force at the time: the gmina results added up, without votes abroad and on ships.`);
  if (S.l === "c" && S.v === "c") notes.push("Circles are sized by electorate.");
  if (measure() !== "turnout" && unitsOf().gmina && unitsOf().gmina.items.includes("other")) notes.push("Lists and candidates under 0.5% nationally with no seat are counted together as others.");
  $("mapnote").textContent = notes.join(" ");
  drawHighlight();
}
function renderLegend(sc, vals) {
  const W = 420, o = [];
  if (sc.kind === "lead") {
    const ids = [...new Set(vals.lead.filter(Boolean))], its = ids.map(itemById).filter(Boolean).sort((a, b) => b.votes - a.votes);
    const counts = {}; vals.lead.forEach(id => { if (id) counts[id] = (counts[id] || 0) + 1; });
    $("legend").innerHTML = `<div class="charttitle" style="margin-left:0">${esc(sc.title)}</div><div class="legend">${its.map(it => `<span><span class="sw" style="background:${colOf(it)}"></span>${esc(nameOf(it))} <span class="muted">${counts[it.id]}</span></span>`).join("")}</div>
      <svg viewBox="0 0 ${W} 34" font-family="${FONT}" style="margin-top:4px">${[0, 0.25, 0.5, 0.75, 1].map((t, k) => `<rect x="${k * 44}" y="4" width="42" height="12" fill="${mix("#767676", 0.3 + 0.7 * t)}"/>`).join("")}
      <text x="0" y="30" font-size="11" fill="#767676">0</text><text x="${4 * 44 + 42}" y="30" text-anchor="end" font-size="11" fill="#767676">${(100 * sc.hi).toFixed(0)} points or more</text>
      <text x="${4 * 44 + 54}" y="14" font-size="11" fill="#4B4B4B">the lead over the runner-up</text></svg>`;
    return;
  }
  const lo = sc.lo, hi = sc.hi, X = v => 8 + (v - lo) / (hi - lo) * (W - 16), stops = [];
  for (let k = 0; k <= 20; k++) { const v = lo + k / 20 * (hi - lo); stops.push(`<stop offset="${k * 5}%" stop-color="${sc.col(v)}"/>`); }
  o.push(`<defs><linearGradient id="lg" x1="0" x2="1">${stops.join("")}</linearGradient></defs><rect x="8" y="4" width="${W - 16}" height="12" fill="url(#lg)"/>`);
  sc.ticks.forEach(t => o.push(`<line x1="${X(t)}" x2="${X(t)}" y1="16" y2="20" stroke="#767676"/><text x="${X(t)}" y="32" text-anchor="middle" font-size="11" fill="#4B4B4B">${sc.kind === "div" ? (t === 0 ? "0" : (100 * Math.abs(t)).toFixed(0)) : (100 * t).toFixed(0) + "%"}</text>`));
  if (sc.chg) o.push(`<text x="${W - 8}" y="46" text-anchor="end" font-size="11" fill="#4B4B4B">gain →</text><text x="8" y="46" font-size="11" fill="#4B4B4B">← loss</text>`);
  else if (sc.kind === "div") {
    const a = mapItems()[S.i], b = mapItems()[S.j];
    o.push(`<text x="${W - 8}" y="46" text-anchor="end" font-size="11" fill="#4B4B4B">${esc(nameOf(a))} ahead →</text><text x="8" y="46" font-size="11" fill="#4B4B4B">← ${esc(nameOf(b))} ahead</text>`);
  }
  $("legend").innerHTML = `<div class="charttitle" style="margin-left:0">${esc(sc.title)}</div><svg viewBox="0 0 ${W} ${sc.kind === "div" ? 50 : 36}" font-family="${FONT}">${o.join("")}</svg><div class="legend"><span><span class="sw" style="background:${NODATA}"></span>${measure() === "share" ? "did not stand" : measure() === "chg" ? "no comparison" : "no data"}</span></div>`;
}
function unitHtml(u, full = false) {
  const { L, vals } = MAPV, A = L.A, nm = unitName(L.kind, L.set, u), ids = A.items;
  const rows = ids.map((id, k) => ({ id, v: A.votes[k][u] })).filter(r => isNum(r.v)).sort((a, b) => b.v - a.v);
  const valid = A.valid[u];
  let html = `<b>${esc(nm.title)}</b><div class="sub">${esc(nm.sub)}</div>`;
  const grp = L.kind === "g" && A.group && A.group[u] >= 0 ? A.groups[A.group[u]] : null;
  if (grp) html += `<div class="sub" style="color:#8B0000">Reported jointly with ${esc(grp.filter(x => x !== u).map(x => G.gminas.name[x]).join(", "))}: the figures are the whole unit's</div>`;
  if (measure() === "chg" && vals.C && vals.C.opt) html += `<div class="r" style="margin-top:4px"><span><b>${esc(vals.C.opt.name)}</b>, since ${esc(contestName(vals.C.cmp))}</span><span><b>${isNum(vals.v[u]) ? signedPts(vals.v[u]) + " points" : NA}</b></span></div>`;
  if (!isPolls()) html += `<div class="r" style="margin-top:4px"><span>Turnout</span><span>${pct(A.electorate[u] > 0 ? A.voters[u] / A.electorate[u] : null)}</span></div>`;
  const shown = full ? rows : rows.slice(0, 6);
  shown.forEach(r => { const it = r.id === "other" ? null : itemById(r.id); html += tipRow(it ? colOf(it) : "#BDBDBD", it ? nameOf(it) : "Others", pct(r.v / valid), full ? n0(r.v) : ""); });
  if (!full && rows.length > shown.length) html += `<div class="sub">and ${rows.length - shown.length} more</div>`;
  if (full && !isPolls()) html += `<div class="r" style="margin-top:4px"><span class="muted">Electorate</span><span>${n0(A.electorate[u])}</span></div><div class="r"><span class="muted">Valid votes</span><span>${n0(valid)}</span></div>`;
  if (full && L.kind === "c" && (S.b === "o" || isPolls()) && CUR.facts.seats_by_okreg) {
    const okr = G.sets[L.set].okreg[u], seats = CUR.facts.seats_by_okreg.rows.filter(r => r.okreg === okr && r.seats > 0).sort((a, b) => b.seats - a.seats);
    if (seats.length) html += `<div class="label" style="margin-top:8px">Seats</div>` + seats.map(r => { const it = itemById(r.item); return tipRow(colOf(it), nameOf(it), r.seats); }).join("");
  }
  return html;
}
function drawHighlight() {
  const g = $("hl"); if (!g || !MAPV) return;
  if (!S.sel || S.sel.l !== S.l + S.b + S.v) { g.innerHTML = ""; return; }
  const u = S.sel.u, L = MAPV.L;
  if (L.kind === "g") g.innerHTML = `<path class="hl" d="${G.gminas.d[u]}"/>`;
  else if (S.v === "c") { const c = G.sets[L.set].circles[u]; g.innerHTML = `<circle class="hl" cx="${c[0]}" cy="${c[1]}" r="${c[2]}"/>`; }
  else g.innerHTML = `<path class="hl" d="${G.sets[L.set].d[u]}"/>`;
}
function renderUnitCard() {
  renderProfile();
  const card = $("unitcard");
  if (!S.sel || !MAPV || S.sel.l !== S.l + S.b + S.v) { card.innerHTML = `<span class="label">${S.l === "g" ? "Gmina" : "Constituency"}</span><p class="note">Hover over the map for a unit's result; click it to keep it here.</p>`; return; }
  card.innerHTML = `<div style="display:flex;justify-content:space-between;gap:8px"><span class="label">${S.l === "g" ? "Gmina" : "Constituency"}</span><button type="button" class="linkbtn" id="unitclose">Close</button></div><div class="tip" style="position:static;box-shadow:none;border:0;padding:0;max-width:none">${unitHtml(S.sel.u, true)}</div>`;
}
function bindMap() {
  const over = (svgEl, box) => {
    svgEl.addEventListener("mousemove", ev => {
      const t = ev.target.closest("[data-u]"), tip = box.querySelector(".tip");
      if (!t || !MAPV) { tip.hidden = true; return; }
      placeTip(tip, box, ev, unitHtml(+t.dataset.u));
    });
    svgEl.addEventListener("mouseleave", () => { box.querySelector(".tip").hidden = true; });
    svgEl.addEventListener("keydown", ev => {
      const t = ev.target.closest("[data-u]");
      if (t && (ev.key === "Enter" || ev.key === " ")) { ev.preventDefault(); t.dispatchEvent(new MouseEvent("click", { bubbles: true })); }
    });
    svgEl.addEventListener("click", ev => {
      const t = ev.target.closest("[data-u]"); if (!t) return;
      const u = +t.dataset.u;
      S.sel = S.sel && S.sel.u === u && S.sel.l === S.l + S.b + S.v ? null : { u, l: S.l + S.b + S.v };
      drawHighlight(); renderUnitCard();
    });
  };
  over($("map"), $("mapbox"));
}

// ---- change maps ----------------------------------------------------------------------------
function contestName(id) {
  const c = CONTEST[id], e = elOf(c.election), n = e.parts.length;
  return `${e.year} ${TYPE_SHORT[e.type]}${n > 1 ? (e.type === "presidential" ? ", round " : ", question ") + c.part : ""}`;
}
// The previous election of the same type (round 1 against round 2 within a presidential election)
function defaultCompare() {
  const cid = partOf().contest_id, ids = E.contests.map(c => c.id), i = ids.indexOf(cid), c = CONTEST[cid];
  if (EL.type === "presidential" && c.part === 2) return EL.parts[0].contest_id;
  for (let k = i - 1; k >= 0; k--) { const o = CONTEST[ids[k]]; if (elOf(o.election).type === EL.type && o.election !== EL.id && o.part === 1) return o.id; }
  return ids[i > 0 ? i - 1 : i + 1];
}
function compareOptions(sel) {
  const cur = partOf().contest_id;
  return ["sejm", "presidential", "ep", "referendum"].map(t => {
    const cs = E.contests.filter(c => c.id !== cur && elOf(c.election).type === t).reverse();
    return `<optgroup label="${TYPES[t]}">${cs.map(c => `<option value="${c.id}"${c.id === sel ? " selected" : ""}>${esc(contestName(c.id))}</option>`).join("")}</optgroup>`;
  }).join("");
}
function previousSejm(id) {
  const s = ELECTIONS.filter(e => e.type === "sejm" && !e.facts_only), i = s.findIndex(e => e.id === id);
  return i > 0 ? s[i - 1].id : null;
}
// What the change map compares: the other contest, how lists are followed between the two, and
// the list, candidate, camp or turnout chosen. Lineage follows Party system analysis's lineages
// (each list against the lists it continues at the previous Sejm election); camp adds up the
// lists or candidates of one palette label in each contest.
function changeSetup() {
  const cmp = S.cc && CONTEST[S.cc] && S.cc !== partOf().contest_id ? S.cc : defaultCompare();
  const c = CONTEST[cmp], f = CACHE.get(c.election);
  if (!f) return { cmp, D: null, modes: [], mode: null, options: [], key: null, opt: null };
  const k = f.results.findIndex(r => r.part === cmp);
  const D = { id: cmp, election: elOf(c.election), items: f.results[k].items, units: f.units[k] };
  const cur = mapItems(), stored = new Set(D.units.gmina.items), modes = [];
  if (EL.type === "sejm" && D.election.type === "sejm" && previousSejm(EL.id) === D.election.id) modes.push(["lineage", "Lineage"]);
  if (cur.some(it => stored.has(it.id))) modes.push(["same", EL.type === "presidential" ? "Candidate" : "Same list"]);
  modes.push(["camp", "Camp"]);
  const mode = modes.find(x => x[0] === S.ck) ? S.ck : modes[0][0];
  let options = [];
  if (mode === "lineage") {
    options = cur.filter(it => it.predecessors).map(it => ({ key: it.id, name: nameOf(it), col: colOf(it), cur: [it.id], cmp: it.predecessors.split(";") }));
  } else if (mode === "same") {
    options = cur.filter(it => stored.has(it.id)).map(it => ({ key: it.id, name: nameOf(it), col: colOf(it), cur: [it.id], cmp: [it.id] }));
  } else if (EL.type === "referendum" && D.election.type === "referendum") {
    options = ["yes", "no"].map(a => ({ key: a, name: a === "yes" ? "Yes" : "No", col: ANSWER[a], cur: [a], cmp: [a] }));
  } else if (EL.type !== "referendum" && D.election.type !== "referendum") {
    const lab = Object.fromEntries(D.items.map(i => [i.id, i.label]));
    const theirs = new Set(D.units.gmina.items.map(id => lab[id]).filter(Boolean));
    const mine = [...new Set(cur.map(i => i.label).filter(l => l && l !== "Other" && l !== "Independent"))];
    options = mine.filter(l => theirs.has(l)).map(l => ({ key: "camp:" + l, name: l, col: E.labels[l], cur: cur.filter(i => i.label === l).map(i => i.id), cmp: D.units.gmina.items.filter(id => lab[id] === l) }));
  }
  options.push({ key: "turnout", name: "Turnout", col: TURN_GAIN, turnout: true });
  const key = options.find(o => o.key === S.cx) ? S.cx : options[0].key;
  return { cmp, D, modes, mode, options, key, opt: options.find(o => o.key === key) };
}
function shareOf(A, ids, u) {
  let v = 0, any = false;
  ids.forEach(id => { const k = A.items.indexOf(id); if (k >= 0 && isNum(A.votes[k][u])) { v += A.votes[k][u]; any = true; } });
  return any && A.valid[u] > 0 ? v / A.valid[u] : null;
}
function changeValues(L) {
  const C = changeSetup(), out = new Array(L.n).fill(null);
  if (!C.D) return { v: out, lead: [], C };
  const A = L.A, B = L.kind === "g" ? C.D.units.gmina : C.D.units.okreg41, o = C.opt;
  for (let u = 0; u < L.n; u++) {
    let a, b;
    if (o.turnout) {
      a = A.electorate[u] > 0 ? A.voters[u] / A.electorate[u] : null;
      b = B.electorate[u] > 0 ? B.voters[u] / B.electorate[u] : null;
    } else {
      a = shareOf(A, o.cur, u); b = shareOf(B, o.cmp, u);
      // where only one side stood in a unit, the other counts as nothing there
      if (a == null && b != null && A.valid[u] > 0) a = 0;
      if (b == null && a != null && B.valid[u] > 0) b = 0;
    }
    out[u] = isNum(a) && isNum(b) ? a - b : null;
  }
  return { v: out, lead: [], C };
}
function changeNote(C, L) {
  if (!C || !C.D) return "";
  const o = C.opt, bits = [];
  bits.push(L.kind === "g" ? "Both elections on today's gminas." : "Both elections added up by today's 41 Sejm constituencies, without votes abroad and on ships.");
  if (o.turnout) bits.push("The change in turnout, in points.");
  else if (C.mode === "lineage") {
    const missing = o.cmp.filter(id => !C.D.units.gmina.items.includes(id));
    bits.push("Each list against the lists it continues at the previous election, by the lineages of Party system indicators.");
    if (missing.length) bits.push(`Part of what ${o.name} continues stood as lists too small to map, so its change is overstated.`);
  } else if (C.mode === "same") bits.push(`The same ${EL.type === "presidential" ? "candidate" : "list"} in both.`);
  else bits.push("The lists or candidates of the same camp (colour) in each election, added up; lists under 0.5% are among the others and left out.");
  bits.push("Gains are in the party's colour and losses in olive, a colour no party has.");
  return bits.join(" ");
}

// ---- "Widać zabory!": results by 1914 partition zone ----------------------------------------
function zoneSums(A) { // each unit, or a 1990s joint unit once, in its first gmina's zone
  const out = Object.fromEntries(ZONES.map(([z]) => [z, { n: 0, electorate: 0, voters: 0, valid: 0, votes: A.items.map(() => 0) }]));
  const seen = new Set();
  for (let u = 0; u < A.valid.length; u++) {
    const g = A.group ? A.group[u] : -1, key = g >= 0 ? "g" + g : u;
    if (seen.has(key)) continue;
    seen.add(key);
    const z = out[G.gminas.zone[u]];
    if (!z || !isNum(A.valid[u])) continue;
    z.n++; z.electorate += A.electorate[u] || 0; z.voters += A.voters[u] || 0; z.valid += A.valid[u] || 0;
    A.items.forEach((id, k) => { z.votes[k] += A.votes[k][u] || 0; });
  }
  return out;
}
function renderZones() {
  const box = $("zonetable");
  if (!S.z || (S.l === "c" && S.v === "c") || isPolls()) { box.innerHTML = ""; return; }
  const A = unitsOf().gmina, Z = zoneSums(A), m = measure(), its = mapItems();
  const sh = (z, ids, AA = A) => { let v = 0; ids.forEach(id => { const k = AA.items.indexOf(id); if (k >= 0) v += z.votes[k]; }); return z.valid > 0 ? v / z.valid : null; };
  let head, cell;
  if (m === "turnout") { head = "Turnout"; cell = z => pct(z.voters / z.electorate); }
  else if (m === "share") { const it = its[S.i]; head = nameOf(it); cell = z => pct(sh(z, [it.id])); }
  else if (m === "h2h") { const a = its[S.i], b = its[S.j]; head = `${nameOf(a)}'s lead over ${nameOf(b)}`; cell = z => signedPts(sh(z, [a.id]) - sh(z, [b.id])) + " points"; }
  else if (m === "chg") {
    const C = changeSetup(); if (!C.D) { box.innerHTML = ""; return; }
    const B = C.D.units.gmina, ZB = zoneSums(B), o = C.opt;
    head = `${o.name}, since ${contestName(C.cmp)}`;
    cell = (z, k) => { const b = ZB[k]; const x = o.turnout ? z.voters / z.electorate - b.voters / b.electorate : sh(z, o.cur) - sh(b, o.cmp, B); return signedPts(x) + " points"; };
  } else {
    head = "Leader";
    cell = z => {
      let best = -1; A.items.forEach((id, k) => { if (id !== "other" && (best < 0 || z.votes[k] > z.votes[best])) best = k; });
      const it = itemById(A.items[best]);
      return `<span class="dot" style="background:${colOf(it)}"></span> ${esc(nameOf(it))} ${pct(z.votes[best] / z.valid)}`;
    };
  }
  box.innerHTML = `<div class="charttitle" style="margin:10px 0 4px">By partition zone of 1914</div><div class="scroll"><table class="t">
    <thead><tr><th class="l">Zone</th><th>${esc(head)}</th><th>Units</th><th>Electorate</th></tr></thead>
    <tbody>${ZONES.map(([k, nm]) => `<tr><td class="l">${nm}</td><td>${cell(Z[k], k)}</td><td>${Z[k].n}</td><td>${n0(Z[k].electorate)}</td></tr>`).join("")}</tbody></table></div>
    <p class="note" style="margin-top:4px">Each of today's gminas in the empire its centre lay in, in 1914 (CShapes 2.0; Schvitz et al. 2022), added up from the gminas without votes abroad. A 1990s unit that covered several of today's gminas counts once.</p>`;
}

// ---- unit profiles and search ---------------------------------------------------------------
const PROF = new Map();
let PROF_TOKEN = 0;
async function loadProfile(file) {
  if (!PROF.has(file)) { const r = await fetch(file); if (!r.ok) throw new Error(file); PROF.set(file, await r.json()); }
  return PROF.get(file);
}
function profileTarget() {
  if (isPolls() || !S.sel || !MAPV || S.sel.l !== S.l + S.b + S.v) return null;
  const L = MAPV.L;
  if (L.kind === "g") return { kind: "g", u: S.sel.u, woj: G.gminas.voivodeship[S.sel.u] };
  if (L.set !== "sejm2001") return { kind: "none" };
  const s = G.sets.sejm2001, okreg = s.okreg[S.sel.u];
  return { kind: "c", okreg, woj: G.gminas.voivodeship[s.members.indexOf(okreg)] };
}
async function renderProfile() {
  const card = $("profilecard"), t = profileTarget(), token = ++PROF_TOKEN;
  if (!t) { card.hidden = true; return; }
  card.hidden = false;
  if (t.kind === "none") { card.innerHTML = `<span class="label">Every election</span><p class="note">Histories are kept for today's gminas and today's 41 constituencies: switch the boundaries to today's 41 for a constituency's.</p>`; return; }
  const v = E.voivodeships.find(x => x.name === t.woj);
  if (!PROF.has(v.file)) card.innerHTML = `<span class="label">Every election</span><p class="note">Loading…</p>`;
  let P;
  try { P = await loadProfile(v.file); } catch (e) { card.innerHTML = `<p class="note">Could not load the history.</p>`; return; }
  if (token !== PROF_TOKEN) return;
  const entry = t.kind === "g" ? P.units.gminas.find(x => x.i === t.u) : P.units.okregs.find(x => x.okreg === t.okreg);
  if (!entry) { card.hidden = true; return; }
  card.innerHTML = profileHtml(entry, t);
}
function profileCamps(entry) { // the camps the unit has a share for, the most often first; Other
  // and Independent mix unrelated lists, so they are left out
  const n = {}, skip = new Set(["yes", "no", "Other", "Independent"]);
  E.contests.forEach((c, k) => (entry.s[k] || []).forEach((v, ci) => { const cp = c.camps[ci]; if (v != null && !skip.has(cp)) n[cp] = (n[cp] || 0) + 1; }));
  return Object.keys(n).sort((a, b) => n[b] - n[a] || a.localeCompare(b));
}
function profileHtml(entry, t) {
  const camps = profileCamps(entry), it = mapItems()[S.i];
  const camp = camps.includes(S.pc) ? S.pc : it && camps.includes(it.label) ? it.label : camps[0];
  const nm = t.kind === "g" ? unitName("g", null, t.u) : { title: `${t.okreg}. ${G.sets.sejm2001.name[G.sets.sejm2001.okreg.indexOf(t.okreg)]}`, sub: "today's 41 Sejm constituencies" };
  const rows = E.contests.map((c, k) => {
    const li = entry.l[k], ci = c.camps.indexOf(camp), s = entry.s[k];
    return {
      c, k, e: elOf(c.election), date: c.date,
      t: entry.t[k] != null ? entry.t[k] / 1000 : null, nt: c.national.turnout,
      lead: li != null && li >= 0 ? { name: c.items[li], col: campCol(c.item_camps[li], c.election, c.item_ids[li]) } : null,
      sh: ci >= 0 && s && s[ci] != null ? s[ci] / 1000 : null, nat: ci >= 0 ? c.national.camps[ci] : null,
      joint: (entry.j || []).includes(k)
    };
  });
  const col = E.labels[camp] || E.labels.Other;
  return `<div style="display:flex;justify-content:space-between;gap:8px"><span class="label">Every election</span></div>
    <div style="font-size:14px"><b style="font-weight:500">${esc(nm.title)}</b><div class="note">${esc(nm.sub)}</div></div>
    <div class="ctrl"><span>Camp</span><select id="profcamp" aria-label="Camp to follow">${camps.map(cp => `<option${cp === camp ? " selected" : ""}>${esc(cp)}</option>`).join("")}</select></div>
    ${profileChart(rows, col)}
    <div class="legend"><span><span class="dot" style="background:${col}"></span>here</span><span><span class="dot" style="background:#fff;border:1.5px solid #767676"></span>Poland</span><span class="muted">lines: Sejm (solid), presidential round 1 (dashed), EP (dotted)</span></div>
    <div class="proftable"><table class="t"><thead><tr><th class="l">Election</th><th>Turnout</th><th class="l">Leader</th><th>${esc(camp)}</th></tr></thead><tbody>
    ${rows.slice().reverse().map(r => `<tr data-c="${r.c.id}" style="cursor:pointer"><td class="l${r.joint ? " j" : ""}" title="${r.joint ? "reported jointly with a neighbour in this election" : ""}"><button type="button" class="tbtn">${esc(contestName(r.c.id))}</button>${r.joint ? " *" : ""}</td>
      <td>${pct(r.t)} <span class="muted">${pct(r.nt, 0)}</span></td>
      <td class="l">${r.lead ? `<span class="dot" style="background:${r.lead.col}"></span> ${esc(r.lead.name)}` : NA}</td>
      <td>${r.sh != null ? pct(r.sh) : '<span class="na">–</span>'} <span class="muted">${r.nat != null ? pct(r.nat, 0) : ""}</span></td></tr>`).join("")}
    </tbody></table></div>
    <p class="note">Here against Poland (in grey). ${rows.some(r => r.joint) ? "* Reported jointly with a neighbouring gmina, so the figures are the joint unit's. " : ""}Camps add up the lists or candidates of one colour. Click a row to open that election.</p>`;
}
function profileChart(rows, col) {
  const W = 320, H = 150, m = { l: 30, r: 8, t: 8, b: 18 }, pts = rows.filter(r => r.sh != null && r.e.type !== "referendum");
  if (!pts.length) return "";
  const yf = s => { const t = dparts(s); return t.getUTCFullYear() + t.getUTCMonth() / 12; };
  const x0 = 1990, x1 = 2026, X = d => m.l + (yf(d) - x0) / (x1 - x0) * (W - m.l - m.r);
  const hi = Math.max(0.1, Math.ceil(Math.max(...pts.map(r => Math.max(r.sh, r.nat || 0))) * 10) / 10), Y = v => m.t + (1 - v / hi) * (H - m.t - m.b);
  const o = [];
  ticks(0, hi, 4).forEach(v => o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${Y(v)}" y2="${Y(v)}" stroke="#EEEEEE"/><text x="${m.l - 4}" y="${Y(v) + 3.5}" text-anchor="end" font-size="9.5" fill="#767676">${Math.round(100 * v)}%</text>`));
  [1990, 2000, 2010, 2020].forEach(y => o.push(`<text x="${m.l + (y - x0) / (x1 - x0) * (W - m.l - m.r)}" y="${H - 4}" text-anchor="middle" font-size="9.5" fill="#767676">${y}</text>`));
  const series = [["sejm", r => r.e.type === "sejm", ""], ["pres", r => r.e.type === "presidential" && r.c.part === 1, "4 3"], ["ep", r => r.e.type === "ep", "1.5 2.5"]];
  series.forEach(([, f, dash]) => {
    const s = pts.filter(f);
    if (s.length > 1) o.push(`<polyline points="${s.map(r => `${X(r.date).toFixed(1)},${Y(r.sh).toFixed(1)}`).join(" ")}" fill="none" stroke="${col}" stroke-width="1.4" stroke-dasharray="${dash}"/>`);
  });
  pts.forEach(r => {
    if (r.nat != null) o.push(`<circle cx="${X(r.date).toFixed(1)}" cy="${Y(r.nat).toFixed(1)}" r="2.6" fill="#fff" stroke="#767676" stroke-width="1"/>`);
    o.push(`<circle cx="${X(r.date).toFixed(1)}" cy="${Y(r.sh).toFixed(1)}" r="3" fill="${col}"><title>${esc(contestName(r.c.id))}: ${pct(r.sh)} (Poland ${pct(r.nat)})</title></circle>`);
  });
  return `<svg viewBox="0 0 ${W} ${H}" font-family="${FONT}" role="img" aria-label="The camp's share here and in Poland at each election">${o.join("")}</svg>`;
}
function bindProfile() {
  $("profilecard").addEventListener("change", e => { if (e.target.id === "profcamp") { S.pc = e.target.value; renderProfile(); } });
  $("profilecard").addEventListener("click", e => {
    const r = e.target.closest("tr[data-c]"); if (!r) return;
    const c = CONTEST[r.dataset.c], sel = S.sel;
    S.p = c.part - 1; S.m = null; S.i = 0; S.j = 1;
    setElection(c.election, true).then(() => { S.sel = sel; drawHighlight(); renderUnitCard(); });
  });
}
const fold = s => s.normalize("NFD").replace(/[̀-ͯ]/g, "").replace(/ł/g, "l").replace(/Ł/g, "L").toLowerCase();
let NAMES = null;
function searchGminas(q) {
  if (!NAMES) NAMES = G.gminas.name.map(fold);
  const n = fold(q.trim());
  if (n.length < 2) return [];
  const hits = [];
  NAMES.forEach((nm, i) => { if (nm.startsWith(n)) hits.push([0, i]); else if (nm.includes(n)) hits.push([1, i]); });
  return hits.sort((a, b) => a[0] - b[0] || G.gminas.name[a[1]].localeCompare(G.gminas.name[b[1]], "pl")).slice(0, 12).map(h => h[1]);
}
function pickGmina(u) {
  $("gresults").innerHTML = ""; $("gsearch").value = G.gminas.name[u];
  if (S.l !== "g") { S.l = "g"; S.sel = null; }
  S.sel = { u, l: S.l + S.b + S.v };
  renderMap(); renderUnitCard(); saveHash();
  $("mapbox").scrollIntoView({ behavior: "smooth", block: "nearest" });
}
function bindSearch() {
  const inp = $("gsearch"), ul = $("gresults");
  let on = -1;
  const show = () => {
    const hits = searchGminas(inp.value); on = -1;
    ul.innerHTML = hits.map(u => { const nm = unitName("g", null, u); return `<li role="option" data-u="${u}">${esc(nm.title)}<span>${esc(nm.sub)}</span></li>`; }).join("");
  };
  inp.addEventListener("input", show);
  inp.addEventListener("keydown", e => {
    const li = [...ul.querySelectorAll("li")];
    if (e.key === "ArrowDown" || e.key === "ArrowUp") { e.preventDefault(); if (!li.length) return; on = (on + (e.key === "ArrowDown" ? 1 : -1) + li.length) % li.length; li.forEach((x, k) => x.classList.toggle("on", k === on)); }
    else if (e.key === "Enter") { const x = li[on >= 0 ? on : 0]; if (x) pickGmina(+x.dataset.u); }
    else if (e.key === "Escape") ul.innerHTML = "";
  });
  ul.addEventListener("click", e => { const x = e.target.closest("li[data-u]"); if (x) pickGmina(+x.dataset.u); });
  document.addEventListener("click", e => { if (!e.target.closest(".search")) ul.innerHTML = ""; });
}

// ---- votes by local characteristics ---------------------------------------------------------
let COV = null, COV_LOADING = null, SCAT = null, SCAT_TOK = 0;
const COV_GROUPS = new Map();
// Muted tones for the partition zones and the sides of the 1937 border, not party colours: Germany is
// the same orange in both, and Poland's red is darker than it so the two differ in lightness too
const ZONECOL = { russian: "#141414", german: "#D4873E", austrian: "#4E79A7" };
const ZONES37 = [["poland", "Poland"], ["germany", "Germany"], ["danzig", "Free City of Danzig"]];
const ZONECOL37 = { poland: "#A8423C", germany: "#D4873E", danzig: "#4E79A7" };
// covariates.json is the index; each group's values are in its own file, fetched when one of
// its characteristics is first chosen
function loadCov() {
  if (COV) return Promise.resolve(COV);
  if (!COV_LOADING) COV_LOADING = fetch("covariates.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); })
    .then(d => { d.values = d.values || {}; COV = d; return d; }).catch(e => { COV_LOADING = null; throw e; });
  return COV_LOADING;
}
function loadCovGroup(id) {
  const g = COV.groups.find(x => x.id === id);
  if (!COV_GROUPS.has(id)) COV_GROUPS.set(id, fetch(g.file).then(r => { if (!r.ok) throw new Error(r.status); return r.json(); })
    .then(d => { Object.assign(COV.values, d.values); return d; }).catch(e => { COV_GROUPS.delete(id); throw e; }));
  return COV_GROUPS.get(id);
}
// How the points are coloured: as chosen, or by 1914 zone while "Widać zabory!" is on
const scatColour = () => S.zc || (S.z ? "1914" : "list");
// The weighted correlation of x and y
function wcor(p) {
  const sw = sum(p.map(q => q.w)), mx = sum(p.map(q => q.w * q.x)) / sw, my = sum(p.map(q => q.w * q.y)) / sw;
  let sxy = 0, sxx = 0, syy = 0;
  p.forEach(q => { sxy += q.w * (q.x - mx) * (q.y - my); sxx += q.w * (q.x - mx) ** 2; syy += q.w * (q.y - my) ** 2; });
  return sxx > 0 && syy > 0 ? sxy / Math.sqrt(sxx * syy) : null;
}
// A weighted local linear fit (LOESS, tricube, span 0.75), as Party ideology's smoothFit() but with
// each gmina weighted by its electorate; a straight line under 20 points
function wsmooth(pts, span = 0.75, k = 48) {
  const n = pts.length;
  if (n < 3) return null;
  const x0 = Math.min(...pts.map(p => p.x)), x1 = Math.max(...pts.map(p => p.x));
  if (x1 - x0 < 1e-9) return null;
  const wls = (w, g) => {
    let sw = 0, sx = 0, sy = 0, sxx = 0, sxy = 0;
    pts.forEach((p, i) => { const v = w[i] * p.w; sw += v; sx += v * p.x; sy += v * p.y; sxx += v * p.x * p.x; sxy += v * p.x * p.y; });
    if (sw <= 0) return null;
    const den = sw * sxx - sx * sx;
    if (Math.abs(den) < 1e-12) return sy / sw;
    const b = (sw * sxy - sx * sy) / den;
    return (sy - b * sx) / sw + b * g;
  };
  if (n < 20) return [x0, x1].map(g => ({ x: g, y: wls(pts.map(() => 1), g) }));
  const q = Math.max(3, Math.floor(span * n)), out = [];
  for (let i = 0; i < k; i++) {
    const g = x0 + (x1 - x0) * i / (k - 1);
    const d = pts.map(p => Math.abs(p.x - g)), h = Math.max([...d].sort((a, b) => a - b)[q - 1], 1e-9) * 1.0001;
    const y = wls(d.map(v => v < h ? (1 - (v / h) ** 3) ** 3 : 0), g);
    if (isNum(y)) out.push({ x: g, y });
  }
  return out.length > 1 ? out : null;
}
// The y of each gmina: the map's measure, with the leader view showing the chosen list's share
function scatterY() {
  const U = unitsOf(), A = U.gmina, n = A.valid.length, m = measure(), its = mapItems();
  const L = { kind: "g", A, set: null, n };
  if (m === "turnout") return { v: A.valid.map((x, u) => A.electorate[u] > 0 ? A.voters[u] / A.electorate[u] : null), label: "Turnout", col: TURN_GAIN, div: false };
  if (m === "chg") { const c = changeValues(L); return c.C && c.C.opt ? { v: c.v, label: `${c.C.opt.name}: change since ${contestName(c.C.cmp)}, points`, col: c.C.opt.col, div: true } : null; }
  if (m === "h2h") { const a = its[S.i], b = its[S.j], v = valuesFor(L).v; return { v, label: `${nameOf(a)}'s lead over ${nameOf(b)}, points`, col: colOf(a), div: true }; }
  const it = its[S.i], k = A.items.indexOf(it.id);
  return { v: A.valid.map((x, u) => isNum(A.votes[k][u]) && x > 0 ? A.votes[k][u] / x : null), label: `${nameOf(it)}, share of the valid votes`, col: colOf(it), div: false };
}
async function renderScatter() {
  const card = $("scatcard"), svg = $("scat");
  if (CUR.facts.facts_only || isPolls()) { $("whosec").hidden = true; return; }
  $("whosec").hidden = false;
  // The list, candidate or answer plotted, as on the map: choosing one here also maps it
  const its = mapItems(), ms = measure(), cur = ms === "turnout" ? "turnout" : ms === "chg" ? "chg" : String(S.i);
  $("scatitemlab").textContent = itemLabel();
  $("scatitem").innerHTML = (ms === "chg" ? `<option value="chg" selected>Change, as on the map</option>` : "") +
    its.map((it, k) => `<option value="${k}"${String(k) === cur ? " selected" : ""}>${esc(nameOf(it))} (${pct(it.pct)})</option>`).join("") +
    `<option value="turnout"${cur === "turnout" ? " selected" : ""}>Turnout</option>`;
  const col = scatColour();
  $("scatcol").value = col;
  const tok = ++SCAT_TOK;
  try { await loadCov(); } catch (e) { card.innerHTML = `<p class="note">Could not load the gminas' characteristics.</p>`; return; }
  const cid = partOf().contest_id, years = COV.years[cid] || {};
  const vars = COV.vars;
  if (!vars.find(v => v.id === S.cv)) S.cv = "higher_ed";
  $("covar").innerHTML = COV.groups.map(g => [g, vars.filter(v => v.group === g.id)]).filter(([, vs]) => vs.length)
    .map(([g, vs]) => `<optgroup label="${esc(g.label)}">${vs.map(v => `<option value="${v.id}"${v.id === S.cv ? " selected" : ""}>${esc(v.label)} (${years[v.id]})</option>`).join("")}</optgroup>`).join("");
  const V = vars.find(v => v.id === S.cv), yr = years[V.id];
  try { await loadCovGroup(V.group); } catch (e) { card.innerHTML = `<p class="note">Could not load the gminas' characteristics.</p>`; return; }
  if (tok !== SCAT_TOK) return;   // a later render has started
  const xs = COV.values[V.id][String(yr)], Y = scatterY();
  if (!Y) { svg.innerHTML = ""; card.innerHTML = `<p class="note">Loading…</p>`; return; }
  const A = unitsOf().gmina, logx = V.id === "density";
  const pts = [];
  let joint = 0;
  for (let u = 0; u < A.valid.length; u++) {
    if (A.group[u] >= 0) { joint++; continue; }
    const x = xs[u], y = Y.v[u];
    if (!isNum(x) || !isNum(y) || (logx && x <= 0)) continue;
    pts.push({ u, x: logx ? Math.log10(x) : x, x0: x, y, w: A.electorate[u] || 0 });
  }
  const W = boxW("scatbox"), H = Math.round(clamp(W * 0.62, 300, 520)), m = { l: 46, r: 14, t: 12, b: 40 };
  const qx = [quantile(pts.map(p => p.x), 0.002), quantile(pts.map(p => p.x), 0.998)], qy = [Math.min(...pts.map(p => p.y)), Math.max(...pts.map(p => p.y))];
  const xt = ticks(qx[0], qx[1], 5), yt = ticks(Y.div ? Math.min(qy[0], -0.01) : 0, qy[1], 5);
  const xd = [Math.min(qx[0], xt[0]), Math.max(qx[1], xt[xt.length - 1])], yd = [Math.min(yt[0], qy[0]), Math.max(yt[yt.length - 1], qy[1])];
  const X = v => m.l + (clamp(v, xd[0], xd[1]) - xd[0]) / (xd[1] - xd[0]) * (W - m.l - m.r), Yy = v => H - m.b - (v - yd[0]) / (yd[1] - yd[0]) * (H - m.t - m.b);
  const emax = Math.max(...pts.map(p => p.w)), rad = p => 1 + 7 * Math.sqrt(p.w / emax);
  const fmtx = v => logx ? (10 ** v >= 1000 ? (10 ** v / 1000).toFixed(0) + "k" : (+(10 ** v).toPrecision(2)).toString()) : V.unit.startsWith("%") ? Math.round(v) + "%" : V.unit.startsWith("PLN") && Math.abs(v) >= 1000 ? +(v / 1000).toFixed(1) + "k" : (+v.toFixed(1)).toString();
  const fmty = v => Y.div ? (v > 0 ? "+" : v < 0 ? "−" : "") + Math.abs(Math.round(100 * v)) : Math.round(100 * v) + "%";
  const o = [];
  (logx ? [0, 1, 2, 3, 4].filter(v => v >= xd[0] - 1e-9 && v <= xd[1] + 1e-9) : xt).forEach(v => o.push(`<line x1="${X(v)}" x2="${X(v)}" y1="${m.t}" y2="${H - m.b}" stroke="#EEEEEE"/><text x="${X(v)}" y="${H - m.b + 15}" text-anchor="middle" font-size="11" fill="#767676">${fmtx(v)}</text>`));
  yt.forEach(v => o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${Yy(v)}" y2="${Yy(v)}" stroke="${Y.div && v === 0 ? "#B5B5B5" : "#EEEEEE"}"/><text x="${m.l - 6}" y="${Yy(v) + 4}" text-anchor="end" font-size="11" fill="#767676">${fmty(v)}</text>`));
  o.push(`<text x="${(m.l + W - m.r) / 2}" y="${H - 6}" text-anchor="middle" font-size="11.5" fill="#4B4B4B">${esc(V.label)}, ${esc(V.unit)}${logx ? " (log scale)" : ""}, ${yr}</text>`);
  const zon = col !== "list", fillOf = p => col === "1914" ? ZONECOL[G.gminas.zone[p.u]] : col === "1937" ? ZONECOL37[COV.zones.zone_1937[p.u]] : Y.col;
  pts.slice().sort((a, b) => b.w - a.w).forEach(p => o.push(`<circle cx="${X(p.x).toFixed(1)}" cy="${Yy(p.y).toFixed(1)}" r="${rad(p).toFixed(2)}" fill="${fillOf(p)}" fill-opacity="${zon ? 0.6 : 0.42}" stroke="#fff" stroke-width="0.4"/>`));
  const fit = wsmooth(pts);
  if (fit) o.push(`<polyline points="${fit.filter(f => f.x >= xd[0] && f.x <= xd[1]).map(f => `${X(f.x).toFixed(1)},${Yy(f.y).toFixed(1)}`).join(" ")}" fill="none" stroke="#141414" stroke-width="2"/>`);
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.setAttribute("font-family", FONT);
  svg.innerHTML = o.join("") + `<g class="hov" pointer-events="none"></g>`;
  SCAT = { pts, X, Yy, W, rad, V, Y, logx };
  const r = wcor(pts), gap = (COV.vars.find(v => v.id === V.id).years || []).length ? Math.abs(yr - EL.year) : 0;
  card.innerHTML = `<span class="label">The pattern</span>
    <div><span class="bigno">${isNum(r) ? (r < 0 ? "−" : "") + Math.abs(r).toFixed(2) : NA}</span> <span class="note">correlation across ${n0(pts.length)} gminas, each weighted by its electorate</span></div>
    <p class="note"><b style="font-weight:500;color:var(--ink)">Up:</b> ${esc(Y.label)}. <b style="font-weight:500;color:var(--ink)">Across:</b> ${esc(V.label)} (${esc(V.unit)}), ${yr}${gap > 2 ? `, the nearest year to the election (${gap} years ${yr > EL.year ? "after" : "before"} it)` : ""}. The line is a weighted local fit.</p>
    ${col === "1914" ? `<div class="legend">${ZONES.map(([k, nm]) => `<span><span class="dot" style="background:${ZONECOL[k]}"></span>${nm}</span>`).join("")}</div><p class="note">Points take their gmina's partition zone of 1914${S.zc ? "" : ` while "Widać zabory!" is on`}.</p>` : ""}
    ${col === "1937" ? `<div class="legend">${ZONES37.map(([k, nm]) => `<span><span class="dot" style="background:${ZONECOL37[k]}"></span>${nm}</span>`).join("")}</div><p class="note">Points take the side of the 1937 border on which their gmina's centre lies (CShapes 2.0).</p>` : ""}
    <p class="note">${joint ? `${joint} gminas reported jointly with a neighbour in this election are left out. ` : ""}These are associations between places, not between voters: a party that does well where many graduates live need not do well among graduates (the ecological fallacy).</p>
    <p class="note">${V.note ? esc(V.note) + " " : ""}Source: ${esc(V.source)}. ${esc(COV.meta.note)}</p>`;
}
function bindScatter() {
  $("covar").addEventListener("change", e => { S.cv = e.target.value; renderScatter(); saveHash(); });
  $("scatitem").addEventListener("change", e => {
    const v = e.target.value;
    if (v === "chg") return;
    if (v === "turnout") S.m = "turnout";
    else {
      S.i = +v; if (S.j === S.i) S.j = S.i === 0 ? 1 : 0;
      if (["turnout", "chg"].includes(measure())) S.m = "share";
    }
    renderMap(); renderUnitCard(); renderResults(); renderScatter(); renderData(); saveHash();
  });
  $("scatcol").addEventListener("change", e => { S.zc = e.target.value; renderScatter(); saveHash(); });
  const svg = $("scat"), box = $("scatbox"), tip = box.querySelector(".tip");
  const near = ev => {
    if (!SCAT) return null;
    const r = svg.getBoundingClientRect(), k = SCAT.W / r.width, mx = (ev.clientX - r.left) * k, my = (ev.clientY - r.top) * k;
    let best = null, bd = 10;
    SCAT.pts.forEach(p => { const d = Math.hypot(SCAT.X(p.x) - mx, SCAT.Yy(p.y) - my) - SCAT.rad(p); if (d < bd) { bd = d; best = p; } });
    return best;
  };
  const hov = p => { const g = $("map").querySelector("#hov"); if (g) g.innerHTML = p && S.l === "g" ? `<path class="hl" d="${G.gminas.d[p.u]}" style="stroke:#8B0000;stroke-width:2.2"/>` : ""; };
  svg.addEventListener("mousemove", ev => {
    const p = near(ev);
    if (!p) { tip.hidden = true; svg.querySelector(".hov").innerHTML = ""; hov(null); return; }
    const nm = unitName("g", null, p.u);
    svg.querySelector(".hov").innerHTML = `<circle cx="${SCAT.X(p.x)}" cy="${SCAT.Yy(p.y)}" r="${SCAT.rad(p) + 2}" fill="none" stroke="#141414" stroke-width="1.6"/>`;
    placeTip(tip, box, ev, `<b>${esc(nm.title)}</b><div class="sub">${esc(nm.sub)}</div>${tipRow(null, SCAT.Y.div ? "Value" : "Share", SCAT.Y.div ? signedPts(p.y) + " points" : pct(p.y))}${tipRow(null, SCAT.V.label, (+p.x0.toPrecision(4)).toLocaleString("en-GB") + " " + SCAT.V.unit)}${tipRow(null, "Electorate", n0(p.w))}`);
    hov(p);
  });
  svg.addEventListener("mouseleave", () => { tip.hidden = true; svg.querySelector(".hov").innerHTML = ""; hov(null); });
  svg.addEventListener("click", ev => {
    const p = near(ev); if (!p) return;
    if (S.l !== "g") { S.l = "g"; renderMap(); }
    S.sel = { u: p.u, l: S.l + S.b + S.v }; drawHighlight(); renderUnitCard();
  });
}

// ---- all elections --------------------------------------------------------------------------
let TURN = null;
function renderAllElections() {
  const rows = ELECTIONS.slice().reverse();
  $("alltable").innerHTML = `<thead><tr><th class="l">Date</th><th class="l">Election</th><th>Turnout</th><th class="l">Won by / largest</th><th>Share</th><th>Lead, points</th><th>Effective number of lists</th></tr></thead>
    <tbody>${rows.map(e => {
      const o = e.overview || {};
      const res = e.facts_only ? '<span class="na">seats only</span>' : e.type === "referendum" ? `yes ${pct(o.yes_pct)}${e.parts.length > 1 ? " (question 1)" : ""}` : `<span class="dot" style="background:${winnerCol(e)}"></span> ${esc(o.winner || "")}`;
      return `<tr data-e="${e.id}" class="${e.id === S.e ? "sel" : ""}" style="cursor:pointer"><td class="l">${dlong(e.date)}${e.date2 && e.type === "presidential" ? `<br><span class="muted">${dlong(e.date2)}</span>` : ""}</td>
        <td class="l"><button type="button" class="tbtn"><span class="tdot ${e.type}"></span> ${TYPES[e.type]}</button>${e.early ? ' <span class="muted">(early)</span>' : ""}</td>
        <td>${pct(o.turnout)}${isNum(o.turnout2) ? `<br><span class="muted">${pct(o.turnout2)}</span>` : ""}</td>
        <td class="l">${res}</td><td>${e.type === "referendum" || e.facts_only ? "" : pct(o.winner_pct)}</td>
        <td>${isNum(o.margin_pp) ? o.margin_pp.toFixed(1) : ""}</td><td>${isNum(o.enp) ? o.enp.toFixed(2) : ""}</td></tr>`;
    }).join("")}</tbody>`;
  turnoutChart();
}
function winnerCol(e) { // the winner's colour, with the 1990 finalists' own colours
  const own = OWN_ALL[e.id];
  if (own) { const c2 = E.contests.find(c => c.election === e.id && c.part === 2); if (c2) return own[c2.item_ids[0]]; }
  return E.labels[e.overview.winner_label] || E.labels.Other;
}
function turnoutChart() {
  const W = boxW("turnbox"), H = Math.round(clamp(W * 0.34, 210, 320)), m = { l: 38, r: 12, t: 10, b: 24 };
  const yf = s => { const t = dparts(s); return t.getUTCFullYear() + (t - Date.UTC(t.getUTCFullYear(), 0, 1)) / 31557600000; };
  const x0 = 1988.5, x1 = 2026.5, X = d => m.l + (yf(d) - x0) / (x1 - x0) * (W - m.l - m.r), Y = v => m.t + (1 - v / 0.8) * (H - m.t - m.b);
  const pts = [];
  ELECTIONS.forEach(e => {
    const o = e.overview || {};
    if (isNum(o.turnout)) pts.push({ e, d: e.date, v: o.turnout, type: e.type, round: 1 });
    if (e.type === "presidential" && isNum(o.turnout2)) pts.push({ e, d: e.date2, v: o.turnout2, type: e.type, round: 2 });
  });
  const o = [];
  [0, 0.2, 0.4, 0.6, 0.8].forEach(v => o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${Y(v)}" y2="${Y(v)}" stroke="#EEEEEE"/><text x="${m.l - 6}" y="${Y(v) + 4}" text-anchor="end" font-size="11" fill="#767676">${100 * v}%</text>`));
  for (let y = 1990; y <= 2025; y += 5) o.push(`<text x="${X(y + "-07-01")}" y="${H - 6}" text-anchor="middle" font-size="11" fill="#767676">${y}</text>`);
  o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${Y(0.5)}" y2="${Y(0.5)}" stroke="#9c2d74" stroke-width="1" stroke-dasharray="3 3"/><text x="${W - m.r}" y="${Y(0.5) - 4}" text-anchor="end" font-size="10.5" fill="#9c2d74">50%: a referendum binds</text>`);
  ["sejm", "presidential", "ep"].forEach(t => {
    const s = pts.filter(p => p.type === t && p.round === 1);
    o.push(`<polyline points="${s.map(p => `${X(p.d).toFixed(1)},${Y(p.v).toFixed(1)}`).join(" ")}" fill="none" stroke="${TYPECOL[t]}" stroke-width="1.6"/>`);
  });
  pts.forEach(p => {
    const sel = p.e.id === S.e;
    o.push(`<circle cx="${X(p.d).toFixed(1)}" cy="${Y(p.v).toFixed(1)}" r="${sel ? 5.5 : 4}" fill="${p.round === 2 ? "#fff" : TYPECOL[p.type]}" stroke="${TYPECOL[p.type]}" stroke-width="${sel ? 2.4 : 1.6}"/>`);
  });
  TURN = { pts, X, Y, W };
  const svg = $("turnchart");
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.setAttribute("font-family", FONT);
  svg.innerHTML = o.join("") + `<g class="hov" pointer-events="none"></g>`;
  $("turnlegend").innerHTML = ["sejm", "presidential", "ep", "referendum"].map(t => `<span><span class="dot" style="background:${TYPECOL[t]}"></span>${TYPES[t]}</span>`).join("") + `<span><span class="dot" style="background:#fff;border:1.5px solid ${TYPECOL.presidential}"></span>presidential round 2</span>`;
}
function bindAll() {
  $("alltable").addEventListener("click", e => { const r = e.target.closest("tr[data-e]"); if (r && r.dataset.e !== S.e) { setElection(r.dataset.e); $("glance").scrollIntoView({ behavior: "smooth" }); } });
  const svg = $("turnchart"), box = $("turnbox"), tip = box.querySelector(".tip");
  const near = ev => {
    if (!TURN) return null;
    const r = svg.getBoundingClientRect(), k = TURN.W / r.width, mx = (ev.clientX - r.left) * k, my = (ev.clientY - r.top) * k;
    let best = null, bd = 14;
    TURN.pts.forEach(p => { const d = Math.hypot(TURN.X(p.d) - mx, TURN.Y(p.v) - my); if (d < bd) { bd = d; best = p; } });
    return best;
  };
  svg.addEventListener("mousemove", ev => {
    const p = near(ev);
    if (!p) { tip.hidden = true; return; }
    placeTip(tip, box, ev, `<b>${esc(TYPES[p.type])}${p.type === "presidential" ? ", round " + p.round : ""}</b><div class="sub">${dlong(p.d)}</div>${tipRow(null, "Turnout", pct(p.v, 2))}`);
  });
  svg.addEventListener("mouseleave", () => { tip.hidden = true; });
  svg.addEventListener("click", ev => {
    const p = near(ev); if (!p) return;
    S.p = p.round - 1; S.m = null; S.i = 0; S.j = 1; S.sel = null;
    setElection(p.e.id, true); $("glance").scrollIntoView({ behavior: "smooth" });
  });
}

// ---- sources and citation -------------------------------------------------------------------
function renderData() {
  const f = CUR.facts, src = (f.sources || []).map(s => `<li>${s.url ? `<a href="${s.url}" target="_blank" rel="noopener">${esc(s.label)}</a>` : esc(s.label)}</li>`);
  if (!f.facts_only && !isPolls()) src.push(`<li>Gmina boundaries: PRG 2022 (GUGiK), simplified; the 1990s gminas mapped to today's through a crosswalk built for this page</li>`);
  $("sources").innerHTML = src.join("");
  $("citation").innerHTML = `${ChartExport.citeHtml(PAGE)} <span class="muted">Permalink:</span> <a href="${esc(shareUrl())}" target="_top">this view</a>.`;
  $("csvgmina").disabled = !!f.facts_only || isPolls();
  $("csvnote").textContent = f.facts_only ? "No files for 1989." : isPolls() ? "The parties' shares and seats, and the projection by constituency." : `Gmina results are on today's boundaries; constituency results are ${S.b === "t" ? "on today's 41 constituencies" : "on the constituencies of the time"} (as on the map).`;
}

// ---- downloads ------------------------------------------------------------------------------
function download(name, blob) {
  const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = name;
  document.body.appendChild(a); a.click(); setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 500);
}
const q = v => v == null || (typeof v === "number" && !isFinite(v)) ? "" : typeof v === "string" && /[",\n]/.test(v) ? `"${v.replace(/"/g, '""')}"` : v;
const csvBlob = rows => new Blob([ChartExport.csvCite(PAGE) + rows.map(r => r.map(q).join(",")).join("\n") + "\n"], { type: "text/csv" });
const fileStem = () => partOf() ? partOf().contest_id : S.e;
function csvNational() {
  if (CUR.facts.facts_only) return;
  if (isPolls()) {
    const head = ["week", "party", "share", "share_lo80", "share_hi80", "p_threshold", "seats", "seats_lo80", "seats_hi80"];
    download(`polls-${CUR.facts.date}.csv`, csvBlob([head, ...resOf().map(it => [CUR.facts.date, it.id, it.pct, it.lo, it.hi, it.p_thr, it.seats, it.seats_lo, it.seats_hi])]));
    return;
  }
  const head = ["contest", "item", "name", "short", "label", "party_ids", "votes", "share", "seats"];
  download(`${fileStem()}-national.csv`, csvBlob([head, ...resOf().map(it => [fileStem(), it.id, it.name, it.short, it.label, it.party_ids, it.votes, it.pct, it.seats])]));
}
function csvUnits(kind) {
  if (CUR.facts.facts_only) return;
  const U = unitsOf(), A = kind === "g" ? U.gmina : (S.b === "t" ? U.okreg41 : U.okreg), ids = A.items;
  const names = ids.map(id => id === "other" ? "others" : id);
  const head = kind === "g" ? ["teryt", "gmina", "powiat", "voivodeship", "joint_unit"] : ["okreg", "name", "boundaries"];
  head.push("electorate", "voters", "valid_votes", "invalid_votes", ...names);
  const rows = [head];
  for (let u = 0; u < A.valid.length; u++) {
    const r = kind === "g"
      ? [G.gminas.teryt[u], G.gminas.name[u], G.gminas.powiat[u], G.gminas.voivodeship[u], A.group[u] >= 0 ? A.groups[A.group[u]].map(x => G.gminas.teryt[x]).join(";") : ""]
      : [G.sets[A.set].okreg[u], G.sets[A.set].name[u], G.sets[A.set].label];
    rows.push([...r, A.electorate[u], A.voters[u], A.valid[u], A.invalid[u], ...ids.map((id, k) => A.votes[k][u])]);
  }
  download(`${fileStem()}-${kind === "g" ? "gminas" : "constituencies"}.csv`, csvBlob(rows));
}
async function png(sec) {
  const svgs = sec === "mapsec" ? [$("map"), ...$("legend").querySelectorAll("svg")] : sec === "allsec" ? [$("turnchart")] : sec === "whosec" ? [$("scat")] : [...$("glancechart").querySelectorAll("svg")];
  const imgs = await Promise.all(svgs.map(async svg => {
    const vb = svg.viewBox.baseVal, ratio = vb.height / vb.width, w = sec === "mapsec" && svg.id === "map" ? 900 : Math.min(900, svg.clientWidth || 600), h = w * ratio, clone = await ChartExport.clone(svg);
    clone.setAttribute("xmlns", "http://www.w3.org/2000/svg"); ChartExport.setSize(clone, w, h);
    return new Promise(res => {
      const img = new Image(); img.onload = () => res({ img, w, h }); img.onerror = () => res(null);
      img.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(clone));
    });
  }));
  await ChartExport.fonts();
  const ok = imgs.filter(Boolean), pad = 24, head = 62, foot = 50;
  const W = Math.max(...ok.map(i => i.w)) + 2 * pad, H = head + foot + sum(ok.map(i => i.h)) + 10 * (ok.length - 1);
  const cv = document.createElement("canvas"), k = ChartExport.scale(W, H); cv.width = W * k; cv.height = H * k;
  const g = cv.getContext("2d"); g.scale(k, k); g.fillStyle = "#fff"; g.fillRect(0, 0, W, H);
  const f = CUR.facts, part = partOf();
  g.fillStyle = ACCENT; g.font = ChartExport.TITLE; g.fillText(sec === "allsec" ? "Elections in Poland since 1989" : `${f.name_en}, ${f.year}${CUR.facts.parts && CUR.facts.parts.length > 1 ? (EL.type === "presidential" ? ", round " : ", question ") + (S.p + 1) : ""}`, pad, 32);
  g.fillStyle = "#4B4B4B"; g.font = ChartExport.SUBTITLE; g.fillText(sec === "mapsec" ? MAPV.sc.title : sec === "allsec" ? "National turnout in every election since 1989" : sec === "whosec" ? `${SCAT.Y.label}, against ${SCAT.V.label.toLowerCase()} (${SCAT.V.unit})` : $("glancetitle").textContent, pad, 52);
  let yy = head;
  ok.forEach(i => { g.drawImage(i.img, pad, yy, i.w, i.h); yy += i.h + 10; });
  g.fillStyle = MUTED; g.font = ChartExport.CREDIT; g.fillText("Data: PKW.", pad, H - 28);
  ChartExport.drawCite(g, PAGE, pad, H - 12, W - 2 * pad);
  cv.toBlob(b => download(`${fileStem()}-${sec === "mapsec" ? "map" : "glance"}.png`, b), "image/png");
}

// ---- rendering ------------------------------------------------------------------------------
function header() {
  $("asof").textContent = `Every presidential, Sejm, European Parliament and referendum vote since 1990, from the files of the National Electoral Commission (PKW). National figures are PKW's announced ones; the maps use PKW's gmina files.`;
}
function renderAll() {
  const fo = CUR.facts.facts_only;
  $("mapsec").hidden = fo; $("results").hidden = fo;
  $("profilecard").hidden = true;
  document.querySelectorAll("#data .card")[0].hidden = fo;
  renderStrip(); renderGlance(); renderResults(); renderMap(); renderUnitCard(); renderScatter(); renderAllElections(); renderData();
  saveHash();
}

// ---- events ---------------------------------------------------------------------------------
function bind() {
  $("types").addEventListener("click", e => {
    const b = e.target.closest("button[data-t]"); if (!b) return;
    S.type = b.dataset.t;
    const shown = ELECTIONS.filter(x => S.type === "all" || x.type === S.type);
    if (!shown.find(x => x.id === S.e)) setElection(shown[shown.length - 1].id); else renderStrip();
  });
  $("elections").addEventListener("click", e => { const b = e.target.closest("button[data-e]"); if (b && b.dataset.e !== S.e) setElection(b.dataset.e); });
  $("parts").addEventListener("click", e => {
    const b = e.target.closest("button[data-p]"); if (!b || +b.dataset.p === S.p) return;
    S.p = +b.dataset.p; S.sel = null; S.i = 0; S.j = 1; S.m = null; renderAll();
  });
  const seg = (id, key, after) => $(id).addEventListener("click", e => {
    const b = e.target.closest(`button[data-${key}]`); if (!b) return;
    const v = b.dataset[key]; if (S[key] === v) return;
    S[key] = v; if (after) after(); renderMap(); renderUnitCard(); renderScatter(); renderData(); saveHash();
  });
  seg("measures", "m"); seg("levels", "l", () => { S.sel = null; }); seg("bounds", "b", () => { S.sel = null; }); seg("shapes", "v", () => { S.sel = null; });
  $("item").addEventListener("change", e => {
    if (measure() === "chg") { S.cx = e.target.value; renderMap(); renderUnitCard(); renderScatter(); saveHash(); return; }
    S.i = +e.target.value; renderScatter(); if (S.j === S.i) S.j = S.i === 0 ? 1 : 0; if (measure() === "leader") S.m = "share"; renderMap(); renderResults(); saveHash(); });
  $("item2").addEventListener("change", e => { S.j = +e.target.value; renderMap(); renderScatter(); saveHash(); });
  $("cmp").addEventListener("change", e => { S.cc = e.target.value; S.ck = null; S.cx = null; renderMap(); renderUnitCard(); renderScatter(); saveHash(); });
  $("modes").addEventListener("click", e => { const b = e.target.closest("button[data-k]"); if (!b || b.dataset.k === S.ck) return; S.ck = b.dataset.k; S.cx = null; renderMap(); renderUnitCard(); renderScatter(); saveHash(); });
  $("zabory").addEventListener("click", () => { S.z = !S.z; renderMap(); renderScatter(); saveHash(); });
  bindSearch(); bindProfile(); bindAll(); bindScatter();
  $("restable").addEventListener("click", e => {
    if (e.target.id === "alllists") { S.allLists = !S.allLists; renderResults(); return; }
    const b = e.target.closest("button[data-id]"); if (!b) return;
    const k = mapItems().findIndex(it => it.id === b.dataset.id);
    if (k < 0) { toast("Too small to map on its own: it is counted with the others"); return; }
    S.i = k; if (S.j === k) S.j = k === 0 ? 1 : 0; if (measure() === "leader" || measure() === "turnout") S.m = "share";
    renderMap(); renderResults(); renderScatter(); saveHash();
    $("mapsec").scrollIntoView({ behavior: "smooth", block: "start" });
  });
  $("unitcard").addEventListener("click", e => { if (e.target.id === "unitclose") { S.sel = null; drawHighlight(); renderUnitCard(); } });
  bindMap();
  document.querySelectorAll("[data-png]").forEach(b => b.addEventListener("click", () => png(b.dataset.png)));
  $("csvresults").addEventListener("click", csvNational); $("csvnational").addEventListener("click", csvNational);
  $("csvgmina").addEventListener("click", () => csvUnits("g")); $("csvokreg").addEventListener("click", () => csvUnits("c"));
  $("jump").addEventListener("click", e => { const a = e.target.closest("a"); if (!a) return; e.preventDefault(); const el = $(a.getAttribute("href").slice(1)); if (!el) return;
    let top = el.getBoundingClientRect().top + window.scrollY - document.querySelector(".strip").offsetHeight - 8;
    // On a short screen, where the map and its controls do not fit together, land with the whole map in view
    if (el.id === "mapsec") top = Math.max(top, $("map").getBoundingClientRect().bottom + window.scrollY - window.innerHeight + 8);
    window.scrollTo({ top, behavior: "smooth" }); });
  $("copylink").addEventListener("click", () => {
    const u = shareUrl();
    (navigator.clipboard ? navigator.clipboard.writeText(u) : Promise.reject()).then(() => toast("Link copied")).catch(() => { window.prompt("Copy this link:", u); });
  });
  // A link pasted or edited by hand (the app itself writes the hash with replaceState)
  window.addEventListener("hashchange", () => { restoreHash(); setElection(S.e, true); });
  // The strip grows a row for elections with two rounds, and wraps on narrow screens
  if (window.ResizeObserver) new ResizeObserver(fitMap).observe(document.querySelector(".strip"));
  let lastW = window.innerWidth;
  window.addEventListener("resize", debounce(() => { fitMap(); if (window.innerWidth !== lastW) { lastW = window.innerWidth; renderGlance(); renderScatter(); turnoutChart(); } }, 150));
}

// ---- load -----------------------------------------------------------------------------------
Promise.all([
  fetch("elections.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }),
  fetch("geo.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }),
  fetch("../polling-tracker/polls.json").then(r => r.ok ? r.json() : null).catch(() => null)
]).then(async ([e, g, pj]) => {
  E = e; G = g; prep();
  try { if (pj && pj.weeks && pj.weeks.length && pj.constituencies) { POLLS = pj; POLLS_EL = pollsEntry(); } } catch (err) { console.warn("Polling tracker data unusable:", err); POLLS = null; }
  restoreHash(); header(); bind();
  await setElection(S.e, true);
}).catch(err => { $("asof").textContent = "Could not load the election results."; console.error(err); });
