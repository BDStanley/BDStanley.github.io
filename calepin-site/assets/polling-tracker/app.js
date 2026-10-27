/* Polling tracker — the web version of Pooling the Poles.
 *
 * Reads polls.json (everything the model publishes for each week) and, when a
 * scenario needs it, draws.json (the model's vote-share draws for every week).
 * The seat rules follow PTP.R's simulation and Polls/R/constants.R:
 * national shares are spread across the 41 constituencies by each party's
 * regional profile, thresholds are national (5% for a party, 8% for a joint
 * list, MN exempt), and seats go by D'Hondt.
 *
 * Model view (the week's own estimates, no edits): every number is the model's,
 * as published. Scenario view: the user's shares are applied to each of the
 * week's simulated draws (each party's draws scaled so their median is the
 * share entered), the coalition-dynamics penalty is applied draw by draw, each
 * party's regional profile is perturbed as in PTP.R, and seats are allocated in
 * every draw, giving medians, 80% ranges and probabilities for the scenario.
 * Without draws.json, scenarios fall back to a single allocation.
 *
 * Past Sejm terms (terms.json, one term-<term>.json each) show the trend and
 * the pollsters only: shares of the parties Wikipedia's tables name, from a
 * model anchored on the elections that open and close the term
 * (Polls/R/terms/ in the Polish political analyses repo).
 */
"use strict";

// ---- utilities ------------------------------------------------------------------------------
const $ = id => document.getElementById(id);
const PAGE = "Polling tracker";   // the page's name in its suggested citation (../chart-export.js)
const esc = s => String(s == null ? "" : s).replace(/[&<>"']/g, c => ({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}[c]));
const MON = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
const MONTHS = ["January","February","March","April","May","June","July","August","September","October","November","December"];
const dateOf = d => new Date(d * 864e5);
const dstr = d => { const t = dateOf(d); return t.getUTCDate() + " " + MON[t.getUTCMonth()] + " " + t.getUTCFullYear(); };
const dlong = d => { const t = dateOf(d); return t.getUTCDate() + " " + MONTHS[t.getUTCMonth()] + " " + t.getUTCFullYear(); };
const iso = d => dateOf(d).toISOString().slice(0, 10);
const fromIso = s => Math.round(Date.parse(s + "T00:00:00Z") / 864e5);
const pct = (v, dp = 1) => (v == null || isNaN(v)) ? "–" : v.toFixed(dp) + "%";
const prob = p => p == null || isNaN(p) ? "–" : p > 0.99 ? ">99%" : p < 0.01 ? "<1%" : Math.round(p * 100) + "%";
const signed = (v, dp = 0) => { const r = +v.toFixed(dp); return (r > 0 ? "+" : r < 0 ? "−" : "±") + Math.abs(r).toFixed(dp); };
const cls = v => v > 0 ? "pos" : v < 0 ? "neg" : "";
const clamp = (v, a, b) => Math.max(a, Math.min(b, v));
const sum = a => a.reduce((x, y) => x + y, 0);
function quant(sorted, p) { // R's quantile type 7
  const n = sorted.length; if (!n) return NaN;
  const h = (n - 1) * p, lo = Math.floor(h);
  return lo + 1 < n ? sorted[lo] + (h - lo) * (sorted[lo + 1] - sorted[lo]) : sorted[lo];
}
const median = s => quant(s, 0.5);
function mulberry32(a) { return function () { a |= 0; a = a + 0x6D2B79F5 | 0; let t = Math.imul(a ^ a >>> 15, 1 | a); t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t; return ((t ^ t >>> 14) >>> 0) / 4294967296; }; }
function debounce(fn, ms) { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => fn(...a), ms); }; }
function toast(msg) { const t = $("toast"); t.textContent = msg; t.classList.add("on"); clearTimeout(toast.t); toast.t = setTimeout(() => t.classList.remove("on"), 1800); }
const OKABE = ["#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00", "#56B4E9"];

// ---- data -----------------------------------------------------------------------------------
let D, R, P, SP, NP, NC, C, VV, MAG, COEFI, PIDX, LAUNCH, WEEKS;
let DRAWS = null, drawsState = "idle", SWING = null;
let TERMS = null, TERM = null;  // terms.json, and the past term on show (null: the current one)
const TCACHE = new Map();
let PENDING = null;             // a past term, and the houses and parties picked in the link, until it loads
// A poll is its house, mid-date and, where one house polled the same days for two clients, the k in the
// fifth element of each of its rows (absent in a past term's data: 0)
function pollKey(r) { return r[0] + "|" + r[1] + "|" + (r[4] || 0); }
// The trend line's value for a party on a day, interpolated between the two nearest dates the trend is
// given for; null where the party has no trend line there
function trendAt(V, party, d) {
  const dates = V.trend.dates, ser = V.trend.series[party];
  if (!ser || d < dates[0] || d > dates[dates.length - 1]) return null;
  let i = dates.findIndex(t => t >= d);
  if (i < 0) return null;
  if (dates[i] === d || i === 0) return ser.med[i];
  const v0 = ser.med[i - 1], v1 = ser.med[i];
  if (v0 == null || v1 == null) return null;
  return v0 + (v1 - v0) * (d - dates[i - 1]) / (dates[i] - dates[i - 1]);
}
// The pop-up for a poll figure picked by a click: the figure under the pointer first, then the house and
// date, then the poll's figures for the other parties, largest first; each with how far it sits above or
// below the trend line on the poll's date
function pollTipHtml(best, rows, V) {
  const row = (q, bold) => {
    const t = trendAt(V, V.parties[q[2]], q[0]);
    return `<div class="r"><span><span class="dot" style="background:${col(V.parties[q[2]])}"></span>${bold ? "<b>" : ""}${esc(V.parties[q[2]])}${bold ? "</b>" : ""}</span><span style="font-variant-numeric:tabular-nums">${q[3].toFixed(1)}%${t != null ? `<span class="muted" style="display:inline-block;min-width:32px;text-align:right">${signed(q[3] - t, 1)}</span>` : ""}</span></div>`;
  };
  const others = rows.filter(q => q !== best).sort((u, v) => v[3] - u[3]);
  return row(best, true) + `<div class="muted">${esc(V.pollsters[best[1]])} · ${dstr(best[0])}</div>` +
    (others.length ? `<div style="margin-top:6px;padding-top:5px;border-top:1px solid #EEE">${others.map(q => row(q, false)).join("")}</div>` : "") +
    `<div class="muted" style="margin-top:5px;font-size:11px;line-height:1.35;width:0;min-width:100%">Grey: points above (+) or below (−) the trend line.</div>`;
}
// What the trend and pollster charts draw: the current term, or a past one
function VD() {
  return TERM
    ? { trend: TERM.trend, polls: TERM.polls, pollsters: TERM.pollsters, parties: TERM.parties, events: TERM.events,
        house: TERM.house, blocs: TERM.parties, results: TERM.results, past: true }
    : { trend: D.trend, polls: D.polls, pollsters: D.pollsters, parties: P, events: R.launches,
        house: D.house, blocs: R.houseEffectBlocs, results: [], past: false };
}
// Political events of the current term, shown with the party launches and splits from the model data
const POLITICAL_EVENTS = [
  ["2023-12-13", "Tusk government sworn in"],
  ["2024-04-07", "Local elections"],
  ["2024-06-09", "European Parliament election"],
  ["2024-09-14", "Floods in south-west Poland"],
  ["2024-11-22", "Trzaskowski wins the KO primary"],
  ["2025-05-18", "Presidential election, first round"],
  ["2025-06-01", "Nawrocki wins the presidential run-off"],
  ["2025-06-11", "Tusk government wins a confidence vote"],
  ["2025-08-06", "Nawrocki sworn in as president"],
  ["2025-09-10", "Russian drones enter Polish airspace"],
];
// Events of the view on show, in date order, numbered through the whole list so a number never changes with the period
function trendEvents() {
  const V = VD(), all = V.events.map(e => ({ date: e.date, event: e.event }));
  if (!V.past) POLITICAL_EVENTS.forEach(([d, t]) => all.push({ date: Date.parse(d) / 864e5, event: t }));
  all.sort((p, q) => p.date - q.date); all.forEach((e, i) => { e.n = i + 1; });
  const dates = V.trend.dates, last = dates[dates.length - 1], span = S.period === "all" ? null : +S.period * 30.44;
  const a = span ? Math.max(dates[0], last - span) : dates[0];
  return all.filter(e => e.date >= a && e.date <= last);
}
const evWhen = d => { const t = dateOf(d); return MON[t.getUTCMonth()] + " " + t.getUTCFullYear(); };
const WD = new Map();          // decoded draws per week index
const MODEL_SIM = new Map();   // model-view simulations per week (constituency ranges)

function prep() {
  R = D.rules; P = R.parties; SP = R.seatParties; NP = P.length;
  C = D.constituencies; NC = C.length;
  VV = C.map(c => c.validvotes); MAG = C.map(c => c.magnitude);
  PIDX = Object.fromEntries(SP.map((p, i) => [p, i]));
  COEFI = SP.map(p => p === "MN"
    ? C.map(c => c.okreg === R.mnOkreg ? 100 * R.mnOpoleShare / R.mnNational2023 : 0)
    : C.map(c => c[R.coef[p]]));
  LAUNCH = {}; R.launches.forEach(l => { LAUNCH[l.party] = l.date; });
  WEEKS = D.weeks;
  S.excl = defaultExcl();
}
const col = n => (TERM && TERM.colours[n]) || (R.colours && R.colours[n]) || "#808080";
const exists = (p, date) => !(p in LAUNCH) || date >= LAUNCH[p];
const short = p => R.short[p] || p;
const sortParties = m => [...m].sort((a, b) => PIDX[a] - PIDX[b]);
// The exclusion rules: pairs of parties that will not govern together (S.excl, by pairKey). They start as
// R.forbidden, and the picker can switch any pair either way.
const pairKey = (a, b) => (PIDX[a] <= PIDX[b] ? [a, b] : [b, a]).join("|");
const pairOf = k => k.split("|");
const defaultExcl = () => new Set(R.forbidden.map(fp => pairKey(fp[0], fp[1])));
const pairText = fp => `${fp[0]} and ${fp[1]}`;
const ruleText = fp => `${pairText(fp)} do not govern together`;
const exclIn = parties => [...S.excl].map(pairOf).filter(fp => fp.every(x => parties.includes(x)));
const compatible = parties => exclIn(parties).length === 0;
// A government of several lists: no excluded pair may span two different lists. Pairs inside one joint list are the
// user's own choice (the list picker warns about them) and do not count.
const compatibleLists = groups => groups.every((g, i) => groups.slice(i + 1).every(h => !g.some(a => h.some(b => S.excl.has(pairKey(a, b))))));
const forbiddenIn = parties => exclIn(parties).map(pairText);
// True if a default exclusion has been switched to Include (the published combinations then are not enough)
const relaxed = () => R.forbidden.some(fp => !S.excl.has(pairKey(fp[0], fp[1])));

// ---- state ----------------------------------------------------------------------------------
const S = { w: 0, shares: {}, joint: [], dyn: 0, view: "trends", period: "all", houses: new Set(), parties: new Set(), points: true, dateLine: false, events: false, fmt: "png",
  hoverParty: null, pollSel: null, hemiCert: false, hemi2023: false, hemiCoal: false, excl: new Set(), coalMsg: null, focusGov: null, pinGov: null, constSel: null, picker: new Set(), showAllGov: false };

function defaults(w) {
  const wk = WEEKS[w], s = {};
  P.forEach((p, i) => { const v = wk.vote[i]; s[p] = v == null || isNaN(v) ? 0 : v; });
  s.MN = R.mnNational2023;
  s.Other = Math.round((100 - sum(SP.map(p => s[p]))) * 100) / 100;
  return s;
}
function edited() { const d = defaults(S.w); return SP.some(p => Math.abs(S.shares[p] - d[p]) > 0.051); }
const dynOn = () => S.joint.length > 0 && S.dyn > 0;
function modelView() { return !edited() && S.joint.length === 0; }
const weekParties = w => P.filter(p => exists(p, WEEKS[w].date));

// ---- seat machinery -------------------------------------------------------------------------
function makeEnts(joint) {
  const ents = joint.map(m => { const mem = sortParties(m); return { name: mem.map(short).join("-"), members: mem, joint: true }; });
  SP.forEach(p => { if (!joint.some(m => m.includes(p))) ents.push({ name: p, members: [p], joint: false }); });
  ents.forEach(e => { e.idx = e.members.map(p => PIDX[p]); e.mnOnly = !e.joint && e.members[0] === "MN"; });
  return ents;
}
// Coalition dynamics: each member of a joint list loses
// strength * PUNISH_MAX_LOSS * (distance to its furthest partner / 100)^2 of its vote.
function punishArr(sh, joint, strength) {
  if (!joint.length || strength <= 0) return 0;
  let lost = 0;
  for (const m of joint) {
    const pos = m.filter(p => p in R.ideology);
    if (pos.length < 2) continue;
    for (const p of pos) {
      let far = 0;
      for (const q of pos) far = Math.max(far, Math.abs(R.ideology[q] - R.ideology[p]));
      const i = PIDX[p], o = sh[i];
      if (!(o > 0)) continue;
      const l = o * strength * R.punishMaxLoss * (far / 100) ** 2;
      sh[i] = o - l; lost += l;
    }
  }
  return lost;
}
function dhondt(votes, n, out) {
  out.fill(0);
  for (let s = 0; s < n; s++) {
    let best = -1, bq = 0;
    for (let k = 0; k < votes.length; k++) {
      if (votes[k] <= 0) continue;
      const q = votes[k] / (out[k] + 1);
      if (q > bq) { bq = q; best = k; }
    }
    if (best < 0) break;
    out[best]++;
  }
}
// Allocate seats for national shares `sh` (fractions, SP order). With `swing`,
// draw d's regional perturbations are applied. Fills nat (E) and cons (NC x E);
// returns each list's national share.
function allocate(sh, ents, swing, d, nat, cons, natOff = 0, consOff = 0) {
  const E = ents.length, tot = new Float64Array(E), active = new Uint8Array(E);
  for (let k = 0; k < E; k++) {
    let t = 0; for (const i of ents[k].idx) t += sh[i];
    tot[k] = t;
    active[k] = t > 0 && (ents[k].mnOnly || t >= (ents[k].joint ? R.thresholdJoint : R.thresholdParty)) ? 1 : 0;
  }
  const votes = new Float64Array(E), seats = new Int16Array(E);
  for (let k = 0; k < E; k++) nat[natOff + k] = 0;
  for (let c = 0; c < NC; c++) {
    for (let k = 0; k < E; k++) {
      if (!active[k]) { votes[k] = 0; continue; }
      let v = 0;
      for (const i of ents[k].idx) {
        const s = sh[i]; if (s <= 0) continue;
        let cf = COEFI[i][c];
        if (swing && i < NP) cf *= swing[(d * NC + c) * NP + i];
        v += cf * s;
      }
      votes[k] = VV[c] * v;
    }
    dhondt(votes, MAG[c], seats);
    for (let k = 0; k < E; k++) { nat[natOff + k] += seats[k]; if (cons) cons[consOff + c * E + k] = seats[k]; }
  }
  return tot;
}
// The draws' regional perturbations, as in PTP.R: a mean-one lognormal factor with
// each party's SD, independent for every draw x constituency x party. Seeded, so
// results are the same on every visit.
function makeSwing(n) {
  const r = mulberry32(780045), a = new Float32Array(n * NC * NP);
  let spare = null;
  const normal = () => { if (spare != null) { const s = spare; spare = null; return s; }
    let u = 0, v = 0; while (u === 0) u = r(); v = r();
    const m = Math.sqrt(-2 * Math.log(u)); spare = m * Math.sin(2 * Math.PI * v); return m * Math.cos(2 * Math.PI * v); };
  const sd = P.map(p => R.swingSd[p] != null ? R.swingSd[p] : R.swingSdPooled);
  for (let d = 0; d < n; d++) for (let c = 0; c < NC; c++) for (let i = 0; i < NP; i++) {
    a[(d * NC + c) * NP + i] = Math.exp(-sd[i] * sd[i] / 2 + sd[i] * normal());
  }
  return a;
}
function weekDraws(w) {
  if (WD.has(w)) return WD.get(w);
  const raw = DRAWS.weeks[String(WEEKS[w].date)];
  if (!raw) return null;
  const K = DRAWS.categories.length, n = raw.length / K;
  const arr = new Float32Array(raw.length);
  for (let i = 0; i < raw.length; i++) arr[i] = raw[i] / 10000;
  // category k -> SP index (Other has none)
  const map = DRAWS.categories.map(c => c in PIDX ? PIDX[c] : -1);
  const med = new Float64Array(SP.length);
  map.forEach((i, k) => { if (i < 0) return; const v = []; for (let d = 0; d < n; d++) v.push(arr[d * K + k]); v.sort((a, b) => a - b); med[i] = median(v); });
  const out = { n, K, arr, map, med };
  WD.set(w, out);
  return out;
}
// Simulate week w. scen = {shares (%), joint, dyn} or null for the model's own draws.
function simulate(w, scen) {
  const wd = weekDraws(w);
  if (!wd) return null;
  if (!SWING || SWING.length < wd.n * NC * NP) SWING = makeSwing(wd.n);
  const ents = makeEnts(scen ? scen.joint : []), E = ents.length, n = wd.n;
  const nat = new Int16Array(n * E), cons = new Int16Array(n * NC * E), tots = new Float32Array(n * E);
  const factor = new Float64Array(SP.length), fixed = new Float64Array(SP.length);
  if (scen) SP.forEach((p, i) => {
    if (p === "MN") return;
    const u = (scen.shares[p] || 0) / 100, m = wd.med[i];
    if (m > 0) factor[i] = u / m; else { factor[i] = 0; fixed[i] = u; }
  });
  const sh = new Float64Array(SP.length), iMN = PIDX.MN;
  for (let d = 0; d < n; d++) {
    sh.fill(0);
    wd.map.forEach((i, k) => { if (i >= 0) sh[i] = wd.arr[d * wd.K + k]; });
    if (scen) for (let i = 0; i < SP.length; i++) if (i !== iMN) sh[i] = factor[i] > 0 || fixed[i] === 0 ? sh[i] * factor[i] : fixed[i];
    sh[iMN] = (scen ? scen.shares.MN : R.mnNational2023) / 100;
    if (scen && scen.joint.length && scen.dyn > 0) punishArr(sh, scen.joint, scen.dyn / 100);
    const tot = allocate(sh, ents, SWING, d, nat, cons, d * E, d * NC * E);
    for (let k = 0; k < E; k++) tots[d * E + k] = tot[k];
  }
  return { n, ents, nat, cons, tots };
}
function summarise(sim) {
  const { n, ents, nat, tots } = sim, E = ents.length;
  return ents.map((e, k) => {
    const v = []; let clear = 0;
    const thr = e.joint ? R.thresholdJoint : R.thresholdParty;
    for (let d = 0; d < n; d++) { v.push(nat[d * E + k]); if (tots[d * E + k] >= thr - 1e-9) clear++; }
    v.sort((a, b) => a - b);
    return { seats: Math.round(median(v)), lo: Math.round(quant(v, 0.1)), hi: Math.round(quant(v, 0.9)), pThr: e.mnOnly ? null : clear / n };
  });
}
function consRange(sim, c, k) {
  const { n, ents } = sim, E = ents.length, v = [];
  for (let d = 0; d < n; d++) v.push(sim.cons[d * NC * E + c * E + k]);
  v.sort((a, b) => a - b);
  return [Math.round(quant(v, 0.1)), Math.round(quant(v, 0.9))];
}

// ---- governing majorities -------------------------------------------------------------------
// Members are named from the most seats to the fewest in the current view
const seatsOfName = m => { if (!CUR) return 0; const k = CUR.ents.findIndex(e => e.name === m); return k >= 0 ? CUR.seats[k] : (CUR.memberSeats && CUR.memberSeats[m]) || 0; };
const optLabel = members => (CUR ? [...members].sort((a, b) => seatsOfName(b) - seatsOfName(a)) : members).map(m => m in PIDX ? short(m) : m).join(" + ");
// Lists that can form a government: without joint lists MN is never a partner; the exclusion rules apply
// between lists, with or without joint lists (compatibleLists).
function govLists(ents, joint, seatsOf) {
  return ents.map((e, k) => k).filter(k => seatsOf(k) > 0 && (joint.length ? true : ents[k].members[0] in R.ideology));
}
function govFromSim(sim, joint) {
  const { n, ents, nat } = sim, E = ents.length;
  const mean = k => { let s = 0; for (let d = 0; d < n; d++) s += nat[d * E + k]; return s / n; };
  const lists = govLists(ents, joint, mean).sort((a, b) => mean(b) - mean(a));
  const L = lists.length, out = [];
  if (!L) return out;
  const full = (1 << L) - 1;
  for (let mask = 1; mask <= full; mask++) {
    if (mask === full && L > 1) continue; // someone must be in opposition
    const ks = lists.filter((k, j) => mask & (1 << j));
    const names = ks.map(k => ents[k].name);
    if (!compatibleLists(ks.map(k => ents[k].members))) continue;
    const v = new Array(n);
    let pm = 0, pv = 0, pc = 0;
    for (let d = 0; d < n; d++) {
      let t = 0; for (const k of ks) t += nat[d * E + k];
      v[d] = t; if (t >= R.seatsMajority) pm++; if (t >= R.seatsVeto) pv++; if (t >= R.seatsConstitutional) pc++;
    }
    v.sort((a, b) => a - b);
    out.push({ members: names, size: ks.length, s: Math.round(median(v)), lo: Math.round(quant(v, 0.1)), hi: Math.round(quant(v, 0.9)), p: pm / n, pv: pv / n, pc: pc / n });
  }
  return out;
}
function govFromWeek(wk) {
  return wk.coalitions.map(c => ({ members: sortParties(c.m), size: c.m.length, s: Math.round(c.s), lo: c.lo, hi: c.hi, p: c.p, pv: c.pv, pc: c.pc }));
}
// Deterministic majorities from one set of seat numbers.
function govDet(ents, joint, seats) {
  const lists = govLists(ents, joint, k => seats[k]).sort((a, b) => seats[b] - seats[a]);
  const L = lists.length, out = [];
  const full = (1 << L) - 1;
  for (let mask = 1; mask <= full; mask++) {
    if (mask === full && L > 1) continue;
    const ks = lists.filter((k, j) => mask & (1 << j));
    if (!compatibleLists(ks.map(k => ents[k].members))) continue;
    const s = sum(ks.map(k => seats[k]));
    out.push({ members: ks.map(k => ents[k].name), size: ks.length, s, lo: null, hi: null, p: s >= R.seatsMajority ? 1 : 0, pv: s >= R.seatsVeto ? 1 : 0, pc: s >= R.seatsConstitutional ? 1 : 0 });
  }
  return out;
}
const isSub = (a, b) => a.size < b.size && a.members.every(x => b.members.includes(x));
// Options worth showing: at least a 5% chance of a majority, leaving out any that
// only add lists to one that already has an even chance. Without uncertainty:
// the majorities in which every list is needed.
function pruneGov(opts, probabilistic) {
  let keep;
  if (probabilistic) {
    const strong = opts.filter(o => o.p >= 0.5);
    keep = opts.filter(o => o.p >= 0.05 && !strong.some(b => isSub(b, o)));
    keep.sort((a, b) => b.p - a.p || a.size - b.size || b.s - a.s);
  } else {
    const maj = opts.filter(o => o.s >= R.seatsMajority);
    keep = maj.filter(o => !maj.some(b => isSub(b, o)));
    keep.sort((a, b) => a.size - b.size || a.s - b.s);
  }
  keep.forEach(o => { o.key = [...o.members].sort().join("|"); });
  return keep;
}
// Ideological plausibility of an option. Its gaps are the seated parties that lie between its most distant members
// on the left-right positions (R.ideology) without being in it: a coalition with no gap is ideologically adjacent.
// Fewer gaps are more plausible, then fewer lists, then a narrower span.
function plaus(o, ents, ms) {
  const ps = o.members.flatMap(nm => { const e = ents.find(x => x.name === nm); return e ? e.members : [nm]; }).filter(p => p in R.ideology);
  if (ps.length < 2) return { gaps: 0, span: 0, skipped: [] };
  const pos = ps.map(p => R.ideology[p]), lo = Math.min(...pos), hi = Math.max(...pos);
  const skipped = Object.keys(R.ideology).filter(p => !ps.includes(p) && R.ideology[p] > lo && R.ideology[p] < hi && (ms[p] || 0) > 0).sort((x, y) => R.ideology[x] - R.ideology[y]);
  return { gaps: skipped.length, span: hi - lo, skipped };
}
const byPlaus = (a, b) => a.pl.gaps - b.pl.gaps || a.size - b.size || a.pl.span - b.pl.span || b.p - a.p || b.s - a.s;

// ---- the current view's numbers -------------------------------------------------------------
let CUR = null;
function compute() {
  const wk = WEEKS[S.w], model = modelView();
  const ents = makeEnts(S.joint), E = ents.length;
  // Shares after coalition dynamics (single allocation and display)
  const sh = new Float64Array(SP.length);
  SP.forEach((p, i) => { sh[i] = Math.max(0, S.shares[p] || 0) / 100; });
  if (dynOn()) punishArr(sh, S.joint, S.dyn / 100);
  const eff = {}; SP.forEach((p, i) => { eff[p] = sh[i] * 100; });
  const single = { nat: new Int16Array(E), cons: new Int16Array(NC * E) };
  const tot = allocate(sh, ents, null, 0, single.nat, single.cons);
  let mode, rows, gov, allGov, sim = null;
  if (model) {
    mode = "model";
    rows = ents.map((e, k) => {
      const i = PIDX[e.name], s = wk.seats[i];
      return { seats: s == null ? single.nat[k] : s, lo: s == null ? null : wk.seatsLo[i], hi: s == null ? null : wk.seatsHi[i], pThr: e.mnOnly ? null : wk.pThr[i] };
    });
    allGov = govFromWeek(wk).filter(o => compatible(o.members));
    // With a rule switched off, the published combinations (which follow every rule) are not enough: the
    // browser's own simulation of the week's draws gives every combination
    if (relaxed() && drawsState === "ready") {
      if (!MODEL_SIM.has(S.w)) MODEL_SIM.set(S.w, simulate(S.w, null));
      const ms = MODEL_SIM.get(S.w);
      if (ms) allGov = govFromSim(ms, []);
    }
    gov = pruneGov(allGov, true);
  } else if (drawsState === "ready" && (sim = simulate(S.w, { shares: S.shares, joint: S.joint, dyn: dynOn() ? S.dyn : 0 }))) {
    mode = "sim";
    rows = summarise(sim);
    allGov = govFromSim(sim, S.joint);
    gov = pruneGov(allGov, true);
  } else {
    mode = "single";
    rows = ents.map((e, k) => ({ seats: single.nat[k], lo: null, hi: null, pThr: null }));
    allGov = govDet(ents, S.joint, Array.from(single.nat));
    gov = pruneGov(allGov, false);
  }
  allGov.forEach(o => { o.key = [...o.members].sort().join("|"); });
  const seats = rows.map(r => r.seats);
  // A joint list's seats split among its members by vote share (largest remainder), for colours and 2023 sums
  const memberSeats = {};
  ents.forEach((e, k) => {
    if (!e.joint) { memberSeats[e.name] = seats[k]; return; }
    const v = e.members.map(p => eff[p]), t = sum(v);
    if (!t || !seats[k]) { e.members.forEach(p => { memberSeats[p] = 0; }); return; }
    const raw = v.map(x => seats[k] * x / t), fl = raw.map(Math.floor);
    let rem = seats[k] - sum(fl);
    raw.map((x, j) => [x - fl[j], j]).sort((a, b) => b[0] - a[0]).forEach(([, j]) => { if (rem > 0) { fl[j]++; rem--; } });
    e.members.forEach((p, j) => { memberSeats[p] = fl[j]; });
  });
  ents.forEach((e, k) => {
    if (e.joint) {
      const top = e.members.reduce((a, b) => (eff[b] > eff[a] ? b : a));
      e.colour = col(top);
    } else e.colour = col(e.name);
    e.vote = sum(e.members.map(p => eff[p]));
    e.rawVote = sum(e.members.map(p => S.shares[p] || 0));
  });
  allGov.forEach(o => { o.pl = plaus(o, ents, memberSeats); });
  gov.sort(byPlaus);
  CUR = { wk, mode, ents, eff, tot, single, rows, seats, gov, govAll: allGov, sim, memberSeats, mwc: null };
  // The most plausible government with a majority in most simulations is the one the hemicycle shows by default
  const top = coalOptions()[0];
  if (top && top.kind === 0) CUR.mwc = Object.assign({}, top.o, { prob: mode !== "single" });
}

// ---- panel ----------------------------------------------------------------------------------
let panelKey = "";
const slug = p => p.replace(/[^A-Za-z0-9]+/g, "_");
function partyRow(p, inJoint) {
  const max = p === "MN" ? 2 : 60, step = p === "MN" ? 0.01 : 0.1;
  const v = S.shares[p];
  return `<div class="prow" id="row-${slug(p)}" data-p="${esc(p)}">
    <span class="pname"><span class="dot" style="background:${col(p)}"></span>${esc(p)}</span>
    <input type="range" min="0" max="${max}" step="${step}" value="${v}" data-p="${esc(p)}" aria-label="${esc(p)} vote share">
    <input type="number" min="0" max="100" step="${step}" value="${v.toFixed(p === "MN" ? 2 : 1)}" data-p="${esc(p)}" aria-label="${esc(p)} vote share, %">
    ${inJoint ? "<span></span>" : `<span class="pseats" id="seat-${slug(p)}"></span>`}
    <span class="psub" id="sub-${slug(p)}"></span>
  </div>`;
}
function renderPanelLayout() {
  const wk = WEEKS[S.w], def = defaults(S.w);
  const shown = weekParties(S.w).concat("MN");
  const order = [...shown].sort((a, b) => (b === "MN" ? -1 : a === "MN" ? 1 : def[b] - def[a]));
  const inJoint = new Set(S.joint.flat());
  let rows = "";
  S.joint.forEach((m, j) => {
    const mem = sortParties(m), name = mem.map(short).join("-");
    rows += `<div class="jgroup" id="jg-${j}"><div class="jhead"><span class="dot" id="jdot-${j}"></span><span class="grow">${esc(name)}</span><span class="label" style="color:var(--muted)">Joint list</span></div>
      ${mem.map(p => partyRow(p, true)).join("")}
      <div class="jtotal" id="jt-${j}"></div></div>`;
  });
  rows += order.filter(p => !inJoint.has(p)).map(p => partyRow(p, false)).join("");
  const avail = weekParties(S.w).filter(p => !inJoint.has(p));
  $("panel").innerHTML = `
  <div class="card">
    <div class="phead">
      <div class="stepper"><button type="button" id="wprev" aria-label="Previous week" ${S.w === 0 ? "disabled" : ""}>‹</button><button type="button" id="wnext" aria-label="Next week" ${S.w === WEEKS.length - 1 ? "disabled" : ""}>›</button></div>
      <h2>${dlong(wk.date)}</h2>
    </div>
    <div style="display:flex;align-items:center;gap:10px;flex-wrap:wrap"><span class="badge" id="badge"></span><button type="button" class="linkbtn" id="reset">Reset to model estimates</button></div>
    <div class="status" id="ustatus"></div>
    <div><span class="label">Vote shares and seats</span></div>
    <div id="prows">${rows}</div>
    <div class="total" id="total"></div>
  </div>
  <div class="card">
    <span class="label">Joint lists</span>
    <p class="help" style="margin:0">Parties standing as one list face an 8% threshold instead of 5%. A government of two or more lists is possible only if no party on one list is excluded from a party on another; the pairs excluded inside a joint list are not checked.</p>
    <div class="chips" id="jpills">${S.joint.map((m, j) => `<span class="jpill">${esc(sortParties(m).map(short).join(" + "))}<button type="button" data-rm="${j}" aria-label="Remove this joint list">×</button></span>`).join("")}</div>
    ${S.joint.length >= 4 ? `<p class="help" style="margin:0">At most four joint lists.</p>` : avail.length < 2 ? "" : `
    <div class="picker">
      <div class="chips" id="jpick">${avail.map(p => `<button type="button" class="chip" data-p="${esc(p)}" aria-pressed="${S.picker.has(p)}"><span class="dot" style="background:${col(p)}"></span>${esc(p)}</button>`).join("")}</div>
      <div class="status" id="jsummary"></div>
      <div><button type="button" class="btn1" id="jadd">Add joint list</button></div>
    </div>`}
    <div style="display:flex;flex-direction:column;gap:4px;margin-top:4px">
      <div style="display:flex;justify-content:space-between;align-items:baseline"><span class="label">Coalition dynamics</span><b id="dynval"></b></div>
      <input type="range" class="wide" id="dyn" min="0" max="100" step="1" value="${S.dyn}" ${S.joint.length ? "" : "disabled"} aria-label="Coalition dynamics strength">
      <p class="help" style="margin:0">${S.joint.length ? "How strongly voters punish a party for allying with ideologically distant partners. At 0% there is no effect; at 100%, parties furthest apart lose three quarters of their vote, and closer ones less." : "Applies to joint lists: build one to use it."}</p>
    </div>
  </div>
  <div class="card">
    <div style="display:flex;justify-content:space-between;align-items:baseline;gap:8px"><span class="label">Government options</span><span class="help" id="govhead"></span></div>
    <div class="gov" id="gov"></div>
    <button type="button" class="linkbtn" id="govmore" hidden></button>
  </div>
  <div class="card soft">
    <span class="label">Share</span>
    <div class="actions">
      <button type="button" class="btn2" id="copylink">Copy link to this view</button>
    </div>
  </div>`;
  panelKey = layoutKey();
}
const layoutKey = () => [S.w, JSON.stringify(S.joint)].join("§");

function updatePanel() {
  if (TERM) return;
  if (layoutKey() !== panelKey) renderPanelLayout();
  const cur = CUR, model = cur.mode === "model";
  const b = $("badge"); b.textContent = model ? "Model estimates" : "Your scenario"; b.className = "badge" + (model ? "" : " edited");
  $("reset").hidden = model && !S.joint.length;
  $("reset").textContent = S.joint.length ? "Reset shares and joint lists" : "Reset to model estimates";
  const st = $("ustatus");
  st.className = "status" + (cur.mode === "single" ? " warn" : "");
  // Until a share is edited, the box invites the reader to build a scenario of their own
  if (model) st.innerHTML = `<div class="cta"><b>Try your own scenario.</b> Drag any party's slider, or type its share, and the seats, their ranges and the chances of each coalition are recalculated for your numbers. You can also put parties on a joint list below.</div>The model's estimates for this week: vote shares and seats are medians, with 80% ranges, and probabilities are shares of the model's simulated parliaments.`;
  else st.textContent = cur.mode === "sim"
      ? `Your scenario, with the model's uncertainty: your shares are applied to each of the week's ${DRAWS.nDraws} simulated draws, with the same regional uncertainty, so seats keep 80% ranges and probabilities.`
      : drawsState === "loading" ? "Loading the simulations for your scenario… Seats below are a single allocation for now."
        : "Seats are a single allocation of your scenario, without uncertainty: the simulation draws are not available.";
  // party rows
  const wk = cur.wk;
  cur.ents.forEach((e, k) => {
    const r = cur.rows[k];
    if (!e.joint) {
      const p = e.name, el = $("seat-" + slug(p)); if (!el) return;
      const d23 = R.seats2023[p] != null ? `<span class="rg ${cls(r.seats - R.seats2023[p])}">${signed(r.seats - R.seats2023[p])} on 2023</span>` : "";
      el.innerHTML = `<span class="n">${r.seats}</span>${r.lo != null ? `<span class="rg">${r.lo}–${r.hi}</span>` : ""}${d23}`;
      const sub = $("sub-" + slug(p));
      const bits = [];
      if (model && p !== "MN") { const i = PIDX[p]; if (wk.voteLo[i] != null) bits.push(`80% range ${wk.voteLo[i].toFixed(1)}–${wk.voteHi[i].toFixed(1)}%`); }
      if (r.pThr != null && r.pThr < 0.995) bits.push(`chance of clearing 5%: ${prob(r.pThr)}`);
      else if (r.pThr == null && !e.mnOnly && cur.tot[k] * 100 < 5 && S.shares[p] > 0) bits.push("below the 5% threshold");
      sub.textContent = bits.join(" · ");
      sub.hidden = !bits.length;
    }
  });
  S.joint.forEach((m, j) => {
    const name = sortParties(m).map(short).join("-");
    const k = cur.ents.findIndex(e => e.name === name); if (k < 0) return;
    const e = cur.ents[k], r = cur.rows[k];
    $("jdot-" + j).style.background = e.colour;
    const below = e.vote < 100 * R.thresholdJoint;
    const s23 = sum(e.members.map(p => R.seats2023[p] || 0));
    $("jt-" + j).innerHTML = `<span>${pct(e.vote)}${dynOn() && Math.abs(e.vote - e.rawVote) > 0.05 ? ` <span class="muted">(${pct(e.rawVote)} before dynamics)</span>` : ""}${below ? ` <span class="neg">below 8%</span>` : ""}${r.pThr != null && r.pThr < 0.995 ? ` <span class="muted">· clears 8%: ${prob(r.pThr)}</span>` : ""}</span>
      <span class="pseats"><span class="n">${r.seats}</span>${r.lo != null ? ` <span class="muted" style="font-size:11.5px">${r.lo}–${r.hi}</span>` : ""} <span class="${cls(r.seats - s23)}" style="font-size:11.5px">${signed(r.seats - s23)}</span></span>`;
  });
  SP.forEach(p => { const row = $("row-" + slug(p)); if (row) row.classList.toggle("hl", S.hoverParty === p); });
  // total
  const named = sum(SP.map(p => S.shares[p] || 0)), other = 100 - named;
  $("panel").classList.toggle("stale", other < -0.05);
  const t = $("total"); t.className = "total" + (other < -0.05 ? " bad" : "");
  t.innerHTML = `<span>Parties ${named.toFixed(1)}% · Other ${other.toFixed(1)}%</span>${other < -0.05 ? "<span>The shares add up to more than 100%: reduce them to recalculate seats.</span>" : other > 6 ? `<span class="muted">Other is unusually large; those votes go to parties that win no seats.</span>` : ""}`;
  // joint-list builder summary
  const js = $("jsummary");
  if (js) {
    const mem = [...S.picker];
    if (mem.length < 2) js.textContent = "Pick at least two parties.";
    else {
      const share = sum(mem.map(p => S.shares[p] || 0)), ok = share >= 100 * R.thresholdJoint;
      const jl = R.jointLists.find(l => l.members.length === mem.length && l.members.every(x => mem.includes(x)));
      const pj = jl && model && wk.jointThr && wk.jointThr[jl.name] != null ? ` · the model's chance of clearing 8%: ${prob(wk.jointThr[jl.name])}` : "";
      const clash = forbiddenIn(mem);
      js.innerHTML = `<span class="${ok ? "pos" : "neg"}">Joint list ${pct(share)}: ${ok ? "clears the 8% threshold" : "below 8%, so it would win no seats"}</span>${pj}${clash.length ? `<br><span class="muted">The tracker treats ${esc(clash.join("; "))} as incompatible in government: allowed here, but not a realistic list.</span>` : ""}`;
    }
    $("jadd").disabled = mem.length < 2;
  }
  $("dynval").textContent = S.dyn + "%";
  renderGov();
}
function renderGov() {
  if (TERM) return;
  const cur = CUR, list = cur.gov, probabilistic = cur.mode !== "single";
  $("govhead").textContent = probabilistic ? "chance of 231+ seats" : "single allocation";
  const shown = S.showAllGov ? list : list.slice(0, 8);
  const focus = S.pinGov || S.focusGov;
  const mn = minorityNote(), mnBox = mn ? `<p class="help" style="margin:0 0 6px;padding:6px 8px;background:var(--tint);border-radius:8px">${mn}</p>` : "";
  $("gov").innerHTML = mnBox + (!list.length
    ? `<p class="help" style="margin:0">No combination of ${S.joint.length ? "lists" : "compatible parties"} reaches a majority${probabilistic ? " in at least 5% of simulations" : ""}.</p>`
    : shown.map(o => {
      const extra = probabilistic
        ? [o.pv >= 0.05 ? `veto override (276) ${prob(o.pv)}` : "", o.pc >= 0.05 ? `constitutional (307) ${prob(o.pc)}` : ""].filter(Boolean).join(" · ")
        : o.s >= R.seatsConstitutional ? "constitutional majority" : o.s >= R.seatsVeto ? "can override a veto" : "";
      return `<div class="grow-row${focus === o.key ? " on" : ""}" data-key="${esc(o.key)}" tabindex="0" role="button" aria-pressed="${S.pinGov === o.key}">
        <span class="nm">${esc(optLabel(o.members))}<br><span class="sub">${o.s} seats${o.lo != null ? ` (${o.lo}–${o.hi})` : ""}${extra ? " · " + extra : ""}</span></span>
        <span class="s">${o.s}</span><span class="p">${probabilistic ? prob(o.p) : "✓"}</span>
        <span class="bar"><i style="width:${(100 * Math.min(o.s, 460) / 460).toFixed(1)}%"></i><u style="left:${(100 * 231 / 460).toFixed(2)}%"></u><u style="left:${(100 * 276 / 460).toFixed(2)}%;opacity:.35"></u><u style="left:${(100 * 307 / 460).toFixed(2)}%;opacity:.35"></u></span>
      </div>`;
    }).join(""));
  const more = $("govmore");
  more.hidden = list.length <= 8;
  more.textContent = S.showAllGov ? "Show fewer" : `Show all ${list.length}`;
}

// ---- trends ---------------------------------------------------------------------------------
const PERIODS = [["all", "All"], ["12", "12 months"], ["6", "6 months"], ["3", "3 months"]];
let TR = null; // geometry of the drawn trend chart, for hover
function monthTicks(a, b, pxW) {
  const start = dateOf(a), out = [];
  const months = (b - a) / 30.4, steps = [1, 2, 3, 4, 6, 12];
  const step = steps.find(s => months / s <= pxW / 64) || 12;
  let y = start.getUTCFullYear(), m = start.getUTCMonth() + 1;
  if (m > 11) { m = 0; y++; }
  while (true) {
    const d = Math.round(Date.UTC(y, m, 1) / 864e5);
    if (d > b) break;
    if (m % step === 0 || step === 1) out.push(d);
    m++; if (m > 11) { m = 0; y++; }
  }
  return out;
}
function tickLabel(d, prevYear) {
  const t = dateOf(d), y = t.getUTCFullYear();
  return [MON[t.getUTCMonth()], y !== prevYear ? String(y) : ""];
}
function repel(items, minGap, lo, hi) { // items: {y}, sorted in place, pushed apart
  items.sort((a, b) => a.y - b.y);
  for (let i = 1; i < items.length; i++) if (items[i].y - items[i - 1].y < minGap) items[i].y = items[i - 1].y + minGap;
  const over = items.length ? items[items.length - 1].y - hi : 0;
  if (over > 0) { items[items.length - 1].y -= over; for (let i = items.length - 2; i >= 0; i--) if (items[i + 1].y - items[i].y < minGap) items[i].y = items[i + 1].y - minGap; }
  if (items.length && items[0].y < lo) { const sh = lo - items[0].y; items.forEach(it => { it.y += sh; }); }
  return items;
}
function renderTrends() {
  $("combobtn").hidden = !!TERM;
  const box = $("trendbox"), W = Math.max(320, box.clientWidth - 12), H = Math.round(clamp(W * 0.5, 300, 520));
  const V = VD(), T = V.trend, dates = T.dates, last = dates[dates.length - 1];
  // Room on the right for the longest end label ("Solidarność / AWS 35.4%")
  const endChars = Math.max(0, ...V.parties.filter(p => T.series[p].med[dates.length - 1] != null).map(p => (W < 600 ? short(p) : p).length + 6));
  const m = { l: 34, r: Math.max(W < 520 ? 92 : 118, 16 + 6.6 * endChars), t: S.events ? 38 : 22, b: 34 };
  const span = S.period === "all" ? null : +S.period * 30.44;
  const a = span ? Math.max(dates[0], last - span) : dates[0], b = last;
  const x = d => m.l + (d - a) / (b - a) * (W - m.l - m.r);
  const hl = S.houses.size > 0;
  // Parties picked in the legend are drawn alone, on an axis scaled to them
  const shown = p => S.parties.size === 0 || S.parties.has(p);
  const parties = V.parties.filter(shown);
  const polls = S.points ? V.polls.filter(r => r[0] >= a && r[0] <= b && shown(V.parties[r[2]])) : [];
  // A poll picked by a click: all its figures, whatever parties are picked in the legend, with every other
  // point and the trend lines greyed out
  const selRows = S.pollSel && S.points ? V.polls.filter(r => pollKey(r) === S.pollSel && r[0] >= a && r[0] <= b) : [];
  if (S.pollSel && !selRows.length) S.pollSel = null;
  const sel = selRows.length > 0;
  let ymax = 0;
  polls.concat(selRows).forEach(r => { ymax = Math.max(ymax, r[3]); });
  parties.forEach(p => { T.series[p].hi.forEach((v, i) => { if (v != null && dates[i] >= a) ymax = Math.max(ymax, v); }); });
  V.results.forEach(r => { if (r.date >= a) Object.entries(r.values).forEach(([p, v]) => { if (shown(p)) ymax = Math.max(ymax, v); }); });
  ymax = Math.ceil((ymax + 1) / 5) * 5;
  const y = v => m.t + (1 - v / ymax) * (H - m.t - m.b);
  const fade = p => sel || (S.hoverParty && S.hoverParty !== p);
  const o = [];
  // grid
  const ystep = ymax > 40 ? 10 : 5;
  for (let v = 0; v <= ymax; v += ystep) o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${y(v)}" y2="${y(v)}" stroke="#EEE"/><text x="${m.l - 6}" y="${y(v) + 4}" text-anchor="end" font-size="11" fill="#767676">${v}%</text>`);
  let prevY = null;
  monthTicks(a, b, W - m.l - m.r).forEach(d => {
    const [lab, yr] = tickLabel(d, prevY); prevY = dateOf(d).getUTCFullYear();
    o.push(`<line x1="${x(d)}" x2="${x(d)}" y1="${m.t}" y2="${H - m.b}" stroke="#F4F4F4"/><text x="${x(d)}" y="${H - m.b + 14}" text-anchor="middle" font-size="11" fill="#767676">${lab}</text>${yr ? `<text x="${x(d)}" y="${H - m.b + 27}" text-anchor="middle" font-size="11" fill="#767676">${yr}</text>` : ""}`);
  });
  // events: a numbered marker over a dotted line, listed under the chart
  const evs = S.events ? trendEvents() : [], evx = [];
  let lastCx = -1e9;
  evs.forEach(l => {
    const ex = x(l.date), cx = Math.max(ex, lastCx + 17); lastCx = cx; evx.push({ cx, e: l });
    o.push(`<line x1="${ex}" x2="${ex}" y1="${m.t - 20}" y2="${H - m.b}" stroke="#AAA" stroke-dasharray="2 3"/><circle cx="${cx}" cy="10" r="8" fill="#fff" stroke="#B5B5B5"/><text x="${cx}" y="13.5" text-anchor="middle" font-size="10" font-weight="500" fill="#4B4B4B">${l.n}</text>`);
  });
  $("trendevents").innerHTML = evs.map(l => `<span class="muted">${l.n}. ${esc(evWhen(l.date))} · ${esc(l.event)}</span>`).join("");
  // bands
  parties.forEach(p => {
    const s = T.series[p]; let top = [], bot = [];
    const flush = () => { if (top.length > 1) o.push(`<path d="M${top.join("L")}L${bot.reverse().join("L")}Z" fill="${col(p)}" fill-opacity="${fade(p) ? 0.04 : 0.16}" stroke="none"/>`); top = []; bot = []; };
    dates.forEach((d, i) => { if (d < a || s.lo[i] == null) { flush(); return; } top.push(`${x(d).toFixed(1)},${y(s.hi[i]).toFixed(1)}`); bot.push(`${x(d).toFixed(1)},${y(s.lo[i]).toFixed(1)}`); });
    flush();
  });
  // polls
  const dots = [];
  polls.forEach(r => {
    if (sel) { if (pollKey(r) !== S.pollSel) dots.push(`<circle cx="${x(r[0]).toFixed(1)}" cy="${y(r[3]).toFixed(1)}" r="2" fill="#BBB" fill-opacity="0.2"/>`); return; }
    const p = V.parties[r[2]], on = !hl || S.houses.has(V.pollsters[r[1]]);
    dots.push(`<circle cx="${x(r[0]).toFixed(1)}" cy="${y(r[3]).toFixed(1)}" r="${on ? 2.4 : 2}" fill="${on ? col(p) : "#BBB"}" fill-opacity="${on ? (fade(p) ? 0.08 : hl ? 0.75 : 0.4) : 0.15}"/>`);
  });
  selRows.forEach(r => dots.push(`<circle cx="${x(r[0]).toFixed(1)}" cy="${y(r[3]).toFixed(1)}" r="4" fill="${col(V.parties[r[2]])}" stroke="#fff" stroke-width="1"/>`));
  o.push(`<g>${dots.join("")}</g>`);
  // lines
  parties.forEach(p => {
    const s = T.series[p]; let seg = [];
    const flush = () => { if (seg.length > 1) o.push(`<path d="M${seg.join("L")}" fill="none" stroke="${col(p)}" stroke-width="${S.hoverParty === p ? 2.6 : 1.8}" stroke-opacity="${fade(p) ? 0.2 : 1}"/>`); seg = []; };
    dates.forEach((d, i) => { if (d < a || s.med[i] == null) { flush(); return; } seg.push(`${x(d).toFixed(1)},${y(s.med[i]).toFixed(1)}`); });
    flush();
  });
  // the election results that anchor a past term's trend
  V.results.forEach(r => {
    if (r.date < a) return;
    Object.entries(r.values).forEach(([p, v]) => {
      if (!shown(p)) return;
      const cx = x(r.date), cy = y(v);
      o.push(`<path d="M${cx},${cy - 5}L${cx + 5},${cy}L${cx},${cy + 5}L${cx - 5},${cy}Z" fill="#fff" stroke="${col(p)}" stroke-width="1.6" stroke-opacity="${fade(p) ? 0.25 : 1}"/>`);
    });
  });
  // selected week
  const sw = WEEKS[S.w].date;
  if (S.dateLine && !V.past && sw >= a && sw <= b) o.push(`<line x1="${x(sw)}" x2="${x(sw)}" y1="${m.t - 8}" y2="${H - m.b}" stroke="#333" stroke-width="1"/><text x="${x(sw)}" y="${m.t - 11}" text-anchor="middle" font-size="10.5" fill="#333">${dstr(sw)}</text>`);
  // end labels
  const li = dates.length - 1;
  const labs = parties.filter(p => T.series[p].med[li] != null).map(p => ({ p, v: T.series[p].med[li], y: y(T.series[p].med[li]) }));
  repel(labs, 13, m.t, H - m.b);
  labs.forEach(l => {
    const ly0 = y(l.v);
    o.push(`<line x1="${x(last) + 3}" x2="${x(last) + 10}" y1="${ly0}" y2="${l.y}" stroke="#CCC"/><text x="${x(last) + 13}" y="${l.y + 4}" font-size="11.5" fill="${col(l.p)}" font-weight="500" fill-opacity="${fade(l.p) ? 0.3 : 1}">${esc(W < 600 ? short(l.p) : l.p)} ${l.v.toFixed(1)}%</text>`);
  });
  o.push(`<g id="trendhover"></g><rect x="${m.l}" y="${m.t}" width="${W - m.l - m.r}" height="${H - m.t - m.b}" fill="transparent" id="trendhit" style="cursor:crosshair"/>`);
  const svg = $("trend");
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.setAttribute("width", W); svg.setAttribute("height", H);
  svg.innerHTML = o.join("");
  TR = { W, H, m, a, b, x, y, polls: sel ? selRows : polls, allPolls: polls, selKey: sel ? S.pollSel : null, hl, shown, evx };
  // legend
  // A past term lists every party it had, with the closing figure for those still standing
  const legend = V.past ? V.parties : P.filter(p => T.series[p].med[li] != null);
  $("trendlegend").classList.toggle("picked", S.parties.size > 0);
  $("trendlegend").innerHTML = legend.map(p => { const v = T.series[p].med[li]; return `<button type="button" data-p="${esc(p)}" aria-pressed="${S.parties.has(p)}"><span class="dot" style="background:${col(p)}"></span>${esc(p)}${v != null ? " " + v.toFixed(1) + "%" : ""}</button>`; }).join("") +
    (S.parties.size ? `<button type="button" class="linkbtn" id="partyclear">Show all parties</button>` : "") +
    (sel ? `<button type="button" class="linkbtn" id="pollclear">Show all polls</button>` : "");
}
function trendHover(ev) {
  if (!TR) return;
  const svg = $("trend"), r = svg.getBoundingClientRect(), k = TR.W / r.width;
  const mx = (ev.clientX - r.left) * k, my = (ev.clientY - r.top) * k;
  const tip = $("trendtip"), g = $("trendhover");
  if (my < TR.m.t - 18) {
    const hit = TR.evx.find(o => Math.abs(o.cx - mx) < 10);
    if (hit) { g.innerHTML = ""; return placeTip(tip, $("trendbox"), ev, `<b>${esc(hit.e.event)}</b><div class="muted">${dstr(hit.e.date)}</div>`); }
  }
  // nearest poll within 8px
  let best = null, bd = 64;
  const V = VD();
  TR.polls.forEach(p => {
    if (!TR.selKey && TR.hl && !S.houses.has(V.pollsters[p[1]])) return;
    const dx = TR.x(p[0]) - mx, dy = TR.y(p[3]) - my, dd = dx * dx + dy * dy;
    if (dd < bd) { bd = dd; best = p; }
  });
  let html;
  if (best) {
    g.innerHTML = `<circle cx="${TR.x(best[0])}" cy="${TR.y(best[3])}" r="${TR.selKey ? 7 : 5}" fill="none" stroke="#141414"/>`;
    html = TR.selKey ? pollTipHtml(best, TR.polls, V) : `<div class="r"><span><span class="dot" style="background:${col(V.parties[best[2]])}"></span><b>${esc(V.parties[best[2]])}</b></span><span>${best[3].toFixed(1)}%</span></div><div class="muted">${esc(V.pollsters[best[1]])} · ${dstr(best[0])}</div>`;
  } else {
    const dates = V.trend.dates, d = TR.a + (mx - TR.m.l) / (TR.W - TR.m.l - TR.m.r) * (TR.b - TR.a);
    let i = 0, bdd = Infinity; dates.forEach((t, j) => { const dd = Math.abs(t - d); if (dd < bdd) { bdd = dd; i = j; } });
    const vals = V.parties.filter(TR.shown).map(p => ({ p, v: V.trend.series[p].med[i], lo: V.trend.series[p].lo[i], hi: V.trend.series[p].hi[i] })).filter(o => o.v != null).sort((p, q) => q.v - p.v);
    g.innerHTML = `<line x1="${TR.x(dates[i])}" x2="${TR.x(dates[i])}" y1="${TR.m.t}" y2="${TR.H - TR.m.b}" stroke="#141414" stroke-opacity=".35"/>`;
    html = `<b>${dstr(dates[i])}</b>` + vals.map(o => `<div class="r"><span><span class="dot" style="background:${col(o.p)}"></span>${esc(o.p)}</span><span>${o.v.toFixed(1)}%${o.lo != null ? ` <span class="muted" style="display:inline">(${o.lo.toFixed(1)}–${o.hi.toFixed(1)})</span>` : ""}</span></div>`).join("") + (V.past ? "" : `<div class="muted" style="margin-top:3px">Click to load the nearest week</div>`);
  }
  placeTip(tip, $("trendbox"), ev, html);
}
function placeTip(tip, box, ev, html) {
  tip.innerHTML = html; tip.hidden = false;
  const br = box.getBoundingClientRect();
  let lx = ev.clientX - br.left + 14, ly = ev.clientY - br.top + 14;
  const tw = tip.offsetWidth, th = tip.offsetHeight;
  if (lx + tw > br.width - 4) lx = ev.clientX - br.left - tw - 14;
  if (ly + th > br.height - 4) ly = Math.max(4, ev.clientY - br.top - th - 14);
  tip.style.left = lx + "px"; tip.style.top = ly + "px";
}
function nearestWeek(d) { let i = 0, bd = Infinity; WEEKS.forEach((w, j) => { const dd = Math.abs(w.date - d); if (dd < bd) { bd = dd; i = j; } }); return i; }

// ---- seats: hemicycle, map, cards -----------------------------------------------------------
function hemiLayout(n, rows = 11) {
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
// ---- the seats in the hemicycle, one dot for each seat in one constituency ---------------------
// A list's k-th seat in a constituency is won in the share of simulated parliaments in which the list wins
// at least k seats there. A list's dots are its most probable seats, as many as it is projected to win.
const SEAT_CERTAIN = 0.975;   // won in at least this share of the simulations: certain
const SEAT_PROBABLE = 0.5;    // from here to SEAT_CERTAIN: probable; below it, a toss-up
const seatCat = p => (p >= SEAT_CERTAIN ? "certain" : p >= SEAT_PROBABLE ? "probable" : "toss-up");
const ord = n => { const v = n % 100; return n + (["th", "st", "nd", "rd"][(v - 20) % 10] || ["th", "st", "nd", "rd"][v] || "th"); };
const SEATPROB = new WeakMap();
function hemiSim() {
  const cur = CUR;
  let sim = cur.sim;
  if (!sim && cur.mode === "model" && drawsState === "ready") {
    if (!MODEL_SIM.has(S.w)) MODEL_SIM.set(S.w, simulate(S.w, null));
    sim = MODEL_SIM.get(S.w);
  }
  return sim || null;
}
// For each list in a simulation: its seats {ci, j, p}, most probable first
function seatTable(sim) {
  if (SEATPROB.has(sim)) return SEATPROB.get(sim);
  const { n, ents, cons } = sim, E = ents.length, tab = {};
  ents.forEach((e, kk) => {
    const list = [];
    for (let ci = 0; ci < NC; ci++) {
      const mag = C[ci].magnitude, cnt = new Int32Array(mag + 1);
      for (let d = 0; d < n; d++) cnt[Math.min(cons[d * NC * E + ci * E + kk], mag)]++;
      let ge = 0;
      for (let j = mag; j >= 1; j--) { ge += cnt[j]; if (ge > 0) list.push({ ci, j, p: ge / n }); }
    }
    list.sort((a, b) => b.p - a.p || a.ci - b.ci || a.j - b.j);
    tab[e.name] = list;
  });
  SEATPROB.set(sim, tab);
  return tab;
}
let HEMI = null;
function renderHemi() {
  const cur = CUR, ents = cur.ents;
  const focusKey = S.pinGov || S.focusGov || (cur.mwc && cur.mwc.key);
  const focus = focusKey ? (cur.gov.find(o => o.key === focusKey) || (cur.govAll || []).find(o => o.key === focusKey) || (cur.mwc && cur.mwc.key === focusKey ? cur.mwc : null)) : null;
  const inFocus = e => focus && focus.members.includes(e.name);
  const won = ents.map((e, k) => ({ e, k, s: cur.seats[k] })).filter(o => o.s > 0);
  const hord = e => Math.min(...e.members.map(p => R.hemicycleOrder.indexOf(p)));
  const winners = won.filter(o => inFocus(o.e)).sort((p, q) => hord(p.e) - hord(q.e));
  let rest = won.filter(o => !inFocus(o.e)).sort((p, q) => q.s - p.s);
  if (rest.length) rest = [...rest.slice(1), rest[0]];
  if (!focus) { rest = won.sort((p, q) => hord(p.e) - hord(q.e)); }
  const ordered = focus ? winners.concat(rest) : rest;
  const blocks = [];
  ordered.forEach(o => {
    const mem = R.hemicycleOrder.filter(p => o.e.members.includes(p));
    mem.forEach(p => { const n = o.e.joint ? cur.memberSeats[p] : o.s; if (n > 0) blocks.push({ p, ent: o.e, k: o.k, n }); });
  });
  const total = sum(blocks.map(b => b.n)), pts = hemiLayout(total);
  // Each list's most probable seats, dealt to its blocks; within a block the most certain are at the end of
  // the arc away from the centre of the chamber, so the toss-ups sit where the blocks meet
  const sim = hemiSim(), tab = sim ? seatTable(sim) : null, info = new Array(total).fill(null), byEnt = new Map();
  { let at = 0; blocks.forEach(bk => { bk.at = at; at += bk.n; if (!byEnt.has(bk.k)) byEnt.set(bk.k, []); byEnt.get(bk.k).push(bk); }); }
  if (tab) byEnt.forEach((bks, k) => {
    const L = tab[ents[k].name] || []; let take = 0;
    bks.forEach(bk => {
      const chunk = L.slice(take, take + bk.n); take += bk.n;
      const m = pts[bk.at + (bk.n >> 1)], left = m.x < 0;
      chunk.forEach((st, j) => { info[bk.at + (left ? j : bk.n - 1 - j)] = st; });
    });
  });
  const cert = !!(S.hemiCert && tab);
  const outer = Math.max(...pts.map(p => Math.hypot(p.x, p.y))), labels = [];
  { let at = 0; blocks.forEach(bk => { const m = pts[at + (bk.n >> 1)]; labels.push({ p: bk.p, n: bk.n, a: Math.atan2(m.y, m.x) }); at += bk.n; }); }
  const rad = Math.min(0.046, 0.046 * Math.sqrt(460 / Math.max(total, 1)));
  let i = 0;
  const o = [];
  const explicit = !!(S.pinGov || S.focusGov), hp = S.hoverParty;
  blocks.forEach(bk => {
    const dim = (explicit && !inFocus(bk.ent)) || (hp && !bk.ent.members.includes(hp) && bk.ent.name !== hp);
    for (let j = 0; j < bk.n; j++, i++) {
      const pt = pts[i], st = info[i], cat = cert && st ? seatCat(st.p) : "certain";
      let fill = col(bk.p), fo = dim ? 0.18 : 1, stroke = "";
      if (cat === "probable") { fo = dim ? 0.1 : 0.5; }
      else if (cat === "toss-up") { fill = "#fff"; fo = dim ? 0.3 : 1; stroke = ` stroke="${col(bk.p)}" stroke-width="0.014"`; }
      o.push(`<circle cx="${pt.x.toFixed(3)}" cy="${(-pt.y).toFixed(3)}" r="${rad.toFixed(3)}" fill="${fill}" fill-opacity="${fo}"${stroke} data-k="${bk.k}" data-i="${i}"/>`);
    }
  });
  o.push(`<line x1="0" x2="0" y1="-0.86" y2="-2.12" stroke="#141414" stroke-width="0.012" stroke-dasharray="0.04 0.04"/><text x="0" y="-2.16" text-anchor="middle" font-size="0.085" fill="#4B4B4B">231</text>`);
  if (focus) o.push(`<text x="0" y="-0.42" text-anchor="middle" font-size="0.13" font-weight="500" fill="#141414">${esc(optLabel(focus.members))}</text><text x="0" y="-0.22" text-anchor="middle" font-size="0.2" font-weight="300" fill="#141414">${focus.s} seats</text>${focus.p != null && cur.mode !== "single" ? `<text x="0" y="-0.07" text-anchor="middle" font-size="0.085" fill="#767676">${prob(focus.p)} chance of a majority</text>` : ""}`);
  else o.push(`<text x="0" y="-0.3" text-anchor="middle" font-size="0.12" fill="#767676">No governing majority</text>`);
  const svg = $("hemi");
  svg.setAttribute("viewBox", "-2.2 -2.28 4.4 2.36");
  svg.innerHTML = o.join("");
  HEMI = { total, outer, labels, info, sim, cert: null };
  const cbtn = $("hemicertbtn");
  cbtn.disabled = !tab; cbtn.setAttribute("aria-pressed", String(cert));
  cbtn.title = tab ? "Shade each seat by how likely it is to be won" : "Needs the seat simulation, which loads with the scenario data";
  if (cert) {
    const entSeats = {};
    const crows = [...byEnt.keys()].map(k => {
      const e = ents[k], L = info.filter((st, ii) => st && blocks.some(bk => bk.k === k && ii >= bk.at && ii < bk.at + bk.n)), c = { certain: 0, probable: 0, "toss-up": 0 };
      L.forEach(st => { c[seatCat(st.p)]++; });
      entSeats[k] = L;
      return { k, name: e.name, colour: e.colour, seats: cur.seats[k], certain: c.certain, probable: c.probable, toss: c["toss-up"] };
    });
    HEMI.cert = { rows: crows };
    HEMI.entSeats = entSeats;
    const rows = crows.map(r => `<tr class="clickrow" data-k="${r.k}" tabindex="0" role="button" title="Seats of ${esc(r.name)} by constituency"><td><span class="dot" style="background:${r.colour};margin-right:6px"></span>${esc(r.name)}</td><td>${r.seats}</td><td>${r.certain}</td><td>${r.probable}</td><td>${r.toss}</td></tr>`).join("");
    $("hemicert").innerHTML = `<div style="display:flex;gap:14px;flex-wrap:wrap;align-items:center;font-size:12px;margin:6px 0 2px">
      <span><svg width="12" height="12"><circle cx="6" cy="6" r="5" fill="#555"/></svg> Certain (${(SEAT_CERTAIN * 100).toFixed(1)}% or more)</span>
      <span><svg width="12" height="12"><circle cx="6" cy="6" r="5" fill="#555" fill-opacity=".5"/></svg> Probable (${Math.round(SEAT_PROBABLE * 100)}–${(SEAT_CERTAIN * 100).toFixed(1)}%)</span>
      <span><svg width="12" height="12"><circle cx="6" cy="6" r="4.6" fill="#fff" stroke="#555" stroke-width="1.2"/></svg> Toss-up (under ${Math.round(SEAT_PROBABLE * 100)}%)</span></div>
      <table class="t"><tr><th>List</th><th>Seats</th><th>Certain</th><th>Probable</th><th>Toss-up</th></tr>${rows}</table>
      <p class="help" style="margin:4px 0 0">Each dot is one seat in one constituency: a list's most probable seats, as many as its projected total. The chance is the share of simulated parliaments in which the list wins at least that many seats there (certain, 97.5% or more; probable, 50% to 97.5%). Within each list the most certain seats are at the outer end of its arc. Click a list for its seats in each constituency.</p>`;
  } else $("hemicert").innerHTML = "";
  const lab = $("hemilabel");
  lab.innerHTML = cur.mwc
    ? `<b style="color:var(--ink)">Most plausible government:</b> ${esc(optLabel(cur.mwc.members))}, ${cur.mwc.s} seats${cur.mwc.prob && cur.mwc.p != null ? ` · ${prob(cur.mwc.p)} chance of a majority` : ""}${cur.mwc.s >= R.seatsConstitutional ? " · constitutional majority" : cur.mwc.s >= R.seatsVeto ? " · can override a presidential veto" : ""}. Hover or click a government option to show it instead.`
    : (minorityNote() || "No combination reaches a governing majority.");
}
// A list's seats in each of the 41 constituencies, by certainty
let MODAL_FROM = null;
function openSeatModal(k) {
  const cur = CUR, e = cur.ents[k], L = (HEMI.entSeats && HEMI.entSeats[k]) || [];
  const per = C.map(() => ({ certain: 0, probable: 0, toss: 0 }));
  L.forEach(st => { const c = per[st.ci], cat = seatCat(st.p); if (cat === "certain") c.certain++; else if (cat === "probable") c.probable++; else c.toss++; });
  const cell = v => v ? v : `<span class="muted">0</span>`;
  const tot = per.reduce((a, c) => ({ certain: a.certain + c.certain, probable: a.probable + c.probable, toss: a.toss + c.toss }), { certain: 0, probable: 0, toss: 0 });
  const rows = C.map((c, ci) => { const x = per[ci], n = x.certain + x.probable + x.toss; return `<tr><td>${c.okreg}. ${esc(c.name)}</td><td>${c.magnitude}</td><td>${n ? `<b>${n}</b>` : `<span class="muted">0</span>`}</td><td>${cell(x.certain)}</td><td>${cell(x.probable)}</td><td>${cell(x.toss)}</td></tr>`; }).join("");
  $("seatmodaltitle").innerHTML = `<span class="dot" style="background:${e.colour};margin-right:6px"></span>${esc(e.name)}: ${cur.seats[k]} seats by constituency`;
  $("seatmodalbody").innerHTML = `<p class="help" style="margin:0 0 6px">The seats shown as dots for this list, by how likely each is. Certain: won in at least ${(SEAT_CERTAIN * 100).toFixed(1)}% of simulations; probable: ${Math.round(SEAT_PROBABLE * 100)}% to ${(SEAT_CERTAIN * 100).toFixed(1)}%; toss-up: under ${Math.round(SEAT_PROBABLE * 100)}%. ${cur.mode === "model" ? "Model estimates" : "Scenario"} for ${dstr(cur.wk.date)}.</p>
    <table class="t hist"><tr><th>Constituency</th><th>District magnitude</th><th>Seats won</th><th>Certain</th><th>Probable</th><th>Toss-up</th></tr>${rows}
    <tr style="font-weight:600"><td>All 41</td><td>${sum(C.map(c => c.magnitude))}</td><td>${tot.certain + tot.probable + tot.toss}</td><td>${tot.certain}</td><td>${tot.probable}</td><td>${tot.toss}</td></tr></table>`;
  MODAL_FROM = document.activeElement;
  $("seatmodal").hidden = false; $("seatmodalclose").focus();
}
function closeSeatModal() { $("seatmodal").hidden = true; if (MODAL_FROM && MODAL_FROM.focus) MODAL_FROM.focus(); MODAL_FROM = null; }
function hemiHover(ev) {
  const c = ev.target.closest("circle[data-k]");
  const tip = $("hemitip");
  if (!c) { tip.hidden = true; return; }
  const k = +c.dataset.k, e = CUR.ents[k], r = CUR.rows[k], st = HEMI && HEMI.info ? HEMI.info[+c.dataset.i] : null;
  let seat = "";
  if (st) {
    const cc = C[st.ci], kk = HEMI.sim.ents.findIndex(x => x.name === e.name), rg = kk >= 0 ? consRange(HEMI.sim, st.ci, kk) : null;
    seat = `<div style="margin-top:6px;padding-top:5px;border-top:1px solid #EEE"><b>${esc(cc.name)}</b> <span class="muted">constituency ${cc.okreg}</span>
      <div class="muted">${esc(e.name)}'s ${ord(st.j)} seat (district magnitude: ${cc.magnitude})</div>
      <div class="r"><span>Chance of winning it</span><span style="margin-left:10px"><b>${prob(st.p)}</b> <span class="muted">${seatCat(st.p)}</span></span></div>
      ${rg ? `<div class="muted">${rg[0] === rg[1] ? rg[0] : rg[0] + "–" + rg[1]} seats here in 80% of simulations</div>` : ""}</div>`;
  }
  placeTip(tip, $("hemibox"), ev, `<div class="r"><span><span class="dot" style="background:${e.colour}"></span><b>${esc(e.name)}</b></span><span>${r.seats} seats</span></div>${r.lo != null ? `<div class="muted">80% range ${r.lo}–${r.hi}</div>` : ""}${e.joint ? e.members.map(p => `<div class="r"><span><span class="dot" style="background:${col(p)}"></span>${esc(p)}</span><span>${CUR.memberSeats[p]}</span></div>`).join("") + `<div class="muted">Joint list: seats split by vote share</div>` : ""}${seat}`);
}
function mix(hex, t) { // blend hex colour towards white: t = 1 full colour
  const n = parseInt(hex.slice(1), 16), r = n >> 16 & 255, g = n >> 8 & 255, b = n & 255;
  const f = v => Math.round(v + (255 - v) * (1 - t));
  return `rgb(${f(r)},${f(g)},${f(b)})`;
}
let OUTLINE = null;
const cartOutline = () => OUTLINE || (OUTLINE = Cartogram.outline(C, [0, 0, ...D.cartogramBox]));
function renderMap() {
  const cur = CUR, E = cur.ents.length, [bw, bh] = D.cartogramBox;
  const hp = S.hoverParty, hk = hp ? cur.ents.findIndex(e => e.name === hp || e.members.includes(hp)) : -1;
  const o = [`<path d="${cartOutline().d}" fill="none" stroke="#141414" stroke-width="2.5" stroke-linejoin="round"/>`];
  C.forEach((c, ci) => {
    const seats = Array.from({ length: E }, (_, k) => cur.single.cons[ci * E + k]);
    const sel = S.constSel === c.okreg;
    o.push(`<g data-c="${ci}" style="cursor:pointer"><circle cx="${c.x}" cy="${c.y}" r="${c.r - 1.5}" fill="transparent" stroke="${sel ? "#141414" : "#9A9A9A"}" stroke-width="${sel ? 4 : 1.5}"/>`);
    // one dot per elected MP, grouped by party and packed in a sunflower spiral
    const dots = [];
    seats.map((s, k) => [s, k]).filter(d => d[0] > 0).sort((p, q) => q[0] - p[0]).forEach(([s, k]) => { for (let j = 0; j < s; j++) dots.push(k); });
    const pk = Cartogram.dots(c, dots.length);
    dots.forEach((k, i) => {
      const on = hk < 0 || k === hk;
      o.push(`<circle cx="${pk.pts[i][0].toFixed(1)}" cy="${pk.pts[i][1].toFixed(1)}" r="${pk.r.toFixed(1)}" fill="${on ? cur.ents[k].colour : "#DEDEDE"}"/>`);
    });
    o.push("</g>");
  });
  const svg = $("map");
  svg.setAttribute("viewBox", `-30 -30 ${bw + 60} ${bh + 60}`);
  svg.innerHTML = o.join("");
  $("mapnote").textContent = (hk >= 0 ? `${cur.ents[hk].name}'s seats in each constituency. ` : "One dot for each MP elected, in the colour of their list. ") +
    "Circles are sized by electorate. A single allocation at the shares on screen, so totals can differ slightly from the medians. Click a constituency for its seats.";
}
function constSeats(ci) {
  const cur = CUR, E = cur.ents.length;
  let sim = cur.sim;
  if (!sim && cur.mode === "model" && drawsState === "ready") {
    if (!MODEL_SIM.has(S.w)) MODEL_SIM.set(S.w, simulate(S.w, null));
    sim = MODEL_SIM.get(S.w);
  }
  return cur.ents.map((e, k) => {
    const s = cur.single.cons[ci * E + k];
    let rg = null;
    if (sim) { const kk = sim.ents.findIndex(x => x.name === e.name); if (kk >= 0) rg = consRange(sim, ci, kk); }
    return { e, s, rg };
  }).filter(r => r.s > 0 || (r.rg && r.rg[1] > 0)).sort((a, b) => b.s - a.s);
}
function renderConstCard() {
  const card = $("constcard");
  const ci = S.constSel == null ? -1 : C.findIndex(c => c.okreg === S.constSel);
  if (ci < 0) { card.innerHTML = `<span class="label">Constituency</span><p class="help" style="margin:0">Click a circle on the map to see how its seats are shared out.</p>`; return; }
  const c = C[ci], rows = constSeats(ci), anyRg = rows.some(r => r.rg);
  // Seats in the elections since 2001, from the lists each party stood on (D.history): a figure, 0 where the
  // party stood and won nothing in this constituency, a dash where it did not stand; † where its list was
  // a joint list with another party in the table. A joint list's seats are shown against each member.
  const hist = D.history || [], years = [...new Set(hist.map(h => h.year))].sort((a, b) => b - a), joints = new Map();
  const past = (e, y) => {
    const ls = hist.filter(h => h.year === y && h.seats[ci] != null && h.members.some(m => e.members.includes(m)));
    if (!ls.length) return `<td class="muted">–</td>`;
    const jl = ls.filter(h => h.members.some(m => !e.members.includes(m)));
    jl.forEach(h => joints.set(h.id, `${h.year} ${h.list} (${h.members.map(short).join(" + ")})`));
    return `<td>${sum(ls.map(h => h.seats[ci]))}${jl.length ? "†" : ""}</td>`;
  };
  const body = rows.map(r => `<tr><td><span class="dot" style="background:${r.e.colour};margin-right:6px"></span>${esc(r.e.name)}</td><td>${r.s}</td>${anyRg ? `<td>${r.rg ? r.rg[0] + "–" + r.rg[1] : "–"}</td>` : ""}${years.map(y => past(r.e, y)).join("")}</tr>`).join("");
  card.innerHTML = `<div style="display:flex;justify-content:space-between;align-items:baseline;gap:8px"><span class="label">Constituency ${c.okreg}</span><button type="button" class="linkbtn" id="constclose">Close</button></div>
    <div style="font-size:20px">${esc(c.name)} <span class="muted" style="font-size:13px">${c.magnitude} seats</span></div>
    <div style="overflow-x:auto"><table class="t hist">
    ${years.length ? `<tr><th colspan="${anyRg ? 3 : 2}" style="border-bottom:0"></th><th colspan="${years.length}" style="font-weight:400;border-bottom:0;text-align:center;white-space:nowrap">Seats in earlier elections</th></tr>` : ""}
    <tr><th>List</th><th>Seats</th>${anyRg ? "<th>80% range</th>" : ""}${years.map(y => `<th>${y}</th>`).join("")}</tr>
    ${body}</table></div>
    ${joints.size ? `<p class="help" style="margin:0">† Stood on a joint list with another party shown here, whose seats are shown against each: ${[...joints.values()].sort().reverse().map(esc).join("; ")}. A dash: the party did not stand.</p>` : `<p class="help" style="margin:0">A dash: the party did not stand.</p>`}
    <a class="linkbtn" style="text-decoration:none" href="../../pages/polish-political-analyses/election-results.html#l=c&amp;u=c${c.okreg}" target="_top">${esc(c.name)} in every election since 1990 →</a>`;
}
// ---- the 2023 comparison and the other possible governments, shown on request under the hemicycle ----
function since2023() {
  const cur = CUR;
  return R.election2023.map(l => ({ l, now: sum(l.parties.map(p => cur.eff[p] || 0)), seats: sum(l.parties.map(p => cur.memberSeats[p] || 0)) }));
}
function render2023() {
  const box = $("hemi2023"), cur = CUR;
  if (!S.hemi2023 || !cur) { box.innerHTML = ""; return; }
  const rows = since2023().map(({ l, now, seats }) =>
    `<tr><td>${esc(l.list)} <span class="muted" style="font-size:11.5px">${l.parties.length > 1 ? "now " + l.parties.map(short).join(" + ") : ""}</span></td><td>${l.vote.toFixed(1)}%</td><td>${now.toFixed(1)}%</td><td class="${cls(now - l.vote)}">${signed(now - l.vote, 1)}</td><td>${l.seats}</td><td>${seats}</td><td class="${cls(seats - l.seats)}">${signed(seats - l.seats)}</td></tr>`).join("");
  box.innerHTML = `<span class="label" style="display:block;margin:10px 0 4px">Change since the 2023 election</span>
    <table class="t"><tr><th>2023 list</th><th>Vote 2023</th><th>Now</th><th></th><th>Seats 2023</th><th>Now</th><th></th></tr>${rows}</table>
    <p class="help" style="margin:4px 0 0">Today's parties summed into the lists they stood on in October 2023 (PKW results). "Now" is the ${cur.mode === "model" ? "model's estimate" : "scenario"} for ${dstr(cur.wk.date)}.</p>`;
}
// Parties on the lists of an option (a joint list stands for its members)
const partiesOf = o => o.members.flatMap(nm => { const e = CUR.ents.find(x => x.name === nm); return e ? e.members : [nm]; });
const breaksOf = o => { const ps = partiesOf(o); return R.forbidden.filter(fp => fp.every(x => ps.includes(x))); };
const COAL_KIND = ["Majority in most simulations", "", "Possible, short of a majority in most simulations"];
// Every combination with at least a 5% chance of a majority, from the most to the least plausible: first those with
// a majority in at least half the simulations, ordered by ideological adjacency (see plaus), then the rest
function coalOptions() {
  const win = o => o.p >= 0.5;
  const items = (CUR.govAll || []).filter(o => o.p >= 0.05).map(o => ({ o, kind: win(o) ? 0 : 2, br: breaksOf(o) }));
  items.sort((a, b) => a.kind - b.kind || byPlaus(a.o, b.o));
  // An oversized majority (one that holds a smaller majority of its own, so some member's seats are not needed)
  // ranks below that smaller majority, however adjacent the extra party is
  for (let pass = 0; pass < items.length; pass++) {
    let moved = false;
    for (let i = 0; i < items.length; i++) {
      let last = -1; items.forEach((b, j) => { if (j !== i && win(b.o) && isSub(b.o, items[i].o)) last = Math.max(last, j); });
      if (last > i) { items.splice(last, 0, items.splice(i, 1)[0]); moved = true; break; }
    }
    if (!moved) break;
  }
  return items;
}
// With no viable majority (no option has one in most simulations, or in the single allocation), the largest list
// has to try to govern alone. Returns the sentence, or "" when a majority is viable.
function minorityNote() {
  const cur = CUR; if (!cur || TERM) return "";
  const probabilistic = cur.mode !== "single";
  if ((cur.govAll || []).some(o => probabilistic ? o.p >= 0.5 : o.s >= R.seatsMajority)) return "";
  let k = -1; cur.ents.forEach((e, i) => { if (k < 0 || cur.seats[i] > cur.seats[k] || (cur.seats[i] === cur.seats[k] && e.vote > cur.ents[k].vote)) k = i; });
  const e = cur.ents[k], n = cur.seats[k], rng = cur.rows[k] && cur.rows[k].lo != null ? ` (${cur.rows[k].lo}–${cur.rows[k].hi})` : "";
  return `<b>No viable majority.</b> The largest ${e.joint ? "list" : "party"}, ${esc(optLabel(e.joint ? e.members : [e.name]))} with ${n} seats${rng}, would have to try to form a minority government${R.seatsMajority - n <= 10 ? " or court defectors from other parties" : ""}.`;
}
const isNewKey = key => !!(S.coalMsg && S.coalMsg.keys.includes(key));
function renderHemiCoal() {
  const box = $("hemicoal");
  if (!S.hemiCoal || !CUR) { box.innerHTML = ""; return; }
  const cur = CUR, probabilistic = cur.mode !== "single", items = coalOptions(), joint = S.joint.length > 0;
  const canRelax = (cur.mode !== "model" || drawsState === "ready");
  const focus = S.pinGov || S.focusGov;
  const dk = defaultExcl(), cp = P.filter(p => p !== "MN");
  const cell = (a, b) => {
    const k = pairKey(a, b), x = S.excl.has(k), changed = x !== dk.has(k), n = items.filter(it => it.br.some(f => pairKey(f[0], f[1]) === k)).length;
    const off = x && !canRelax;
    return `<button type="button" class="xc ${x ? "ex" : "in"}${changed ? " chg" : ""}" data-pair="${esc(k)}" aria-pressed="${x}" aria-label="${esc(pairText([a, b]))}: ${x ? "excluded" : "included"}${changed ? ", changed from the default" : ""}" title="${esc(pairText([a, b]))}: ${x ? "excluded" : "included"}${x && !off ? " (click to include)" : off ? "" : " (click to exclude)"}${!x && n ? ` · ${n} option${n > 1 ? "s" : ""} below ${n > 1 ? "break" : "breaks"} the default` : ""}"${off ? " disabled" : ""}>${x ? "✕" : ""}</button>`;
  };
  const head = `<span></span>${cp.slice(0, -1).map(p => `<button type="button" class="xh" data-party="${esc(p)}"${!canRelax ? " disabled" : ""} title="Toggle every pair with ${esc(p)}">${esc(short(p))}</button>`).join("")}`;
  const body = cp.slice(1).map((r, i) => `<button type="button" class="xh xr" data-party="${esc(r)}"${!canRelax ? " disabled" : ""} title="Toggle every pair with ${esc(r)}">${esc(short(r))}</button>${cp.slice(0, cp.length - 1).map((c, j) => j < i + 1 ? cell(r, c) : "<span></span>").join("")}`).join("");
  const nExcl = S.excl.size, nChg = [...dk].filter(k => !S.excl.has(k)).length + [...S.excl].filter(k => !dk.has(k)).length;
  const rules = `<div class="xgrid" style="grid-template-columns:auto repeat(${cp.length - 1},34px)" role="group" aria-label="Pairs of parties that will not govern together">${head}${body}</div>
    <div class="xkey"><span><i class="xc ex">✕</i> excluded</span><span><i class="xc in"></i> included</span><span><i class="xc in chg"></i> changed from default</span>${nChg ? `<button type="button" class="linkbtn" id="xreset">Reset to defaults</button>` : ""}</div>`;
  let kind = -1;
  const rows = items.map(({ o, kind: k, br }) => {
    const head = k !== kind ? `<div class="muted" style="font-size:11.5px;margin:8px 0 2px;font-weight:500">${k === 0 ? "Majority in most simulations, most plausible first" : COAL_KIND[k]}</div>` : "";
    kind = k;
    const sub = `${isNewKey(o.key) ? `<b style="color:var(--accent)">new</b> · ` : ""}${o.s} seats${o.lo != null ? ` (${o.lo}–${o.hi})` : ""}${br.length ? ` · <span class="neg">breaks: ${esc(br.map(fp => fp.join(" and ")).join("; "))}</span>` : ""}`;
    const isNew = S.coalMsg && S.coalMsg.keys.includes(o.key);
    return head + `<div class="grow-row${focus === o.key ? " on" : ""}${isNew ? " newopt" : ""}" data-key="${esc(o.key)}" tabindex="0" role="button" aria-pressed="${S.pinGov === o.key}">
      <span class="nm">${esc(optLabel(o.members))}<br><span class="sub">${sub}</span></span>
      <span class="s">${o.s}</span><span class="p">${probabilistic ? prob(o.p) : "✓"}</span>
      <span class="bar"><i style="width:${(100 * Math.min(o.s, 460) / 460).toFixed(1)}%"></i><u style="left:${(100 * 231 / 460).toFixed(2)}%"></u></span></div>`;
  }).join("");
  const m = S.coalMsg;
  const msg = (m ? `<p class="help" style="margin:2px 0 6px;padding:6px 8px;background:var(--tint);border-radius:8px">${m.text}</p>` : "");
  box.innerHTML = `<span class="label" style="display:block;margin:10px 0 2px">Possible governments</span>${msg}
    <p class="help" style="margin:0 0 4px">From the most to the least plausible: coalitions whose parties are ideologically adjacent come first (none skips a seated party lying between its most distant members), whether or not they are minimum winning, then those with fewer parties; a majority with a party it does not need ranks below the smaller majority inside it. ${probabilistic ? `The chance is the share of simulations with ${R.seatsMajority}+ seats; options under 5% are left out.` : "Single allocation at the shares on screen."} Click one to show it on the hemicycle.</p>
    <div style="max-height:340px;overflow-y:auto">${rows || `<p class="help" style="margin:0">No combination reaches a majority in at least 5% of simulations.</p>`}</div>
    <details open style="margin-top:8px"><summary class="muted" style="cursor:pointer;font-size:12.5px;font-weight:500">Coalition configurations (${nExcl} pair${nExcl === 1 ? "" : "s"} excluded)</summary>
      <div style="margin-top:4px">${rules}</div>
      <p class="help" style="margin:2px 0 0">${joint ? "With joint lists on, a pair excludes two lists from governing together when one party is on each; pairs inside a list are not checked. " : ""}${canRelax ? "Switching a pair recalculates the options (and the government options in the panel) from the simulations." : "Including an excluded pair needs the simulation, which loads with the scenario data."}</p></details>`;
}
function renderHemiBlocks() {
  $("hemi2023btn").setAttribute("aria-pressed", String(S.hemi2023)); $("hemicoalbtn").setAttribute("aria-pressed", String(S.hemiCoal));
  render2023(); renderHemiCoal();
  const first = S.coalMsg && S.coalMsg.keys.length ? document.querySelector("#hemicoal .grow-row.newopt") : null;
  if (first) { const sc = first.parentElement; sc.scrollTop = Math.max(0, first.offsetTop - sc.offsetTop - 24); }
}

// ---- over time ------------------------------------------------------------------------------
const TIMECH = {};
function lineChart(id, series, opts) {
  // series: [{name, colour, pts:[[d, v, lo, hi]...]}]; opts: {ymax, fmt, ref:[v], refLab, pctAxis}
  const box = $(id + "box"), W = Math.max(300, box.clientWidth - 12), H = Math.round(clamp(W * 0.62, 240, 420));
  // room on the right for the end labels
  const longest = Math.max(...series.map(s => (s.label || s.name).length)) + 5;
  const m = { l: 36, r: clamp(longest * 6.4 + 14, 70, W * 0.4), t: 14, b: 34 };
  const all = series.flatMap(s => s.pts.map(p => p[0]));
  if (!all.length) { $(id).innerHTML = ""; return; }
  const a = Math.min(...all), b = Math.max(...all);
  const x = d => m.l + (d - a) / (b - a || 1) * (W - m.l - m.r);
  const ymax = opts.ymax;
  const y = v => m.t + (1 - v / ymax) * (H - m.t - m.b);
  const o = [];
  opts.yticks.forEach(v => o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${y(v)}" y2="${y(v)}" stroke="#EEE"/><text x="${m.l - 6}" y="${y(v) + 4}" text-anchor="end" font-size="11" fill="#767676">${opts.fmt(v)}</text>`));
  (opts.ref || []).forEach(v => o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${y(v)}" y2="${y(v)}" stroke="#999" stroke-dasharray="3 3"/>`));
  let prevY = null;
  monthTicks(a, b, W - m.l - m.r).forEach(d => { const [lab, yr] = tickLabel(d, prevY); prevY = dateOf(d).getUTCFullYear(); o.push(`<text x="${x(d)}" y="${H - m.b + 14}" text-anchor="middle" font-size="11" fill="#767676">${lab}</text>${yr ? `<text x="${x(d)}" y="${H - m.b + 27}" text-anchor="middle" font-size="11" fill="#767676">${yr}</text>` : ""}`); });
  series.forEach(s => {
    if (s.pts.length && s.pts[0][2] != null) {
      const top = s.pts.map(p => `${x(p[0]).toFixed(1)},${y(p[3]).toFixed(1)}`), bot = s.pts.map(p => `${x(p[0]).toFixed(1)},${y(p[2]).toFixed(1)}`).reverse();
      o.push(`<path d="M${top.join("L")}L${bot.join("L")}Z" fill="${s.colour}" fill-opacity="${S.hoverParty && S.hoverParty !== s.name ? 0.03 : 0.14}"/>`);
    }
  });
  series.forEach(s => o.push(`<path d="M${s.pts.map(p => `${x(p[0]).toFixed(1)},${y(p[1]).toFixed(1)}`).join("L")}" fill="none" stroke="${s.colour}" stroke-width="${S.hoverParty === s.name ? 2.6 : 1.8}" stroke-opacity="${S.hoverParty && S.hoverParty !== s.name ? 0.2 : 1}"/>`));
  const sw = WEEKS[S.w].date;
  if (S.dateLine) o.push(`<line x1="${x(sw)}" x2="${x(sw)}" y1="${m.t}" y2="${H - m.b}" stroke="#333"/>`);
  const labs = series.map(s => { const lp = s.pts[s.pts.length - 1]; return { s, v: lp[1], y: y(lp[1]), d: lp[0] }; });
  repel(labs, 13, m.t, H - m.b);
  labs.forEach(l => o.push(`<line x1="${x(b) + 3}" x2="${x(b) + 9}" y1="${y(l.v)}" y2="${l.y}" stroke="#CCC"/><text x="${x(b) + 12}" y="${l.y + 4}" font-size="11.5" font-weight="500" fill="${l.s.colour}">${esc(l.s.label || l.s.name)} ${opts.fmt(l.v)}</text>`));
  o.push(`<g class="hov"></g><rect x="${m.l}" y="${m.t}" width="${W - m.l - m.r}" height="${H - m.t - m.b}" fill="transparent" class="hit" style="cursor:crosshair"/>`);
  const svg = $(id);
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.setAttribute("width", W); svg.setAttribute("height", H);
  svg.innerHTML = o.join("");
  TIMECH[id] = { W, H, m, a, b, x, y, series, opts };
}
function timeHover(id, ev) {
  const g = TIMECH[id]; if (!g) return;
  const svg = $(id), r = svg.getBoundingClientRect(), k = g.W / r.width, mx = (ev.clientX - r.left) * k;
  const d = g.a + (mx - g.m.l) / (g.W - g.m.l - g.m.r) * (g.b - g.a), wi = nearestWeek(d), wd = WEEKS[wi].date;
  svg.querySelector(".hov").innerHTML = `<line x1="${g.x(wd)}" x2="${g.x(wd)}" y1="${g.m.t}" y2="${g.H - g.m.b}" stroke="#141414" stroke-opacity=".35"/>`;
  const vals = g.series.map(s => ({ s, p: s.pts.find(p => p[0] === wd) })).filter(o => o.p).sort((p, q) => q.p[1] - p.p[1]);
  placeTip($(id + "tip"), $(id + "box"), ev, `<b>${dstr(wd)}</b>` + vals.map(o => `<div class="r"><span><span class="dot" style="background:${o.s.colour}"></span>${esc(o.s.label || o.s.name)}</span><span>${g.opts.fmt(o.p[1])}${o.p[2] != null ? ` <span class="muted">${g.opts.fmt(o.p[2])}–${g.opts.fmt(o.p[3])}</span>` : ""}</span></div>`).join(""));
}
let COALTREND = null;
function coalitionTrend() { // >=10% in some week, minimal, top six by latest
  if (COALTREND) return COALTREND;
  const byKey = new Map();
  WEEKS.forEach((w, wi) => w.coalitions.forEach(c => {
    const key = [...c.m].sort().join("|");
    if (!byKey.has(key)) byKey.set(key, { key, members: sortParties(c.m), pts: [], max: 0, latest: 0 });
    const o = byKey.get(key); o.pts.push([w.date, c.p * 100]); o.max = Math.max(o.max, c.p);
    if (wi === WEEKS.length - 1) o.latest = c.p;
  }));
  let cand = [...byKey.values()].filter(o => o.max >= 0.1);
  cand = cand.filter(o => !cand.some(b => b !== o && b.members.length < o.members.length && b.members.every(x => o.members.includes(x))));
  cand.sort((p, q) => q.latest - p.latest || q.max - p.max);
  COALTREND = cand.slice(0, OKABE.length).map((o, i) => ({ name: o.key, label: optLabel(o.members), colour: OKABE[i], pts: o.pts }));
  return COALTREND;
}
function renderTime() {
  const seatSeries = SP.map((p, i) => ({ name: p, colour: col(p), pts: WEEKS.filter(w => exists(p, w.date) && w.seats[i] != null).map(w => [w.date, w.seats[i], w.seatsLo[i], w.seatsHi[i]]) }))
    .filter(s => s.pts.some(pt => pt[3] > 0));
  const smax = Math.ceil((Math.max(...seatSeries.flatMap(s => s.pts.map(p => p[3]))) + 10) / 50) * 50;
  lineChart("seattime", seatSeries, { ymax: smax, yticks: Array.from({ length: smax / 50 + 1 }, (_, i) => i * 50), fmt: v => String(Math.round(v)) });
  lineChart("cointime", coalitionTrend(), { ymax: 100, yticks: [0, 25, 50, 75, 100], ref: [50], fmt: v => Math.round(v) + "%" });
  const thrSeries = P.map((p, i) => ({ name: p, colour: col(p), pts: WEEKS.filter(w => exists(p, w.date) && w.pThr[i] != null).map(w => [w.date, w.pThr[i] * 100]) }))
    .filter(s => s.pts.some(pt => pt[1] > 2 && pt[1] < 98));
  lineChart("thrtime", thrSeries, { ymax: 100, yticks: [0, 25, 50, 75, 100], ref: [50], fmt: v => Math.round(v) + "%" });
  $("timenote").innerHTML = `<span class="label">About these charts</span>
    <p class="help" style="margin:0">Seats: the model's median projection each week, with 80% ranges. Majorities: the share of the model's simulated parliaments in which each governing option holds at least 231 seats, for options that reached 10% in some week, leaving out any that only add parties to a smaller one. Thresholds: the chance that each party clears 5%, for parties whose chances have been in doubt. These follow the model's own estimates, not your scenario.</p>`;
}

// ---- pollsters ------------------------------------------------------------------------------
let HOUSE = null;
function renderHouse() {
  const V = VD(), H0 = V.house;
  $("househelp").textContent = V.past
    ? "How far each polling house's readings sat from the trend anchored on the two election results, in percentage points, averaged over its own polls, with 80% intervals. A positive figure means the house showed the party higher than the anchored trend. Click a house to highlight its polls on the trend chart."
    : "How far each polling house leans against the average house, in percentage points, with 80% intervals. A positive lean means the house shows the party higher than the others do. Click a house to highlight its polls on the trend chart.";
  if (!H0 || !H0.length) { $("house").innerHTML = ""; return; }
  const blocs = V.blocs, box = $("housebox"), W = Math.max(560, box.clientWidth - 12);
  const lw = 190, rowH = 30, cw = (W - lw - 10) / blocs.length;
  // party names wrap at spaces to the column width; the header grows with the longest
  const mc = document.createElement("canvas").getContext("2d"); mc.font = "500 12px Jost, sans-serif";
  const wrapName = b => { const out = []; let c = ""; b.split(" ").forEach(wd => { const t = c ? c + " " + wd : wd; if (mc.measureText(t).width > cw - 8 && c) { out.push(c); c = wd; } else c = t; }); out.push(c); return out; };
  const names = blocs.map(wrapName), nl = Math.max(...names.map(n => n.length)), top = 44 + (nl - 1) * 14, H = top + H0.length * rowH + 10;
  let M = 0; H0.forEach(h => blocs.forEach(b => { const e = h.effects[b]; if (e) M = Math.max(M, Math.abs(e[1]), Math.abs(e[2])); }));
  M = Math.ceil(M);
  const o = [];
  blocs.forEach((b, j) => {
    const cx = lw + j * cw, x = v => cx + 8 + (v + M) / (2 * M) * (cw - 16);
    o.push(`<text text-anchor="middle" font-size="12" font-weight="500" fill="${col(b)}">${names[j].map((l, k) => `<tspan x="${cx + cw / 2}" y="${16 + k * 14}">${esc(l)}</tspan>`).join("")}</text><text x="${x(-M)}" y="${top - 10}" font-size="10" fill="#767676" text-anchor="start">−${M}</text><text x="${x(M)}" y="${top - 10}" font-size="10" fill="#767676" text-anchor="end">+${M}</text><line x1="${x(0)}" x2="${x(0)}" y1="${top - 6}" y2="${H - 6}" stroke="#CCC"/>`);
    H0.forEach((h, i) => {
      const e = h.effects[b], y = top + i * rowH + rowH / 2; if (!e) return;
      o.push(`<line x1="${x(e[1])}" x2="${x(e[2])}" y1="${y}" y2="${y}" stroke="${col(b)}" stroke-width="2" stroke-opacity=".55"/><circle cx="${x(e[0])}" cy="${y}" r="4" fill="${col(b)}"><title>${esc(h.org)}, ${esc(b)}: ${signed(e[0], 1)} pp (80% ${e[1].toFixed(1)} to ${e[2].toFixed(1)})</title></circle>`);
    });
  });
  H0.forEach((h, i) => {
    const y = top + i * rowH, on = S.houses.has(h.org);
    o.unshift(`<rect x="0" y="${y}" width="${W}" height="${rowH}" fill="${on ? "#F6EBEB" : i % 2 ? "#FAFAFA" : "#fff"}" data-org="${esc(h.org)}" style="cursor:pointer"/>`);
    o.push(`<text x="8" y="${y + rowH / 2 + 4}" font-size="12.5" fill="${on ? "#8B0000" : "#141414"}" font-weight="${on ? 600 : 400}" data-org="${esc(h.org)}" style="cursor:pointer">${esc(h.org)}<tspan fill="#767676" font-weight="400"> · ${h.n} poll${h.n === 1 ? "" : "s"}</tspan></text>`);
  });
  const svg = $("house");
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.setAttribute("width", W); svg.setAttribute("height", H);
  svg.innerHTML = o.join("");
}

// ---- static controls ------------------------------------------------------------------------
function renderChips() {
  $("pchips").innerHTML = VD().pollsters.map(o => `<button type="button" class="chip" data-org="${esc(o)}" aria-pressed="${S.houses.has(o)}">${esc(o)}</button>`).join("") +
    (S.houses.size ? `<button type="button" class="linkbtn" id="houseclear">Show all houses</button>` : "");
  $("periods").innerHTML = PERIODS.map(([k, l]) => `<button type="button" data-k="${k}" aria-pressed="${S.period === k}">${l}</button>`).join("");
  [...$("views").children].forEach(b => {
    b.setAttribute("aria-pressed", S.view === b.dataset.v); b.setAttribute("aria-selected", S.view === b.dataset.v);
    b.hidden = !!TERM && (b.dataset.v === "seats" || b.dataset.v === "time");
  });
  ["trends", "seats", "time", "pollsters"].forEach(v => { $("v-" + v).hidden = S.view !== v; });
  $("pts").checked = S.points;
  $("ptswrap").hidden = S.view !== "trends";
  $("evwrap").hidden = S.view !== "trends";
  $("ev").checked = S.events;
  $("dlwrap").hidden = !((S.view === "trends" && !TERM) || S.view === "time");
  $("dl").checked = S.dateLine;
  $("trendhelp").textContent = TERM ? "Hover for every party's estimate. Diamonds are the election results the trend is anchored on. Click a poll figure to see that poll alone and how far each of its figures sits from the trend. Click parties below the chart to show them alone."
    : "Hover for every party's estimate. Click a poll figure to see that poll alone and how far each of its figures sits from the trend (click again or press Escape to let go), or click elsewhere to load that week into the scenario. Click parties below the chart to show them alone.";
}

// ---- rendering ------------------------------------------------------------------------------
function renderView() {
  if (TERM && (S.view === "seats" || S.view === "time")) { S.view = "trends"; renderChips(); }
  if (S.view === "trends") renderTrends();
  else if (S.view === "seats") { renderHemi(); renderHemiBlocks(); renderMap(); renderConstCard(); }
  else if (S.view === "time") renderTime();
  else renderHouse();
}
const sharesValid = () => 100 - sum(SP.map(p => S.shares[p] || 0)) >= -0.05;
// While the named shares exceed 100%, the last valid numbers stay on screen, faded.
function update() {
  if (TERM) { renderTermPanel(); renderView(); saveHash(); return; }
  if (sharesValid() || !CUR) compute();
  updatePanel(); renderView(); saveHash();
}
const updateSoon = debounce(update, 60);

// ---- share links ----------------------------------------------------------------------------
function hashTarget() { try { if (window.top !== window && window.top.location.host === location.host) return window.top; } catch (e) { /* cross-origin */ } return window; }
function stateString() {
  const q = new URLSearchParams();
  if (TERM) {
    q.set("t", TERM.term.id);
    if (S.houses.size) q.set("h", [...S.houses].join(";"));
    if (S.parties.size) q.set("pp", [...S.parties].join(";"));
    if (S.view !== "trends") q.set("view", S.view);
    if (S.period !== "all") q.set("p", S.period);
    if (!S.points) q.set("dp", "0");
    return q.toString();
  }
  if (S.w !== WEEKS.length - 1) q.set("w", iso(WEEKS[S.w].date));
  const d = defaults(S.w), ed = SP.filter(p => Math.abs(S.shares[p] - d[p]) > 0.051);
  if (ed.length) q.set("v", ed.map(p => p + ":" + S.shares[p]).join(","));
  const dk = defaultExcl(), xi = [...dk].filter(k => !S.excl.has(k)), xx = [...S.excl].filter(k => !dk.has(k));
  if (xi.length) q.set("xi", xi.join(";"));
  if (xx.length) q.set("xx", xx.join(";"));
  if (S.joint.length) q.set("j", S.joint.map(m => m.join(",")).join(";"));
  if (S.joint.length && S.dyn) q.set("s", S.dyn);
  if (S.houses.size) q.set("h", [...S.houses].join(";"));
  if (S.parties.size) q.set("pp", [...S.parties].join(";"));
  if (S.view !== "trends") q.set("view", S.view);
  if (S.period !== "all") q.set("p", S.period);
  if (!S.points) q.set("dp", "0");
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
  S.w = WEEKS.length - 1;
  if (q.get("w")) { const d = fromIso(q.get("w")), i = WEEKS.findIndex(w => w.date === d); if (i >= 0) S.w = i; }
  S.shares = defaults(S.w);
  if (q.get("v")) q.get("v").split(",").forEach(kv => { const i = kv.lastIndexOf(":"), p = kv.slice(0, i), v = parseFloat(kv.slice(i + 1)); if (p in PIDX && isFinite(v)) S.shares[p] = clamp(v, 0, 100); });
  S.shares.Other = 100 - sum(SP.map(p => S.shares[p]));
  if (q.get("j")) q.get("j").split(";").forEach(g => { const m = g.split(",").filter(p => P.includes(p)); if (m.length >= 2 && !m.some(p => S.joint.flat().includes(p)) && S.joint.length < 4) S.joint.push(m); });
  const okPair = k => { const p = pairOf(k); return p.length === 2 && p[0] !== p[1] && p.every(x => P.includes(x)); };
  if (q.get("xi")) q.get("xi").split(";").filter(okPair).forEach(k => S.excl.delete(pairKey(...pairOf(k))));
  if (q.get("xx")) q.get("xx").split(";").filter(okPair).forEach(k => S.excl.add(pairKey(...pairOf(k))));
  if (q.get("s")) S.dyn = clamp(parseInt(q.get("s"), 10) || 0, 0, 100);
  const list = k => q.get(k) ? q.get(k).split(";") : [];
  if (q.get("t")) PENDING = { term: q.get("t"), houses: list("h"), parties: list("pp") };
  else {
    list("h").forEach(o => { if (D.pollsters.includes(o)) S.houses.add(o); });
    list("pp").forEach(p => { if (P.includes(p)) S.parties.add(p); });
  }
  if (["trends", "seats", "time", "pollsters"].includes(q.get("view"))) S.view = q.get("view");
  if (PERIODS.some(p => p[0] === q.get("p"))) S.period = q.get("p");
  S.points = q.get("dp") !== "0";
}
function shareUrl() {
  const t = hashTarget(), s = stateString();
  return t.location.origin + t.location.pathname + (s ? "#" + s : "");
}

// ---- downloads ------------------------------------------------------------------------------
function download(name, blob) {
  const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = name;
  document.body.appendChild(a); a.click(); setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 500);
}
function csv(which) {
  if (which === "trend") return trendCsv();
  const cur = CUR, wk = cur.wk, inJ = new Set(S.joint.flat());
  const head = ["date", "list", "model_vote_pct", "model_vote_lower80", "model_vote_upper80", "model_p_clear_5pct", "model_seats", "model_seats_lower80", "model_seats_upper80",
    "scenario_vote_pct", "scenario_effective_vote_pct", "scenario_seats", "scenario_seats_lower80", "scenario_seats_upper80", "scenario_p_clear_threshold"];
  const q = v => v == null || (typeof v === "number" && isNaN(v)) ? "" : typeof v === "string" && /[",]/.test(v) ? `"${v.replace(/"/g, '""')}"` : v;
  const rows = [head];
  SP.forEach(p => {
    const i = PIDX[p], k = cur.ents.findIndex(e => e.name === p), r = k >= 0 ? cur.rows[k] : null;
    rows.push([iso(wk.date), p, wk.vote[i], wk.voteLo[i], wk.voteHi[i], wk.pThr[i], wk.seats[i], wk.seatsLo[i], wk.seatsHi[i],
      S.shares[p], +cur.eff[p].toFixed(2), inJ.has(p) ? "" : r.seats, inJ.has(p) || !r || r.lo == null ? "" : r.lo, inJ.has(p) || !r || r.hi == null ? "" : r.hi, inJ.has(p) || !r || r.pThr == null ? "" : +r.pThr.toFixed(3)]);
  });
  cur.ents.forEach((e, k) => { if (!e.joint) return; const r = cur.rows[k]; rows.push([iso(wk.date), e.name, "", "", "", "", "", "", "", +e.rawVote.toFixed(2), +e.vote.toFixed(2), r.seats, r.lo == null ? "" : r.lo, r.hi == null ? "" : r.hi, r.pThr == null ? "" : +r.pThr.toFixed(3)]); });
  download(`polling-tracker-${iso(wk.date)}.csv`, new Blob([ChartExport.csvCite(PAGE) + rows.map(r => r.map(q).join(",")).join("\n") + "\n"], { type: "text/csv" }));
}
function exportExtras(which, clone) {
  if (which === "map") {
    const [bw, bh] = D.cartogramBox, o = [];
    o.push(Cartogram.badges(C, C.map(c => c.okreg), cartOutline().pts));
    o.push(Cartogram.key(C.map(c => [c.okreg, esc(c.name)]), bw + 40, 8, bh));
    clone.insertAdjacentHTML("beforeend", o.join(""));
    return [-30, -30, bw + 60 + 290, bh + 60];
  }
  if (which !== "hemi") return null;
  const o = [];
  // the count sits a fixed gap after the name: both are placed from the measured width, not with dx, which PDF drawing does not keep
  const mc = document.createElement("canvas").getContext("2d"), FS = 0.11, GAP = 0.05, M = 1000;
  const tw = (t, wt) => { mc.font = `${wt} ${FS * M}px Jost, sans-serif`; return mc.measureText(t).width / M; };
  HEMI.labels.forEach(l => {
    const cs = Math.cos(l.a), top = Math.abs(cs) < 0.12, r = HEMI.outer + 0.14 + (top ? 0.24 : 0);
    const x = r * cs, y = -r * Math.sin(l.a) + 0.035, pw = tw(l.p, 500), nw = tw(String(l.n), 400), tot = pw + GAP + nw;
    const x0 = top ? x - tot / 2 : cs > 0 ? x : x - tot;
    o.push(`<text x="${x0.toFixed(3)}" y="${y.toFixed(3)}" font-size="${FS}" font-weight="500" fill="${col(l.p)}">${esc(l.p)}</text>`);
    o.push(`<text x="${(x0 + pw + GAP).toFixed(3)}" y="${y.toFixed(3)}" font-size="${FS}" font-weight="400" fill="${col(l.p)}" fill-opacity="0.7">${l.n}</text>`);
  });
  clone.insertAdjacentHTML("beforeend", o.join(""));
  return [-3.1, -2.75, 6.2, 2.85];
}
// The export format: every PNG button becomes a PDF button, and the combined image's PDF is one A4 page
function setFmt(f) {
  S.fmt = f; try { localStorage.setItem("pt-fmt", f); } catch (e) { /* private window */ }
  document.querySelectorAll("#fmt button").forEach(b => b.setAttribute("aria-pressed", String(b.dataset.f === f)));
  document.querySelectorAll("[data-png]").forEach(b => { b.textContent = b.textContent.replace(/^(PNG|PDF)/, f.toUpperCase()).replace(/ \(A4\)$/, "") + (f === "pdf" && b.id === "combobtn" ? " (A4)" : ""); });
}
async function png(only) {
  const combo = only === "combo", ids = combo ? ["trend", "hemi"] : [only];
  // PDF is drawn as vectors by ChartPdf; the combined image on A4 is laid out at a fixed width so its type stays readable
  const pdf = S.fmt === "pdf", a4 = pdf && combo, tbox = $("trendbox");
  if (a4) { tbox.style.width = "864px"; renderTrends(); }
  // the combined image takes the seat projection from the Seats view, which may not be on screen
  const seatsView = $("v-seats"), seatsWasHidden = seatsView.hidden;
  if (combo) { seatsView.hidden = false; renderHemi(); }
  const past = TERM ? `Sejm term ${TERM.term.label}` : null;
  const titles = { trend: "Voting intention", hemi: "Seat projection", map: "Seats by constituency", seattime: "Seats over time", cointime: "Chance of a majority over time", thrtime: "Chance of clearing the threshold over time", house: "House effects", combo: "Polish party polling: trends and projections" };
  const fname = { trend: "trends", hemi: "hemicycle", map: "constituencies", seattime: "seats-over-time", cointime: "majority-over-time", thrtime: "threshold-over-time", house: "pollsters", combo: "trends-and-projections" }[only];
  // ChartExport (../chart-export.js) keeps the labels in Jost in the image.
  await ChartExport.fonts();
  // in the combined image the hemicycle is drawn small and the trends fill the width
  const HEMI_K = 0.7, hsvg = $("hemi"), hemiW = a4 ? 600 : Math.max(hsvg.clientWidth, 640) * 6.2 / hsvg.viewBox.baseVal.width;
  const imgs = await Promise.all(ids.map(async id => {
    const svg = $(id), vb = svg.viewBox.baseVal;
    const clone = await ChartExport.clone(svg);
    // the single-graphic exports get labels the page doesn't show, and a wider canvas to hold them
    const vb2 = exportExtras(id, clone), vw = vb2 ? vb2[2] : vb.width, vh = vb2 ? vb2[3] : vb.height;
    let w = a4 && vb2 ? 600 : vb2 ? Math.max(svg.clientWidth, 640) * vw / vb.width : svg.clientWidth || vb.width;
    if (combo && id === "trend") w = Math.max(w, hemiW * 1.3);
    const h = w * vh / vw;
    if (vb2) clone.setAttribute("viewBox", vb2.join(" "));
    clone.setAttribute("xmlns", "http://www.w3.org/2000/svg"); ChartExport.setSize(clone, w, h);
    clone.querySelectorAll(".hit,#trendhit,#trendhover,.hov").forEach(n => n.remove());
    if (pdf) return { img: { _svg: clone }, w, h };
    return new Promise(res => {
      const img = new Image(); img.onload = () => res({ img, w, h });
      img.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(clone));
    });
  }));
  if (combo) seatsView.hidden = seatsWasHidden;
  if (a4) { tbox.style.width = ""; renderTrends(); }
  await ChartExport.fonts();
  const evl = (only === "trend" || combo) && S.events ? trendEvents() : [];
  const side = false, pad = 24, evRows = Math.min(4, evl.length), evH = evl.length ? evRows * 17 + 24 : 0, foot = 34 + evH;
  const W = side ? sum(imgs.map(i => i.w)) + pad * (imgs.length + 1) : Math.max(...imgs.map(i => i.w)) + 2 * pad;
  // the subtitle names the pollsters whose polls the estimates rest on, whether or not the polls are drawn; two lines at most
  const base = past ? `Shares of the parties named, anchored on the elections of ${dlong(TERM.term.start)} and ${dlong(TERM.term.end)}`
    : combo ? `Estimates to ${dlong(WEEKS[WEEKS.length - 1].date)}; seat projection for ${dlong(CUR.wk.date)}`
    : S.view === "seats" ? `${dlong(CUR.wk.date)} · ${CUR.mode === "model" ? "model estimates" : "scenario"}` : `Estimates to ${dlong(WEEKS[WEEKS.length - 1].date)}`;
  // a hypothetical scenario is not the model's estimate: no pollsters are named, and the image says it is hypothetical
  const hypo = !past && CUR.mode !== "model" && (only === "hemi" || only === "map" || only === "trend" || combo);
  const sub = hypo && only === "trend" ? `Estimates to ${dlong(WEEKS[WEEKS.length - 1].date)}. The lines are the model's estimates from published polls. This image was made with a hypothetical scenario set in the tracker, whose vote shares are not poll estimates.` : hypo ? `Hypothetical scenario for ${dlong(CUR.wk.date)}: vote shares set by the user, not poll estimates. Seats are projected from those shares.${combo ? " The trend lines are the model's estimates from published polls." : ""}` : `${base}. Based on polls by ${VD().pollsters.join(", ")}.`, mc = document.createElement("canvas").getContext("2d");
  const wrap = size => { mc.font = `${size}px Jost, sans-serif`; const out = []; let cur = ""; sub.split(" ").forEach(wd => { const t = cur ? cur + " " + wd : wd; if (mc.measureText(t).width > W - 2 * pad && cur) { out.push(cur); cur = wd; } else cur = t; }); out.push(cur); return out; };
  let sfs = 13, lines = wrap(sfs);
  while (lines.length > 2 && sfs > 10) { sfs -= 0.5; lines = wrap(sfs); }
  const slh = Math.round(sfs * 1.35), head = 62 + (lines.length - 1) * slh;
  // Under the hemicycle the image can carry what the page shows on request: the certainty key and counts, the
  // change since 2023, and the possible governments with the exclusion rules. Each is a section with a height
  // and a drawing function, stacked in that order.
  const secs = [], F = "Jost, 'Avenir Next', sans-serif";
  const wrapTo = (txt, font, maxW) => { mc.font = font; const out = []; let c = ""; txt.split(" ").forEach(wd => { const t = c ? c + " " + wd : wd; if (mc.measureText(t).width > maxW && c) { out.push(c); c = wd; } else c = t; }); out.push(c); return out; };
  const ell = (t, font, maxW) => { mc.font = font; let u = t; if (mc.measureText(u).width <= maxW) return u; while (u.length > 1 && mc.measureText(u + "…").width > maxW) u = u.slice(0, -1); return u + "…"; };
  // in the combined image the two tables sit side by side under the hemicycle (stacked if the image is too narrow), each wrapping to its column
  const two = combo && (W - 2 * pad - 40) / 2 >= 600, colW = combo ? (two ? 600 : Math.min(600, W - 2 * pad)) : 0;
  const SW = combo ? colW + 2 * pad : W;
  const POS = "#2E7D32", NEG = "#A31515";
  if (only === "hemi" && HEMI && HEMI.cert) {
    const cert = HEMI.cert;
    const certNote = `Each dot is one seat in one constituency: a list's most probable seats, as many as its projected total. A seat's chance is the share of the simulated parliaments in which the list wins at least that many seats in the constituency, for example its 3rd seat in a district with a magnitude of 8 (eight seats). Seats of one list in one constituency are interchangeable: the model has no candidates. Within each list the most certain seats are at the outer end of its arc; the toss-ups sit where the lists meet. ${CUR.mode === "model" ? "Model estimates" : "Scenario"} for ${dlong(CUR.wk.date)}, from ${HEMI.sim ? HEMI.sim.n : 250} simulations.`;
    const certLines = wrapTo(certNote, "10.5px " + F, SW - 2 * pad);
    secs.push({ h: 26 + (cert.rows.length + 1) * 17 + 10 + certLines.length * 14 + 18, draw: y0 => {
      const key = [["certain", `Certain (${(SEAT_CERTAIN * 100).toFixed(1)}% or more of simulations)`], ["probable", `Probable (${Math.round(SEAT_PROBABLE * 100)}–${(SEAT_CERTAIN * 100).toFixed(1)}%)`], ["toss-up", `Toss-up (under ${Math.round(SEAT_PROBABLE * 100)}%)`]];
      g.font = "11px " + F; g.textAlign = "left";
      let lx = pad;
      key.forEach(([c, t]) => {
        g.beginPath(); g.arc(lx + 6, y0 + 6, 5, 0, 2 * Math.PI);
        if (c === "toss-up") { g.fillStyle = "#fff"; g.fill(); g.strokeStyle = "#555"; g.lineWidth = 1.2; g.stroke(); }
        else { g.globalAlpha = c === "probable" ? 0.5 : 1; g.fillStyle = "#555"; g.fill(); g.globalAlpha = 1; }
        g.fillStyle = "#4B4B4B"; g.fillText(t, lx + 16, y0 + 10); lx += 16 + g.measureText(t).width + 20;
      });
      y0 += 26;
      const nx = [pad + 200, pad + 262, pad + 332, pad + 402];
      g.fillStyle = "#767676"; g.font = "500 11px " + F; g.fillText("List", pad + 16, y0 + 10);
      g.textAlign = "right"; ["Seats", "Certain", "Probable", "Toss-up"].forEach((t, j) => g.fillText(t, nx[j], y0 + 10));
      g.strokeStyle = "#DDD"; g.lineWidth = 1; g.beginPath(); g.moveTo(pad, y0 + 15); g.lineTo(pad + 410, y0 + 15); g.stroke();
      cert.rows.forEach((r, ri) => {
        const ry = y0 + 17 + (ri + 1) * 17 - 5;
        g.textAlign = "left"; g.fillStyle = r.colour; g.beginPath(); g.arc(pad + 5, ry - 4, 4, 0, 2 * Math.PI); g.fill();
        g.fillStyle = "#141414"; g.font = "11px " + F; g.fillText(r.name, pad + 16, ry);
        g.textAlign = "right"; [r.seats, r.certain, r.probable, r.toss].forEach((v, j) => g.fillText(String(v), nx[j], ry));
      });
      g.textAlign = "left";
      const ty = y0 + 17 + (cert.rows.length + 1) * 17 + 10;
      g.fillStyle = "#767676"; g.font = "10.5px " + F;
      certLines.forEach((l, li) => g.fillText(l, pad, ty + li * 14));
    } });
  }
  if ((combo || only === "hemi" && S.hemi2023) && !TERM) {
    const rows23 = since2023(), note23 = `Today's parties summed into the lists they stood on in October 2023 (PKW results). "Now" is the ${CUR.mode === "model" ? "model's estimate" : "scenario"} for ${dlong(CUR.wk.date)}.`;
    const lines23 = wrapTo(note23, "10.5px " + F, SW - 2 * pad);
    secs.push({ h: 28 + (rows23.length + 1) * 17 + 10 + lines23.length * 14 + 14, draw: y0 => {
      g.textAlign = "left"; g.fillStyle = "#141414"; g.font = "500 12px " + F; g.fillText("Change since the 2023 election", pad, y0 + 12);
      y0 += 22;
      const nx = [pad + 270, pad + 335, pad + 395, pad + 475, pad + 535, pad + 595];
      g.fillStyle = "#767676"; g.font = "500 11px " + F; g.fillText("2023 list", pad, y0 + 10);
      g.textAlign = "right"; ["Vote 2023", "Now", "", "Seats 2023", "Now", ""].forEach((t, j) => g.fillText(t, nx[j], y0 + 10));
      g.strokeStyle = "#DDD"; g.lineWidth = 1; g.beginPath(); g.moveTo(pad, y0 + 15); g.lineTo(pad + 600, y0 + 15); g.stroke();
      rows23.forEach(({ l, now, seats }, ri) => {
        const ry = y0 + 17 + (ri + 1) * 17 - 5;
        g.textAlign = "left"; g.fillStyle = "#141414"; g.font = "11px " + F; g.fillText(l.list, pad, ry);
        if (l.parties.length > 1) { const w0 = g.measureText(l.list + " ").width; g.fillStyle = "#767676"; g.font = "10px " + F; g.fillText("now " + l.parties.map(short).join(" + "), pad + w0, ry); }
        g.font = "11px " + F; g.textAlign = "right";
        const dv = now - l.vote, ds = seats - l.seats;
        [[l.vote.toFixed(1) + "%", "#141414"], [now.toFixed(1) + "%", "#141414"], [signed(dv, 1), dv > 0 ? POS : dv < 0 ? NEG : "#141414"], [String(l.seats), "#141414"], [String(seats), "#141414"], [signed(ds), ds > 0 ? POS : ds < 0 ? NEG : "#141414"]].forEach(([t, c], j) => { g.fillStyle = c; g.fillText(t, nx[j], ry); });
      });
      g.textAlign = "left";
      const ty = y0 + 17 + (rows23.length + 1) * 17 + 10;
      g.fillStyle = "#767676"; g.font = "10.5px " + F;
      lines23.forEach((l, li) => g.fillText(l, pad, ty + li * 14));
    } });
  }
  if ((combo || only === "hemi" && S.hemiCoal) && !TERM) {
    const items = coalOptions(), shownC = items.slice(0, a4 ? 8 : 12), more = items.length - shownC.length, joint = S.joint.length > 0, probabilistic = CUR.mode !== "single";
    const label = o => optLabel(o.members);
    const applied = [...S.excl].map(pairOf);
    const ruleSentence = applied.length ? `Excluded coalition possibilities: ${applied.map(pairText).join("; ")}.` : "No coalition possibilities are excluded.";
    const ruleLines = [{ lines: wrapTo(ruleSentence, "10.5px " + F, SW - 2 * pad) }];
    const note = `${probabilistic ? `Chance: the share of simulations in which the coalition has ${R.seatsMajority} or more seats; options under 5% are left out.` : "Single allocation at the shares on screen."} Ordered from the most to the least plausible: coalitions of ideologically adjacent parties (none skips a seated party between its most distant members) first, then those with fewer parties, then a narrower span; a majority with a party it does not need ranks below the smaller majority inside it; options without a majority in most simulations come last. ${CUR.mode === "model" ? "Model estimates" : "Scenario"} for ${dlong(CUR.wk.date)}.`;
    const noteLines = wrapTo(note, "10.5px " + F, SW - 2 * pad);
    const rulesH = sum(ruleLines.map(r => r.lines.length * 13 + 3));
    secs.push({ col: "R", h: 24 + noteLines.length * 14 + 6 + 20 + shownC.length * 31 + (more > 0 ? 16 : 0) + 22 + rulesH + 10, draw: y0 => {
      g.textAlign = "left"; g.fillStyle = "#141414"; g.font = "500 12px " + F; g.fillText("Possible governments", pad, y0 + 12);
      g.fillStyle = "#767676"; g.font = "10.5px " + F;
      noteLines.forEach((l, li) => g.fillText(l, pad, y0 + 28 + li * 14));
      let y = y0 + 24 + noteLines.length * 14 + 6;
      const nx = [pad + 330, pad + 410, pad + 480];
      g.fillStyle = "#767676"; g.font = "500 11px " + F; g.fillText("Coalition", pad, y + 10);
      g.textAlign = "right"; ["Seats", "80% range", "Chance"].forEach((t, j) => g.fillText(t, nx[j], y + 10));
      g.strokeStyle = "#DDD"; g.lineWidth = 1; g.beginPath(); g.moveTo(pad, y + 15); g.lineTo(pad + 485, y + 15); g.stroke();
      y += 20;
      shownC.forEach(({ o, kind, br }) => {
        g.textAlign = "left"; g.fillStyle = "#141414"; g.font = (kind === 0 ? "500 " : "") + "11px " + F; g.fillText(ell(label(o), (kind === 0 ? "500 " : "") + "11px " + F, 300), pad, y + 11);
        g.textAlign = "right"; g.font = "11px " + F; g.fillText(String(o.s), nx[0], y + 11); g.fillText(o.lo != null ? `${o.lo}–${o.hi}` : "–", nx[1], y + 11);
        g.font = "500 11px " + F; g.fillText(probabilistic ? prob(o.p) : "✓", nx[2], y + 11);
        g.textAlign = "left"; g.font = "10px " + F;
        g.fillStyle = "#767676"; g.fillText(COAL_KIND[kind], pad, y + 24);
        y += 31;
      });
      if (more > 0) { g.fillStyle = "#767676"; g.font = "10.5px " + F; g.textAlign = "left"; g.fillText(`${more} further option${more > 1 ? "s" : ""} not shown`, pad, y + 11); y += 16; }
      y += 6;
      g.fillStyle = "#141414"; g.font = "500 12px " + F; g.textAlign = "left"; g.fillText("Exclusions", pad, y + 12); y += 22;
      ruleLines.forEach(r => {
        g.fillStyle = "#4B4B4B"; g.font = "10.5px " + F;
        r.lines.forEach((l, li) => g.fillText(l, pad, y + 9 + li * 13));
        y += r.lines.length * 13 + 3;
      });
    } });
  }
  const colL = secs.filter(x => x.col !== "R"), colR = secs.filter(x => x.col === "R");
  const hk = combo ? Math.min(HEMI_K, (W - 2 * pad) / imgs[1].w) : 1, hw = combo ? imgs[1].w * hk : 0, hh = combo ? imgs[1].h * hk : 0;
  const hL = sum(colL.map(x => x.h)), hR = sum(colR.map(x => x.h));
  const H = combo ? head + foot + imgs[0].h + pad + hh + 10 + (two ? Math.max(hL, hR) : hL + hR) : head + foot + sum(secs.map(x => x.h)) + (side ? Math.max(...imgs.map(i => i.h)) : sum(imgs.map(i => i.h)) + pad * (imgs.length - 1));
  const cv = document.createElement("canvas"), sc = ChartExport.scale(W, H); cv.width = W * sc; cv.height = H * sc;
  const docTitle = combo ? titles.combo : `Polling tracker · ${titles[only]}${past ? " · " + past : ""}`;
  const g = pdf ? await ChartPdf.context(W, H, { a4, title: docTitle + (hypo ? " (hypothetical scenario)" : ""), subject: sub }) : cv.getContext("2d");
  if (!pdf) { g.scale(sc, sc); g.fillStyle = "#fff"; g.fillRect(0, 0, W, H); }
  g.fillStyle = "#8B0000"; g.font = ChartExport.TITLE; g.fillText(combo ? titles.combo : `Polling tracker · ${titles[only]}${past ? " · " + past : ""}${hypo ? " · Hypothetical scenario" : ""}`, pad, 32);
  g.fillStyle = "#4B4B4B"; g.font = `${sfs}px Jost, sans-serif`; lines.forEach((l, i) => g.fillText(l, pad, 52 + i * slh));
  const drawEvents = y0 => {
    if (evl.length) {
    // at most four to a column; the type shrinks if the columns would run past the image
    const txt = l => `${l.n}. ${evWhen(l.date)} · ${l.event}`, cols = Array.from({ length: Math.ceil(evl.length / 4) }, (_, c) => evl.slice(c * 4, c * 4 + 4));
    let fs = 10.5, widths;
    do { g.font = `${fs}px Jost, 'Avenir Next', sans-serif`; widths = cols.map(c => Math.max(...c.map(l => g.measureText(txt(l)).width))); fs -= 0.5; }
    while (sum(widths) + 28 * (cols.length - 1) > W - 2 * pad && fs > 8);
    g.fillStyle = "#A0A0A0";
    let cx = pad;
    cols.forEach((c, k) => { c.forEach((l, i) => g.fillText(txt(l), cx, y0 + 8 + i * 17)); cx += widths[k] + 28; });
  }
  };
  let xx = pad, yy = head;
  if (combo) {
    // trend across the top, the hemicycle centred under it, then the change table and the possible governments side by side, centred
    const [ti, hi] = imgs;
    g.drawImage(ti.img, pad + (W - 2 * pad - ti.w) / 2, yy, ti.w, ti.h);
    yy += ti.h + pad; drawEvents(yy); if (evl.length) yy += evH;
    g.drawImage(hi.img, (W - hw) / 2, yy, hw, hh);
    const x0 = two ? (W - 2 * colW - 40) / 2 : (W - colW) / 2;
    const rowY = yy + hh + 10;
    let ly = rowY; g.save(); g.translate(x0 - pad, 0); colL.forEach(sec => { sec.draw(ly); ly += sec.h; }); g.restore();
    let ry = two ? rowY : rowY + hL; g.save(); g.translate((two ? x0 + colW + 40 : x0) - pad, 0); colR.forEach(sec => { sec.draw(ry); ry += sec.h; }); g.restore();
  } else {
    imgs.forEach(i => {
      g.drawImage(i.img, xx + (side ? 0 : (W - 2 * pad - i.w) / 2), yy, i.w, i.h);
      if (side) xx += i.w + pad; else yy += i.h + pad;
    });
    { let sy = yy - pad + 10; secs.forEach(sec => { sec.draw(sy); sy += sec.h; }); }
    drawEvents(yy);
  }
  g.fillStyle = "#767676"; ChartExport.drawCite(g, PAGE, pad, H - 12, W - 2 * pad);
  const fileBase = `polling-tracker-${fname}${hypo ? "-hypothetical-scenario" : ""}-${past ? TERM.term.id : iso(CUR.wk.date)}`;
  if (pdf) download(fileBase + ".pdf", await g.finish());
  else cv.toBlob(b => download(fileBase + ".png", b), "image/png");
}

// ---- events ---------------------------------------------------------------------------------
function setWeek(i) {
  S.w = clamp(i, 0, WEEKS.length - 1); S.shares = defaults(S.w); S.focusGov = null; S.pinGov = null;
  S.picker.forEach(p => { if (!exists(p, WEEKS[S.w].date)) S.picker.delete(p); });
  update();
}
function setShare(p, v, from) {
  if (!isFinite(v)) return;
  S.shares[p] = clamp(v, 0, 100);
  S.shares.Other = 100 - sum(SP.map(q => S.shares[q]));
  const row = $("row-" + slug(p));
  if (row) {
    if (from !== "range") row.querySelector("input[type=range]").value = S.shares[p];
    if (from !== "number") row.querySelector("input[type=number]").value = S.shares[p].toFixed(p === "MN" ? 2 : 1);
  }
  updateSoon();
}
function bind() {
  $("term").addEventListener("change", e => setTerm(e.target.value));
  $("views").addEventListener("click", e => { const b = e.target.closest("button"); if (!b) return; S.view = b.dataset.v; renderChips(); renderView(); saveHash(); });
  $("periods").addEventListener("click", e => { const b = e.target.closest("button"); if (!b) return; S.period = b.dataset.k; renderChips(); renderTrends(); saveHash(); });
  $("pchips").addEventListener("click", e => {
    if (e.target.id === "houseclear") { S.houses.clear(); }
    else { const b = e.target.closest("button[data-org]"); if (!b) return; const o = b.dataset.org; S.houses.has(o) ? S.houses.delete(o) : S.houses.add(o); }
    renderChips(); renderTrends(); saveHash();
  });
  document.querySelectorAll("[data-png]").forEach(b => b.addEventListener("click", () => png(b.dataset.png).catch(e => toast("Could not make the " + S.fmt.toUpperCase() + ": " + e.message))));
  $("fmt").addEventListener("click", e => { const b = e.target.closest("button"); if (b) setFmt(b.dataset.f); });
  try { setFmt(localStorage.getItem("pt-fmt") === "pdf" ? "pdf" : "png"); } catch (e) { setFmt("png"); }
  document.querySelectorAll("[data-csv]").forEach(b => b.addEventListener("click", () => csv(b.dataset.csv)));
  $("ev").addEventListener("change", e => { S.events = e.target.checked; renderTrends(); });
  $("dl").addEventListener("change", e => { S.dateLine = e.target.checked; if (S.view === "trends") renderTrends(); else renderView(); });
  $("pts").addEventListener("change", e => { S.points = e.target.checked; renderTrends(); saveHash(); });
  // trend chart
  document.addEventListener("keydown", e => { if (e.key === "Escape" && S.pollSel) { S.pollSel = null; renderTrends(); } });
  $("trend").addEventListener("mousemove", trendHover);
  $("trend").addEventListener("mouseleave", () => { $("trendtip").hidden = true; const g = $("trendhover"); if (g) g.innerHTML = ""; });
  $("trend").addEventListener("click", ev => {
    if (!TR) return;
    const r = $("trend").getBoundingClientRect(), k = TR.W / r.width, mx = (ev.clientX - r.left) * k, my = (ev.clientY - r.top) * k;
    // A click on a poll figure (within 8px, as in the hover) picks that poll; a second click on it, a click
    // anywhere else on the chart, or Escape lets go
    const V = VD();
    let hit = null, hd = 64;
    (TR.allPolls || []).forEach(p => {
      if (TR.hl && !S.houses.has(V.pollsters[p[1]])) return;
      const dx = TR.x(p[0]) - mx, dy = TR.y(p[3]) - my, dd = dx * dx + dy * dy;
      if (dd < hd) { hd = dd; hit = p; }
    });
    if (hit) { const key = pollKey(hit); S.pollSel = S.pollSel === key ? null : key; renderTrends(); trendHover(ev); return; }
    if (S.pollSel) { S.pollSel = null; renderTrends(); trendHover(ev); return; }
    if (TERM) return;
    if (mx < TR.m.l || mx > TR.W - TR.m.r) return;
    setWeek(nearestWeek(TR.a + (mx - TR.m.l) / (TR.W - TR.m.l - TR.m.r) * (TR.b - TR.a)));
    toast("Loaded the week of " + dstr(WEEKS[S.w].date));
  });
  $("trendlegend").addEventListener("mouseover", e => { const s = e.target.closest("[data-p]"); const p = s ? s.dataset.p : null; if (p !== S.hoverParty) { S.hoverParty = p; renderTrends(); } });
  $("trendlegend").addEventListener("mouseleave", () => { S.hoverParty = null; renderTrends(); });
  $("trendlegend").addEventListener("click", e => {
    if (e.target.closest("#pollclear")) S.pollSel = null;
    else if (e.target.closest("#partyclear")) S.parties.clear();
    else { const b = e.target.closest("button[data-p]"); if (!b) return; const p = b.dataset.p; S.parties.has(p) ? S.parties.delete(p) : S.parties.add(p); }
    renderTrends(); saveHash();
  });
  // over-time charts
  ["seattime", "cointime", "thrtime"].forEach(id => {
    $(id).addEventListener("mousemove", ev => timeHover(id, ev));
    $(id).addEventListener("mouseleave", () => { $(id + "tip").hidden = true; const h = $(id).querySelector(".hov"); if (h) h.innerHTML = ""; });
    $(id).addEventListener("click", ev => {
      const g = TIMECH[id]; if (!g) return;
      const r = $(id).getBoundingClientRect(), mx = (ev.clientX - r.left) * g.W / r.width;
      setWeek(nearestWeek(g.a + (mx - g.m.l) / (g.W - g.m.l - g.m.r) * (g.b - g.a)));
      toast("Loaded the week of " + dstr(WEEKS[S.w].date));
    });
  });
  // seats
  $("hemi").addEventListener("mousemove", hemiHover);
  $("hemicertbtn").addEventListener("click", () => { S.hemiCert = !S.hemiCert; renderHemi(); });
  $("hemicert").addEventListener("click", e => { const tr = e.target.closest("tr[data-k]"); if (tr) openSeatModal(+tr.dataset.k); });
  $("hemicert").addEventListener("keydown", e => { const tr = e.target.closest("tr[data-k]"); if (tr && (e.key === "Enter" || e.key === " ")) { e.preventDefault(); openSeatModal(+tr.dataset.k); } });
  $("seatmodalclose").addEventListener("click", closeSeatModal);
  $("seatmodal").addEventListener("click", e => { if (e.target.id === "seatmodal") closeSeatModal(); });
  document.addEventListener("keydown", e => { if (e.key === "Escape" && !$("seatmodal").hidden) closeSeatModal(); });
  $("hemi2023btn").addEventListener("click", () => { S.hemi2023 = !S.hemi2023; renderHemiBlocks(); });
  $("hemicoalbtn").addEventListener("click", () => { S.hemiCoal = !S.hemiCoal; renderHemiBlocks(); });
  const hc = $("hemicoal"), markOn = () => [...document.querySelectorAll(".grow-row")].forEach(g => g.classList.toggle("on", g.dataset.key === (S.pinGov || S.focusGov)));
  const toggled = (before, text) => {
    S.pinGov = null; S.focusGov = null; S.coalMsg = null; update();
    const after = coalOptions(), added = after.filter(i => !before.has(i.o.key)), gone = [...before.keys()].filter(key => !after.some(i => i.o.key === key));
    const desc = i => `${esc(optLabel(i.o.members))} (${CUR.mode === "single" ? i.o.s + " seats" : prob(i.o.p) + (i.kind === 2 ? ", short of a majority in most simulations" : "")})`;
    const nopt = (n, w) => `${n} option${n > 1 ? "s" : ""} ${w}`;
    S.coalMsg = { keys: added.map(i => i.o.key), text: `<b>${text}</b>: ${added.length ? `adds ${added.length} option${added.length > 1 ? "s" : ""}, highlighted below: ${added.slice(0, 4).map(desc).join("; ")}${added.length > 4 ? "…" : ""}` : "adds no option with at least a 5% chance of a majority"}${gone.length ? `; ${nopt(gone.length, "no longer possible")}` : ""}.` };
    renderHemiBlocks();
  };
  const setPairs = (keys, ex, text) => {
    const before = new Map(coalOptions().map(i => [i.o.key, i]));
    keys.forEach(k => { ex ? S.excl.add(k) : S.excl.delete(k); });
    toggled(before, text);
  };
  hc.addEventListener("click", e => {
    const b = e.target.closest("button[data-pair],button[data-party],#xreset"); if (!b || b.disabled) return;
    if (b.id === "xreset") { const dk = defaultExcl(), before = new Map(coalOptions().map(i => [i.o.key, i])); S.excl = dk; toggled(before, "Exclusions reset to the defaults"); return; }
    if (b.dataset.pair) { const k = b.dataset.pair, fp = pairOf(k), ex = !S.excl.has(k); setPairs([k], ex, `${ex ? "Excluding" : "Including"} ${pairText(fp)}`); return; }
    const p = b.dataset.party, keys = P.filter(x => x !== p && x !== "MN").map(x => pairKey(p, x)), ex = !keys.every(k => S.excl.has(k));
    setPairs(keys, ex, `${ex ? "Excluding" : "Including"} ${p} with every other party`);
  });
  hc.addEventListener("click", e => { const gr = e.target.closest(".grow-row"); if (!gr) return; S.pinGov = S.pinGov === gr.dataset.key ? null : gr.dataset.key; renderHemi(); renderMap(); renderGov(); markOn(); });
  hc.addEventListener("keydown", e => { const gr = e.target.closest(".grow-row"); if (gr && (e.key === "Enter" || e.key === " ")) { e.preventDefault(); gr.click(); } });
  hc.addEventListener("mouseover", e => { const gr = e.target.closest(".grow-row"), key = gr ? gr.dataset.key : null; if (key !== S.focusGov) { S.focusGov = key; renderHemi(); renderMap(); markOn(); } });
  hc.addEventListener("mouseleave", () => { if (S.focusGov) { S.focusGov = null; renderHemi(); renderMap(); markOn(); } });
  $("hemi").addEventListener("mouseleave", () => { $("hemitip").hidden = true; });
  $("map").addEventListener("mousemove", ev => {
    const g = ev.target.closest("g[data-c]"), tip = $("maptip");
    if (!g) { tip.hidden = true; return; }
    const ci = +g.dataset.c, c = C[ci], E = CUR.ents.length;
    const rows = CUR.ents.map((e, k) => ({ e, s: CUR.single.cons[ci * E + k] })).filter(r => r.s > 0).sort((a, b) => b.s - a.s);
    placeTip(tip, $("mapbox"), ev, `<b>${esc(c.name)}</b> <span class="muted">${c.magnitude} seats</span>` + rows.map(r => `<div class="r"><span><span class="dot" style="background:${r.e.colour}"></span>${esc(r.e.name)}</span><span>${r.s}</span></div>`).join(""));
  });
  $("map").addEventListener("mouseleave", () => { $("maptip").hidden = true; });
  $("map").addEventListener("click", ev => { const g = ev.target.closest("g[data-c]"); if (!g) return; const ok = C[+g.dataset.c].okreg; S.constSel = S.constSel === ok ? null : ok; renderMap(); renderConstCard(); });
  $("constcard").addEventListener("click", e => { if (e.target.id === "constclose") { S.constSel = null; renderMap(); renderConstCard(); } });
  // pollsters
  $("house").addEventListener("click", e => { const t = e.target.closest("[data-org]"); if (!t) return; const o = t.dataset.org; S.houses.has(o) ? S.houses.delete(o) : S.houses.add(o); renderChips(); renderHouse(); saveHash(); toast(S.houses.has(o) ? `${o} highlighted on the trend chart` : `${o} no longer highlighted`); });
  // panel (delegated: its contents are rebuilt)
  const panel = $("panel");
  panel.addEventListener("input", e => {
    const t = e.target;
    if (t.id === "dyn") { S.dyn = +t.value; $("dynval").textContent = S.dyn + "%"; updateSoon(); return; }
    if (t.dataset.p && (t.type === "range" || t.type === "number")) setShare(t.dataset.p, parseFloat(t.value), t.type);
  });
  panel.addEventListener("click", e => {
    const t = e.target;
    if (t.id === "wprev") return setWeek(S.w - 1);
    if (t.id === "wnext") return setWeek(S.w + 1);
    if (t.id === "reset") { S.shares = defaults(S.w); S.joint = []; S.dyn = 0; S.pinGov = null; S.focusGov = null; return update(); }
    if (t.id === "jadd") { if (S.picker.size >= 2) { S.joint.push(sortParties([...S.picker])); S.picker.clear(); S.pinGov = null; update(); } return; }
    if (t.dataset.rm != null) { S.joint.splice(+t.dataset.rm, 1); S.pinGov = null; return update(); }
    const pick = t.closest("#jpick button[data-p]");
    if (pick) { const p = pick.dataset.p; S.picker.has(p) ? S.picker.delete(p) : S.picker.add(p); pick.setAttribute("aria-pressed", S.picker.has(p)); updatePanel(); return; }
    if (t.id === "govmore") { S.showAllGov = !S.showAllGov; return renderGov(); }
    const gr = t.closest(".grow-row");
    if (gr) { S.pinGov = S.pinGov === gr.dataset.key ? null : gr.dataset.key; renderGov(); if (S.view !== "seats") { S.view = "seats"; renderChips(); } renderView(); saveHash(); return; }
    if (t.id === "copylink") { const u = shareUrl(); (navigator.clipboard ? navigator.clipboard.writeText(u) : Promise.reject()).then(() => toast("Link copied"), () => window.prompt("Copy this link:", u)); return; }
  });
  panel.addEventListener("keydown", e => { const gr = e.target.closest(".grow-row"); if (gr && (e.key === "Enter" || e.key === " ")) { e.preventDefault(); gr.click(); } });
  panel.addEventListener("mouseover", e => {
    const gr = e.target.closest(".grow-row"), row = e.target.closest(".prow");
    const key = gr ? gr.dataset.key : null, p = row ? row.dataset.p : null;
    let dirty = false;
    if (key !== S.focusGov) { S.focusGov = key; dirty = true; }
    if (p !== S.hoverParty) { S.hoverParty = p; dirty = true; }
    if (dirty && S.view === "seats") { renderHemi(); renderMap(); }
    if (dirty && S.view === "trends" && !gr) renderTrends();
    if (dirty) { [...document.querySelectorAll(".grow-row")].forEach(g => g.classList.toggle("on", g.dataset.key === (S.pinGov || S.focusGov))); }
  });
  panel.addEventListener("mouseleave", () => { S.focusGov = null; S.hoverParty = null; if (S.view === "seats") { renderHemi(); renderMap(); } else if (S.view === "trends") renderTrends(); renderGov(); });
  window.addEventListener("resize", debounce(renderView, 150));
}

// ---- past terms -----------------------------------------------------------------------------
// The selector lists the current term and every past one in terms.json; a past
// term's file is fetched the first time it is chosen.
function loadTerms() {
  fetch("terms.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).then(d => {
    TERMS = d;
    if (!d.terms || !d.terms.length) return;
    const cur = d.current ? d.current.label : "current";
    $("term").innerHTML = `<option value="">${esc(cur)}</option>` + d.terms.map(t => `<option value="${esc(t.id)}">${esc(t.label)}</option>`).join("");
    $("termwrap").hidden = false;
    if (PENDING) setTerm(PENDING.term);
  }).catch(() => { /* no past terms: the selector stays hidden */ });
}
function setTerm(id) {
  const t = TERMS && TERMS.terms.find(x => x.id === id);
  if (!t) {
    TERM = null; PENDING = null; $("term").value = ""; S.houses.clear(); S.parties.clear(); S.hoverParty = null; S.pollSel = null;
    header(); renderChips(); update(); return;
  }
  const show = d => {
    TERM = d; S.houses.clear(); S.parties.clear(); S.pollSel = null;
    if (PENDING && PENDING.term === id) {
      PENDING.houses.forEach(o => { if (d.pollsters.includes(o)) S.houses.add(o); });
      PENDING.parties.forEach(p => { if (d.parties.includes(p)) S.parties.add(p); });
    }
    PENDING = null; S.hoverParty = null; $("term").value = id;
    if (S.view === "seats" || S.view === "time") S.view = "trends";
    header(); renderChips(); update();
  };
  if (TCACHE.has(id)) return show(TCACHE.get(id));
  fetch(t.file).then(r => { if (!r.ok) throw new Error(r.status); return r.json(); })
    .then(d => { TCACHE.set(id, d); show(d); })
    .catch(() => { PENDING = null; $("term").value = TERM ? TERM.term.id : ""; toast("Could not load that term"); });
}
function renderTermPanel() {
  const t = TERM.term, m = TERM.meta;
  const rows = TERM.final.filter(f => f.result != null).map(f => {
    const miss = f.polls - f.result;
    return `<tr><td><span class="dot" style="background:${col(f.party)};margin-right:6px"></span>${esc(f.party)}</td><td>${f.result.toFixed(1)}%</td>
      <td>${f.polls.toFixed(1)}% <span class="muted" style="font-size:11.5px">${f.lo.toFixed(1)}–${f.hi.toFixed(1)}</span></td><td class="${Math.abs(miss) >= 1 ? cls(miss) : ""}">${signed(miss, 1)}</td></tr>`;
  }).join("");
  $("panel").innerHTML = `
  <div class="card">
    <span class="label">Sejm term ${esc(t.label)}</span>
    <h2 style="margin:0;font-size:20px;font-weight:400">${dlong(t.start)} to ${dlong(t.end)}</h2>
    <p class="help">Each line is a party's share of the vote for the parties the polls named, so the figures add up to 100 without the others and the undecided, whom Wikipedia's tables lump together. The trend starts and ends at the election results (the diamonds) and runs between them through the polls, each house's lean estimated over the whole term. A party joins the chart when the polls first named it and leaves when they stopped; a joint list replaces its members from when the polls began reporting it.</p>
  </div>
  <div class="card">
    <span class="label">The polls against the result</span>
    <table class="t"><tr><th>Party</th><th>Result</th><th>Polls</th><th>Miss</th></tr>${rows}</table>
    <p class="help" style="margin:0">"Polls" is what the houses that polled in the final month (${m.finalHouses.map(esc).join(", ")}) would have shown on election day, each with its own lean, averaged, with an 80% range. "Miss" is polls minus result, in points. Both are shares of the parties named, which is why they differ from the official percentages.</p>
  </div>
  <div class="card soft">
    <span class="label">Share</span>
    <div class="actions">
      <button type="button" class="btn2" id="copylink">Copy link to this view</button>
    </div>
  </div>`;
  panelKey = "";
}
function trendCsv() {
  const V = VD(), T = V.trend, rows = [["date", "party", "median_pct", "lower80", "upper80"]];
  T.dates.forEach((d, i) => V.parties.forEach(p => {
    const s = T.series[p]; if (s.med[i] != null) rows.push([iso(d), p, s.med[i], s.lo[i], s.hi[i]]);
  }));
  const q = v => typeof v === "string" && /[",]/.test(v) ? `"${v.replace(/"/g, '""')}"` : v;
  download(`polling-tracker-${TERM ? TERM.term.id : "trend"}.csv`, new Blob([ChartExport.csvCite(PAGE) + rows.map(r => r.map(q).join(",")).join("\n") + "\n"], { type: "text/csv" }));
}

// ---- load -----------------------------------------------------------------------------------
function loadDraws() {
  if (drawsState !== "idle") return;
  drawsState = "loading";
  fetch("draws.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).then(d => {
    DRAWS = d; drawsState = "ready";
    if (!modelView()) update();
  }).catch(() => { drawsState = "missing"; if (!modelView()) update(); else updatePanel(); });
}
function header() {
  if (TERM) {
    const m = TERM.meta, t = TERM.term;
    $("asof").innerHTML = `Sejm term ${esc(t.label)}: ${m.nPolls} polls by ${m.pollsterCount} pollsters between the elections of ${dlong(t.start)} and ${dlong(t.end)}, as listed on <a href="${esc(m.source)}" target="_blank" rel="noopener">Wikipedia</a>. Shares of the parties the tables name, which lump the rest with the undecided; the trend is anchored on both election results.`;
    return;
  }
  const m = D.meta, last = WEEKS[WEEKS.length - 1].date;
  $("asof").innerHTML = `Estimates as of ${dlong(last)}${m.nPolls ? `, from ${m.nPolls} polls by ${m.pollsterCount} pollsters (fieldwork ${dlong(m.firstFieldwork)} to ${dlong(m.lastFieldwork)})` : ""}, as listed on Wikipedia.`;
}
fetch("polls.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).then(d => {
  D = d; prep(); restoreHash(); header(); renderChips(); bind();
  loadTerms();
  // set on the SVGs themselves so PNG exports keep the typeface
  ["trend", "hemi", "map", "seattime", "cointime", "thrtime", "house"].forEach(id => $(id).setAttribute("font-family", "Jost, 'Avenir Next', -apple-system, sans-serif"));
  if (!modelView()) loadDraws();
  update();
  setTimeout(loadDraws, 1500);
}).catch(err => { $("asof").textContent = "Could not load the polling data."; console.error(err); });
