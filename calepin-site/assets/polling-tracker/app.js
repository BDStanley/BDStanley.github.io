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
// The Forecast page (Polls/current term/docs/forecast_plan.md, section 2.1): election-day draws from forecast.json, the
// latest week's poll draws widened by how far past polls were from the result at the same distance. S.fh is "election"
// (those draws) or "now" (the week's own figures, as on Seats).
let FC = null, fcState = "idle", FCW = null, FCSWING = null, FCW0 = null, FCW_KEY = null, FC_SIM0 = null;
// The sliders' changes by group (section 2.3), as a key for the caches
const flKey = () => JSON.stringify(S.fl.filter(x => x.c));
const FORECAST = (() => { try { return new URLSearchParams(location.search).get("s") === "forecast"; } catch (e) { return false; } })();
const fcOn = () => FORECAST && S.fh === "election";
let JP = null, JPAIR = {};
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
// Political events of the current term, shown with the party launches and splits from the model data. They are kept, each with
// its source, in Polls/current term/data-raw/events.csv (Polish political analyses repo) and come here as events.json
// ({date, event, url}), which scripts/sync-analyses.py copies in; without it the chart shows the launches and splits only.
let POLITICAL_EVENTS = [];
fetch("events.json").then(r => (r.ok ? r.json() : [])).then(d => {
  POLITICAL_EVENTS = (Array.isArray(d) ? d : d.events || []).filter(e => e.date && e.event).map(e => [e.date, e.event, e.url || ""]);
  if (typeof S !== "undefined" && S.events && S.view === "trends" && D) renderTrends();
}).catch(() => {});
// Events of the view on show, in date order, numbered through the whole list so a number never changes with the period
function trendEvents() {
  const V = VD(), all = V.events.map(e => ({ date: e.date, event: e.event }));
  if (!V.past) POLITICAL_EVENTS.forEach(([d, t, u]) => all.push({ date: Date.parse(d) / 864e5, event: t, url: u }));
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

// ---- sections -------------------------------------------------------------------------------
// The tracker is one app at four addresses (decision 18 of Polls/current term/docs/election_2027_plan.md): each page of the site embeds this file
// with ?s=<section>, which fixes the views it offers. Links between the pages carry the week and the scenario.
const SECTION = (() => { try { const s = new URLSearchParams(location.search).get("s"); return ["polls", "seats", "geography", "scenarios", "forecast"].includes(s) ? s : "polls"; } catch (e) { return "polls"; } })();
const SECTIONS = {
  polls: [["trends", "Trends"], ["groups", "By group"], ["pollsters", "Pollsters"]],
  seats: [["seats", "Sejm"], ["everyseat", "Every seat"]],
  geography: [["map", "Map"], ["worth", "What a vote is worth"], ["population", "If seats followed population"]],
  scenarios: [["lists", "Parties and lists"], ["levers", "Groups"], ["whovotes", "Who votes"]],
  // Polls/current term/docs/forecast_plan.md: kept apart from the polling pages (decisions 1 and 5)
  forecast: [["seats", "Sejm"], ["senate", "Senate"], ["sliders", "Sliders"]]
};
// Views reached from inside another view, without a tab of their own: on the Forecast, Every seat is part of Sejm
// (decision 14 of Polls/current term/docs/forecast_plan.md)
const SUBVIEWS = { forecast: { everyseat: "seats" } };
const VIEWS = SECTIONS[SECTION].map(v => v[0]).concat(Object.keys(SUBVIEWS[SECTION] || {}));
const VIEW_EL = { trends: "v-trends", groups: "v-groups", pollsters: "v-pollsters", seats: "v-seats", lists: "v-seats", levers: "v-seats",
  everyseat: "v-everyseat", senate: "v-senate", sliders: "v-sliders", whovotes: "v-whovotes", map: "v-geomap", worth: "v-worth", population: "v-population" };
const HEMI_VIEWS = ["seats", "lists", "levers"];
// The site's page for each section, from this file (assets/polling-tracker/index.html)
const SECTION_PAGE = { polls: "polling-tracker.html", seats: "polling-tracker/seats.html", geography: "polling-tracker/geography.html", scenarios: "polling-tracker/scenarios.html", forecast: "polling-tracker/forecast.html" };
const sectionUrl = s => new URL("../../pages/polish-political-analyses/" + SECTION_PAGE[s], location.href).href;
// The keys a link to another section carries: the week and the scenario
const SCEN_KEYS = ["w", "v", "j", "r", "ra", "xi", "xx", "g", "gb", "gh"];

// ---- state ----------------------------------------------------------------------------------
// levers: changes by group on the Scenarios page's Groups view ({group, measure, change, from}); base: their starting
// point ("groups" or "polls"); hand: parties whose share was set by hand, which the levers leave where they were set
const S = { fh: "election", fl: [], sp: {}, sinc: true, scoal: new Set(), snSort: "okreg", w: 0, shares: {}, joint: [], resp: "medium", adv: null, advOpen: false, view: VIEWS[0], period: "all", houses: new Set(), parties: new Set(), points: true, dateLine: false, events: false, fmt: "png",
  hoverParty: null, pollSel: null, hemiCert: false, hemi2023: false, hemiCoal: true, excl: new Set(), coalMsg: null, focusGov: null, pinGov: null, constSel: 19, picker: new Set(), showAllGov: false,
  gDim: "age", gList: "KO", gPts: false,
  levers: [], base: "groups", hold: false, hand: new Set(), wiMode: "all", wiMetric: "vote", wiGroup: {},
  esC: "", esL: "", esCat: "all", esSort: "const" };

function defaults(w) {
  const wk = WEEKS[w], s = {};
  P.forEach((p, i) => { const v = wk.vote[i]; s[p] = v == null || isNaN(v) ? 0 : v; });
  s.MN = R.mnNational2023;
  s.Other = Math.round((100 - sum(SP.map(p => s[p]))) * 100) / 100;
  return s;
}
function edited() { const d = defaults(S.w); return SP.some(p => Math.abs(S.shares[p] - d[p]) > 0.051); }
// The shares a scenario starts from before any hand edit: the week's estimates, or what the group levers make of them
function baseShares() { return S.levers.length && LEVRES ? leverShares(LEVRES) : defaults(S.w); }
const respOn = () => !!JP && S.joint.length > 0 && S.resp !== "none";
function modelView() { return !edited() && S.joint.length === 0; }
const weekParties = w => P.filter(p => exists(p, WEEKS[w].date));

// ---- seat machinery -------------------------------------------------------------------------
function makeEnts(joint) {
  const ents = joint.map(m => { const mem = sortParties(m); return { name: mem.map(short).join("-"), members: mem, joint: true }; });
  SP.forEach(p => { if (!joint.some(m => m.includes(p))) ents.push({ name: p, members: [p], joint: false }); });
  ents.forEach(e => { e.idx = e.members.map(p => PIDX[p]); e.mnOnly = !e.joint && e.members[0] === "MN"; });
  return ents;
}
// ---- voters' response to joint lists (joint_penalty.json, written by Voting behaviour/analysis/79b_joint_penalty_export.R) ----
// For each party on a joint list: the voters who are reluctant about any partner on it (pair reluctance, drawn on the logit scale in the
// simulation) and who defect (rp.conv of them) leave. Each defector's alternative is weighed over every other party by how likely their
// electorate is ever to vote for it (weight: like squared); an alternative on the list being left means abstaining, and a share rp.abst of
// the rest abstains anyway (since 17 November 2026: before, every defector who voted went to a party off the list, which in a contest of two
// blocs sent centre-left defectors to the far right). Pairs that split recently carry at least rp.split reluctance when ticked.
// Families (JP.families): parties sharing one set of ratings (R+ takes PiS's). Voters of a third party on a list holding two or more members of
// a family are reluctant about the family once: their reluctance about its first member on the list, times the family's union ratio for them.
const lgt = p => Math.log(p / (1 - p)), expt = x => 1 / (1 + Math.exp(-x)), clampP = x => Math.min(0.995, Math.max(0.005, x));
const RESP_NAMES = { none: "None", low: "Low", medium: "Medium", high: "High" };
function prepJP(d) {
  JP = d; JPAIR = {};
  d.pairs.forEach(q => { (JPAIR[q.a] = JPAIR[q.a] || {})[q.b] = q; });
}
function respParams() {
  if (S.adv) return S.adv;
  const b = JP.levels[S.resp === "none" ? "medium" : S.resp];
  return { conv: b.conv, shift: b.shift, split: b.split, abst: JP.abstain, splitsOn: JP.splitsOn.slice() };
}
function pairMu(a, b, rp) {
  let mu = JPAIR[a][b].mu;
  JP.splits.forEach((sp, i) => { if (rp.splitsOn[i] && ((sp[0] === a && sp[1] === b) || (sp[0] === b && sp[1] === a))) mu = Math.max(mu, lgt(clampP(rp.split))); });
  return mu;
}
// Partner b's weight in a's reluctance on list m: 1 normally; the union ratio for a family's first member on the list and 0 for its other members
function familyWeights(a, m) {
  const out = new Map();
  (JP.families || []).forEach(f => {
    if (f.members.includes(a)) return;
    const on = f.members.filter(x => m.includes(x) && JPAIR[a][x]);
    if (on.length < 2) return;
    on.forEach((x, i) => out.set(x, i === 0 ? (f.union[a] != null ? f.union[a] : f.unionDefault) : 0));
  });
  return out;
}
const newFlow = () => P.map(() => ({ ab: 0, to: new Float64Array(SP.length) }));
function stdNormal(seed) {
  const r = mulberry32(seed); let spare = null;
  return () => { if (spare != null) { const t = spare; spare = null; return t; }
    let u = 0, v = 0; while (u === 0) u = r(); v = r();
    const m = Math.sqrt(-2 * Math.log(u)); spare = m * Math.sin(2 * Math.PI * v); return m * Math.cos(2 * Math.PI * v); };
}
// sh: national shares (fractions, SP order), changed in place. rnd: a standard normal generator for a simulation draw, or null for the point estimate.
// flow: optional newFlow() to add the votes moved to. w: the week, for the parties that exist.
function applyResponse(sh, joint, rp, rnd, flow, w) {
  const loss = new Float64Array(SP.length), gain = new Float64Array(SP.length), av = new Set(weekParties(w));
  for (const m of joint) for (const a of m) {
    const ia = PIDX[a]; if (!JPAIR[a] || !(sh[ia] > 0)) continue;
    let keep = 1;
    const fw = familyWeights(a, m);
    for (const b of m) if (b !== a && JPAIR[a][b]) {
      const f = fw.has(b) ? fw.get(b) : 1; if (!f) continue;
      keep *= 1 - Math.min(0.995, f * expt(pairMu(a, b, rp) + JPAIR[a][b].sd * (rp.shift + (rnd ? rnd() : 0))));
    }
    const lost = sh[ia] * (1 - keep) * rp.conv; if (!(lost > 0)) continue;
    loss[ia] += lost;
    // Alternatives are weighed over every other party; the weight of the partners on the list being left goes to abstention
    const wAll = P.map(b => (b === a || !av.has(b) || !JPAIR[a][b]) ? 0 : JPAIR[a][b].like ** 2), totAll = sum(wAll);
    const wt = P.map((b, j) => m.includes(b) ? 0 : wAll[j]), tot = sum(wt);
    let ab0 = tot ? rp.abst : 1;
    if (rnd && ab0 > 0 && ab0 < 1) ab0 = expt(lgt(Math.min(0.98, Math.max(0.02, ab0))) + 0.4 * rnd());
    const ab = tot ? ab0 + (1 - ab0) * (1 - tot / totAll) : 1;
    if (flow) flow[ia].ab += lost * ab;
    P.forEach((b, j) => { if (!wt[j]) return; const g = lost * (1 - ab0) * wt[j] / totAll; gain[PIDX[b]] += g; if (flow) flow[ia].to[PIDX[b]] += g; });
  }
  for (let i = 0; i < SP.length; i++) sh[i] += gain[i] - loss[i];
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
function fcRaw() {
  if (FCW0) return FCW0;
  const K = FC.categories.length, n = FC.nDraws, arr = new Float32Array(FC.draws.length);
  for (let i = 0; i < arr.length; i++) arr[i] = FC.draws[i] / 10000;
  return (FCW0 = { n, K, arr, map: FC.categories.map(c => c in PIDX ? PIDX[c] : -1) });
}
// The election-day draws, moved by the sliders: each draw's shares times its factors (one of the model's coefficient
// draws in turn; a party takes its 2023 list's factor), renormalised. Without sliders, the draws as exported.
function fcDraws() {
  const key = flKey();
  if (FCW && FCW_KEY === key) return FCW;
  const raw = fcRaw(), { n, K, map } = raw;
  let arr = raw.arr;
  const fl = S.fl.filter(x => x.c);
  if (fl.length && CELLS && FC.attitudes && window.E27Engine && E27Engine.attitudeFactors) {
    const f = E27Engine.attitudeFactors(FC.attitudes, CELLS, fl), L = f.lists.length, nD = f.factor.length / L;
    const parent = {}; (CELLS.parties || []).forEach(p => { parent[p.id] = p.parent; });
    const li = FC.categories.map(c => f.lists.indexOf(parent[c] || c));
    arr = new Float32Array(raw.arr.length);
    for (let d = 0; d < n; d++) {
      let t = 0; const j = d % nD;
      for (let k = 0; k < K; k++) { const v = raw.arr[d * K + k] * (li[k] >= 0 ? f.factor[j * L + li[k]] : 1); arr[d * K + k] = v; t += v; }
      for (let k = 0; k < K; k++) arr[d * K + k] /= t;
    }
  }
  const med = new Float64Array(SP.length), lo = new Float64Array(SP.length), hi = new Float64Array(SP.length);
  map.forEach((i, k) => { if (i < 0) return; const v = []; for (let d = 0; d < n; d++) v.push(arr[d * K + k]); v.sort((a, b) => a - b); med[i] = median(v); lo[i] = quant(v, 0.1); hi[i] = quant(v, 0.9); });
  FCW = { n, K, arr, map, med, lo, hi }; FCW_KEY = key; FC_SIM = null;
  return FCW;
}
function weekDraws(w) {
  if (fcOn() && FC && w === WEEKS.length - 1) return fcDraws();
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
// Simulate week w. scen = {shares (%), joint, resp (respParams() or null)} or null for the model's own draws.
function simulate(w, scen) {
  const wd = weekDraws(w);
  if (!wd) return null;
  // On the Forecast, election day's constituency patterns (engine.js forecastSwing, from forecast.json's profiles)
  let sw;
  if (fcOn() && FC && FC.profiles && window.E27Engine && E27Engine.forecastSwing) sw = FCSWING || (FCSWING = E27Engine.forecastSwing(FC.profiles, P, C, COEFI, wd.n, 780046));
  else { if (!SWING || SWING.length < wd.n * NC * NP) SWING = makeSwing(wd.n); sw = SWING; }
  const ents = makeEnts(scen ? scen.joint : []), E = ents.length, n = wd.n;
  const nat = new Int16Array(n * E), cons = new Int16Array(n * NC * E), tots = new Float32Array(n * E);
  const factor = new Float64Array(SP.length), fixed = new Float64Array(SP.length);
  if (scen) SP.forEach((p, i) => {
    if (p === "MN") return;
    const u = (scen.shares[p] || 0) / 100, m = wd.med[i];
    if (m > 0) factor[i] = u / m; else { factor[i] = 0; fixed[i] = u; }
  });
  const sh = new Float64Array(SP.length), iMN = PIDX.MN;
  const rnd = scen && scen.resp ? stdNormal(780046) : null, flow = rnd ? newFlow() : null;
  for (let d = 0; d < n; d++) {
    sh.fill(0);
    wd.map.forEach((i, k) => { if (i >= 0) sh[i] = wd.arr[d * wd.K + k]; });
    if (scen) for (let i = 0; i < SP.length; i++) if (i !== iMN) sh[i] = factor[i] > 0 || fixed[i] === 0 ? sh[i] * factor[i] : fixed[i];
    sh[iMN] = (scen ? scen.shares.MN : R.mnNational2023) / 100;
    if (rnd && scen.joint.length) applyResponse(sh, scen.joint, scen.resp, rnd, flow, w);
    const tot = allocate(sh, ents, sw, d, nat, cons, d * E, d * NC * E);
    for (let k = 0; k < E; k++) tots[d * E + k] = tot[k];
  }
  if (flow) flow.forEach(f => { f.ab /= n; for (let i = 0; i < f.to.length; i++) f.to[i] /= n; });
  return { n, ents, nat, cons, tots, flow };
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
let CUR = null, FC_SIM = null;
function compute() {
  const wk = WEEKS[S.w], model = modelView();
  const ents = makeEnts(S.joint), E = ents.length;
  // Shares after coalition dynamics (single allocation and display)
  const sh = new Float64Array(SP.length);
  SP.forEach((p, i) => { sh[i] = Math.max(0, S.shares[p] || 0) / 100; });
  const flowS = respOn() ? newFlow() : null;
  if (flowS) applyResponse(sh, S.joint, respParams(), null, flowS, S.w);
  const eff = {}; SP.forEach((p, i) => { eff[p] = sh[i] * 100; });
  const single = { nat: new Int16Array(E), cons: new Int16Array(NC * E) };
  const tot = allocate(sh, ents, null, 0, single.nat, single.cons);
  let mode, rows, gov, allGov, sim = null;
  if (fcOn() && fcState === "ready") fcDraws();   // brings the draws, and so FC_SIM, up to the sliders
  if (fcOn() && fcState === "ready" && (sim = FC_SIM || (FC_SIM = simulate(S.w, null)))) {
    mode = "forecast";
    rows = summarise(sim);
    allGov = govFromSim(sim, []);
    gov = pruneGov(allGov, true);
  } else if (fcOn()) {
    mode = "single";
    rows = ents.map((e, k) => ({ seats: single.nat[k], lo: null, hi: null, pThr: null }));
    allGov = govDet(ents, S.joint, Array.from(single.nat));
    gov = pruneGov(allGov, false);
  } else if (model) {
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
  } else if (drawsState === "ready" && (sim = simulate(S.w, { shares: S.shares, joint: S.joint, resp: respOn() ? respParams() : null }))) {
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
  CUR = { wk, mode, ents, eff, tot, single, rows, seats, gov, govAll: allGov, sim, memberSeats, mwc: null, flow: sim && sim.flow ? sim.flow : flowS };
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
// A joint list's total, before voter response: moving it scales the list's parties in proportion
function listRow(j) {
  const t = listTotal(j);
  return `<div class="prow jlist" id="jl-${j}" title="Moving the list's total scales its parties in proportion">
    <span class="pname">List total</span>
    <input type="range" min="0" max="100" step="0.1" value="${t}" data-j="${j}" aria-label="Joint list's total vote share">
    <input type="number" min="0" max="100" step="0.1" value="${t.toFixed(1)}" data-j="${j}" aria-label="Joint list's total vote share, %">
    <span></span>
  </div>`;
}
function respBlock() {
  const rp = JP ? respParams() : { conv: 0, shift: 0, split: 0, abst: 0, splitsOn: [false, false, false, false] };
  const row = (id, label, min, max, step, val) => `<div class="advrow"><label for="a_${id}" class="advl"><span>${label}</span><b id="o_${id}"></b></label><input type="range" class="wide" id="a_${id}" min="${min}" max="${max}" step="${step}" value="${val}"></div>`;
  const cal = JP && JP.meta.calibration, convMed = cal && (cal.conv_median != null ? cal.conv_median : cal.pooled);
  const method = JP ? `<p class="help" style="margin:6px 0 0">Reluctance is the share of a party's voters who would rate a partner 0 to 2 out of 10, ${JP.meta.quarter ? `in ${esc(JP.meta.quarter)}, ` : ""}from a model that combines the Polish National Election Studies of 2023 and 2025, the European Election Study of 2024, CBOS and SWPS University's surveys of 2023 to 2026, pulled towards ideological distance and the PiS divide. Of reluctant voters, ${Math.round(100 * convMed)}% defect (${Math.round(100 * (cal.conv_q10 != null ? cal.conv_q10 : cal.party_q25))}% and ${Math.round(100 * (cal.conv_q90 != null ? cal.conv_q90 : cal.party_q75))}%: the Low and High settings), a rate calibrated on CBOS's questions about joint opposition lists in 2023. Each defector's alternative is chosen among all other parties by how likely their electorate is ever to vote for each (European Election Study 2024); a defector whose alternative is on the list they are leaving stays at home, and a share of the rest abstain anyway. R+ has no ratings of its own and takes PiS's, except between PiS and R+ voters, which comes from CBOS's question on desired coalition partners (September and October 2026); other voters count PiS and R+ on one list as one partner. The abstention share and the penalties for recent splits are judgement.</p>` : "";
  return `<div style="display:flex;flex-direction:column;gap:6px;margin-top:4px">
      <span class="label">Voter response</span>
      <div class="seg small" id="resp" role="group" aria-label="How strongly voters react to joint lists">${["none", "low", "medium", "high"].map(k => `<button type="button" data-resp="${k}">${RESP_NAMES[k]}</button>`).join("")}<button type="button" data-resp="custom" hidden>Custom</button></div>
      <p class="help" id="respnote" style="margin:0"></p>
      <div id="respflow"></div>
      ${JP ? `<details id="adv"${S.advOpen ? " open" : ""}><summary class="help" style="cursor:pointer">Advanced settings</summary>
        ${row("conv", "Reluctant voters who defect", 0, 100, 5, Math.round(100 * rp.conv))}
        ${row("shift", "Reluctance against the estimate (SDs)", -2, 2, 0.25, rp.shift)}
        ${row("abst", "Defectors with a party off the list who abstain anyway", 0, 100, 5, Math.round(100 * rp.abst))}
        ${row("split", "Reluctance between recently split parties", 0, 40, 1, Math.round(100 * rp.split))}
        <div class="chips" style="margin-top:4px">${JP.splits.map((sp, i) => `<label class="chip"><input type="checkbox" id="a_sp${i}"${rp.splitsOn[i] ? " checked" : ""}> ${esc(short(sp[0]))} / ${esc(short(sp[1]))}</label>`).join("")}</div>
        ${method}</details>` : ""}
    </div>`;
}
const pctPt = x => (100 * x).toFixed(1);
function updateRespUI() {
  if (!$("resp")) return;
  const custom = !!S.adv, joint = S.joint.length > 0;
  document.querySelectorAll("#resp button").forEach(b => { const k = b.dataset.resp; b.setAttribute("aria-pressed", k === "custom" ? custom : (!custom && k === S.resp)); b.hidden = k === "custom" && !custom; b.disabled = !joint || !JP; });
  const note = $("respnote");
  if (!JP) note.textContent = "The voter-response data could not be loaded, so a joint list simply adds its members' votes.";
  else if (!joint) note.textContent = "Applies to joint lists: build one to use it.";
  else if (custom) note.textContent = "Advanced settings are changed from Low, Medium and High.";
  else if (S.resp === "none") note.textContent = "Voters do not react: a joint list simply adds its members' votes.";
  else { const rp = respParams(); note.textContent = `Of a party's voters who rate a partner on the list 0 to 2 out of 10, ${Math.round(100 * rp.conv)}% defect: those whose likeliest alternative is on the same list stay at home, and of the rest ${Math.round(100 * rp.abst)}% abstain anyway.${rp.splitsOn.some(Boolean) ? ` Ticked recently split partners carry at least ${Math.round(100 * rp.split)}% reluctance.` : ""}`; }
  if (!JP) return;
  const rp = respParams(), set = (id, v, t) => { const el = $("a_" + id), o = $("o_" + id); if (el && document.activeElement !== el) el.value = v; if (o) o.textContent = t; };
  set("conv", Math.round(100 * rp.conv), Math.round(100 * rp.conv) + "%"); set("shift", rp.shift, (rp.shift > 0 ? "+" : "") + rp.shift.toFixed(2));
  set("abst", Math.round(100 * rp.abst), Math.round(100 * rp.abst) + "%"); set("split", Math.round(100 * rp.split), Math.round(100 * rp.split) + "%");
  rp.splitsOn.forEach((on, i) => { const c = $("a_sp" + i); if (c) c.checked = on; });
  document.querySelectorAll("#adv input").forEach(i => { i.disabled = !joint; });
  renderRespFlow();
}
// Where the votes lost in this scenario go, in points of the national vote
function renderRespFlow() {
  const box = $("respflow"); if (!box) return;
  if (!respOn() || !CUR || !CUR.flow) { box.innerHTML = ""; return; }
  const lines = [];
  S.joint.forEach(m => m.forEach(a => {
    const f = CUR.flow[PIDX[a]], lost = f.ab + sum(Array.from(f.to)); if (lost < 0.0005) return;
    const dest = P.map((b, j) => [b, f.to[PIDX[b]]]).filter(x => x[1] >= 0.0005).sort((x, y) => y[1] - x[1]);
    const shown = dest.slice(0, 3).map(([b, v]) => `${pctPt(v)} to ${esc(short(b))}`);
    const rest = sum(dest.slice(3).map(x => x[1]));
    if (rest >= 0.0005) shown.push(`${pctPt(rest)} to others`);
    if (f.ab >= 0.0005) shown.push(`${pctPt(f.ab)} abstain or other`);
    lines.push(`<div><b>${esc(short(a))}</b> loses ${pctPt(lost)}: ${shown.join(", ")}</div>`);
  }));
  box.innerHTML = lines.length ? `<div class="help" style="display:grid;gap:2px;padding:6px 8px;background:var(--tint);border-radius:8px"><span style="font-weight:500">Votes lost to the joint lists (points of the national vote)</span>${lines.join("")}</div>` : "";
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
      ${listRow(j)}
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
    <p class="help" style="margin:0">Parties standing as one list face an 8% threshold instead of 5%. A government of two or more lists is possible only if no party on one list is excluded from a party on another; the pairs excluded inside a joint list are not checked. On a joint list, the list's total scales its parties in proportion, and moving one party resizes the others so that the list keeps its total.</p>
    <div class="chips" id="jpills">${S.joint.map((m, j) => `<span class="jpill">${esc(sortParties(m).map(short).join(" + "))}<button type="button" data-rm="${j}" aria-label="Remove this joint list">×</button></span>`).join("")}</div>
    ${S.joint.length >= 4 ? `<p class="help" style="margin:0">At most four joint lists.</p>` : avail.length < 2 ? "" : `
    <div class="picker">
      <div class="chips" id="jpick">${avail.map(p => `<button type="button" class="chip" data-p="${esc(p)}" aria-pressed="${S.picker.has(p)}"><span class="dot" style="background:${col(p)}"></span>${esc(p)}</button>`).join("")}</div>
      <div class="status" id="jsummary"></div>
      <div><button type="button" class="btn1" id="jadd">Add joint list</button></div>
    </div>`}
    ${respBlock()}
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
      // the chance is shown only where the threshold is in doubt (the site's rule, SiteUI.inDoubt)
      if (r.pThr != null && SiteUI.inDoubt(cur.tot[k], R.thresholdParty, r.pThr)) bits.push(`chance of clearing 5%: ${prob(r.pThr)}`);
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
    const lr = $("jl-" + j); if (lr && !lr.contains(document.activeElement)) syncList(j);
    const below = e.vote < 100 * R.thresholdJoint;
    const s23 = sum(e.members.map(p => R.seats2023[p] || 0));
    $("jt-" + j).innerHTML = `<span>${pct(e.vote)}${respOn() && Math.abs(e.vote - e.rawVote) > 0.05 ? ` <span class="muted">(${pct(e.rawVote)} before voter response)</span>` : ""}${below ? ` <span class="neg">below 8%</span>` : ""}${r.pThr != null && SiteUI.inDoubt(e.vote / 100, R.thresholdJoint, r.pThr) ? ` <span class="muted">· clears 8%: ${prob(r.pThr)}</span>` : ""}</span>
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
  updateRespUI();
  renderGov();
}
function renderGov() {
  if (TERM || !$("gov")) return;
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
  const endChars = Math.max(0, ...V.parties.filter(p => T.series[p].med[dates.length - 1] != null).map(p => (W < 600 ? short(p) : p).length + 8));
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
    o.push(`<line x1="${x(d)}" x2="${x(d)}" y1="${m.t}" y2="${H - m.b}" stroke="#ECECEC"/><text x="${x(d)}" y="${H - m.b + 14}" text-anchor="middle" font-size="11" fill="#767676">${lab}</text>${yr ? `<text x="${x(d)}" y="${H - m.b + 27}" text-anchor="middle" font-size="11" fill="#767676">${yr}</text>` : ""}`);
  });
  // events: a numbered marker over a dotted line, listed under the chart
  const evs = S.events ? trendEvents() : [], evx = [];
  let lastCx = -1e9;
  evs.forEach(l => {
    const ex = x(l.date), cx = Math.max(ex, lastCx + 17); lastCx = cx; evx.push({ cx, e: l });
    o.push(`<line x1="${ex}" x2="${ex}" y1="${m.t - 20}" y2="${H - m.b}" stroke="#C9C9C9" stroke-dasharray="2 3"/><circle cx="${cx}" cy="10" r="8" fill="#fff" stroke="#B5B5B5"/><text x="${cx}" y="13.5" text-anchor="middle" font-size="10" font-weight="500" fill="#4B4B4B">${l.n}</text>`);
  });
  $("trendevents").innerHTML = evs.map(l => `<span class="muted">${l.n}. ${esc(evWhen(l.date))} · ${l.url ? `<a href="${esc(l.url)}" target="_blank" rel="noopener" style="color:inherit;text-decoration:underline;text-decoration-color:var(--faint)">${esc(l.event)}</a>` : esc(l.event)}</span>`).join("");
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
    // House style (SiteUI.endLabels): a dot in the party's colour, the name in ink, the figure in the party's colour;
    // the labels are already pushed apart above, so endLabels draws each where repel() put it
    const txt = SiteUI.endLabels([{ y: l.y, label: W < 600 ? short(l.p) : l.p, value: l.v.toFixed(1) + "%", colour: col(l.p) }], x(last) + 13, { size: 11.5 });
    o.push(`<line x1="${x(last) + 3}" x2="${x(last) + 10}" y1="${ly0}" y2="${l.y}" stroke="#CCC"/>` + txt.replace("<text ", `<text font-weight="500" fill-opacity="${fade(l.p) ? 0.3 : 1}" `));
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
// at least k seats there. The dots are dealt out by seatAssign(): no constituency holds more dots than it has seats
// and no list more than its projected total, so each dot is a seat the list is projected to hold.
const SEAT_CERTAIN = 0.975;   // won in at least this share of the simulations: certain
const SEAT_PROBABLE = 0.66;   // from here to SEAT_CERTAIN: probable
const SEAT_POSSIBLE = 0.5;    // from here to SEAT_PROBABLE: possible (in play, but the list is more likely than not to take it); below it, a toss-up
// A dot is a seat the list is projected to hold, so its category is always the chance of that list, never of a contender
const seatCat = p => (p >= SEAT_CERTAIN ? "certain" : p >= SEAT_PROBABLE ? "probable" : p >= SEAT_POSSIBLE ? "possible" : "toss-up");
const pc1 = v => (v * 100).toFixed(1).replace(/\.0$/, ""), pc0 = v => String(Math.round(v * 100));
const SEAT_CAT_NOTE = () => `Certain: won in ${pc1(SEAT_CERTAIN)}% or more of the simulations. Probable: ${pc0(SEAT_PROBABLE)}% to ${pc1(SEAT_CERTAIN)}%. Possible: ${pc0(SEAT_POSSIBLE)}% to ${pc0(SEAT_PROBABLE)}%, the seat is in play but the list is more likely than not to take it. Toss-up: under ${pc0(SEAT_POSSIBLE)}%.`;
const ord = n => { const v = n % 100; return n + (["th", "st", "nd", "rd"][(v - 20) % 10] || ["th", "st", "nd", "rd"][v] || "th"); };
const SEATPROB = new WeakMap(), SEATASSIGN = new WeakMap();
const SEAT_OUTSIDE = 0.33;    // "outside possibility": a seat projected for another list, where this list is the next most likely to take it with at least this chance
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
// Deal each constituency's seats to the lists, most probable first: a list's j-th seat in a constituency goes to it
// only if the constituency still has a seat and the list has not reached its projected total. A seat a list could win
// but another list is more likely to hold is not its dot; outside possibilities (below) are counted apart
// (also[k][ci]). Only if a list is still short of its total (medians need not add up exactly) are
// its next seats placed beyond a constituency's magnitude.
function seatAssign(sim, names, totals) {
  const key = names.join("|") + "#" + totals.join(","), hit = SEATASSIGN.get(sim);
  if (hit && hit.key === key) return hit.res;
  const tab = seatTable(sim), cand = [];
  names.forEach((nm, k) => (tab[nm] || []).forEach(st => cand.push({ k, ci: st.ci, j: st.j, p: st.p, used: false })));
  cand.sort((a, b) => b.p - a.p || a.ci - b.ci || a.j - b.j || a.k - b.k);
  const filled = new Int32Array(NC), taken = names.map(() => new Int32Array(NC)), got = names.map(() => []);
  const pass = cap => {
    for (let changed = true; changed;) {
      changed = false;
      for (const st of cand) {
        if (st.used || got[st.k].length >= totals[st.k] || st.j !== taken[st.k][st.ci] + 1) continue;
        if (cap && filled[st.ci] >= C[st.ci].magnitude) continue;
        st.used = true; taken[st.k][st.ci]++; filled[st.ci]++; got[st.k].push(st); changed = true;
      }
    }
  };
  pass(true); pass(false);
  const also = names.map(() => new Int32Array(NC));
  // Outside possibilities: in each constituency the least secure projected seats are
  // matched, in order, to the likeliest seats beyond the projection; a list counts one where it is that next most
  // likely list, with a chance of SEAT_OUTSIDE or more
  for (let ci = 0; ci < NC; ci++) {
    const held = cand.filter(st => st.used && st.ci === ci).sort((a, b) => a.p - b.p);
    const next = cand.filter(st => !st.used && st.ci === ci).sort((a, b) => b.p - a.p);
    next.forEach((st, i) => { if (i < held.length && st.p >= SEAT_OUTSIDE && held[i].k !== st.k) also[st.k][ci]++; });
  }
  const res = { got: got.map(L => L.map(({ ci, j, p }) => ({ ci, j, p }))), also };
  SEATASSIGN.set(sim, { key, res });
  return res;
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
  const asg = sim ? seatAssign(sim, ents.map(e => e.name), ents.map((e, k) => cur.seats[k])) : null;
  { let at = 0; blocks.forEach(bk => { bk.at = at; at += bk.n; if (!byEnt.has(bk.k)) byEnt.set(bk.k, []); byEnt.get(bk.k).push(bk); }); }
  if (tab) byEnt.forEach((bks, k) => {
    const L = asg.got[k]; let take = 0;
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
      else if (cat === "possible") { fo = dim ? 0.05 : 0.2; stroke = ` stroke="${col(bk.p)}" stroke-opacity="${dim ? 0.3 : 1}" stroke-width="0.01"`; }
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
  HEMI = { total, outer, labels, info, sim, cert: null, asg };
  const cbtn = $("hemicertbtn");
  cbtn.disabled = !tab; cbtn.setAttribute("aria-pressed", String(cert));
  cbtn.title = tab ? "Shade each seat by how likely it is to be won" : "Needs the seat simulation, which loads with the scenario data";
  if (cert) {
    const entSeats = {};
    const crows = [...byEnt.keys()].map(k => {
      const e = ents[k], L = info.filter((st, ii) => st && blocks.some(bk => bk.k === k && ii >= bk.at && ii < bk.at + bk.n)), c = { certain: 0, probable: 0, possible: 0, "toss-up": 0 };
      L.forEach(st => { c[seatCat(st.p)]++; });
      entSeats[k] = L;
      return { k, name: e.name, colour: e.colour, seats: cur.seats[k], certain: c.certain, probable: c.probable, possible: c.possible, toss: c["toss-up"] };
    });
    HEMI.cert = { rows: crows };
    HEMI.entSeats = entSeats;
    const rows = crows.map(r => `<tr class="clickrow" data-k="${r.k}" tabindex="0" role="button" title="Seats of ${esc(r.name)} by constituency"><td><span class="dot" style="background:${r.colour};margin-right:6px"></span>${esc(r.name)}</td><td>${r.seats}</td><td>${r.certain}</td><td>${r.probable}</td><td>${r.possible}</td><td>${r.toss}</td></tr>`).join("");
    $("hemicert").innerHTML = `<div style="display:flex;gap:14px;flex-wrap:wrap;align-items:center;font-size:12px;margin:6px 0 2px">
      <span><svg width="12" height="12"><circle cx="6" cy="6" r="5" fill="#555"/></svg> Certain (${pc1(SEAT_CERTAIN)}% or more)</span>
      <span><svg width="12" height="12"><circle cx="6" cy="6" r="5" fill="#555" fill-opacity=".5"/></svg> Probable (${pc0(SEAT_PROBABLE)}–${pc1(SEAT_CERTAIN)}%)</span>
      <span><svg width="12" height="12"><circle cx="6" cy="6" r="4.7" fill="#555" fill-opacity=".2" stroke="#555" stroke-width=".9"/></svg> Possible (${pc0(SEAT_POSSIBLE)}–${pc0(SEAT_PROBABLE)}%)</span>
      <span><svg width="12" height="12"><circle cx="6" cy="6" r="4.6" fill="#fff" stroke="#555" stroke-width="1.2"/></svg> Toss-up (under ${pc0(SEAT_POSSIBLE)}%)</span></div>
      <table class="t"><tr><th>List</th><th>Seats</th><th>Certain</th><th>Probable</th><th>Possible</th><th>Toss-up</th></tr>${rows}</table>
      <p class="help" style="margin:4px 0 0">Each dot is one seat in one constituency, dealt to the list projected to hold it: each constituency's seats go to the lists most likely to win them, as many as it has, and each list gets as many dots as its projected total. A seat a list could win but another list is more likely to hold is not counted for it. The chance is the share of simulated parliaments in which the list wins at least that many seats there. ${SEAT_CAT_NOTE()} Within each list the most certain seats are at the outer end of its arc. Click a list for its seats in each constituency, and its outside possibilities.</p>`;
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
  const per = C.map(() => ({ certain: 0, probable: 0, possible: 0, toss: 0 }));
  L.forEach(st => { const c = per[st.ci], cat = seatCat(st.p); if (cat === "certain") c.certain++; else if (cat === "probable") c.probable++; else if (cat === "possible") c.possible++; else c.toss++; });
  const cell = v => v ? v : `<span class="muted">0</span>`;
  const also = HEMI.asg ? HEMI.asg.also[k] : null, alsoTot = also ? sum(Array.from(also)) : 0;
  const tot = per.reduce((a, c) => ({ certain: a.certain + c.certain, probable: a.probable + c.probable, possible: a.possible + c.possible, toss: a.toss + c.toss }), { certain: 0, probable: 0, possible: 0, toss: 0 });
  const rows = C.map((c, ci) => { const x = per[ci], n = x.certain + x.probable + x.possible + x.toss; return `<tr><td>${c.okreg}. ${esc(c.name)}</td><td>${c.magnitude}</td><td>${n ? `<b>${n}</b>` : `<span class="muted">0</span>`}</td><td>${cell(x.certain)}</td><td>${cell(x.probable)}</td><td>${cell(x.possible)}</td><td>${cell(x.toss)}</td>${also ? `<td>${cell(also[ci])}</td>` : ""}</tr>`; }).join("");
  $("seatmodaltitle").innerHTML = `<span class="dot" style="background:${e.colour};margin-right:6px"></span>${esc(e.name)}: ${cur.seats[k]} seats by constituency`;
  $("seatmodalbody").innerHTML = `<p class="help" style="margin:0 0 6px">The seats shown as dots for this list, by how likely each is: the seats it is projected to hold. ${SEAT_CAT_NOTE()}${also ? ` Outside possibility: a seat projected for another list, where this list is the next most likely to take it, with a chance of ${pc0(SEAT_OUTSIDE)}% or more; these are not counted in its seats.` : ""} ${cur.mode === "model" ? "Model estimates" : "Scenario"} for ${dstr(cur.wk.date)}.</p>
    <table class="t hist"><tr><th>Constituency</th><th style="white-space:normal;line-height:1.15">District magnitude</th><th>Seats won</th><th>Certain</th><th>Probable</th><th>Possible</th><th>Toss-up</th>${also ? `<th style="white-space:normal;line-height:1.15">Outside possibility</th>` : ""}</tr>${rows}
    <tr style="font-weight:600"><td>All 41</td><td>${sum(C.map(c => c.magnitude))}</td><td>${tot.certain + tot.probable + tot.possible + tot.toss}</td><td>${tot.certain}</td><td>${tot.probable}</td><td>${tot.possible}</td><td>${tot.toss}</td>${also ? `<td>${alsoTot}</td>` : ""}</tr></table>`;
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
    <p class="help" style="margin:4px 0 0">Today's parties summed into the lists they stood on in October 2023 (PKW results). "Now" is ${cur.mode === "model" ? `the model's estimate for ${dstr(cur.wk.date)}` : cur.mode === "forecast" ? `the forecast for ${dstr(fromIso(FC.electionDate))}: vote shares are the expected ones (this week's estimates), seats the forecast's medians` : "the hypothetical scenario"}.</p>`;
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
      <p class="help" style="margin:2px 0 0">${joint ? "With joint lists on, a pair excludes two lists from governing together when one party is on each; pairs inside a list are not checked. " : ""}${canRelax ? "Switching a pair recalculates the options from the simulations." : "Including an excluded pair needs the simulation, which loads with the scenario data."}</p></details>`;
}
function renderHemiBlocks() {
  $("hemi2023btn").setAttribute("aria-pressed", String(S.hemi2023)); $("hemicoalbtn").setAttribute("aria-pressed", String(S.hemiCoal));
  render2023(); renderHemiCoal();
  const first = S.coalMsg && S.coalMsg.keys.length ? document.querySelector("#hemicoal .grow-row.newopt") : null;
  if (first) { const sc = first.parentElement; sc.scrollTop = Math.max(0, first.offsetTop - sc.offsetTop - 24); }
}

// ---- pollsters ------------------------------------------------------------------------------
let HOUSE = null;
function renderHouse() {
  renderHousePanel();
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

// ---- by group -------------------------------------------------------------------------------
// The pollsters' group tables (Opinia24, CBOS Flash), pooled by Polls' group model into groups.json, which is
// fetched the first time the view opens. Lists are the 2023 lists with today's splinters counted in them.
let GD = null, groupsState = "idle", GR = null;
const G_DIMS = { sex: "Sex", age: "Age", education: "Education", town: "Town size" };
// short names for the end labels; the tables use the file's own labels
const G_SHORT = { sex: ["Men", "Women"], age: ["18–29", "30–39", "40–49", "50–59", "60+"],
  education: ["Primary", "Vocational", "Secondary", "Higher"], town: ["Village", "Town <20k", "Town 20–100k", "City 100–500k", "City 500k+"] };
// groups are not parties: an ordered ramp from teal to plum
const gRamp = n => [...Array(n).keys()].map(i => { const a = [27, 127, 121], b = [123, 63, 140], t = n === 1 ? 0 : i / (n - 1);
  return "#" + a.map((v, j) => Math.round(v + t * (b[j] - v)).toString(16).padStart(2, "0")).join(""); });
const gMonthDay = m => Math.round(Date.UTC(+m.slice(0, 4), +m.slice(5, 7) - 1, 15) / 864e5);
const gMonthName = (m, long) => (long ? MONTHS : MON)[+m.slice(5, 7) - 1] + " " + m.slice(0, 4);
const gList = id => id === "turnout" ? { id: "turnout", label: "Turnout", colour: "#4B4B4B" } : GD.lists.find(l => l.id === id);
const gIsT = () => S.gList === "turnout" && !!GD.turnout;
const gSigned = v => Math.abs(v) < 0.05 ? "0.0" : signed(v, 1);
const gSure = c => c.lo > 0 || c.hi < 0;
function loadGroups() {
  if (groupsState !== "idle") return;
  groupsState = "loading";
  fetch("groups.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).then(d => {
    GD = d; groupsState = "ready";
    if (!(S.gDim in GD.dims)) S.gDim = Object.keys(GD.dims)[0];
    if (!(S.gList === "turnout" && GD.turnout) && !GD.lists.some(l => l.id === S.gList && l.id !== "Other")) S.gList = GD.lists[0].id;
    if (S.view === "groups") renderGroups();
  }).catch(() => { groupsState = "missing"; if (S.view === "groups") renderGroups(); });
}
function renderGroups() {
  const status = $("groupstatus"), body = $("groupbody");
  if (groupsState !== "ready") {
    if (groupsState === "idle") loadGroups();
    status.hidden = false; body.hidden = true; $("panel3").innerHTML = "";
    status.textContent = groupsState === "missing" ? "The estimates by group could not be loaded." : "Loading the estimates by group…";
    $("gdim").innerHTML = ""; $("glist").innerHTML = "";
    return;
  }
  status.hidden = true; body.hidden = false;
  // "Turnout" is stated turnout intention by group (GD.turnout), drawn like a list
  const isT = gIsT(), tdim = isT ? GD.turnout.dims[S.gDim] : null;
  const dim = GD.dims[S.gDim], li = GD.lists.findIndex(l => l.id === S.gList), L = gList(S.gList);
  const tr = isT ? tdim.trend : dim.trend[S.gList], nat = isT ? GD.turnout.national : GD.national[S.gList];
  const chg = gi => isT ? tdim.change[gi] : dim.change[gi][S.gList], vi = isT ? 4 : 4 + li;
  const M = GD.months.length, cols = gRamp(dim.levels.length);
  const short = G_SHORT[S.gDim] || dim.labels;
  $("gdim").innerHTML = Object.keys(GD.dims).map(k => `<button type="button" data-k="${k}" aria-pressed="${S.gDim === k}">${esc(G_DIMS[k] || GD.dims[k].label)}</button>`).join("");
  $("glist").innerHTML = GD.lists.filter(l => l.id !== "Other").map(l => `<button type="button" data-k="${esc(l.id)}" aria-pressed="${S.gList === l.id}">${esc(l.label)}</button>`).join("") +
    (GD.turnout ? `<button type="button" data-k="turnout" aria-pressed="${isT}">Turnout</button>` : "");
  $("gpts").checked = S.gPts;
  // chart
  const box = $("groupbox"), W = Math.max(320, box.clientWidth - 12), H = Math.round(clamp(W * 0.5, 300, 480));
  const mday = GD.months.map(gMonthDay);
  const rd = S.gPts ? (isT ? tdim.readings : dim.readings) : [];
  const endChars = Math.max("All voters".length, ...short.map(s => s.length)) + 7;
  const m = { l: 34, r: Math.max(W < 520 ? 92 : 118, 16 + 6.6 * endChars), t: 16, b: 34 };
  const a = Math.min(mday[0], ...rd.map(r => r[1])), b = Math.max(mday[M - 1], ...rd.map(r => r[1]));
  const x = d => m.l + (d - a) / (b - a) * (W - m.l - m.r);
  let ymax = 0;
  tr.forEach(g => g.hi.forEach(v => { ymax = Math.max(ymax, v); }));
  nat.forEach(v => { ymax = Math.max(ymax, v); });
  rd.forEach(r => { ymax = Math.max(ymax, r[vi]); });
  ymax = Math.min(100, Math.ceil((ymax + 1) / 5) * 5);
  // turnout intentions sit high: the axis starts near the lowest value instead of at 0
  let ymin = 0;
  if (isT) { let lo = 100; tr.forEach(g => g.lo.forEach(v => { lo = Math.min(lo, v); })); rd.forEach(r => { lo = Math.min(lo, r[vi]); }); ymin = Math.max(0, Math.floor((lo - 2) / 10) * 10); }
  const y = v => m.t + (1 - (v - ymin) / (ymax - ymin)) * (H - m.t - m.b);
  const o = [], ystep = ymax - ymin > 40 ? 10 : 5;
  for (let v = ymin; v <= ymax; v += ystep) o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${y(v)}" y2="${y(v)}" stroke="#EEE"/><text x="${m.l - 6}" y="${y(v) + 4}" text-anchor="end" font-size="11" fill="#767676">${v}%</text>`);
  let prevY = null;
  monthTicks(a, b, W - m.l - m.r).forEach(d => {
    const [lab, yr] = tickLabel(d, prevY); prevY = dateOf(d).getUTCFullYear();
    o.push(`<line x1="${x(d)}" x2="${x(d)}" y1="${m.t}" y2="${H - m.b}" stroke="#ECECEC"/><text x="${x(d)}" y="${H - m.b + 14}" text-anchor="middle" font-size="11" fill="#767676">${lab}</text>${yr ? `<text x="${x(d)}" y="${H - m.b + 27}" text-anchor="middle" font-size="11" fill="#767676">${yr}</text>` : ""}`);
  });
  const pt = (i, v) => `${x(mday[i]).toFixed(1)},${y(v).toFixed(1)}`;
  tr.forEach((g, gi) => {
    const ix = [...Array(M).keys()];
    o.push(`<path d="M${ix.map(i => pt(i, g.hi[i])).join("L")}L${ix.reverse().map(i => pt(i, g.lo[i])).join("L")}Z" fill="${cols[gi]}" fill-opacity="0.13" stroke="none"/>`);
  });
  // the polls' own readings: circles CBOS, squares Opinia24
  const dots = rd.map(r => {
    const cx = x(r[1]), cy = y(r[vi]), c = cols[r[2]];
    return r[0] === 0 ? `<circle cx="${cx.toFixed(1)}" cy="${cy.toFixed(1)}" r="2.4" fill="${c}" fill-opacity="0.4"/>`
      : `<rect x="${(cx - 2.2).toFixed(1)}" y="${(cy - 2.2).toFixed(1)}" width="4.4" height="4.4" fill="${c}" fill-opacity="0.4"/>`;
  });
  o.push(`<g>${dots.join("")}</g>`);
  tr.forEach((g, gi) => o.push(`<path d="M${g.s.map((v, i) => pt(i, v)).join("L")}" fill="none" stroke="${cols[gi]}" stroke-width="1.8"/>`));
  o.push(`<path d="M${nat.map((v, i) => pt(i, v)).join("L")}" fill="none" stroke="#141414" stroke-width="1.4" stroke-dasharray="4 3"/>`);
  // end labels (house style): the dot in the group's colour, the name in ink, the figure in house red
  const lx = x(mday[M - 1]);
  const labs = tr.map((g, gi) => ({ y: y(g.s[M - 1]), label: short[gi], value: g.s[M - 1].toFixed(1) + "%", colour: cols[gi] }));
  labs.push({ y: y(nat[M - 1]), label: isT ? "Everyone" : "All voters", value: nat[M - 1].toFixed(1) + "%", colour: "#141414" });
  o.push(SiteUI.endLabels(labs, lx + 8, { size: 11.5, top: m.t, bottom: H - m.b })
    .replace(/<tspan fill="[^"]*">([^<]*)<\/tspan><\/text>/g, `<tspan fill="${SiteUI.RED}" font-weight="500">$1</tspan></text>`));
  o.push(`<g id="grouphover" class="hov"></g><rect class="hit" x="${m.l}" y="${m.t}" width="${W - m.l - m.r}" height="${H - m.t - m.b}" fill="transparent" style="cursor:crosshair"/>`);
  const svg = $("groups");
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.setAttribute("width", W); svg.setAttribute("height", H);
  svg.setAttribute("aria-label", `${L.label} by ${(G_DIMS[S.gDim] || dim.label).toLowerCase()}, ${gMonthName(GD.months[0], true)} to ${gMonthName(GD.months[M - 1], true)}`);
  svg.innerHTML = o.join("");
  GR = { W, H, m, a, b, x, y, mday, cols, rd, li };
  // note
  const dg = dim.diagnostics || {};
  $("groupnote").textContent = isT
    ? `Stated turnout intention: the share of each group who say they would definitely or probably vote (don't know counted as not), from Opinia24's and CBOS Flash's tables by group${tdim.diagnostics && tdim.diagnostics.polls ? `, ${tdim.diagnostics.polls} polls` : ""}. ` +
      `People overstate their turnout, so the level is well above real turnout (74.4% in 2023); what the polls show reliably is how the groups have moved against each other. ` +
      `Each group's gap follows a monthly random walk, each pollster's lean on each group is estimated, and the line is the average of the two houses; it is not pinned to any national figure. Bands are 80% intervals; the dashed line is everyone.` +
      (S.gPts ? ` Points are the polls' own readings: circles CBOS, squares Opinia24.` : "")
    : `From the pollsters' tables by group: Opinia24 (monthly, telephone and online) and CBOS Flash (about twice a month, telephone and online)${dg.polls ? `, ${dg.polls} polls with this breakdown` : ""}. Decided voters only; today's splinters are counted with the list they stood on in 2023. ` +
    `Each group's gap from the national share follows a monthly random walk; each pollster's lean on each group is estimated, and the line is the average of the two. ` +
    `The lines are pinned to this tracker's national trend as it stood when the model was last fitted (${dlong(fromIso(GD.meta.fitted))}, months to ${gMonthName(GD.months[M - 1], true)}), so the groups average to it. ` +
    `Bands are 80% intervals; the dashed line is all voters.` +
    (S.gPts ? ` Points are the polls' own readings: circles CBOS, squares Opinia24${S.gDim === "age" && !dim.readings.some(r => r[0] === 0) ? " (CBOS's age bands differ from these and are not drawn)" : ""}.` : "");
  // every list, every group
  const lists = GD.lists.filter(l => l.id !== "Other"), fm = gMonthName(GD.months[0], true);
  const tcell = c => `<td${gSure(c) ? "" : ` class="unsure"`}>${gSigned(c.gap)}<span class="iv">${gSigned(c.lo)} to ${gSigned(c.hi)}</span></td>`;
  $("groupall").innerHTML = `<tr><th>${esc(G_DIMS[S.gDim] || dim.label)}</th>${lists.map(l => `<th><span class="dot" style="background:${l.colour}"></span>${esc(l.label)}</th>`).join("")}${GD.turnout ? "<th>Turnout</th>" : ""}</tr>` +
    dim.labels.map((lab, gi) => `<tr><td><span class="dot" style="background:${cols[gi]};margin-right:6px"></span>${esc(lab)}</td>${lists.map(l => tcell(dim.change[gi][l.id])).join("")}${GD.turnout ? tcell(GD.turnout.dims[S.gDim].change[gi]) : ""}</tr>`).join("");
  $("groupallnote").textContent = `The change in each group's gap from all voters' share since ${fm}, in percentage points, with the 80% interval under each figure; grey where the interval includes zero. A group that moved with the country shows 0.` +
    (GD.turnout ? ` Turnout: the change in the group's gap from everyone's stated intention to vote.` : "");
  // side panel: the list on show
  const lm = gMonthName(GD.months[M - 1]), f0 = gMonthName(GD.months[0]);
  const rows = tr.map((g, gi) => { const c = chg(gi);
    return `<tr><td><span class="dot" style="background:${cols[gi]};margin-right:6px"></span>${esc(dim.labels[gi])}</td><td>${g.s[0].toFixed(1)}%</td><td>${g.s[M - 1].toFixed(1)}%</td><td${gSure(c) ? "" : ` class="unsure"`}>${gSigned(c.gap)}</td></tr>`; }).join("");
  $("panel3").innerHTML = `
  <div class="card">
    <span class="label">By ${esc((G_DIMS[S.gDim] || dim.label).toLowerCase())}</span>
    <h2 style="margin:0;font-size:20px;font-weight:400"><span class="dot" style="background:${L.colour};margin-right:8px;vertical-align:1px"></span>${esc(L.label)}</h2>
    <table class="t"><tr><th>Group</th><th>${f0}</th><th>${lm}</th><th>Against all</th></tr>${rows}
      <tr><td>${isT ? "Everyone" : "All voters"}</td><td>${nat[0].toFixed(1)}%</td><td>${nat[M - 1].toFixed(1)}%</td><td></td></tr></table>
    <p class="help" style="margin:0">Against all: the change in the group's gap from all voters' share since ${fm}, in points; grey where the 80% interval includes zero.</p>
  </div>
  <div class="card soft">
    <span class="label">Share</span>
    <div class="actions"><button type="button" class="btn2" id="copylink">Copy link to this view</button></div>
  </div>`;
}
function groupHover(ev) {
  if (!GR || !GD) return;
  const svg = $("groups"), r = svg.getBoundingClientRect(), k = GR.W / r.width;
  const mx = (ev.clientX - r.left) * k, d = GR.a + (mx - GR.m.l) / (GR.W - GR.m.l - GR.m.r) * (GR.b - GR.a);
  let i = 0, bd = Infinity; GR.mday.forEach((t, j) => { const dd = Math.abs(t - d); if (dd < bd) { bd = dd; i = j; } });
  const isT = gIsT(), dim = GD.dims[S.gDim], tr = isT ? GD.turnout.dims[S.gDim].trend : dim.trend[S.gList], nat = isT ? GD.turnout.national : GD.national[S.gList];
  $("grouphover").innerHTML = `<line x1="${GR.x(GR.mday[i])}" x2="${GR.x(GR.mday[i])}" y1="${GR.m.t}" y2="${GR.H - GR.m.b}" stroke="#141414" stroke-opacity=".35"/>`;
  const html = `<b>${esc(gList(S.gList).label)} · ${gMonthName(GD.months[i], true)}</b>` +
    tr.map((g, gi) => `<div class="r"><span><span class="dot" style="background:${GR.cols[gi]}"></span>${esc(dim.labels[gi])}</span><span>${g.s[i].toFixed(1)}% <span class="muted" style="display:inline">(${g.lo[i].toFixed(1)}–${g.hi[i].toFixed(1)})</span></span></div>`).join("") +
    `<div class="r" style="border-top:1px solid var(--rule);margin-top:3px;padding-top:3px"><span>${isT ? "Everyone" : "All voters"}</span><span>${nat[i].toFixed(1)}%</span></div>`;
  placeTip($("grouptip"), $("groupbox"), ev, html);
}

// ---- static controls ------------------------------------------------------------------------
function renderChips() {
  $("pchips").innerHTML = VD().pollsters.map(o => `<button type="button" class="chip" data-org="${esc(o)}" aria-pressed="${S.houses.has(o)}">${esc(o)}</button>`).join("") +
    (S.houses.size ? `<button type="button" class="linkbtn" id="houseclear">Show all houses</button>` : "");
  $("periods").innerHTML = PERIODS.map(([k, l]) => `<button type="button" data-k="${k}" aria-pressed="${S.period === k}">${l}</button>`).join("");
  [...$("views").children].forEach(b => {
    const on = S.view === b.dataset.v || (SUBVIEWS[SECTION] || {})[S.view] === b.dataset.v;
    b.setAttribute("aria-pressed", on); b.setAttribute("aria-selected", on);
    b.hidden = !!TERM && b.dataset.v === "groups";
  });
  [...new Set(Object.values(VIEW_EL))].forEach(id => { $(id).hidden = id !== VIEW_EL[S.view]; });
  $("panel").hidden = !TERM && !(SECTION === "scenarios" && S.view === "lists");
  $("panel2").hidden = !!TERM || S.view !== "pollsters";
  $("panel3").hidden = !!TERM || S.view !== "groups";
  $("panel4").hidden = !!TERM || S.view !== "seats";
  $("panel5").hidden = !!TERM || S.view !== "levers";
  if (SECTION !== "polls") $("termwrap").hidden = true;
  $("pts").checked = S.points;
  $("ptswrap").hidden = S.view !== "trends";
  $("evwrap").hidden = S.view !== "trends";
  $("ev").checked = S.events;
  $("dlwrap").hidden = !(S.view === "trends" && !TERM);
  $("dl").checked = S.dateLine;
  $("trendhelp").textContent = TERM ? "Hover for every party's estimate. Diamonds are the election results the trend is anchored on. Click a poll figure to see that poll alone and how far each of its figures sits from the trend. Click parties below the chart to show them alone."
    : "Hover for every party's estimate. Click a poll figure to see that poll alone and how far each of its figures sits from the trend (click again or press Escape to let go). Click parties below the chart to show them alone.";
}


function renderHousePanel() {
  if (TERM) return;
  const V = VD(), picked = [...S.houses];
  if (picked.length !== 1) {
    $("panel2").innerHTML = `<div class="card"><span class="label">Polling house</span><p class="help" style="margin:0">Click a house in the chart to see its polls (shift-click to highlight several) and how it leans on each party. Its polls are then highlighted on the Trends view.${picked.length > 1 ? " Several houses are highlighted; pick one to see its details." : ""}</p></div>`;
    return;
  }
  const org = picked[0], oi = V.pollsters.indexOf(org), h = (V.house || []).find(x => x.org === org);
  const polls = new Map(); V.polls.forEach(r => { if (r[1] === oi) polls.set(pollKey(r), r[0]); });
  const dates = [...polls.values()].sort((a, b) => a - b), last = WEEKS[WEEKS.length - 1].date;
  const recent = dates.filter(d => d > last - 90).length;
  const eff = h ? V.blocs.filter(b => h.effects[b]).map(b => { const e = h.effects[b];
    return `<tr><td><span class="dot" style="background:${col(b)};margin-right:6px"></span>${esc(b)}</td><td class="${cls(e[0])}">${signed(e[0], 1)}</td><td class="muted">${e[1].toFixed(1)} to ${e[2].toFixed(1)}</td></tr>`; }).join("") : "";
  $("panel2").innerHTML = `
  <div class="card">
    <span class="label">Polling house</span>
    <h2 style="margin:0;font-size:20px;font-weight:400">${esc(org)}</h2>
    <table class="t"><tr><th>Polls</th><th>First</th><th>Latest</th><th>Last 90 days</th></tr><tr><td>${dates.length}</td><td>${dates.length ? dstr(dates[0]) : "–"}</td><td>${dates.length ? dstr(dates[dates.length - 1]) : "–"}</td><td>${recent}</td></tr></table>
  </div>
  <div class="card">
    <span class="label">Lean against the average house</span>
    <table class="t"><tr><th>Party</th><th>Lean (pp)</th><th>80% interval</th></tr>${eff}</table>
    <p class="help" style="margin:0">A positive lean means the house shows the party higher than the others do.</p>
  </div>`;
}

// ---- rendering ------------------------------------------------------------------------------
function renderView() {
  if (TERM && S.view === "groups") { S.view = "trends"; renderChips(); }
  renderBanner();
  if (S.view === "trends") renderTrends();
  else if (HEMI_VIEWS.includes(S.view)) { renderHemi(); renderHemiBlocks(); if (S.view === "seats") renderSeatsPanel(); if (S.view === "levers") renderLevers(); }
  else if (S.view === "everyseat") renderEverySeat();
  else if (S.view === "senate") renderSenate();
  else if (S.view === "sliders") renderSliders();
  else if (S.view === "whovotes") renderWhoVotes();
  else if (S.view === "map") { if (window.Geography) Geography.render("map"); else { renderMap(); renderConstCard(); } }
  else if (S.view === "worth" || S.view === "population") { if (window.Geography) Geography.render(S.view); else $(VIEW_EL[S.view]).innerHTML = `<p class="help">Loading…</p>`; }
  else if (S.view === "groups") renderGroups();
  else renderHouse();
}
// What the scenario on screen is, in words: for the banner, the export subtitles and the Every seat CSV
function scenText() {
  const bits = [], d = defaults(S.w);
  if (S.levers.length) bits.push((S.base === "polls" ? "polls now, uniform swing" : "polls now, groups as polled") + "; " + S.levers.map(leverText).join("; "));
  const hand = SP.filter(p => (S.levers.length ? S.hand.has(p) : Math.abs((S.shares[p] || 0) - d[p]) > 0.051));
  if (hand.length) bits.push(hand.map(p => `${p} ${(S.shares[p] || 0).toFixed(1)}% (model ${d[p].toFixed(1)}%)`).join(", "));
  if (S.joint.length) bits.push("joint lists: " + S.joint.map(m => sortParties(m).map(short).join(" + ")).join("; ") + (respOn() ? `, voter response ${S.adv ? "custom" : RESP_NAMES[S.resp].toLowerCase()}` : ", no voter response"));
  return bits.join("; ");
}
// On Seats and Geography a scenario arriving in the link is shown with a banner and a way back to the model; on
// Scenarios the banner says what the scenario is
function renderBanner() {
  const b = $("scenbanner");
  if (FORECAST) {
    const fl = S.fl.filter(x => x.c);
    if (!fl.length || S.view === "sliders" || (!fcOn() && S.view !== "senate")) { b.hidden = true; return; }
    b.innerHTML = `<b>With the sliders</b>: ${esc(fl.map(flText).join("; "))}. <button type="button" class="linkbtn" id="toslide">Change them</button> · <button type="button" class="linkbtn" id="flclear2">Clear them</button>`;
    b.hidden = false; return;
  }
  if (TERM || SECTION === "polls" || !CUR || CUR.mode === "model" || S.view === "whovotes") { b.hidden = true; return; }
  const edit = SECTION === "scenarios" ? "" : ` · <a href="${esc(sectionUrl("scenarios") + "#" + carryString())}" target="_top">Change it in Scenarios</a> · <button type="button" class="linkbtn" id="tomodel">Back to the model</button>`;
  b.innerHTML = `<b>Hypothetical scenario</b> for the week of ${dlong(CUR.wk.date)}: ${esc(scenText())}.${edit}`;
  b.hidden = false;
}
const sharesValid = () => 100 - sum(SP.map(p => S.shares[p] || 0)) >= -0.05;
// While the named shares exceed 100%, the last valid numbers stay on screen, faded.
function update() {
  if (TERM) { renderTermPanel(); renderView(); saveHash(); return; }
  if (sharesValid() || !CUR) compute();
  updatePanel(); renderView(); saveHash();
}
const updateSoon = debounce(update, 60);
const flSoon = debounce(update, 250);

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
  if (FORECAST && S.fh === "now") q.set("fh", "now");
  if (FORECAST) { const fl = S.fl.filter(x => x.c); if (fl.length) q.set("fl", fl.map(x => [x.m, x.c, ...FL_DIMS.map(k => x.g[k] == null ? "" : x.g[k])].join(".")).join(";")); }
  if (FORECAST && S.view === "senate") {
    const sp = Object.entries(S.sp).filter(([, v]) => v).map(([p, v]) => p + ":" + v).join(";");
    if (sp) q.set("sp", sp);
    if (!S.sinc) q.set("si", "0");
    if (S.scoal.size) q.set("sc", [...S.scoal].join(";"));
    if (S.snSort === "doubt") q.set("so", "doubt");
  }
  if (S.w !== WEEKS.length - 1) q.set("w", iso(WEEKS[S.w].date));
  const bs = baseShares(), ed = SP.filter(p => S.hand.has(p) && Math.abs(S.shares[p] - bs[p]) > 0.051);
  if (ed.length) q.set("v", ed.map(p => p + ":" + S.shares[p]).join(","));
  const dk = defaultExcl(), xi = [...dk].filter(k => !S.excl.has(k)), xx = [...S.excl].filter(k => !dk.has(k));
  if (xi.length) q.set("xi", xi.join(";"));
  if (xx.length) q.set("xx", xx.join(";"));
  if (S.joint.length) q.set("j", S.joint.map(m => m.join(",")).join(";"));
  if (S.joint.length && S.adv) q.set("ra", [S.adv.conv, S.adv.shift, S.adv.abst, S.adv.split, S.adv.splitsOn.map(x => +x).join("")].join(","));
  else if (S.joint.length && S.resp !== "medium") q.set("r", S.resp);
  if (S.levers.length) { q.set("g", S.levers.map(leverKey).join(";")); if (S.base !== "groups") q.set("gb", S.base); if (S.hold) q.set("gh", "1"); }
  if (S.houses.size) q.set("h", [...S.houses].join(";"));
  if (S.parties.size) q.set("pp", [...S.parties].join(";"));
  if (S.view !== VIEWS[0]) q.set("view", S.view);
  if (S.view === "groups") { q.set("gd", S.gDim); q.set("gl", S.gList); if (S.gPts) q.set("gp", "1"); }
  if (S.view === "everyseat") { if (S.esC) q.set("ec", S.esC); if (S.esL) q.set("el", S.esL); if (S.esCat !== "all") q.set("ek", S.esCat); if (S.esSort !== "const") q.set("eo", S.esSort); }
  if (S.view === "whovotes") { if (S.wiMode !== "all") q.set("wm", S.wiMode); if (S.wiMetric !== "vote") q.set("wv", S.wiMetric); }
  if (SECTION === "geography" && window.Geography && Geography.writeState) Geography.writeState(q);
  if (S.period !== "all") q.set("p", S.period);
  if (!S.points) q.set("dp", "0");
  return q.toString();
}
// The week and the scenario, for links to another section
function carryString() {
  const q = new URLSearchParams(stateString()), out = new URLSearchParams();
  SCEN_KEYS.forEach(k => { if (q.has(k)) out.set(k, q.get(k)); });
  return out.toString();
}
// The section links on the host page (a[data-s], in each page's .typ) carry the week and the scenario, except to Polls
function syncNav() {
  let doc = null; try { if (hashTarget() !== window) doc = window.top.document; } catch (e) { doc = null; }
  if (!doc) return;
  const carry = carryString();
  doc.querySelectorAll("a[data-s]").forEach(a => {
    const s = a.getAttribute("data-s"); if (!SECTION_PAGE[s]) return;
    const base = a.getAttribute("href").split("#")[0];
    a.setAttribute("href", base + (s !== "polls" && carry ? "#" + carry : ""));
  });
}
const saveHash = debounce(() => {
  const t = hashTarget(), s = stateString();
  try { t.history.replaceState(null, "", t.location.pathname + t.location.search + (s ? "#" + s : "")); } catch (e) { /* ignore */ }
  syncNav();
}, 250);
// Before the tracker had four addresses, its seat views and scenarios were all on one page: a link that names them is
// sent on to the page that now holds them (Seats, or Scenarios for an edited scenario). Returns false when it does.
function forwardOld(q) {
  if (SECTION !== "polls" || q.get("t")) return true;
  const scen = ["v", "j", "r", "ra", "xi", "xx", "g"].some(k => q.has(k)), view = q.get("view");
  const to = view === "seats" ? (scen ? "scenarios" : "seats") : scen ? "scenarios" : null;
  if (!to) return true;
  q.delete("view");
  try { hashTarget().location.replace(sectionUrl(to) + (q.toString() ? "#" + q : "")); return false; } catch (e) { return true; }
}
function restoreHash() {
  let h = "";
  try { h = hashTarget().location.hash.slice(1); } catch (e) { h = location.hash.slice(1); }
  const q = new URLSearchParams(h);
  if (!forwardOld(q)) return false;
  S.w = WEEKS.length - 1;
  if (q.get("w")) { const d = fromIso(q.get("w")), i = WEEKS.findIndex(w => w.date === d); if (i >= 0) S.w = i; }
  S.shares = defaults(S.w);
  if (q.get("v")) q.get("v").split(",").forEach(kv => { const i = kv.lastIndexOf(":"), p = kv.slice(0, i), v = parseFloat(kv.slice(i + 1)); if (p in PIDX && isFinite(v)) { S.shares[p] = clamp(v, 0, 100); S.hand.add(p); } });
  if (q.get("g")) S.levers = q.get("g").split(";").map(parseLever).filter(Boolean);
  if (q.get("gb") === "polls") S.base = "polls";
  S.hold = q.get("gh") === "1";
  if (q.get("ec")) S.esC = q.get("ec");
  if (q.get("el")) S.esL = q.get("el");
  if (["open", "play", "toss-up"].includes(q.get("ek"))) S.esCat = q.get("ek");
  if (q.get("eo") === "doubt") S.esSort = "doubt";
  if (q.get("wm") === "only") S.wiMode = "only";
  if (q.get("wv") === "seats") S.wiMetric = "seats";
  if (SECTION === "geography" && window.Geography && Geography.readState) Geography.readState(q);
  S.shares.Other = 100 - sum(SP.map(p => S.shares[p]));
  if (q.get("j")) q.get("j").split(";").forEach(g => { const m = g.split(",").filter(p => P.includes(p)); if (m.length >= 2 && !m.some(p => S.joint.flat().includes(p)) && S.joint.length < 4) S.joint.push(m); });
  const okPair = k => { const p = pairOf(k); return p.length === 2 && p[0] !== p[1] && p.every(x => P.includes(x)); };
  if (q.get("xi")) q.get("xi").split(";").filter(okPair).forEach(k => S.excl.delete(pairKey(...pairOf(k))));
  if (q.get("xx")) q.get("xx").split(";").filter(okPair).forEach(k => S.excl.add(pairKey(...pairOf(k))));
  if (["none", "low", "medium", "high"].includes(q.get("r"))) S.resp = q.get("r");
  if (q.get("ra")) { const a = q.get("ra").split(","), n = a.slice(0, 4).map(Number), mk = (a[4] || "").padEnd(4, "0");
    if (n.every(isFinite)) S.adv = { conv: clamp(n[0], 0, 1), shift: clamp(n[1], -2, 2), abst: clamp(n[2], 0, 1), split: clamp(n[3], 0, 0.4), splitsOn: [0, 1, 2, 3].map(i => mk[i] === "1") }; }
  const list = k => q.get(k) ? q.get(k).split(";") : [];
  if (q.get("t") && SECTION === "polls") PENDING = { term: q.get("t"), houses: list("h"), parties: list("pp") };
  else {
    list("h").forEach(o => { if (D.pollsters.includes(o)) S.houses.add(o); });
    list("pp").forEach(p => { if (P.includes(p)) S.parties.add(p); });
  }
  if (VIEWS.includes(q.get("view"))) S.view = q.get("view");
  // checked against groups.json when it loads
  if (q.get("gd")) S.gDim = q.get("gd");
  if (q.get("gl")) S.gList = q.get("gl");
  S.gPts = q.get("gp") === "1";
  if (PERIODS.some(p => p[0] === q.get("p"))) S.period = q.get("p");
  S.points = q.get("dp") !== "0";
  // The forecast is of the model alone, for the latest week: a scenario or a week in the link is not carried here
  if (FORECAST) {
    S.fh = q.get("fh") === "now" ? "now" : "election";
    if (q.get("fl")) q.get("fl").split(";").forEach(t => { const [m, c, ...g] = t.split("."), v = parseFloat(c);
      if (["turnout", "econ", "soc"].includes(m) && isFinite(v)) S.fl.push({ m, c: v, g: Object.fromEntries(FL_DIMS.map((k, i) => [k, g[i] === "" || g[i] == null ? null : +g[i]])) }); });
    if (q.get("sp")) q.get("sp").split(";").forEach(kv => { const [p, v] = kv.split(":"); if (p in PIDX && ["A", "B", "C"].includes(v)) S.sp[p] = v; });
    S.sinc = q.get("si") !== "0";
    if (q.get("sc")) q.get("sc").split(";").forEach(k => S.scoal.add(k));
    if (q.get("so") === "doubt") S.snSort = "doubt";
    S.w = WEEKS.length - 1; S.shares = defaults(S.w); S.shares.Other = 100 - sum(SP.map(p => S.shares[p]));
    S.joint = []; S.levers = []; S.hand.clear();
  }
  return true;
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
  const items = HEMI.labels.map(l => {
    const cs = Math.cos(l.a), top = Math.abs(cs) < 0.12, r = HEMI.outer + 0.14 + (top ? 0.24 : 0);
    const x = r * cs, y = -r * Math.sin(l.a) + 0.035, pw = tw(l.p, 500), nw = tw(String(l.n), 400), tot = pw + GAP + nw;
    return { l, cs, y, pw, w: tot, x0: top ? x - tot / 2 : cs > 0 ? x : x - tot };
  });
  // neighbouring small blocks can put their labels on top of each other: lift the one further from the top until they clear
  const box = it => [it.x0, it.x0 + it.w, it.y - FS, it.y + 0.02];
  for (let pass = 0; pass < 30; pass++) {
    let moved = false;
    for (let i = 0; i < items.length; i++) for (let j = i + 1; j < items.length; j++) {
      const A = box(items[i]), B = box(items[j]), pad = 0.03;
      if (A[0] < B[1] + pad && B[0] < A[1] + pad && A[2] < B[3] && B[2] < A[3]) {
        (Math.abs(items[i].cs) >= Math.abs(items[j].cs) ? items[i] : items[j]).y -= FS * 1.15; moved = true;
      }
    }
    if (!moved) break;
  }
  items.forEach(it => {
    const l = it.l;
    o.push(`<text x="${it.x0.toFixed(3)}" y="${it.y.toFixed(3)}" font-size="${FS}" font-weight="500" fill="${col(l.p)}">${esc(l.p)}</text>`);
    o.push(`<text x="${(it.x0 + it.pw + GAP).toFixed(3)}" y="${it.y.toFixed(3)}" font-size="${FS}" font-weight="400" fill="${col(l.p)}" fill-opacity="0.7">${l.n}</text>`);
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
  const grp = only === "groups";
  if (grp && !GD) return;
  const titles = { trend: "Voting intention", hemi: "Seat projection", map: "Seats by constituency", house: "House effects", combo: "Polish party polling: trends and projections",
    groups: grp ? `${gList(S.gList).label} by ${(G_DIMS[S.gDim] || "group").toLowerCase()}` : "",
    geo: only === "geo" && window.Geography ? Geography.exportTitle() : "" };
  const fname = { trend: "trends", hemi: "hemicycle", map: "constituencies", house: "pollsters", combo: "trends-and-projections", groups: grp ? `by-${S.gDim}-${slug(S.gList)}` : "",
    geo: only === "geo" && window.Geography ? Geography.exportName() : "" }[only];
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
    : (only === "hemi" || only === "map") ? (CUR.mode === "model" ? `${dlong(CUR.wk.date)} · model estimates` : CUR.mode === "forecast" ? fcSubtitle() : "Hypothetical scenario") : `Estimates to ${dlong(WEEKS[WEEKS.length - 1].date)}`;
  // a hypothetical scenario is not the model's estimate: no pollsters are named, and the image says it is hypothetical
  const hypo = !past && CUR.mode !== "model" && CUR.mode !== "forecast" && (only === "hemi" || only === "map" || only === "trend" || combo);
  const sub = grp ? `Share among decided voters, ${gMonthName(GD.months[0], true)} to ${gMonthName(GD.months[GD.months.length - 1], true)}, with 80% intervals; dashed line: all voters. From Opinia24's and CBOS Flash's tables by group, pinned to this tracker's national trend as fitted on ${dlong(fromIso(GD.meta.fitted))}. Splinters are counted with their 2023 list.${S.gPts ? " Points: the polls' readings (circles CBOS, squares Opinia24)." : ""}`
    : only === "geo" ? Geography.exportSub()
    : hypo && only === "trend" ? `Estimates to ${dlong(WEEKS[WEEKS.length - 1].date)}. The lines are the model's estimates from published polls. This image was made with a hypothetical scenario set in the tracker, whose vote shares are not poll estimates.` : hypo ? `Hypothetical scenario for the week of ${dlong(CUR.wk.date)}: ${scenText()}. These are not poll estimates; seats are projected from these shares.${combo ? " The trend lines are the model's estimates from published polls." : ""}` : `${base}. Based on polls by ${VD().pollsters.join(", ")}.`, mc = document.createElement("canvas").getContext("2d");
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
    const certNote = `Each dot is one seat in one constituency, dealt to the list projected to hold it: each constituency's seats go to the lists most likely to win them, and each list gets as many as its projected total. A seat's chance is the share of the simulated parliaments in which the list wins at least that many seats in the constituency, for example its 3rd seat in a district with a magnitude of 8 (eight seats). ${SEAT_CAT_NOTE()} Seats of one list in one constituency are interchangeable: the model has no candidates. Within each list the most certain seats are at the outer end of its arc; the toss-ups sit where the lists meet. ${CUR.mode === "model" ? "Model estimates" : "Scenario"} for ${dlong(CUR.wk.date)}, from ${HEMI.sim ? HEMI.sim.n : 250} simulations.`;
    const certLines = wrapTo(certNote, "10.5px " + F, SW - 2 * pad);
    secs.push({ h: 26 + (cert.rows.length + 1) * 17 + 10 + certLines.length * 14 + 18, draw: y0 => {
      const key = [["certain", `Certain (${pc1(SEAT_CERTAIN)}% or more)`], ["probable", `Probable (${pc0(SEAT_PROBABLE)}–${pc1(SEAT_CERTAIN)}%)`], ["possible", `Possible (${pc0(SEAT_POSSIBLE)}–${pc0(SEAT_PROBABLE)}%)`], ["toss-up", `Toss-up (under ${pc0(SEAT_POSSIBLE)}%)`]];
      g.font = "11px " + F; g.textAlign = "left";
      let lx = pad;
      key.forEach(([c, t]) => {
        g.beginPath(); g.arc(lx + 6, y0 + 6, 5, 0, 2 * Math.PI);
        if (c === "toss-up") { g.fillStyle = "#fff"; g.fill(); g.strokeStyle = "#555"; g.lineWidth = 1.2; g.stroke(); }
        else { g.globalAlpha = c === "probable" ? 0.5 : c === "possible" ? 0.2 : 1; g.fillStyle = "#555"; g.fill(); g.globalAlpha = 1; if (c === "possible") { g.strokeStyle = "#555"; g.lineWidth = 0.9; g.stroke(); } }
        g.fillStyle = "#4B4B4B"; g.fillText(t, lx + 16, y0 + 10); lx += 16 + g.measureText(t).width + 20;
      });
      y0 += 26;
      const nx = [pad + 200, pad + 262, pad + 332, pad + 402, pad + 472];
      g.fillStyle = "#767676"; g.font = "500 11px " + F; g.fillText("List", pad + 16, y0 + 10);
      g.textAlign = "right"; ["Seats", "Certain", "Probable", "Possible", "Toss-up"].forEach((t, j) => g.fillText(t, nx[j], y0 + 10));
      g.strokeStyle = "#DDD"; g.lineWidth = 1; g.beginPath(); g.moveTo(pad, y0 + 15); g.lineTo(pad + 480, y0 + 15); g.stroke();
      cert.rows.forEach((r, ri) => {
        const ry = y0 + 17 + (ri + 1) * 17 - 5;
        g.textAlign = "left"; g.fillStyle = r.colour; g.beginPath(); g.arc(pad + 5, ry - 4, 4, 0, 2 * Math.PI); g.fill();
        g.fillStyle = "#141414"; g.font = "11px " + F; g.fillText(r.name, pad + 16, ry);
        g.textAlign = "right"; [r.seats, r.certain, r.probable, r.possible, r.toss].forEach((v, j) => g.fillText(String(v), nx[j], ry));
      });
      g.textAlign = "left";
      const ty = y0 + 17 + (cert.rows.length + 1) * 17 + 10;
      g.fillStyle = "#767676"; g.font = "10.5px " + F;
      certLines.forEach((l, li) => g.fillText(l, pad, ty + li * 14));
    } });
  }
  if ((combo || only === "hemi" && S.hemi2023) && !TERM) {
    const rows23 = since2023(), note23 = `Today's parties summed into the lists they stood on in October 2023 (PKW results). "Now" is ${CUR.mode === "model" ? `the model's estimate for ${dlong(CUR.wk.date)}` : CUR.mode === "forecast" ? `the forecast for ${dlong(fromIso(FC.electionDate))}: vote shares are the expected ones, seats the forecast's medians` : "the hypothetical scenario"}.`;
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
    const note = `${probabilistic ? `Chance: the share of simulations in which the coalition has ${R.seatsMajority} or more seats; options under 5% are left out.` : "Single allocation at the shares on screen."} Ordered from the most to the least plausible: coalitions of ideologically adjacent parties (none skips a seated party between its most distant members) first, then those with fewer parties, then a narrower span; a majority with a party it does not need ranks below the smaller majority inside it; options without a majority in most simulations come last. ${CUR.mode === "model" ? `Model estimates for ${dlong(CUR.wk.date)}.` : "Hypothetical scenario."}`;
    const none = !shownC.length, noteLines = none ? [] : wrapTo(note, "10.5px " + F, SW - 2 * pad);
    const rulesH = sum(ruleLines.map(r => r.lines.length * 13 + 3));
    secs.push({ col: "R", h: (none ? 24 + 18 : 24 + noteLines.length * 14 + 6 + 20 + shownC.length * 31 + (more > 0 ? 16 : 0)) + 22 + rulesH + 10, draw: y0 => {
      g.textAlign = "left"; g.fillStyle = "#141414"; g.font = "500 12px " + F; g.fillText("Possible governments", pad, y0 + 12);
      let y;
      if (none) { g.fillStyle = "#141414"; g.font = "11px " + F; g.fillText("None", pad, y0 + 30); y = y0 + 36; }
      else {
        g.fillStyle = "#767676"; g.font = "10.5px " + F;
        noteLines.forEach((l, li) => g.fillText(l, pad, y0 + 28 + li * 14));
        y = y0 + 24 + noteLines.length * 14 + 6;
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
      }
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
  const fileBase = `polling-tracker-${fname}${hypo ? "-hypothetical-scenario" : `-${grp ? GD.months[GD.months.length - 1] : past ? TERM.term.id : iso(CUR.wk.date)}`}`;
  if (pdf) download(fileBase + ".pdf", await g.finish());
  else cv.toBlob(b => download(fileBase + ".png", b), "image/png");
}

// ---- events ---------------------------------------------------------------------------------
function setWeek(i) {
  if (FORECAST) return;   // the forecast is always from the latest week
  S.w = clamp(i, 0, WEEKS.length - 1); S.shares = defaults(S.w); S.hand.clear(); S.focusGov = null; S.pinGov = null;
  S.picker.forEach(p => { if (!exists(p, WEEKS[S.w].date)) S.picker.delete(p); });
  // changes by group start from the week the cells are pinned to: elsewhere they are set aside
  if (S.levers.length && cellsState === "ready" && cellsWeek() !== S.w) { S.levers = []; LEVRES = null; toast("Changes by group start from the week of " + dlong(fromIso(CELLS.tracker.week)) + ", so they were set aside"); }
  applyLevers();
  update();
}
// ---- joint lists: a list's total and its parties' shares move together
const jointIndex = p => S.joint.findIndex(m => m.includes(p));
const listTotal = j => sum(S.joint[j].map(p => S.shares[p] || 0));
const tenth = x => Math.round(x * 10) / 10;
// Split t (%) among parties in proportion to weights w (equally if all are zero), in tenths, so that the parts add up to t exactly
function apportion(t, w) {
  const tenths = Math.max(0, Math.round(t * 10)), W = sum(w);
  const raw = w.map(x => W > 0 ? tenths * x / W : tenths / w.length), fl = raw.map(Math.floor);
  raw.map((x, i) => [x - fl[i], i]).sort((a, b) => b[0] - a[0]).slice(0, tenths - sum(fl)).forEach(([, i]) => { fl[i]++; });
  return fl.map(x => x / 10);
}
// The proportions a drag started from, kept while it lasts (the shares are still the ones it set), so that pushing one party to the
// whole list and back, or the list to zero and back, restores the others' proportions
let JDRAG = null;
function dragWeights(key, ps) {
  const cur = ps.map(p => S.shares[p] || 0);
  return JDRAG && JDRAG.key === key && JDRAG.set.length === cur.length && JDRAG.set.every((x, i) => Math.abs(x - cur[i]) < 1e-9) ? JDRAG.w : cur;
}
const keepDrag = (key, ps, w) => { JDRAG = { key, w, set: ps.map(p => S.shares[p] || 0) }; };
function syncShare(p, from) {
  const row = $("row-" + slug(p)); if (!row) return;
  if (from !== "range") row.querySelector("input[type=range]").value = S.shares[p];
  if (from !== "number") row.querySelector("input[type=number]").value = S.shares[p].toFixed(p === "MN" ? 2 : 1);
}
function syncList(j, from) {
  const row = $("jl-" + j); if (!row || !S.joint[j]) return;
  const t = listTotal(j);
  if (from !== "range") row.querySelector("input[type=range]").value = t;
  if (from !== "number") row.querySelector("input[type=number]").value = t.toFixed(1);
}
function setShare(p, v, from) {
  if (!isFinite(v)) return;
  const j = jointIndex(p);
  if (j >= 0 && S.joint[j].length > 1) {
    // On a joint list the list keeps its total: the list's other parties take up the difference in proportion to their shares
    const others = S.joint[j].filter(q => q !== p), t = tenth(listTotal(j)), key = "p" + j + "|" + p;
    const own = tenth(clamp(v, 0, t)), w = dragWeights(key, others);
    S.shares[p] = own; S.hand.add(p); others.forEach(q => S.hand.add(q));
    apportion(t - own, w).forEach((x, i) => { S.shares[others[i]] = x; });
    keepDrag(key, others, w);
    others.forEach(q => syncShare(q));
    syncShare(p, Math.abs(own - v) > 1e-9 ? null : from);
  } else {
    S.shares[p] = clamp(v, 0, 100); S.hand.add(p);
    syncShare(p, from);
  }
  S.shares.Other = 100 - sum(SP.map(q => S.shares[q]));
  updateSoon();
}
// Moving a joint list's total scales its parties in proportion
function setListShare(j, v, from) {
  if (!isFinite(v) || !S.joint[j]) return;
  const mem = S.joint[j], key = "l" + j, w = dragWeights(key, mem), t = tenth(clamp(v, 0, 100));
  apportion(t, w).forEach((x, i) => { S.shares[mem[i]] = x; S.hand.add(mem[i]); });
  keepDrag(key, mem, w);
  mem.forEach(q => syncShare(q));
  syncList(j, Math.abs(t - v) > 1e-9 ? null : from);
  S.shares.Other = 100 - sum(SP.map(q => S.shares[q]));
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
  $("dl").addEventListener("change", e => { S.dateLine = e.target.checked; renderTrends(); });
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
    // Otherwise a click inside the plot sets the week the Seats view will show (nearest week), without leaving Trends
    if (TERM || mx < TR.m.l || mx > TR.W - TR.m.r || my < TR.m.t || my > TR.H - TR.m.b) return;
    const wi = nearestWeek(TR.a + (mx - TR.m.l) / (TR.W - TR.m.l - TR.m.r) * (TR.b - TR.a));
    setWeek(wi); saveHash();
    toast(`Seats, Geography and Scenarios now open at the week of ${dlong(WEEKS[wi].date)}`);
  });
  $("trendlegend").addEventListener("mouseover", e => { const s = e.target.closest("[data-p]"); const p = s ? s.dataset.p : null; if (p !== S.hoverParty) { S.hoverParty = p; renderTrends(); } });
  $("trendlegend").addEventListener("mouseleave", () => { S.hoverParty = null; renderTrends(); });
  $("trendlegend").addEventListener("click", e => {
    if (e.target.closest("#pollclear")) S.pollSel = null;
    else if (e.target.closest("#partyclear")) S.parties.clear();
    else { const b = e.target.closest("button[data-p]"); if (!b) return; const p = b.dataset.p; S.parties.has(p) ? S.parties.delete(p) : S.parties.add(p); }
    renderTrends(); saveHash();
  });
  // by group
  $("gdim").addEventListener("click", e => { const b = e.target.closest("button[data-k]"); if (!b) return; S.gDim = b.dataset.k; renderGroups(); saveHash(); });
  $("glist").addEventListener("click", e => { const b = e.target.closest("button[data-k]"); if (!b) return; S.gList = b.dataset.k; renderGroups(); saveHash(); });
  $("gpts").addEventListener("change", e => { S.gPts = e.target.checked; renderGroups(); saveHash(); });
  $("groups").addEventListener("mousemove", groupHover);
  $("groups").addEventListener("mouseleave", () => { $("grouptip").hidden = true; const g = $("grouphover"); if (g) g.innerHTML = ""; });
  $("panel3").addEventListener("click", e => { if (e.target.id === "copylink") { const u = shareUrl(); (navigator.clipboard ? navigator.clipboard.writeText(u) : Promise.reject()).then(() => toast("Link copied"), () => window.prompt("Copy this link:", u)); } });
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
  $("map").addEventListener("click", ev => { const g = ev.target.closest("g[data-c]"); if (!g) return; const ok = C[+g.dataset.c].okreg; S.constSel = S.constSel === ok ? null : ok; renderView(); saveHash(); });
  $("constcard").addEventListener("click", e => { if (e.target.id === "constclose") { S.constSel = null; renderView(); saveHash(); } });
  // pollsters
  $("house").addEventListener("click", e => { const t = e.target.closest("[data-org]"); if (!t) return; const o = t.dataset.org; if (e.shiftKey) { S.houses.has(o) ? S.houses.delete(o) : S.houses.add(o); } else if (S.houses.size === 1 && S.houses.has(o)) S.houses.clear(); else { S.houses.clear(); S.houses.add(o); } renderChips(); renderHouse(); saveHash(); toast(S.houses.has(o) ? `${o} highlighted on the trend chart` : `${o} no longer highlighted`); });
  // panel (delegated: its contents are rebuilt)
  const panel = $("panel");
  panel.addEventListener("input", e => {
    const t = e.target;
    if (t.id && t.id.startsWith("a_") && JP) {
      if (!S.adv) { const b = respParams(); S.adv = { conv: b.conv, shift: b.shift, split: b.split, abst: b.abst, splitsOn: b.splitsOn.slice() }; if (S.resp === "none") S.resp = "medium"; }
      const k = t.id.slice(2);
      if (k === "conv") S.adv.conv = +t.value / 100; else if (k === "shift") S.adv.shift = +t.value; else if (k === "abst") S.adv.abst = +t.value / 100;
      else if (k === "split") S.adv.split = +t.value / 100; else if (k.startsWith("sp")) S.adv.splitsOn[+k.slice(2)] = t.checked;
      updateRespUI(); updateSoon(); return;
    }
    if (t.dataset.j != null && (t.type === "range" || t.type === "number")) return setListShare(+t.dataset.j, parseFloat(t.value), t.type);
    if (t.dataset.p && (t.type === "range" || t.type === "number")) setShare(t.dataset.p, parseFloat(t.value), t.type);
  });
  panel.addEventListener("click", e => {
    const t = e.target;
    if (t.id === "wprev") return setWeek(S.w - 1);
    if (t.id === "wnext") return setWeek(S.w + 1);
    if (t.id === "reset") return backToModel();
    const rb = t.closest("#resp button[data-resp]");
    if (rb) { if (rb.dataset.resp !== "custom" && !rb.disabled) { S.resp = rb.dataset.resp; S.adv = null; update(); } return; }
    if (t.id === "jadd") { if (S.picker.size >= 2) { S.joint.push(sortParties([...S.picker])); S.picker.clear(); S.pinGov = null; update(); } return; }
    if (t.dataset.rm != null) { S.joint.splice(+t.dataset.rm, 1); S.pinGov = null; return update(); }
    const pick = t.closest("#jpick button[data-p]");
    if (pick) { const p = pick.dataset.p; S.picker.has(p) ? S.picker.delete(p) : S.picker.add(p); pick.setAttribute("aria-pressed", S.picker.has(p)); updatePanel(); return; }
        const gr = t.closest(".grow-row");
    if (gr) { S.pinGov = S.pinGov === gr.dataset.key ? null : gr.dataset.key; renderGov(); renderView(); saveHash(); return; }
    if (t.id === "copylink") { const u = shareUrl(); (navigator.clipboard ? navigator.clipboard.writeText(u) : Promise.reject()).then(() => toast("Link copied"), () => window.prompt("Copy this link:", u)); return; }
  });
  panel.addEventListener("toggle", e => { if (e.target.id === "adv") S.advOpen = e.target.open; }, true);
  panel.addEventListener("keydown", e => { const gr = e.target.closest(".grow-row"); if (gr && (e.key === "Enter" || e.key === " ")) { e.preventDefault(); gr.click(); } });
  panel.addEventListener("mouseover", e => {
    const gr = e.target.closest(".grow-row"), row = e.target.closest(".prow");
    const key = gr ? gr.dataset.key : null, p = row ? row.dataset.p : null;
    let dirty = false;
    if (key !== S.focusGov) { S.focusGov = key; dirty = true; }
    if (p !== S.hoverParty) { S.hoverParty = p; dirty = true; }
    if (dirty && HEMI_VIEWS.includes(S.view)) renderHemi();
    if (dirty && S.view === "trends" && !gr) renderTrends();
    if (dirty) { [...document.querySelectorAll(".grow-row")].forEach(g => g.classList.toggle("on", g.dataset.key === (S.pinGov || S.focusGov))); }
  });
  panel.addEventListener("mouseleave", () => { S.focusGov = null; S.hoverParty = null; if (HEMI_VIEWS.includes(S.view)) renderHemi(); else if (S.view === "trends") renderTrends(); renderGov(); });
  window.addEventListener("resize", debounce(renderView, 150));
}

// ---- changes by group (Scenarios: Groups) and Who votes ---------------------------------------
// cells.json (Polls/current term/R/export_cells.R, from Voting behaviour's stages 81-82) holds the 2023 electorate as 280 cells of age, sex, education and place of
// residence, each with a chance of voting and a vote, raked to PKW's result and Ipsos's exit poll, and the polls-by-group
// starting point for one week. engine.js (Polls/web/engine.js, copied here by sync-analyses.py) rakes the
// cells to each change. The national shares that come out become the scenario's shares; everything after them (the week's
// draws, joint lists and the voters' response, seats) is the tracker's own, as for any scenario.
let CELLS = null, cellsState = "idle", LEVRES = null;
const GDIMS = ["age", "sex", "education", "residence"], GDIM_NAME = { age: "Age", sex: "Sex", education: "Education", residence: "Residence" };
const ENGINE = () => window.E27Engine || null;
function loadCells() {
  if (cellsState === "ready" || cellsState === "missing") return Promise.resolve();
  if (cellsState === "loading") return loadCells.p;
  cellsState = "loading";
  return (loadCells.p = fetch("cells.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); })
    .then(d => { CELLS = d; cellsState = ENGINE() ? "ready" : "missing"; })
    .catch(() => { cellsState = "missing"; }));
}
// The week the cells' starting point is pinned to: its index in WEEKS, or -1
const cellsWeek = () => (CELLS ? WEEKS.findIndex(w => iso(w.date) === CELLS.tracker.week) : -1);
// A change by group in the link: age, sex, education, residence as a0.s1 (or "all"), then what changes, by how many points, and from whom
const leverKey = lv => [GDIMS.filter(d => lv.group[d] != null).map(d => d[0] + lv.group[d]).join(".") || "all", lv.measure, lv.change, lv.from || ""].join("~");
function parseLever(s) {
  const [g, measure, change, from] = s.split("~"), ch = parseFloat(change);
  if (!measure || !isFinite(ch)) return null;
  const group = {};
  if (g && g !== "all") g.split(".").forEach(t => { const d = GDIMS.find(x => x[0] === t[0]), i = parseInt(t.slice(1), 10); if (d && i >= 0) group[d] = i; });
  return { group, measure, change: clamp(ch, -40, 40), from: from || null };
}
// A group in words: "Men aged 18-29, higher education, in a village"
function groupLabel(g) {
  const L = d => (g[d] == null ? null : CELLS ? CELLS.dims[d].labels[g[d]] : GDIM_NAME[d] + " " + (g[d] + 1));
  const sex = L("sex"), age = L("age"), edu = L("education"), res = L("residence");
  if (!sex && !age && !edu && !res) return "Everyone";
  let s = (sex || (age ? "People" : "")) + (age ? " aged " + age : "");
  if (edu) s += (s ? ", " : "") + (s ? edu.toLowerCase() : edu) + " education";
  if (res) s += (s ? ", in a " + res.toLowerCase() : res);
  return s;
}
const leverText = lv => `${groupLabel(lv.group)}: ${lv.measure === "turnout" ? "turnout" : lv.measure} ${signed(lv.change, 1)} points${lv.from ? " from " + lv.from : ""}`;
const LEVER_EXAMPLES = [
  ["Young voters stay at home", [{ group: { age: 0 }, measure: "turnout", change: -10, from: null }]],
  ["Young men move to KKP", [{ group: { age: 0, sex: 0 }, measure: "KKP", change: 8, from: "KO" }]],
  ["Graduates turn out more", [{ group: { education: 3 }, measure: "turnout", change: 4, from: null }]],
  ["Over-60s cool on PiS", [{ group: { age: 4 }, measure: "PiS", change: -5, from: null }]]
];
// The engine's national shares as the tracker's: per cent, MN at its usual figure, Other the remainder
function leverShares(res) {
  const s = defaults(S.w), sh = res.national.shares;
  SP.forEach(p => { if (p !== "MN" && sh[p] != null) s[p] = Math.round(10000 * sh[p]) / 100; });
  s.Other = Math.round((100 - sum(SP.map(p => s[p]))) * 100) / 100;
  return s;
}
function leverRun() {
  const E = ENGINE(); if (!E || !CELLS) return null;
  return E.scenario(CELLS, { base: S.base === "polls" ? "polls" : "groups", levers: S.levers, hold: S.hold });
}
// The scenario's shares are what the changes by group make of the starting point, except for the parties set by hand
function applyLevers() {
  if (!S.levers.length) {
    LEVRES = null;
    const d = defaults(S.w); SP.forEach(p => { if (!S.hand.has(p)) S.shares[p] = d[p]; });
    S.shares.Other = 100 - sum(SP.map(p => S.shares[p])); return;
  }
  if (cellsState !== "ready" || cellsWeek() !== S.w) return;
  LEVRES = leverRun(); if (!LEVRES) return;
  const b = leverShares(LEVRES);
  SP.forEach(p => { if (!S.hand.has(p)) S.shares[p] = b[p]; });
  S.shares.Other = 100 - sum(SP.map(p => S.shares[p]));
}
const leversChanged = () => { applyLevers(); update(); };
function renderLevers() {
  const box = $("panel5");
  if (cellsState !== "ready") {
    box.innerHTML = `<div class="card"><span class="label">Changes by group</span><p class="help" style="margin:0">${cellsState === "missing" ? "The data for changes by group could not be loaded." : "Loading the data for changes by group…"}</p></div>`;
    if (cellsState !== "missing") loadCells().then(() => { if (S.view === "levers") leversChanged(); });
    return;
  }
  const cw = cellsWeek();
  if (cw !== S.w) {
    box.innerHTML = `<div class="card"><span class="label">Changes by group</span><p class="help" style="margin:0">Changes by group start from the electorate as it stood in the week of ${esc(dlong(fromIso(CELLS.tracker.week)))}, when its groups were last fitted to the polls.${cw >= 0 ? "" : " That week is not in the tracker's data yet."}</p>${cw >= 0 ? `<div><button type="button" class="btn1" id="lvweek">Go to that week</button></div>` : ""}</div>`;
    return;
  }
  const E = ENGINE(), res = LEVRES || leverRun(), parties = CELLS.parties.map(p => p.id).filter(p => p !== "Other");
  const opt = (v, t, on) => `<option value="${esc(String(v))}"${on ? " selected" : ""}>${esc(t)}</option>`;
  const tot = res ? E.summarise(res.base).size : 1;
  const cards = S.levers.map((lv, j) => {
    const sels = GDIMS.map(d => `<select data-lv="${j}" data-f="${d}" aria-label="${GDIM_NAME[d]}">${opt("", GDIM_NAME[d] + ": all", lv.group[d] == null)}${CELLS.dims[d].labels.map((l, i) => opt(i, l, lv.group[d] === i)).join("")}</select>`).join("");
    const what = `<select data-lv="${j}" data-f="measure" aria-label="What changes">${opt("turnout", "Turnout", lv.measure === "turnout")}${parties.map(p => opt(p, p, lv.measure === p)).join("")}</select>`;
    const num = `<input type="number" step="0.5" min="-40" max="40" value="${lv.change}" data-lv="${j}" data-f="change" aria-label="Change in percentage points"><span class="help">points</span>`;
    const from = lv.measure === "turnout" ? "" : `<div class="row"><select data-lv="${j}" data-f="from" aria-label="Taken from">${opt("", "from all the others, in proportion", !lv.from)}${parties.filter(p => p !== lv.measure).map(p => opt(p, "from " + p, lv.from === p)).join("")}</select></div>`;
    let now = "";
    if (res) {
      const idx = E.members(res.cells, lv.group), a = E.summarise(res.base, idx), b = E.summarise(res.cells, idx), val = s => (lv.measure === "turnout" ? s.turnout : s.shares[lv.measure]);
      now = `<div class="now">${esc(groupLabel(lv.group))} (${pct(100 * a.size / tot)} of the electorate): ${lv.measure === "turnout" ? "turnout" : esc(lv.measure) + " among its voters"} <b>${pct(100 * val(a))}</b> → <b>${pct(100 * val(b))}</b></div>`;
    }
    return `<div class="lever"><button type="button" class="x" data-rmlv="${j}" aria-label="Remove this change">×</button><div class="g">${sels}</div><div class="row">${what}${num}</div>${from}${now}</div>`;
  }).join("");
  const warn = res && res.info.err > 1e-4 ? `<p class="notice">These changes cannot all hold at once (they pull the same voters in opposite directions); the result is the closest compromise.</p>` : "";
  const hand = SP.filter(p => S.hand.has(p));
  box.innerHTML = `<div class="card">
    <div class="phead"><h2>${dlong(WEEKS[S.w].date)}</h2></div>
    <span class="label">Starting point</span>
    <div class="basepick">
      <label><input type="radio" name="lvbase" value="groups"${S.base !== "polls" ? " checked" : ""}${CELLS.groupTrends ? "" : " disabled"}> <span><b>Polls now, groups as polled</b><br><span class="help">This week's estimates, each group moved as Opinia24's and CBOS's tables show it has moved since December 2023, and its turnout as its stated intention to vote has moved (national turnout kept at 2023's).</span></span></label>
      <label><input type="radio" name="lvbase" value="polls"${S.base === "polls" ? " checked" : ""}> <span><b>Polls now, uniform swing</b><br><span class="help">This week's estimates with every group moved alike since the 2023 election, at 2023 turnout.</span></span></label>
    </div>
  </div>
  <div class="card">
    <span class="label">Changes by group</span>
    ${S.levers.length ? cards : `<p class="help" style="margin:0">No changes yet. Add one, or start from an example.</p>`}
    ${warn}
    <div><button type="button" class="btn2" id="lvadd">Add a change</button>${S.levers.length ? ` <button type="button" class="linkbtn" id="lvclear" style="margin-left:8px">Remove all</button>` : ""}</div>
    <span class="help">Examples</span>
    <div class="chips">${LEVER_EXAMPLES.map(([l], i) => `<button type="button" class="chip" data-ex="${i}">${esc(l)}</button>`).join("")}</div>
    <label class="switch"><input type="checkbox" id="lvhold"${S.hold ? " checked" : ""}> Hold the national result</label>
    ${hand.length ? `<p class="help" style="margin:0">Set by hand under Parties and lists, and left where they were set: ${esc(hand.join(", "))}.</p>` : ""}
    <p class="help" style="margin:0">A change sets turnout, or one party's share of the voters, in a group or an intersection of groups (men aged 18–29 in towns, say). The cells are raked until every change is met, and the national result follows from the groups' sizes; with "Hold the national result" the groups move against each other and the national shares stay at the starting point. Seats then come from the tracker's simulation, as for any scenario. The regional profiles do not follow the groups yet, so a change among the young moves every constituency alike.</p>
  </div>
  <div class="card soft"><span class="label">Share</span><div class="actions"><button type="button" class="btn2" id="copylink">Copy link to this view</button></div></div>`;
}
// Who votes: everyone in a group votes, or only the group does, from the Groups starting point
const WICACHE = new Map();
function pointSeats(shares) {
  const sh = new Float64Array(SP.length); SP.forEach((p, i) => { sh[i] = p === "MN" ? R.mnNational2023 / 100 : (shares[p] || 0); });
  const ents = makeEnts([]), nat = new Int16Array(ents.length); allocate(sh, ents, null, 0, nat, null);
  const out = {}; ents.forEach((e, k) => { out[e.name] = nat[k]; }); return out;
}
function wiRun(group) {
  const key = [S.base, S.wiMode, JSON.stringify(group)].join("|");
  if (WICACHE.has(key)) return WICACHE.get(key);
  const E = ENGINE(), any = Object.keys(group).length > 0, base = S.base === "polls" ? "polls" : "groups";
  const levers = S.wiMode === "all" ? [{ group, measure: "turnout", to: 0.999 }] : any ? [{ group, invert: true, measure: "turnout", to: 0.001 }] : [];
  const res = E.scenario(CELLS, { base, levers }), out = { national: res.national, seats: pointSeats(res.national.shares) };
  WICACHE.set(key, out); return out;
}
function renderWhoVotes() {
  const ctl = $("wictl");
  if (cellsState !== "ready") {
    ctl.innerHTML = ""; $("witable").innerHTML = ""; $("wifoot").textContent = "";
    $("winote").textContent = cellsState === "missing" ? "The data for Who votes could not be loaded." : "Loading…";
    if (cellsState !== "missing") loadCells().then(() => { if (S.view === "whovotes") renderWhoVotes(); });
    return;
  }
  const seg = (id, opts, cur) => `<div class="seg small" id="${id}" role="group">${opts.map(([k, l]) => `<button type="button" data-k="${k}" aria-pressed="${k === cur}">${l}</button>`).join("")}</div>`;
  const sel = d => `<select data-wg="${d}" aria-label="${GDIM_NAME[d]}"><option value="">${GDIM_NAME[d]}: all</option>${CELLS.dims[d].labels.map((l, i) => `<option value="${i}"${S.wiGroup[d] === i ? " selected" : ""}>${esc(l)}</option>`).join("")}</select>`;
  ctl.innerHTML = seg("wimode", [["all", "Everyone in the group votes"], ["only", "Only the group votes"]], S.wiMode) +
    seg("wimetric", [["vote", "Vote share"], ["seats", "Seats"]], S.wiMetric) +
    `<span class="fsel" style="flex-direction:row;align-items:center;gap:6px">Your own group: ${GDIMS.map(sel).join("")}</span>`;
  const base = S.base === "polls" ? "polls" : "groups", wk = dlong(fromIso(CELLS.tracker.week));
  $("winote").textContent = (S.wiMode === "all" ? "Turnout in the group is raised to 100%; everyone else votes as in the starting point. " : "Everyone outside the group stays at home; the group votes at its starting-point turnout. ") +
    `The starting point is ${base === "polls" ? "the polls of the week of " + wk + " with every group moved alike since 2023" : "the polls of the week of " + wk + " with each group as the pollsters' tables have moved it"} (chosen on the Groups view).`;
  const E = ENGINE(), ref = E.scenario(CELLS, { base, levers: [] }), refSeats = pointSeats(ref.national.shares), seatsOn = S.wiMetric === "seats";
  const cols = CELLS.parties.map(p => p.id).filter(p => p !== "Other"), all = E.summarise(ref.cells).size;
  const GOV = ["KO", "Lewica", "Razem", "Polska 2050", "PSL"], RIGHT = ["PiS", "R+", "Konfederacja", "KKP"], bloc = (s, ids) => sum(ids.map(id => s[id] || 0));
  const val = (N, seats, p) => (seatsOn ? seats[p] || 0 : 100 * N.shares[p]);
  const fmt = x => (seatsOn ? String(x) : x.toFixed(1));
  const dlt = (x, y) => (seatsOn ? signed(x - y) : signed(x - y, 1));
  const row = (label, N, seats, ref0, size) => {
    const g = bloc(seats, GOV), r = bloc(seats, RIGHT);
    return `<tr${ref0 ? ' class="ref"' : ""}><td>${esc(label)}</td><td>${size != null ? pct(100 * size) : ""}</td><td>${pct(100 * N.turnout)}</td><td class="${g >= R.seatsMajority ? "yes" : ""}">${g}</td><td class="${r >= R.seatsMajority ? "yes" : ""}">${r}</td>` +
      cols.map(p => `<td>${fmt(val(N, seats, p))}${ref0 ? "" : `<span class="chg">${dlt(val(N, seats, p), val(ref.national, refSeats, p))}</span>`}</td>`).join("") + "</tr>";
  };
  const groups = [["Everyone", {}], ["Men", { sex: 0 }], ["Women", { sex: 1 }]]
    .concat(CELLS.dims.age.labels.map((l, i) => ["Aged " + l, { age: i }]))
    .concat(CELLS.dims.education.labels.map((l, i) => [l, { education: i }]))
    .concat(CELLS.dims.residence.labels.map((l, i) => [l, { residence: i }]));
  const custom = Object.keys(S.wiGroup).length ? [["Your group: " + groupLabel(S.wiGroup), S.wiGroup]] : [];
  let t = `<tr><th>Group</th><th>Share of electorate</th><th>Turnout</th><th>Governing parties' seats</th><th>Right's seats</th>${cols.map(p => `<th><span class="dot" style="background:${col(p)};margin-right:4px"></span>${esc(short(p))}</th>`).join("")}</tr>`;
  t += row("Starting point", ref.national, refSeats, true, null);
  custom.concat(S.wiMode === "all" ? groups : groups.slice(1)).forEach(([lab, g]) => {
    const size = E.summarise(ref.cells, E.members(ref.cells, g)).size / all, r = wiRun(g);
    t += row(lab === "Everyone" ? "Everyone (full turnout)" : lab, r.national, r.seats, false, size);
  });
  $("witable").innerHTML = t;
  $("wifoot").textContent = `Dark red: a bloc with ${R.seatsMajority} seats or more (governing parties: ${GOV.join(", ")}; right: ${RIGHT.join(", ")}), from a single allocation by the tracker's rules, without joint lists. The figures under each number are the change from the starting point, in ${seatsOn ? "seats" : "percentage points"}. The extra voters are assumed to vote like the voters already in their cell (same age, sex, education and place of residence); marginal voters often differ, so read this as an upper bound on what composition alone can do.`;
}

// ---- every seat (Seats: Every seat) ---------------------------------------------------------------
// A constituency's seats are its m most probable list-seats, a list's k-th seat being won in the share of simulations in which it
// wins at least k there. When the projected list loses a seat in a simulation, the seat goes to the list that wins a seat beyond its
// projection there (the least secure lost seat matched to the likeliest contender), so each seat's chances add up to 100%.
// Ported from Polls/web/engine.js (seatByConstituency), which Polls/current term/js/test_engine.js checks against the tracker.
const ESCACHE = new WeakMap();
const CATLAB = { certain: "Certain", probable: "Probable", possible: "Possible", "toss-up": "Toss-up" };
function seatByConstituency(sim) {
  if (ESCACHE.has(sim)) return ESCACHE.get(sim);
  const { n, ents, cons } = sim, E = ents.length;
  const out = C.map((c, ci) => {
    const mag = c.magnitude, lists = [], all = [];
    ents.forEach((e, k) => {
      const cnt = new Int32Array(mag + 1);
      for (let d = 0; d < n; d++) cnt[Math.min(cons[d * NC * E + ci * E + k], mag)]++;
      const p = []; let ge = 0;
      for (let j = mag; j >= 1; j--) { ge += cnt[j]; p[j - 1] = ge / n; }
      while (p.length && p[p.length - 1] === 0) p.pop();
      if (!p.length) return;
      lists.push({ id: e.name, k, p });
      p.forEach((pp, j) => all.push({ id: e.name, k, j: j + 1, p: pp }));
    });
    const tot = {}; lists.forEach(l => { tot[l.id] = sum(l.p); });
    all.sort((a, b) => b.p - a.p || tot[b.id] - tot[a.id] || a.k - b.k || a.j - b.j);
    const projected = all.slice(0, mag), contenders = all.slice(mag);
    projected.forEach((s, i) => { s.rank = i + 1; s.cat = seatCat(s.p); s.tally = {}; });
    const proj = {}; lists.forEach(l => { proj[l.k] = projected.filter(s => s.k === l.k).length; });
    const crank = {}; contenders.forEach((x, i) => { crank[x.k + ":" + x.j] = i; });
    for (let d = 0; d < n; d++) {
      const lost = [], gained = [];
      for (let i = mag - 1; i >= 0; i--) { const s = projected[i]; if (cons[d * NC * E + ci * E + s.k] < s.j) lost.push(s); }
      if (!lost.length) continue;
      lists.forEach(l => { const s = cons[d * NC * E + ci * E + l.k]; for (let j = proj[l.k] + 1; j <= s; j++) gained.push({ id: l.id, r: crank[l.k + ":" + j] }); });
      gained.sort((a, b) => a.r - b.r);
      lost.forEach((s, i) => { const g = gained[i]; if (g) s.tally[g.id] = (s.tally[g.id] || 0) + 1; });
    }
    projected.forEach(s => {
      s.take = [{ id: s.id, p: s.p }].concat(Object.keys(s.tally).map(id => ({ id, p: s.tally[id] / n }))).sort((a, b) => b.p - a.p);
      s.next = s.take.find(x => x.id !== s.id) || null;
      delete s.tally;
    });
    return { ci, okreg: c.okreg, name: c.name, magnitude: mag, projected };
  });
  ESCACHE.set(sim, out); return out;
}
const entColour = id => { const e = CUR && CUR.ents.find(x => x.name === id); return e ? e.colour : col(id); };
// A link to a constituency on the Geography page, keeping the week and the scenario
const geoLink = okreg => { const q = new URLSearchParams(carryString()); q.set("c", okreg); return sectionUrl("geography") + "#" + q; };
// ---- the Forecast page's Sliders view (Polls/current term/docs/forecast_plan.md, section 2.3) ----------------------
// Changes by group in turnout, economic or socio-cultural position, applied to the election-day draws (engine.js
// attitudeFactors on Voting behaviour's 2023 model); the Sejm, Every seat and Senate views follow them.
const FL_DIMS = ["age", "sex", "education", "residence"];
const FL_M = { turnout: ["Turnout", "points", -20, 20, 1], econ: ["Economic position", "points to the right (market-liberal)", -3, 3, 0.5], soc: ["Socio-cultural position", "points more conservative", -3, 3, 0.5] };
function flGroupText(g) {
  if (!CELLS) return "";
  const bits = FL_DIMS.filter(k => g[k] != null).map(k => CELLS.dims[k].labels[g[k]].toLowerCase());
  return bits.length ? bits.join(", ") : "everyone";
}
function flText(x) {
  const m = FL_M[x.m];
  const pts = Math.abs(x.c) === 1 ? "point" : "points";
  return x.m === "turnout" ? `turnout ${signed(x.c)} ${pts} among ${flGroupText(x.g)}` : `${flGroupText(x.g)} ${Math.abs(x.c)} ${pts} ${x.c > 0 ? (x.m === "econ" ? "to the right economically" : "more conservative") : (x.m === "econ" ? "to the left economically" : "more progressive")}`;
}
function fcBaseSim() {   // the forecast without sliders, for the before-and-after table
  if (FC_SIM0) return FC_SIM0;
  const keep = { fl: S.fl, w: FCW, k: FCW_KEY, sim: FC_SIM };
  S.fl = []; FCW = null; FC_SIM0 = simulate(S.w, null); const w0 = FCW;
  S.fl = keep.fl; FCW = keep.w; FCW_KEY = keep.k; FC_SIM = keep.sim;
  FC_SIM0.draws = w0;
  return FC_SIM0;
}
function renderSliders() {
  const sum = $("flsum");
  if (S.fh !== "election") { S.fh = "election"; loadForecast(); }   // the sliders move the election-day forecast (decision 6)
  if (!FC || fcState !== "ready" || !FC.attitudes) { ["flctl", "flres", "flmaj", "flnote"].forEach(id => { $(id).innerHTML = ""; }); sum.textContent = fcState === "missing" ? "The forecast could not be loaded." : "Loading the forecast…"; loadForecast(); return; }
  if (cellsState !== "ready") { sum.textContent = cellsState === "missing" ? "The electorate cells could not be loaded, so the sliders are not available." : "Loading the electorate cells…"; if (cellsState === "idle") loadCells().then(() => { FCW = null; update(); }); return; }
  sum.innerHTML = `What follows for the election on ${dlong(fromIso(FC.electionDate))} if groups of voters turn out more or less, or move to the left or right, economically or on social and cultural questions. Positions are on 0–10 scales from the 2023 Polish National Election Study.`;
  const opt = (k, v) => `<option value=""${v == null ? " selected" : ""}>${k === "age" ? "All ages" : k === "sex" ? "Both sexes" : k === "education" ? "All education" : "Everywhere"}</option>` + CELLS.dims[k].labels.map((l, i) => `<option value="${i}"${v === i ? " selected" : ""}>${esc(l)}</option>`).join("");
  $("flctl").innerHTML = S.fl.map((x, i) => { const m = FL_M[x.m];
    return `<div class="card soft" style="display:flex;flex-direction:row;flex-wrap:wrap;justify-content:flex-start;gap:8px 12px;align-items:flex-end;margin:0 0 8px">${FL_DIMS.map(k => `<label class="fsel">${k[0].toUpperCase() + k.slice(1)}<select data-fl="${i}" data-k="${k}">${opt(k, x.g[k])}</select></label>`).join("")}
      <label class="fsel">What changes<select data-fl="${i}" data-k="m">${Object.entries(FL_M).map(([v, d]) => `<option value="${v}"${x.m === v ? " selected" : ""}>${d[0]}</option>`).join("")}</select></label>
      <label class="fsel" style="min-width:220px">By <b>${x.c > 0 ? "+" : ""}${x.c}</b> ${esc(m[1])}<input type="range" data-fl="${i}" data-k="c" min="${m[2]}" max="${m[3]}" step="${m[4]}" value="${x.c}"></label>
      <button type="button" class="btn2 sm" data-rmfl="${i}">Remove</button></div>`; }).join("") +
    `<div class="btnrow"><button type="button" class="btn2" id="fladd">Add a change</button>${S.fl.length ? ` <button type="button" class="btn2" id="flclear">Clear all</button>` : ""}</div>`;
  const s0 = fcBaseSim(); fcDraws(); compute();
  const s1 = CUR.sim, w0 = s0.draws, w1 = fcDraws();
  if (!s1) { $("flres").innerHTML = ""; return; }
  const seatQ = (sim, k) => { const v = []; for (let d = 0; d < sim.n; d++) v.push(sim.nat[d * sim.ents.length + k]); v.sort((a, b) => a - b); return [Math.round(median(v)), Math.round(quant(v, 0.1)), Math.round(quant(v, 0.9))]; };
  const rows = s1.ents.map((e, k) => ({ e, k, i: PIDX[e.name], a: seatQ(s0, k), b: seatQ(s1, k) })).filter(r => r.a[2] > 0 || r.b[2] > 0 || (r.i != null && w1.med[r.i] > 0.01)).sort((x, y) => y.b[0] - x.b[0]);
  $("flres").innerHTML = `<tr><th>Party</th><th class="n">Vote now</th><th class="n">With the sliders</th><th class="n">Seats now</th><th class="n">With the sliders</th></tr>` +
    rows.map(r => `<tr><td><span class="dot" style="background:${col(r.e.name)};margin-right:6px"></span>${esc(r.e.name)}</td><td class="n muted">${r.i != null ? (100 * w0.med[r.i]).toFixed(1) : ""}</td><td class="n"><b>${r.i != null ? (100 * w1.med[r.i]).toFixed(1) : ""}</b>${r.i != null ? ` <span class="${cls(w1.med[r.i] - w0.med[r.i])}">${signed(100 * (w1.med[r.i] - w0.med[r.i]), 1)}</span>` : ""}</td><td class="n muted">${r.a[0]} <span style="font-size:11px">(${r.a[1]}–${r.a[2]})</span></td><td class="n"><b>${r.b[0]}</b> <span class="muted" style="font-size:11px">(${r.b[1]}–${r.b[2]})</span></td></tr>`).join("");
  const pr = (sim, ps) => { const ks = sim.ents.map((e, k) => (ps.includes(e.name) ? k : -1)).filter(k => k >= 0); let c = 0; for (let d = 0; d < sim.n; d++) { let t = 0; ks.forEach(k => { t += sim.nat[d * sim.ents.length + k]; }); if (t >= R.seatsMajority) c++; } return c / sim.n; };
  const gov = ["KO", "Polska 2050", "PSL", "Lewica", "Razem"], right = ["PiS", "R+", "Konfederacja", "KKP"];
  $("flmaj").innerHTML = `Chance of ${R.seatsMajority} seats in the Sejm: KO, Polska 2050, PSL, Lewica and Razem <b>${prob(pr(s1, gov))}</b> <span class="muted">(${prob(pr(s0, gov))} without the sliders)</span>; PiS, R+, Konfederacja and KKP <b>${prob(pr(s1, right))}</b> <span class="muted">(${prob(pr(s0, right))})</span>. The Sejm, Every seat and Senate views show the forecast with the sliders.`;
  $("flnote").innerHTML = `How it works: the 2023 Polish National Election Study gives each voter's economic position (0 = state-led, 10 = market-liberal) and socio-cultural position (0 = progressive, 10 = conservative), averaged over the questions asked that year. A model of the 2023 vote, not voting included, on those positions and on age, sex, education and place of residence says how each group's vote would differ if its positions differed; moving a group's positions by the amount set applies that difference to the group's 2023 vote, and the change in each party's national share is carried over to the forecast. Today's parties that split from a 2023 list (R+, KKP, Razem, PSL and Polska 2050) move with that list. The ranges include the model's own uncertainty. This is what follows if voters with those positions voted as such voters did in 2023, not a measured effect of opinion on the vote, and the positions are comparable within 2023 only. A check against the 2025 Polish National Election Study (vote intention, with the questions both surveys asked) found the economic effects unchanged, but a conservative shift worth less to PiS than in 2023 (about two thirds as much) and more to Konfederacja and KKP: as conservative voters have spread across the right, the slider probably overstates PiS's gains and understates theirs.`;
}
// ---- the Forecast page's Senate view (Polls/current term/docs/forecast_plan.md, section 3) -----------------------
// Every party stands alone unless put in a pact (decision 4); one generic independent stands everywhere. The
// simulation is engine.js senateSimulate() on forecast.json's election-day draws and constituency patterns.
let SN = null;   // { key, res }
const snParties = () => P.filter(p => FC && FC.senate && p in FC.senate.lineage);
function snGroups() {
  const ps = snParties(), pacts = { A: [], B: [], C: [] };
  ps.forEach(p => { if (S.sp[p]) pacts[S.sp[p]].push(p); });
  const groups = ["A", "B", "C"].map(k => pacts[k]).filter(g => g.length);
  ps.filter(p => !S.sp[p]).forEach(p => groups.push([p]));
  return groups;
}
const snLabel = g => g.map(short).join(" + ");
const snKey = g => [...g].sort().join("+");
function snColour(g) { if (g[0] === "Independent") return "#9a9a9a"; const fw = fcDraws(); return col(g.reduce((a, b) => (fw.med[PIDX[b]] > fw.med[PIDX[a]] ? b : a))); }
function snRun() {
  const groups = snGroups(), key = JSON.stringify([groups, S.sinc, flKey()]);
  if (SN && SN.key === key) return SN.res;
  const E = window.E27Engine; if (!E || !E.senateSimulate) return null;
  if (!FCSWING) FCSWING = E.forecastSwing(FC.profiles, P, C, COEFI, FC.nDraws, 780046);
  const fw = fcDraws(), K = fw.K, kOf = {}; FC.categories.forEach((c, k) => { kOf[c] = k; });
  const nat = (d, p) => fw.arr[d * K + kOf[p]];
  const cons = (d, c, p) => { const i = PIDX[p]; return COEFI[i][c] * FCSWING[(d * NC + c) * NP + i]; };
  const res = E.senateSimulate(FC.senate, groups, nat, cons, FC.nDraws, { incumbency: S.sinc });
  SN = { key, res };
  return res;
}
function renderSenate() {
  const sum = $("snsum");
  if (!FC || fcState !== "ready" || !FC.senate) {
    ["snctl", "snseats", "sncoal", "snfilt", "sntable", "snnote"].forEach(id => { $(id).innerHTML = ""; });
    sum.textContent = fcState === "missing" ? "The forecast could not be loaded." : fcState === "stale" ? "The forecast file is from another week than the polls; it will be right after the next run." : "Loading the forecast…";
    loadForecast(); return;
  }
  const ps = snParties();
  $("snctl").innerHTML = ps.map(p => `<label class="fsel">${esc(short(p))}<select data-sp="${esc(p)}">${[["", "Stands alone"], ["A", "Pact A"], ["B", "Pact B"], ["C", "Pact C"]].map(([v, t]) => `<option value="${v}"${(S.sp[p] || "") === v ? " selected" : ""}>${t}</option>`).join("")}</select></label>`).join("") +
    `<label style="display:inline-flex;align-items:center;gap:6px;font-size:13px;margin-left:8px"><input type="checkbox" id="sninc"${S.sinc ? " checked" : ""}> Sitting senators' parties get the incumbency effect</label>`;
  const res = snRun(); if (!res) { sum.textContent = "The Senate simulation is not available."; return; }
  const { groups, seats, n, G, wins } = res, maj = FC.senate.majority;
  const dist = k => { const v = []; for (let d = 0; d < n; d++) v.push(seats[d * G + k]); v.sort((a, b) => a - b); return v; };
  const rows = groups.map((g, k) => { const v = dist(k); let m = 0; v.forEach(x => { if (x >= maj) m++; }); return { g, k, key: snKey(g), med: Math.round(median(v)), lo: Math.round(quant(v, 0.1)), hi: Math.round(quant(v, 0.9)), p: m / n }; })
    .filter(r => r.hi > 0 || r.med > 0).sort((a, b) => b.med - a.med || b.hi - a.hi);
  const pacts = groups.filter(g => g.length > 1);
  sum.innerHTML = `Forecast for the Senate election on ${dlong(fromIso(FC.electionDate))}${FC.provisional ? " (provisional date)" : ""}: ${FC.nDraws.toLocaleString("en-GB")} simulated elections in 100 single-member districts, ${pacts.length ? "with " + pacts.map(snLabel).join("; ") + " fielding one candidate each" : "every party standing alone"}. A majority is ${maj} seats.`;
  $("snseats").innerHTML = `<tr><th>Candidates of</th><th class="n">Seats</th><th class="n">80%</th><th class="n">Chance of ${maj}+</th></tr>` +
    rows.map(r => `<tr><td><span class="dot" style="background:${snColour(r.g)};margin-right:6px"></span>${esc(r.g[0] === "Independent" ? "Independents" : snLabel(r.g))}</td><td class="n"><b>${r.med}</b></td><td class="n muted">${r.lo}–${r.hi}</td><td class="n">${prob(r.p)}</td></tr>`).join("");
  // combined majorities: tick groups
  [...S.scoal].forEach(k => { if (!rows.some(r => r.key === k)) S.scoal.delete(k); });
  const sel = rows.filter(r => S.scoal.has(r.key));
  let pm = 0; const tot = [];
  for (let d = 0; d < n; d++) { let t = 0; sel.forEach(r => { t += seats[d * G + r.k]; }); tot.push(t); if (t >= maj) pm++; }
  tot.sort((a, b) => a - b);
  $("sncoal").innerHTML = `<span class="label">Together</span><div style="display:flex;flex-wrap:wrap;gap:6px 14px;font-size:13px;margin:4px 0">` +
    rows.filter(r => r.g[0] !== "Independent").map(r => `<label style="display:inline-flex;align-items:center;gap:5px"><input type="checkbox" data-sc="${esc(r.key)}"${S.scoal.has(r.key) ? " checked" : ""}>${esc(snLabel(r.g))}</label>`).join("") + `</div>` +
    (sel.length ? `<p style="margin:2px 0;font-size:13.5px">${esc(sel.map(r => snLabel(r.g)).join(", "))}: <b>${Math.round(median(tot))}</b> seats (80%: ${Math.round(quant(tot, 0.1))}–${Math.round(quant(tot, 0.9))}), a chance of <b>${prob(pm / n)}</b> of ${maj} or more.</p>` : `<p class="help" style="margin:2px 0">Tick the candidates' parties to see their chance of a majority together.</p>`);
  $("snfilt").innerHTML = `<label class="fsel">Order<select id="sn-o"><option value="okreg"${S.snSort === "okreg" ? " selected" : ""}>By district</option><option value="doubt"${S.snSort === "doubt" ? " selected" : ""}>Least certain first</option></select></label>`;
  const D = FC.senate.districts;
  let list = D.map((dd, i) => { const order = [...wins[i].keys()].sort((a, b) => wins[i][b] - wins[i][a]); return { dd, f: order[0], pf: wins[i][order[0]], s: order[1], ps: wins[i][order[1]] }; });
  if (S.snSort === "doubt") list = list.slice().sort((a, b) => a.pf - b.pf);
  const incLab = { pact: "a pact independent", none: "an independent" };
  const gl = k => groups[k][0] === "Independent" ? "Independent" : snLabel(groups[k]);
  $("sntable").innerHTML = `<tr><th>District</th><th>Favourite</th><th class="n">Chance</th><th>Next most likely</th><th>Won in 2023 by</th></tr>` +
    list.map(r => `<tr><td>${r.dd.okreg}. ${esc(r.dd.name)}</td><td><span class="dot" style="background:${snColour(groups[r.f])};margin-right:6px"></span>${esc(gl(r.f))}</td><td class="n"><b>${prob(r.pf)}</b></td><td>${r.ps > 0 ? `<span class="dot" style="background:${snColour(groups[r.s])};margin-right:6px"></span>${esc(gl(r.s))} <span class="muted">${prob(r.ps)}</span>` : `<span class="muted">–</span>`}</td><td class="muted">${esc(r.dd.winner || "")}${r.dd.inc ? ` (${esc(incLab[r.dd.inc] || short(r.dd.inc))})` : ""}</td></tr>`).join("");
  const co = FC.senate.coef;
  $("snnote").innerHTML = `How it works: from the Senate elections of 2011–2023, a candidate's vote rises with the Sejm vote of the parties behind them (an elasticity of ${co.x.toFixed(2)}: Senate candidates draw votes across party lines), and sitting senators run about ${Math.round(100 * (Math.exp(co.incumbent) - 1))}% ahead. A pact's single candidate gets about what the pact parties' Sejm votes together predict. Each party's Sejm vote in a district is the forecast's election-day vote in its Sejm constituency, spread over the constituency's Senate districts as its party line's vote was in 2023. Each candidate also gets a random personal vote, and every candidate of one camp a shared one, sized so that past elections' seat totals fall within the ranges as often as they should. Strong local independents, who win a few seats at every election, cannot be foreseen and are underrepresented. Checked by predicting each of the four elections from the other three: 87–89 of 100 winners right in 2011, 2019 and 2023, 71 in 2015.`;
}
function renderEverySeat() {
  const sim = hemiSim(), f = $("esfilters");
  if (!sim) {
    f.innerHTML = ""; $("estable").innerHTML = ""; $("esnote").textContent = "";
    $("essum").textContent = drawsState === "missing" ? "The seat simulations could not be loaded, so the seats cannot be listed." : "Loading the seat simulations…";
    loadDraws(); return;
  }
  const sb = seatByConstituency(sim), all = sb.flatMap(k => k.projected.map(s => ({ k, s }))), listIds = [...new Set(all.map(r => r.s.id))];
  if (S.esL && !listIds.includes(S.esL)) S.esL = "";
  const sel = (id, label, opts, val) => `<label class="fsel">${label}<select id="${id}">${opts.map(([v, t]) => `<option value="${esc(v)}"${String(v) === String(val) ? " selected" : ""}>${esc(t)}</option>`).join("")}</select></label>`;
  f.innerHTML = sel("es-c", "Constituency", [["", "All constituencies"]].concat(sb.map(k => [String(k.okreg), `${k.okreg}. ${k.name}`])), S.esC) +
    sel("es-l", "List", [["", "All lists"]].concat(listIds.map(id => [id, id])), S.esL) +
    sel("es-k", "Certainty", [["all", "Every seat"], ["open", "Not certain"], ["play", "Possible or toss-up"], ["toss-up", "Toss-ups only"]], S.esCat) +
    sel("es-o", "Order", [["const", "By constituency"], ["doubt", "Least certain first"]], S.esSort) +
    `<span class="btnrow" style="margin-left:auto">${FORECAST ? `<button type="button" class="btn2 sm" id="toseats">← The Sejm</button> ` : ""}<button type="button" class="btn2 sm" id="escsv">CSV of all ${all.length} seats</button></span>`;
  const outside = s => !!s.next && s.next.id === S.esL && s.next.p >= SEAT_OUTSIDE;
  let rows = all.filter(r => (!S.esC || String(r.k.okreg) === S.esC) && (!S.esL || r.s.id === S.esL || outside(r.s)) &&
    (S.esCat === "all" || (S.esCat === "open" ? r.s.cat !== "certain" : S.esCat === "play" ? (r.s.cat === "possible" || r.s.cat === "toss-up") : r.s.cat === "toss-up")));
  if (S.esSort === "doubt") rows = rows.slice().sort((a, b) => a.s.p - b.s.p);
  const own = S.esL ? rows.filter(r => r.s.id === S.esL) : rows, other = rows.length - own.length;
  const count = L => { const c = { certain: 0, probable: 0, possible: 0, "toss-up": 0 }; L.forEach(r => { c[r.s.cat]++; }); return c; };
  const n = count(own), t = count(all), when = CUR.mode === "forecast" ? fcSubtitle() + "." : `${CUR.mode === "model" ? "Model estimates" : "Hypothetical scenario"} for the week of ${dlong(CUR.wk.date)}.`;
  $("essum").innerHTML = S.esL
    ? `<b>${esc(S.esL)}</b> is projected to win ${own.length} of the seats shown: ${n.certain} certain, ${n.probable} probable, ${n.possible} possible, ${n["toss-up"]} toss-up.${other ? ` ${other} more ${other === 1 ? "is an outside possibility" : "are outside possibilities"}: projected for another list, with ${esc(S.esL)} the next most likely to take ${other === 1 ? "it" : "them"} (${pc0(SEAT_OUTSIDE)}% or more).` : ""} ${when}`
    : `Of the ${all.length} seats, <b>${t.certain}</b> are certain, <b>${t.probable}</b> probable, <b>${t.possible}</b> possible and <b>${t["toss-up"]}</b> toss-ups.${rows.length !== all.length ? ` Shown: ${rows.length}.` : ""} ${when}`;
  $("estable").innerHTML = `<tr><th>Constituency</th><th>Seat</th><th>Projected list</th><th class="n">Chance</th><th>Certainty</th><th>Next most likely</th></tr>` +
    rows.map(({ k, s }) => `<tr${S.esL && s.id !== S.esL ? ' class="muted"' : ""}><td><a href="${esc(geoLink(k.okreg))}" target="_top">${k.okreg}. ${esc(k.name)}</a></td><td>${s.rank} of ${k.magnitude}</td><td><span class="dot" style="background:${entColour(s.id)};margin-right:6px"></span>${esc(s.id)}${s.j > 1 ? ` <span class="muted">(its ${ord(s.j)} seat here)</span>` : ""}</td><td class="n"><b>${prob(s.p)}</b></td><td><span class="catchip ${s.cat}">${CATLAB[s.cat]}</span></td><td>${s.next ? `<span class="dot" style="background:${entColour(s.next.id)};margin-right:6px"></span>${esc(s.next.id)} <span class="muted">${prob(s.next.p)}</span>` : `<span class="muted">–</span>`}</td></tr>`).join("") +
    (rows.length ? "" : `<tr><td colspan="6" class="muted">No seats match these filters.</td></tr>`);
  $("esnote").innerHTML = `${SEAT_CAT_NOTE()} A constituency's seats are its most probable list-seats: a list's k-th seat there is won in the share of simulations in which it wins at least k seats. When the projected list loses a seat in a simulation, the seat goes to the list that wins a seat beyond its projection there, so each seat's chances add up to 100% across the lists that could hold it (every list's chance is in the CSV). With a list chosen, its outside possibilities are listed too, greyed: seats projected for another list where the chosen list is the next most likely to take them, with a chance of ${pc0(SEAT_OUTSIDE)}% or more. Totals by constituency can differ by a seat or two from the medians on the Sejm view. Click a constituency to open it in Geography.`;
}
function everySeatCsv() {
  const sim = hemiSim(); if (!sim) return;
  const sb = seatByConstituency(sim), ids = sim.ents.map(e => e.name), scen = CUR.mode === "model" ? "model estimates" : CUR.mode === "forecast" ? "forecast: " + fcSubtitle() : "hypothetical scenario: " + scenText();
  const rows = [["week", "figures", "okreg", "constituency", "magnitude", "seat_rank", "projected_list", "list_seat_number", "chance", "certainty", ...ids.map(id => "chance_" + id)]];
  sb.forEach(k => k.projected.forEach(s => { const pm = Object.fromEntries(s.take.map(t => [t.id, t.p])); rows.push([iso(CUR.wk.date), scen, k.okreg, k.name, k.magnitude, s.rank, s.id, s.j, +s.p.toFixed(3), s.cat, ...ids.map(id => (pm[id] != null ? +pm[id].toFixed(3) : 0))]); }));
  const q = v => (typeof v === "string" && /[",]/.test(v) ? `"${v.replace(/"/g, '""')}"` : v);
  download(`polling-tracker-every-seat-${iso(CUR.wk.date)}${CUR.mode === "model" ? "" : CUR.mode === "forecast" ? "-forecast-" + FC.electionDate : "-scenario"}.csv`, new Blob([ChartExport.csvCite(PAGE) + rows.map(r => r.map(q).join(",")).join("\n") + "\n"], { type: "text/csv" }));
}

// ---- the Seats page's side panel: the week's figures, read only --------------------------------
const fcSubtitle = () => FC ? `Forecast for election day, ${dlong(fromIso(FC.electionDate))}${FC.provisional ? " (provisional date)" : ""}, from the polls to ${dlong(fromIso(FC.asOf))}` : "Forecast";
function loadForecast() {
  if (fcState !== "idle") return;
  fcState = "loading";
  fetch("forecast.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).then(d => {
    FC = d; fcState = iso(WEEKS[WEEKS.length - 1].date) === d.asOf ? "ready" : "stale";
    if (fcState === "stale") console.warn("forecast.json is for " + d.asOf + ", polls.json for " + iso(WEEKS[WEEKS.length - 1].date));
    update();
  }).catch(() => { fcState = "missing"; update(); });
}
// The Forecast page's side panel: the switch between election day and this week, then the figures
function renderForecastPanel() {
  const cur = CUR, el = cur.mode === "forecast", now = S.fh === "now";
  const sw = `<div class="seg" id="fhseg" role="group" aria-label="Forecast horizon"><button type="button" data-fh="election" aria-pressed="${!now}">On election day</button><button type="button" data-fh="now" aria-pressed="${now}">If held this week</button></div>`;
  const wait = !now && !el ? `<p class="help">${fcState === "missing" ? "The forecast could not be loaded." : fcState === "stale" ? "The forecast file is from another week than the polls; it will be right after the next run." : "Loading the forecast…"}</p>` : "";
  const fw = el ? fcDraws() : null, wk = cur.wk;
  const list = cur.ents.map((e, k) => ({ e, k, r: cur.rows[k] })).filter(x => !x.e.mnOnly || x.r.seats > 0).filter(x => x.r.seats > 0 || x.e.vote >= 0.5)
    .sort((a, b) => b.r.seats - a.r.seats || b.e.vote - a.e.vote);
  const rows = list.map(({ e, r }) => {
    const i = PIDX[e.name];
    const v = el && i != null ? 100 * fw.med[i] : e.vote;
    const vr = el && i != null ? `${(100 * fw.lo[i]).toFixed(1)}–${(100 * fw.hi[i]).toFixed(1)}` : !el && i != null && wk.voteLo[i] != null ? `${wk.voteLo[i].toFixed(1)}–${wk.voteHi[i].toFixed(1)}` : "";
    return `<tr><td><span class="dot" style="background:${e.colour};margin-right:6px"></span>${esc(e.name)}</td><td style="color:var(--accent);font-weight:500">${v.toFixed(1)}</td><td class="muted">${vr}</td><td><b>${r.seats}</b></td><td class="muted">${r.lo != null ? r.lo + "–" + r.hi : ""}</td></tr>`;
  }).join("");
  const doubt = cur.ents.map((e, k) => ({ e, r: cur.rows[k], t: el && PIDX[e.name] != null ? fw.med[PIDX[e.name]] : cur.tot[k] })).filter(x => x.r.pThr != null && SiteUI.inDoubt(x.t, R.thresholdParty, x.r.pThr));
  const days = FC ? FC.daysToElection : null;
  $("panel4").innerHTML = `<div class="card">
    ${sw}
    <h2 style="margin:4px 0 0">${el || (!now && FC) ? dlong(fromIso(FC.electionDate)) : dlong(wk.date)}</h2>
    <div><span class="badge${el ? " edited" : ""}">${el ? "Forecast" : now ? "If held this week" : "Forecast"}</span></div>
    ${wait}
    <span class="label">Vote shares and seats</span>
    <table class="t"><tr><th>Party</th><th>Vote %</th><th>80%</th><th>Seats</th><th>80%</th></tr>${rows}</table>
    <p class="help" style="margin:0">${el ? `Medians of ${FC.nDraws.toLocaleString("en-GB")} simulated elections, with 80% ranges. ${days} days before ${FC.provisional ? "the provisional election date (the President sets it by 15 August 2027; 24 October is the middle of the legal window)" : "the election"}, the polls of this week can still be far from the result: the ranges are as wide as the polls' misses at this distance since 1993.` : now ? "The model's estimates for this week, as on the Seats page: medians of its simulated parliaments, with 80% ranges." : ""}</p>
  </div>
  ${doubt.length ? `<div class="card"><span class="label">Thresholds in doubt</span>
    <table class="t"><tr><th>Party</th><th>Vote %</th><th>Chance of clearing</th></tr>${doubt.map(x => `<tr><td>${esc(x.e.name)}</td><td>${(100 * x.t).toFixed(1)}</td><td>${prob(x.r.pThr)} <span class="muted">of 5%</span></td></tr>`).join("")}</table>
    <p class="help" style="margin:0">Only parties within 3 points of the threshold, or with a chance between 2% and 98% of clearing it, are listed.</p></div>` : ""}
  <div class="card soft"><span class="label">More</span>
    <div style="display:flex;flex-direction:column;gap:6px;font-size:13px">
      <button type="button" class="linkbtn" id="toevery" style="text-align:left;font-size:13px">Every seat, with each list's chance →</button>
      <a href="${esc(sectionUrl("seats"))}" target="_top">This week's figures on Seats →</a>
      <a href="${esc(sectionUrl("scenarios"))}" target="_top">Build a scenario in Scenarios →</a>
    </div>
    <div class="actions" style="margin-top:6px"><button type="button" class="btn2" id="copylink">Copy link to this view</button></div>
  </div>`;
}
function renderSeatsPanel() {
  const cur = CUR; if (!cur || TERM) return;
  if (FORECAST) return renderForecastPanel();
  const wk = cur.wk, model = cur.mode === "model";
  const list = cur.ents.map((e, k) => ({ e, k, r: cur.rows[k] })).filter(x => !x.e.mnOnly || x.r.seats > 0).filter(x => x.r.seats > 0 || x.e.vote >= 0.5)
    .sort((a, b) => b.r.seats - a.r.seats || b.e.vote - a.e.vote);
  const name = e => (e.joint ? e.members.map(short).join(" + ") : e.name);
  const rows = list.map(({ e, r }) => {
    const i = PIDX[e.name], vr = model && !e.joint && i != null && wk.voteLo[i] != null ? `${wk.voteLo[i].toFixed(1)}–${wk.voteHi[i].toFixed(1)}` : "";
    return `<tr><td><span class="dot" style="background:${e.colour};margin-right:6px"></span>${esc(name(e))}</td><td style="color:var(--accent);font-weight:500">${e.vote.toFixed(1)}</td><td class="muted">${vr}</td><td><b>${r.seats}</b></td><td class="muted">${r.lo != null ? r.lo + "–" + r.hi : ""}</td></tr>`;
  }).join("");
  const doubt = cur.ents.map((e, k) => ({ e, r: cur.rows[k], t: cur.tot[k] })).filter(x => x.r.pThr != null && SiteUI.inDoubt(x.t, x.e.joint ? R.thresholdJoint : R.thresholdParty, x.r.pThr));
  const carry = carryString(), q = carry ? "#" + carry : "";
  $("panel4").innerHTML = `<div class="card">
    <div class="phead">
      <div class="stepper"><button type="button" id="wprev" aria-label="Previous week" ${S.w === 0 ? "disabled" : ""}>‹</button><button type="button" id="wnext" aria-label="Next week" ${S.w === WEEKS.length - 1 ? "disabled" : ""}>›</button></div>
      <h2>${dlong(wk.date)}</h2>
    </div>
    <div><span class="badge${model ? "" : " edited"}">${model ? "Model estimates" : "Hypothetical scenario"}</span></div>
    <span class="label">Vote shares and seats</span>
    <table class="t"><tr><th>${S.joint.length ? "List" : "Party"}</th><th>Vote %</th><th>80%</th><th>Seats</th><th>80%</th></tr>${rows}</table>
    <p class="help" style="margin:0">${model ? "The model's estimates: medians of its simulated parliaments, with 80% ranges." : cur.mode === "sim" ? "The scenario's shares applied to each of the week's simulated draws: medians with 80% ranges." : "A single allocation of the scenario's shares, without uncertainty."}</p>
  </div>
  ${doubt.length ? `<div class="card"><span class="label">Thresholds in doubt</span>
    <table class="t"><tr><th>${S.joint.length ? "List" : "Party"}</th><th>Vote %</th><th>Chance of clearing</th></tr>${doubt.map(x => `<tr><td>${esc(name(x.e))}</td><td>${(100 * x.t).toFixed(1)}</td><td>${prob(x.r.pThr)} <span class="muted">of ${x.e.joint ? "8%" : "5%"}</span></td></tr>`).join("")}</table>
    <p class="help" style="margin:0">Only lists within 3 points of their threshold, or with a chance between 2% and 98% of clearing it, are listed.</p></div>` : ""}
  <div class="card soft"><span class="label">More</span>
    <div style="display:flex;flex-direction:column;gap:6px;font-size:13px">
      <a href="${esc(sectionUrl("scenarios") + q)}" target="_top">${model ? "Change these figures" : "Change this scenario"} in Scenarios →</a>
      <button type="button" class="linkbtn" id="toevery" style="text-align:left;font-size:13px">Every seat, with each list's chance →</button>
      <a href="${esc(sectionUrl("geography") + q)}" target="_top">Where the seats fall, constituency by constituency →</a>
    </div>
    <div class="actions" style="margin-top:6px"><button type="button" class="btn2" id="copylink">Copy link to this view</button></div>
  </div>`;
}
const copyLink = () => { const u = shareUrl(); (navigator.clipboard ? navigator.clipboard.writeText(u) : Promise.reject()).then(() => toast("Link copied"), () => window.prompt("Copy this link:", u)); };
function backToModel() { S.shares = defaults(S.w); S.joint = []; S.levers = []; S.hand.clear(); LEVRES = null; S.resp = "medium"; S.adv = null; S.pinGov = null; S.focusGov = null; update(); }
function bindSections() {
  $("scenbanner").addEventListener("click", e => {
    if (e.target.id === "tomodel") backToModel();
    if (e.target.id === "toslide") { S.view = "sliders"; renderChips(); update(); }
    if (e.target.id === "flclear2") { S.fl = []; update(); }
  });
  $("panel4").addEventListener("click", e => {
    const t = e.target;
    if (t.id === "wprev") return setWeek(S.w - 1);
    if (t.id === "wnext") return setWeek(S.w + 1);
    if (t.id === "toevery") { S.view = "everyseat"; renderChips(); renderView(); saveHash(); return; }
    const fh = t.closest("button[data-fh]");
    if (fh && fh.dataset.fh !== S.fh) { S.fh = fh.dataset.fh; if (fcOn()) loadForecast(); else loadDraws(); update(); return; }
    if (t.id === "copylink") copyLink();
  });
  $("esfilters").addEventListener("change", e => {
    const t = e.target, k = { "es-c": "esC", "es-l": "esL", "es-k": "esCat", "es-o": "esSort" }[t.id];
    if (k) { S[k] = t.value; renderEverySeat(); saveHash(); }
  });
  $("esfilters").addEventListener("click", e => { if (e.target.id === "escsv") everySeatCsv(); if (e.target.id === "toseats") { S.view = "seats"; renderChips(); update(); } });
  $("flctl").addEventListener("input", e => {
    const t = e.target, i = t.dataset.fl; if (i == null || t.dataset.k !== "c") return;
    S.fl[+i].c = parseFloat(t.value); flSoon();
  });
  $("flctl").addEventListener("change", e => {
    const t = e.target, i = t.dataset.fl; if (i == null) return;
    const x = S.fl[+i], k = t.dataset.k;
    if (k === "m") { x.m = t.value; x.c = 0; } else if (k === "c") x.c = parseFloat(t.value); else x.g[k] = t.value === "" ? null : +t.value;
    update();
  });
  $("flctl").addEventListener("click", e => {
    const t = e.target;
    if (t.id === "fladd") { S.fl.push({ g: { age: null, sex: null, education: null, residence: null }, m: "soc", c: 0 }); update(); }
    else if (t.id === "flclear") { S.fl = []; update(); }
    else if (t.dataset.rmfl != null) { S.fl.splice(+t.dataset.rmfl, 1); update(); }
  });
  $("snctl").addEventListener("change", e => {
    const t = e.target;
    if (t.dataset.sp) { if (t.value) S.sp[t.dataset.sp] = t.value; else delete S.sp[t.dataset.sp]; }
    else if (t.id === "sninc") S.sinc = t.checked; else return;
    renderSenate(); saveHash();
  });
  $("sncoal").addEventListener("change", e => { const k = e.target.dataset.sc; if (k == null) return; if (e.target.checked) S.scoal.add(k); else S.scoal.delete(k); renderSenate(); saveHash(); });
  $("snfilt").addEventListener("change", e => { if (e.target.id === "sn-o") { S.snSort = e.target.value; renderSenate(); saveHash(); } });
  const p5 = $("panel5");
  p5.addEventListener("change", e => {
    const t = e.target;
    if (t.name === "lvbase") { S.base = t.value; return leversChanged(); }
    if (t.id === "lvhold") { S.hold = t.checked; return leversChanged(); }
    if (t.dataset.lv == null) return;
    const lv = S.levers[+t.dataset.lv], f = t.dataset.f; if (!lv) return;
    if (GDIMS.includes(f)) { if (t.value === "") delete lv.group[f]; else lv.group[f] = +t.value; }
    else if (f === "measure") { lv.measure = t.value; if (lv.from === lv.measure || lv.measure === "turnout") lv.from = null; }
    else if (f === "from") lv.from = t.value || null;
    else if (f === "change") { const v = parseFloat(t.value); if (!isFinite(v)) { t.value = lv.change; return; } lv.change = clamp(v, -40, 40); }
    leversChanged();
  });
  p5.addEventListener("keydown", e => { if (e.key === "Enter" && e.target.dataset && e.target.dataset.f === "change") { e.preventDefault(); e.target.blur(); } });
  p5.addEventListener("click", e => {
    const t = e.target;
    if (t.id === "lvweek") return setWeek(cellsWeek());
    if (t.id === "lvadd") { S.levers.push({ group: {}, measure: "turnout", change: 0, from: null }); return leversChanged(); }
    if (t.id === "lvclear") { S.levers = []; return leversChanged(); }
    if (t.dataset.rmlv != null) { S.levers.splice(+t.dataset.rmlv, 1); return leversChanged(); }
    const ex = t.closest("button[data-ex]");
    if (ex) { S.levers = LEVER_EXAMPLES[+ex.dataset.ex][1].map(lv => ({ group: Object.assign({}, lv.group), measure: lv.measure, change: lv.change, from: lv.from })); return leversChanged(); }
    if (t.id === "copylink") copyLink();
  });
  $("wictl").addEventListener("click", e => {
    const b = e.target.closest("button[data-k]"); if (!b) return;
    if (b.parentElement.id === "wimode") S.wiMode = b.dataset.k; else if (b.parentElement.id === "wimetric") S.wiMetric = b.dataset.k; else return;
    renderWhoVotes(); saveHash();
  });
  $("wictl").addEventListener("change", e => { const d = e.target.dataset.wg; if (!d) return; if (e.target.value === "") delete S.wiGroup[d]; else S.wiGroup[d] = +e.target.value; renderWhoVotes(); });
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
    if (S.view === "groups") S.view = "trends";
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
    // the seat views need the simulations even in the model view (certainty, every seat, constituency ranges)
    if (!modelView() || SECTION !== "polls") update();
  }).catch(() => { drawsState = "missing"; if (!modelView() || SECTION !== "polls") update(); else updatePanel(); });
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
Promise.all([fetch("polls.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }),
  fetch("joint_penalty.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).catch(() => null)]).then(([d, jp]) => {
  if (jp) prepJP(jp);
  D = d; prep();
  if (restoreHash() === false) return;   // an old link sent on to the page that now holds its view
  $("views").innerHTML = SECTIONS[SECTION].map(([k, l]) => `<button type="button" role="tab" data-v="${k}">${esc(l)}</button>`).join("");
  header(); renderChips(); bind(); bindSections();
  if (SECTION === "polls") loadTerms();
  // set on the SVGs themselves so PNG exports keep the typeface
  ["trend", "hemi", "map", "house", "groups"].forEach(id => $(id).setAttribute("font-family", "Jost, 'Avenir Next', -apple-system, sans-serif"));
  if (FORECAST && fcOn()) loadForecast();
  if (FORECAST && (S.fl.length || S.view === "sliders")) loadCells().then(() => { FCW = null; update(); });
  if (!modelView() || SECTION !== "polls") loadDraws();
  // a link with changes by group needs the cells before its shares are known
  if (S.levers.length || ["levers", "whovotes"].includes(S.view)) loadCells().then(() => { applyLevers(); update(); });
  update();
  setTimeout(loadDraws, 1500);
}).catch(err => { $("asof").textContent = "Could not load the polling data."; console.error(err); });
