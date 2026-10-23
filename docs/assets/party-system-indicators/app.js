/* Party system indicators — the Polish party system in every Sejm election since 1991, and now.
 *
 * Reads party_system.json (written by Party system analysis/analysis/export_web.R) and the Polling
 * tracker's ../polling-tracker/polls.json. The polls column is computed here from the tracker's
 * latest estimate, by the rules of Party system analysis.R: the parties' median vote shares
 * without Other, rescaled to sum to 1 (MN enters only the seats), and the median seat projection.
 * Only measures that need nothing but vote and seat shares exist for the polls. If polls.json
 * cannot be read, the analysis's own poll snapshot is used instead.
 *
 * One election is selected at a time (the strip at the top); every section highlights it and
 * shows what lies behind its numbers. State lives in the URL hash of the parent page, as in the
 * Polling tracker. Plain SVG; no libraries.
 */
"use strict";

// ---- utilities ------------------------------------------------------------------------------
const $ = id => document.getElementById(id);
const esc = s => String(s == null ? "" : s).replace(/[&<>"']/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
const MON = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
const MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
const dateOf = d => new Date(d * 864e5);
const dstr = d => { const t = dateOf(d); return t.getUTCDate() + " " + MON[t.getUTCMonth()] + " " + t.getUTCFullYear(); };
const dlong = d => { const t = dateOf(d); return t.getUTCDate() + " " + MONTHS[t.getUTCMonth()] + " " + t.getUTCFullYear(); };
const iso = d => dateOf(d).toISOString().slice(0, 10);
const yearFrac = d => 1970 + d / 365.2425;
const clamp = (v, a, b) => Math.max(a, Math.min(b, v));
const sum = a => a.reduce((x, y) => x + y, 0);
const isNum = v => v != null && typeof v === "number" && isFinite(v);
const NA = "–";
const fx = dp => v => isNum(v) ? v.toFixed(dp) : NA;
const f1 = fx(1), f2 = fx(2), f3 = fx(3);
const int = v => isNum(v) ? String(Math.round(v)) : NA;
const pct = (v, dp = 1) => isNum(v) ? (100 * v).toFixed(dp) + "%" : NA;
const pts = (v, dp = 1) => isNum(v) ? (100 * v).toFixed(dp) : NA;
const signedPts = (v, dp = 1) => { if (!isNum(v)) return NA; const r = +(100 * v).toFixed(dp); return (r > 0 ? "+" : r < 0 ? "−" : "±") + Math.abs(r).toFixed(dp); };
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
function niceDomain(lo, hi, n = 5) {
  const st = niceStep(hi - lo || Math.abs(hi) || 1, n);
  return [Math.floor(lo / st + 1e-9) * st, Math.ceil(hi / st - 1e-9) * st];
}
function repel(items, gap, lo, hi) { // items: {y}; sorted in place and pushed apart
  items.sort((a, b) => a.y - b.y);
  for (let i = 1; i < items.length; i++) if (items[i].y - items[i - 1].y < gap) items[i].y = items[i - 1].y + gap;
  const over = items.length ? items[items.length - 1].y - hi : 0;
  if (over > 0) { items[items.length - 1].y -= over; for (let i = items.length - 2; i >= 0; i--) if (items[i + 1].y - items[i].y < gap) items[i].y = items[i + 1].y - gap; }
  if (items.length && items[0].y < lo) { const sh = lo - items[0].y; items.forEach(it => { it.y += sh; }); }
  return items;
}
function mix(hex, t) { // towards white; t = 1 is the full colour
  const n = parseInt(hex.slice(1), 16), r = n >> 16, g = (n >> 8) & 255, b = n & 255, w = c => Math.round(255 - (255 - c) * t);
  return "#" + [w(r), w(g), w(b)].map(c => c.toString(16).padStart(2, "0")).join("");
}
// Lists with no party id carry their committee's name; keep it short on charts
function shortList(name) {
  let t = String(name).replace(/^(Komitet Wyborczy Wyborców|Komitet Wyborczy|KWW|KW)\s+/i, "");
  if (t.length > 14 && t === t.toUpperCase()) t = t.charAt(0) + t.slice(1).toLowerCase();
  return t.length > 18 ? t.slice(0, 17).trimEnd() + "…" : t;
}
// Scatter labels: {x, y, text}; a label that overlaps one already placed moves down
function declutter(labs, h = 12) {
  labs.forEach(l => { l.w = l.text.length * 6.2; });
  labs.sort((a, b) => a.y - b.y);
  const placed = [];
  labs.forEach(l => {
    let n = 0;
    while (n++ < 24 && placed.some(p => Math.abs(p.y - l.y) < h && l.x < p.x + p.w && p.x < l.x + l.w)) l.y += 3;
    placed.push(l);
  });
  return labs;
}
const ACCENT = "#8B0000", INK = "#141414", MUTED = "#767676", GRID = "#EEEEEE";
const FONT = "Jost, 'Avenir Next', -apple-system, sans-serif";

// ---- data -----------------------------------------------------------------------------------
let D, PJ = null, POLL = null, YEARS, EBY, FAM, LISTS_BY_YEAR;
const S = { e: null, frag: "lt", vol: "ab", nat: "constituency", pol: "galtan", gov: "n", natHl: null };
const famCol = id => (FAM[id] && FAM[id].colour) || "#9E9E9E";
const famLab = id => (FAM[id] && FAM[id].label) || id;
const colOf = label => D.colours[label] || null;
const listCol = l => (l.label && colOf(l.label)) || famCol(l.family);
const pollCol = p => colOf(p) || (PJ && PJ.rules.colours[p]) || "#808080";
const eLabel = e => e === "polls" ? "the polls" : String(e);
const pollX = () => yearFrac(POLL.date);

function prep() {
  YEARS = D.elections.map(e => e.year);
  EBY = Object.fromEntries(D.elections.map(e => [e.year, e]));
  FAM = Object.fromEntries(D.families.map((f, i) => [f.id, { ...f, i }]));
  LISTS_BY_YEAR = {};
  D.lists.forEach(l => { (LISTS_BY_YEAR[l.year] = LISTS_BY_YEAR[l.year] || []).push(l); });
  D.nationalisation.forEach(n => { n.l = (LISTS_BY_YEAR[n.year] || []).find(l => l.id === n.id); });
}

// The same statistics as system_stats() in Party system analysis.R, from vote and seat shares
function systemStats(v, s) {
  const top = [...v].sort((a, b) => b - a);
  let imax = 0; v.forEach((x, i) => { if (x > v[imax]) imax = i; });
  const vmax = top[0];
  return {
    lists_above_5: v.filter(x => x >= 0.05).length,
    parliamentary_parties: s.filter(x => x > 0).length,
    enp_votes: 1 / sum(v.map(x => x * x)),
    enp_seats: 1 / sum(s.map(x => x * x)),
    golosov_votes: sum(v.map(x => x / (x + vmax * vmax - x * x))),
    golosov_seats: (() => { const sm = Math.max(...s); return sum(s.map(x => x / (x + sm * sm - x * x))); })(),
    n_largest: 1 / vmax,
    largest_vshare: vmax,
    top2_vshare: top[0] + top[1],
    margin_top2: top[0] - top[1],
    fragmentation_rae: 1 - sum(v.map(x => x * x)),
    gallagher: Math.sqrt(0.5 * sum(v.map((x, i) => (x - s[i]) ** 2))),
    wasted_votes: sum(v.filter((x, i) => s[i] === 0)),
    largest_sshare: s[imax],
    seat_bonus: s[imax] - vmax
  };
}
// The tracker's latest estimate: trend medians (without Other, rescaled) and the week's median seats
function livePolls(pj) {
  const R = pj.rules, T = pj.trend, wk = pj.weeks[pj.weeks.length - 1];
  let ti = T.dates.length - 1;
  while (ti > 0 && T.dates[ti] > wk.date) ti--;
  const raw = R.seatParties.map(p => { const s = T.series[p]; return s && isNum(s.med[ti]) ? s.med[ti] : 0; });
  const tot = sum(raw), seatTot = sum(wk.seats.map(x => x || 0));
  if (!(tot > 0) || !(seatTot > 0)) throw new Error("empty poll estimate");
  const parties = R.seatParties.map((p, i) => ({ party: p, v: raw[i] / tot, seats: wk.seats[i] || 0, s: (wk.seats[i] || 0) / seatTot }));
  return {
    live: true, date: T.dates[ti], other: 1 - tot / 100, seatTotal: seatTot, parties,
    stats: systemStats(parties.map(p => p.v), parties.map(p => p.s))
  };
}
function snapshotPolls() {
  const ps = D.pollSnapshot;
  return { live: false, date: ps.date, other: ps.otherShare, seatTotal: sum(ps.parties.map(p => p.seats)), parties: ps.parties, stats: Array.isArray(ps.stats) ? ps.stats[0] : ps.stats };
}

// ---- indicator rows (overview and CSV) --------------------------------------------------------
const POLL_KEYS = new Set(["lists_above_5", "parliamentary_parties", "enp_votes", "enp_seats", "golosov_votes", "golosov_seats", "n_largest",
  "largest_vshare", "top2_vshare", "margin_top2", "fragmentation_rae", "gallagher", "wasted_votes", "largest_sshare", "seat_bonus"]);
function cheFor(year) { return D.ches.index.find(c => c.ref === year) || null; }
function val(k, e) {
  if (k.startsWith("pol_")) { if (e === "polls") return null; const c = cheFor(e); return c ? c[k.slice(4)] : null; }
  if (e === "polls") return POLL_KEYS.has(k) ? POLL.stats[k] : null;
  const r = EBY[e]; return r && isNum(r[k]) ? r[k] : null;
}
const GROUPS = [
  { sec: "overview", label: "Turnout", rows: [{ k: "turnout", lab: "Turnout", f: v => pct(v) }] },
  { sec: "fragmentation", label: "Fragmentation", rows: [
    { k: "enp_votes", lab: "Effective number of parties, votes", f: f2, set: { frag: "lt" } },
    { k: "enp_seats", lab: "Effective number of parties, seats", f: f2, set: { frag: "lt" } },
    { k: "golosov_votes", lab: "Golosov effective number, votes", f: f2, set: { frag: "golosov" } },
    { k: "largest_vshare", lab: "Largest list's vote", f: v => pct(v), set: { frag: "inv" } },
    { k: "parliamentary_parties", lab: "Lists winning seats", f: int }] },
  { sec: "disproportionality", label: "Disproportionality", rows: [
    { k: "gallagher", lab: "Gallagher index", f: v => pts(v) },
    { k: "wasted_votes", lab: "Wasted votes", f: v => pct(v) },
    { k: "seat_bonus", lab: "Largest list's seat bonus, points", f: v => signedPts(v) }] },
  { sec: "volatility", label: "Volatility", rows: [
    { k: "volatility_total", lab: "Total volatility", f: v => pts(v), set: { vol: "ab" } },
    { k: "volatility_type_a", lab: "From new and withdrawn lists (Type A)", f: v => pts(v), set: { vol: "ab" } },
    { k: "volatility_between_family", lab: "Between party families", f: v => pts(v), set: { vol: "fam" } },
    { k: "new_list_vshare", lab: "Vote for new lists", f: v => pct(v), set: { vol: "new" } }] },
  { sec: "nationalisation", label: "Nationalisation", rows: [
    { k: "psns_constituency", lab: "Party nationalisation, constituencies", f: f3, set: { nat: "constituency" } },
    { k: "psns_gmina", lab: "Party nationalisation, gminas", f: f3, set: { nat: "gmina" } },
    { k: "enp_inflation", lab: "ENP inflation", f: v => pct(v) }] },
  { sec: "polarisation", label: "Polarisation (CHES wave weighted by this election)", rows: [
    { k: "pol_galtan", lab: "GAL–TAN", f: f2, set: { pol: "galtan" }, desc: "Dalton polarisation index (0–10) on the CHES GAL–TAN scale, for the survey wave weighted by this election's results" },
    { k: "pol_lrgen", lab: "General left–right", f: f2, set: { pol: "lrgen" }, desc: "Dalton polarisation index (0–10) on the CHES general left–right scale, for the survey wave weighted by this election's results" },
    { k: "pol_lrecon", lab: "Economic left–right", f: f2, set: { pol: "lrecon" }, desc: "Dalton polarisation index (0–10) on the CHES economic left–right scale, for the survey wave weighted by this election's results" }] },
  { sec: "government", label: "Government", rows: [
    { k: "gov", lab: "Government formed after the election", gov: true, desc: "How the first cabinet formed after the election is coded: alternation, formula and access, filled when closed" }] }
];

// ---- state, election helpers --------------------------------------------------------------------
const COLS = () => [...YEARS, "polls"];
function electionDate(e) { return e === "polls" ? POLL.date : EBY[e].date; }
function prevYear(y) { const i = YEARS.indexOf(y); return i > 0 ? YEARS[i - 1] : null; }
function waveFor(e) { // the CHES wave whose weights come from this election, or the nearest
  const W = D.ches.index;
  if (e === "polls") return { c: W[W.length - 1], exact: false, why: "the latest wave" };
  const ex = W.find(c => c.ref === e); if (ex) return { c: ex, exact: true };
  const before = W.filter(c => c.ref < e);
  return before.length ? { c: before[before.length - 1], exact: false, why: "the latest wave before it" } : { c: W[0], exact: false, why: "the first wave" };
}
function cabinetsAfter(e) { // cabinets formed in the term that began with this election
  const G = D.governments;
  if (e === "polls") return G.filter(g => g.ongoing);
  const d0 = EBY[e].date, i = YEARS.indexOf(e), d1 = i < YEARS.length - 1 ? EBY[YEARS[i + 1]].date : Infinity;
  return G.filter(g => g.start > d0 && g.start <= d1);
}
function electionBefore(day) { let y = YEARS[0]; YEARS.forEach(yy => { if (EBY[yy].date <= day) y = yy; }); return y; }

// ---- chart plumbing -------------------------------------------------------------------------
const CH = {};
function frameW(boxId) { return Math.max(280, $(boxId).clientWidth - 12); }
function put(svgId, W, H, html, title) {
  const svg = $(svgId);
  if (title) svg.dataset.title = title;
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.setAttribute("width", W); svg.setAttribute("height", H);
  svg.setAttribute("font-family", FONT);
  svg.innerHTML = html + `<g class="hov" pointer-events="none"></g>`;
}
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
function bindChart(svgId) {
  const svg = $(svgId), pos = e => { const c = CH[svgId], r = svg.getBoundingClientRect(), k = c.W / r.width; return [(e.clientX - r.left) * k, (e.clientY - r.top) * k]; };
  const tipOf = c => c.box.querySelector(".tip");
  svg.addEventListener("mousemove", e => {
    const c = CH[svgId]; if (!c || !c.move) return;
    const [mx, my] = pos(e), html = c.move(mx, my, e), tip = tipOf(c);
    if (html) placeTip(tip, c.box, e, html); else { tip.hidden = true; svg.querySelector(".hov").innerHTML = ""; }
  });
  svg.addEventListener("mouseleave", () => {
    const c = CH[svgId]; if (!c) return;
    tipOf(c).hidden = true; const h = svg.querySelector(".hov"); if (h) h.innerHTML = "";
    if (c.leave) c.leave();
  });
  svg.addEventListener("click", e => { const c = CH[svgId]; if (c && c.click) { const [mx, my] = pos(e); c.click(mx, my, e); } });
}
const tipRow = (col, name, value, sub) => `<div class="r"><span>${col ? `<span class="dot" style="background:${col}"></span>` : ""}${esc(name)}</span><span>${value}${sub ? ` <span class="sub">${sub}</span>` : ""}</span></div>`;
const tipHead = (title, sub) => `<b>${esc(title)}</b>${sub ? `<div class="sub">${sub}</div>` : ""}`;
function marker(kind, x, y, r, fill, stroke, extra = "") {
  if (kind === "tri") { const h = r * 1.25; return `<path d="M${x},${y - h}L${x + h},${y + h * 0.8}L${x - h},${y + h * 0.8}Z" fill="${fill}" stroke="${stroke}" stroke-width="1.4" ${extra}/>`; }
  if (kind === "sq") return `<rect x="${x - r}" y="${y - r}" width="${2 * r}" height="${2 * r}" fill="${fill}" stroke="${stroke}" stroke-width="1.4" ${extra}/>`;
  return `<circle cx="${x}" cy="${y}" r="${r}" fill="${fill}" stroke="${stroke}" stroke-width="1.4" ${extra}/>`;
}
const electionPts = (withPolls = true) => [...YEARS.map(y => ({ k: y, x: y, lab: String(y) })), ...(withPolls ? [{ k: "polls", x: pollX(), lab: "Polls" }] : [])];
const ptTitle = k => k === "polls" ? `Polls, ${dstr(POLL.date)}` : `${k} election`;

/* A line chart over elections (or CHES waves).
 * cfg: pts [{k,x,lab}], series [{name, colour, dash, marker, width, get(k)}], fmt, H, ymin/ymax, log,
 *      area [i, j] (shade between two series), endLabels, hl (series name to bring forward), sel (k),
 *      tipHead(k), tipExtra(k), onPick(k), yLabel */
function lineChart(svgId, boxId, cfg) {
  const W = frameW(boxId), H = cfg.H || Math.round(clamp(W * 0.46, 220, 340));
  const pts = cfg.pts;
  const endName = s => (W < 640 && s.short) ? s.short : s.name;
  const endW = cfg.endLabels ? clamp(Math.max(...cfg.series.map(s => endName(s).length)) * 6.4 + 22, 64, W * 0.32) : 16;
  const m = { l: cfg.ml || 38, r: endW, t: cfg.mt || 12, b: 26 };
  const xs = pts.map(p => p.x), pad = cfg.padX ?? 0.9, x0 = Math.min(...xs) - pad, x1 = Math.max(...xs) + pad;
  const X = v => m.l + (v - x0) / (x1 - x0) * (W - m.l - m.r);
  const gaps = xs.slice(1).map((x, i) => X(x) - X(xs[i])), short = gaps.length && Math.min(...gaps) < 40;
  let vals = [];
  cfg.series.forEach(s => pts.forEach(p => { const v = s.get(p.k); if (isNum(v)) vals.push(v); }));
  if (!vals.length) vals = [0, 1];
  let lo, hi, Y, yt;
  if (cfg.log) {
    lo = cfg.ymin ?? 1; hi = cfg.ymax ?? Math.max(...vals) * 1.15;
    const L = v => Math.log10(v);
    Y = v => m.t + (1 - (L(v) - L(lo)) / (L(hi) - L(lo))) * (H - m.t - m.b);
    yt = [1, 2, 5, 10, 20, 50, 100, 200].filter(v => v >= lo && v <= hi);
  } else {
    [lo, hi] = niceDomain(cfg.ymin ?? Math.min(...vals), cfg.ymax ?? Math.max(...vals), cfg.yn || 4);
    if (cfg.ymin != null) lo = cfg.ymin;
    if (cfg.ymax != null) hi = cfg.ymax;
    Y = v => m.t + (1 - (v - lo) / (hi - lo || 1)) * (H - m.t - m.b);
    yt = ticks(lo, hi, cfg.yn || 4);
  }
  const st = yt.length > 1 ? yt[1] - yt[0] : 1, dp = st >= 1 ? 0 : st >= 0.1 ? 1 : st >= 0.01 ? 2 : 3;
  const fmtY = cfg.fmtAxis || (v => v.toFixed(dp));
  const o = [];
  yt.forEach(v => o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${Y(v)}" y2="${Y(v)}" stroke="${GRID}"/><text x="${m.l - 6}" y="${Y(v) + 4}" text-anchor="end" font-size="11" fill="${MUTED}">${fmtY(v)}</text>`));
  (cfg.refs || []).forEach(r => o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${Y(r.v)}" y2="${Y(r.v)}" stroke="#999" stroke-dasharray="3 3"/>${r.lab ? `<text x="${W - m.r - 4}" y="${Y(r.v) - 4}" text-anchor="end" font-size="10.5" fill="${MUTED}">${esc(r.lab)}</text>` : ""}`));
  // selected point: a light band
  const sp = pts.find(p => p.k === cfg.sel);
  if (sp) o.push(`<rect x="${X(sp.x) - 9}" y="${m.t - 4}" width="18" height="${H - m.t - m.b + 8}" rx="6" fill="#F6EBEB"/>`);
  const minD = short ? 22 : 34, shown = sp ? [X(sp.x)] : [];
  pts.forEach(p => {
    const px = X(p.x);
    if (p !== sp && shown.some(q => Math.abs(q - px) < minD)) return;
    if (p !== sp) shown.push(px);
    const lab = short && p.k !== "polls" && /^\d{4}$/.test(p.lab) ? "’" + p.lab.slice(2) : p.lab;
    o.push(`<text x="${X(p.x)}" y="${H - m.b + 15}" text-anchor="middle" font-size="11" fill="${p.k === cfg.sel ? ACCENT : MUTED}" font-weight="${p.k === cfg.sel ? 600 : 400}" ${p.k === "polls" ? 'font-style="italic"' : ""}>${esc(lab)}</text>`);
  });
  const segs = s => { // runs of consecutive points with values; polls joins by a dashed link
    const out = [], ep = pts.filter(p => p.k !== "polls");
    let cur = [];
    ep.forEach(p => { const v = s.get(p.k); if (isNum(v)) cur.push([X(p.x), Y(v)]); else { if (cur.length) out.push(cur); cur = []; } });
    if (cur.length) out.push(cur);
    return out;
  };
  if (cfg.area) {
    const [a, b] = cfg.area.map(i => cfg.series[i]), ep = pts.filter(p => p.k !== "polls" && isNum(a.get(p.k)) && isNum(b.get(p.k)));
    if (ep.length > 1) {
      const top = ep.map(p => `${X(p.x).toFixed(1)},${Y(a.get(p.k)).toFixed(1)}`), bot = ep.map(p => `${X(p.x).toFixed(1)},${Y(b.get(p.k)).toFixed(1)}`).reverse();
      o.push(`<path d="M${top.join("L")}L${bot.join("L")}Z" fill="${cfg.areaColour || ACCENT}" fill-opacity=".08"/>`);
    }
  }
  const fade = s => cfg.hl && cfg.hl !== s.name;
  const order = [...cfg.series].sort((a, b) => (fade(b) ? 1 : 0) - (fade(a) ? 1 : 0)); // highlighted last (on top)
  order.forEach(s => {
    const op = fade(s) ? 0.15 : 1, w = (s.width || 1.8) * (cfg.hl === s.name ? 1.4 : 1);
    segs(s).forEach(run => { if (run.length > 1) o.push(`<path d="M${run.map(q => q.map(n => n.toFixed(1)).join(",")).join("L")}" fill="none" stroke="${s.colour}" stroke-width="${w}" stroke-opacity="${op}" ${s.dash ? `stroke-dasharray="${s.dash}"` : ""} stroke-linejoin="round"/>`); });
    const pp = pts.find(p => p.k === "polls");
    if (pp && isNum(s.get("polls"))) {
      const last = [...pts].reverse().find(p => p.k !== "polls" && isNum(s.get(p.k)));
      if (last) o.push(`<path d="M${X(last.x)},${Y(s.get(last.k))}L${X(pp.x)},${Y(s.get("polls"))}" stroke="${s.colour}" stroke-width="1.2" stroke-dasharray="2 3" stroke-opacity="${op}" fill="none"/>`);
    }
    pts.forEach(p => {
      const v = s.get(p.k); if (!isNum(v)) return;
      const big = p.k === cfg.sel, r = big ? 4.6 : 3.2, open = p.k === "polls" || s.open;
      o.push(marker(s.marker || "dot", X(p.x), Y(v), r, open ? "#fff" : s.colour, open ? s.colour : "#fff", `opacity="${op}"`));
    });
  });
  if (cfg.endLabels) {
    const labs = cfg.series.map(s => { const lp = [...pts].reverse().find(p => isNum(s.get(p.k))); return lp ? { s, y: Y(s.get(lp.k)), y0: Y(s.get(lp.k)), x: X(lp.x) } : null; }).filter(Boolean);
    repel(labs, 13, m.t, H - m.b);
    labs.forEach(l => o.push(`<line x1="${l.x + 6}" x2="${W - m.r + 6}" y1="${l.y0}" y2="${l.y}" stroke="#DDD"/><text x="${W - m.r + 9}" y="${l.y + 4}" font-size="11.5" font-weight="500" fill="${l.s.colour}">${esc(endName(l.s))}</text>`));
  }
  if (cfg.yLabel) o.push(`<text x="${m.l}" y="${m.t - 2}" font-size="10.5" fill="${MUTED}">${esc(cfg.yLabel)}</text>`);
  put(svgId, W, H, o.join(""), cfg.title);
  CH[svgId] = {
    W, box: $(boxId),
    near(mx) { let b = null, bd = Infinity; pts.forEach(p => { const d = Math.abs(X(p.x) - mx); if (d < bd) { bd = d; b = p; } }); return bd < 40 ? b : null; },
    move(mx, my) {
      if (mx < m.l - 10 || mx > W - m.r + 10) return null;
      const p = this.near(mx); if (!p) return null;
      $(svgId).querySelector(".hov").innerHTML = `<line x1="${X(p.x)}" x2="${X(p.x)}" y1="${m.t}" y2="${H - m.b}" stroke="${INK}" stroke-opacity=".3"/>`;
      const rows = cfg.series.map(s => ({ s, v: s.get(p.k) })).filter(r => isNum(r.v));
      if (!rows.length && !cfg.tipExtra) return null;
      if (cfg.tipSort) rows.sort((a, b) => b.v - a.v);
      return tipHead(cfg.tipHead ? cfg.tipHead(p.k) : ptTitle(p.k), cfg.tipSub ? cfg.tipSub(p.k) : "") +
        rows.map(r => tipRow(r.s.colour, r.s.name, cfg.fmt(r.v))).join("") + (cfg.tipExtra ? cfg.tipExtra(p.k) : "");
    },
    click(mx) { const p = this.near(mx); if (p && cfg.onPick) cfg.onPick(p.k); }
  };
}

// ---- strip, hash, links ---------------------------------------------------------------------
function renderStrip() {
  $("elections").innerHTML = COLS().map(e => `<button type="button" data-e="${e}" class="${e === "polls" ? "polls" : ""}" aria-pressed="${e === S.e}" title="${e === "polls" ? `Polls, ${dstr(POLL.date)}` : `Sejm election of ${dlong(EBY[e].date)}`}">${e === "polls" ? "Polls" : e}</button>`).join("");
  const b = $("elections").querySelector('[aria-pressed="true"]');
  if (b && b.scrollIntoView) { const seg = $("elections"); seg.scrollLeft = Math.max(0, b.offsetLeft - seg.clientWidth / 2); }
}
function hashTarget() { try { if (window.top !== window && window.top.location.host === location.host) return window.top; } catch (e) { /* cross-origin */ } return window; }
function stateString(section) {
  const q = new URLSearchParams();
  if (S.e !== YEARS[YEARS.length - 1]) q.set("e", S.e);
  if (S.frag !== "lt") q.set("f", S.frag);
  if (S.vol !== "ab") q.set("v", S.vol);
  if (S.nat !== "constituency") q.set("n", S.nat === "gmina" ? "g" : "c");
  if (S.pol !== "galtan") q.set("p", S.pol);
  if (S.gov !== "n") q.set("g", S.gov);
  if (section && section !== "overview") q.set("s", section);
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
  S.e = YEARS[YEARS.length - 1];
  const e = q.get("e");
  if (e === "polls") S.e = "polls"; else if (e && YEARS.includes(+e)) S.e = +e;
  if (["lt", "golosov", "inv"].includes(q.get("f"))) S.frag = q.get("f");
  if (["ab", "fam", "plain", "new"].includes(q.get("v"))) S.vol = q.get("v");
  if (q.get("n") === "g") S.nat = "gmina";
  if (["galtan", "lrgen", "lrecon", "map"].includes(q.get("p"))) S.pol = q.get("p");
  if (q.get("g") === "d") S.gov = "d";
  return q.get("s");
}
function currentSection() {
  let cur = "overview";
  document.querySelectorAll(".sec").forEach(s => { if (s.getBoundingClientRect().top < 90) cur = s.id; });
  return cur;
}
function shareUrl() {
  const t = hashTarget(), s = stateString(currentSection());
  return t.location.origin + t.location.pathname + (s ? "#" + s : "");
}

// ---- overview -------------------------------------------------------------------------------
function govCodes(g) {
  if (!g || !g.coded) return `<span class="codes" title="The first cabinet after 1991 is the baseline"><i class="x"></i><i class="x"></i><i class="x"></i></span>`;
  const c = [g.alternation !== "partial", g.familiar, g.access === "closed"];
  return `<span class="codes">${c.map(x => `<i class="${x ? "c" : ""}"></i>`).join("")}</span>`;
}
function spark(vals) {
  const w = 74, h = 18, ok = vals.map((v, i) => [i, v]).filter(p => isNum(p[1]));
  if (ok.length < 2) return "";
  const lo = Math.min(...ok.map(p => p[1])), hi = Math.max(...ok.map(p => p[1])), n = vals.length - 1;
  const X = i => 2 + i / n * (w - 4), Y = v => h - 3 - (v - lo) / (hi - lo || 1) * (h - 6);
  const si = YEARS.indexOf(S.e), sv = si >= 0 ? vals[si] : null;
  return `<svg width="${w}" height="${h}" viewBox="0 0 ${w} ${h}" aria-hidden="true"><path d="M${ok.map(p => `${X(p[0]).toFixed(1)},${Y(p[1]).toFixed(1)}`).join("L")}" fill="none" stroke="${ACCENT}" stroke-width="1.3"/>${isNum(sv) ? `<circle cx="${X(si)}" cy="${Y(sv)}" r="2.6" fill="${ACCENT}"/>` : ""}</svg>`;
}
function renderOverview() {
  const cols = COLS(), o = [];
  o.push(`<thead><tr><th class="ind" style="cursor:default;color:var(--muted)">Indicator</th><th class="sp" style="cursor:default"></th>${cols.map(e => `<th data-e="${e}" class="${e === S.e ? "sel" : ""} ${e === "polls" ? "pc" : ""}" title="Select ${e === "polls" ? "the polls" : e}">${e === "polls" ? "Polls" : e}</th>`).join("")}</tr></thead><tbody>`);
  GROUPS.forEach(g => {
    if (g.sec !== "overview") o.push(`<tr class="grp"><th colspan="${cols.length + 2}" data-sec="${g.sec}"><span class="label">${esc(g.label)}</span></th></tr>`);
    g.rows.forEach(r => {
      const desc = r.desc || D.codebook[r.k] || "";
      const lab = `<th class="ind" data-sec="${g.sec}" data-row="${r.k}">${esc(r.lab)}${desc ? `<i class="info" title="${esc(desc)}">i</i>` : ""}</th>`;
      if (r.gov) {
        o.push(`<tr>${lab}<td></td>${cols.map(e => { const g0 = cabinetsAfter(e)[0]; return `<td class="v ${e === S.e ? "sel" : ""} ${e === "polls" ? "pc" : ""}" data-e="${e}" title="${g0 ? esc(g0.name + ": " + g0.parties.join(", ")) : ""}">${g0 ? govCodes(g0) : `<span class="na">${NA}</span>`}</td>`; }).join("")}</tr>`);
        return;
      }
      const vals = cols.map(e => val(r.k, e)), ok = vals.filter(isNum), lo = Math.min(...ok), hi = Math.max(...ok);
      o.push(`<tr>${lab}<td class="spark">${spark(vals.slice(0, YEARS.length))}</td>${cols.map((e, i) => {
        const v = vals[i];
        if (!isNum(v)) return `<td class="v na ${e === S.e ? "sel" : ""} ${e === "polls" ? "pc" : ""}" data-e="${e}">${NA}</td>`;
        const a = hi > lo ? 0.04 + 0.30 * (v - lo) / (hi - lo) : 0.1;
        return `<td class="v ${e === S.e ? "sel" : ""} ${e === "polls" ? "pc" : ""}" data-e="${e}" style="background:rgba(139,0,0,${a.toFixed(3)})">${r.f(v)}</td>`;
      }).join("")}</tr>`);
    });
  });
  o.push(`</tbody>`);
  $("scorecard").innerHTML = o.join("");
  const sc = $("scorecard").parentElement, th = sc.querySelector("thead th.sel"), lab = sc.querySelector("thead th.ind");
  if (th && sc.scrollWidth > sc.clientWidth) { // on narrow screens, keep the selected column in view
    const left = th.offsetLeft - lab.offsetWidth, right = th.offsetLeft + th.offsetWidth;
    if (left < sc.scrollLeft || right > sc.scrollLeft + sc.clientWidth) sc.scrollLeft = Math.max(0, right - sc.clientWidth + 8);
  }
}

// ---- fragmentation --------------------------------------------------------------------------
const FRAG = {
  lt: { lab: "Laakso–Taagepera", v: "enp_votes", s: "enp_seats", note: "1 / Σ v², over every list's share" },
  golosov: { lab: "Golosov", v: "golosov_votes", s: "golosov_seats", note: "Σ vᵢ / (vᵢ + v₁² − vᵢ²), with v₁ the largest share" },
  inv: { lab: "1 / largest share", v: "n_largest", s: null, note: "the inverse of the largest list's vote share" }
};
function renderFrag() {
  $("fragm").innerHTML = Object.entries(FRAG).map(([k, f]) => `<button type="button" data-k="${k}" aria-pressed="${S.frag === k}">${f.lab}</button>`).join("");
  const F = FRAG[S.frag];
  const series = [{ name: "Votes", colour: ACCENT, get: k => val(F.v, k) }];
  if (F.s) series.push({ name: "Seats", colour: INK, marker: "tri", dash: "5 3", get: k => val(F.s, k) });
  lineChart("frag", "fragbox", {
    pts: electionPts(), series, fmt: f2, sel: S.e, ymin: 0, endLabels: true, area: F.s ? [0, 1] : null,
    yLabel: S.frag === "inv" ? "Inverse of the largest share" : "Effective number of parties",
    title: `${F.lab}: ${F.s ? "votes (solid) and seats (dashed)" : "inverse of the largest list's vote share"}`,
    onPick: setElection
  });
  lineChart("count", "countbox", {
    pts: electionPts(), H: 190, log: true, title: "Lists standing, clearing 5% and winning seats (log scale)", ymin: 1, ymax: 150, fmt: int, sel: S.e, endLabels: true,
    series: [
      { name: "Standing", colour: MUTED, get: k => k === "polls" ? null : val("lists", k) },
      { name: "Over 5%", colour: ACCENT, marker: "sq", get: k => val("lists_above_5", k) },
      { name: "Won seats", colour: INK, marker: "tri", get: k => val("parliamentary_parties", k) }
    ],
    onPick: setElection
  });
  renderFragPanel();
}
function listsFor(e) { // vote and seat shares behind one column, largest first
  if (e === "polls") return POLL.parties.map(p => ({ list: p.party, name: p.party, v: p.v, s: p.s, seats: p.seats, col: pollCol(p.party) })).sort((a, b) => b.v - a.v);
  return (LISTS_BY_YEAR[e] || []).map(l => ({ list: shortList(l.list), name: l.name, v: l.v, s: l.s, seats: l.seats, col: listCol(l), family: l.family, votes: l.votes })).sort((a, b) => b.v - a.v);
}
function partyBars(svgId, boxId, rows, opts = {}) { // horizontal vote bars with seat bars beneath
  const W = frameW(boxId), rh = 26, lw = clamp(Math.max(...rows.map(r => r.list.length)) * 6.6 + 10, 54, 130), vw = 92;
  const H = rows.length * rh + 22, mx = Math.max(...rows.map(r => Math.max(r.v, r.s || 0)), 0.05);
  const X = v => lw + v / mx * (W - lw - vw - 6);
  const o = [];
  ticks(0, mx, 4).forEach(t => o.push(`<line x1="${X(t)}" x2="${X(t)}" y1="0" y2="${H - 18}" stroke="${GRID}"/><text x="${X(t)}" y="${H - 5}" text-anchor="middle" font-size="10.5" fill="${MUTED}">${Math.round(t * 100)}%</text>`));
  rows.forEach((r, i) => {
    const y = i * rh + 3;
    o.push(`<g data-i="${i}" style="cursor:default"><rect x="0" y="${y - 2}" width="${W}" height="${rh}" fill="transparent"/>`);
    o.push(`<text x="${lw - 7}" y="${y + 13}" text-anchor="end" font-size="12" font-weight="500" fill="${INK}">${esc(r.list)}</text>`);
    o.push(`<rect x="${lw}" y="${y + 2}" width="${Math.max(0.5, X(r.v) - lw)}" height="9" rx="2" fill="${r.col}"/>`);
    if (r.s != null) o.push(`<rect x="${lw}" y="${y + 13}" width="${Math.max(r.s > 0 ? 1 : 0, X(r.s) - lw)}" height="5" rx="1.5" fill="${r.col}" fill-opacity=".45"/>`);
    o.push(`<text x="${W - vw + 4}" y="${y + 13}" font-size="11.5" fill="${INK}">${pct(r.v)}<tspan fill="${MUTED}"> · ${r.s != null ? pct(r.s) : ""}</tspan></text></g>`);
  });
  put(svgId, W, H, o.join(""));
  CH[svgId] = {
    W, box: $(boxId),
    move(mx_, my, e) {
      const g = e.target.closest("[data-i]"); if (!g) return null;
      const r = rows[+g.dataset.i];
      return tipHead(r.list, r.name !== r.list ? esc(r.name) : "") + tipRow(r.col, "Vote", pct(r.v, 2)) + (r.s != null ? tipRow(null, "Seats", `${r.seats != null ? int(r.seats) + " · " : ""}${pct(r.s, 1)}`) : "") + (opts.extra ? opts.extra(r) : "");
    }
  };
}
function renderFragPanel() {
  const e = S.e, F = FRAG[S.frag], all = listsFor(e);
  const shown = all.length > 11 ? all.slice(0, 10) : all, rest = all.slice(shown.length);
  const rows = [...shown];
  if (rest.length) rows.push({ list: `${rest.length} more`, name: `${rest.length} smaller lists`, v: sum(rest.map(r => r.v)), s: sum(rest.map(r => r.s)), seats: sum(rest.map(r => r.seats || 0)), col: "#BDBDBD" });
  const st = id => val(id, e);
  $("fragpanel").innerHTML = `<h3>Inside <b>${esc(eLabel(e))}</b>${e === "polls" ? ` <span class="muted" style="font-size:13px">${dstr(POLL.date)}</span>` : ""}</h3>
    <div class="kpis"><span>${esc(F.lab)} <b>${f2(st(F.v))}</b>${F.s ? ` votes · <b>${f2(st(F.s))}</b> seats` : ""}</span></div>
    <div class="kpis"><span>Lists ${e === "polls" ? "polled" : "standing"} <b>${e === "polls" ? POLL.parties.filter(p => p.v > 0).length : int(st("lists"))}</b></span><span>over 5% <b>${int(st("lists_above_5"))}</b></span><span>won seats <b>${int(st("parliamentary_parties"))}</b></span></div>
    <div class="chartbox plain" id="fragbarsbox"><svg id="fragbars" role="img" aria-label="Vote and seat shares of each list"></svg><div class="tip" hidden></div></div>
    <p class="note">Thick bars are vote shares, thin bars seat shares. ${esc(F.lab)}: ${esc(F.note)}.${e === "polls" ? ` Poll shares leave out Other (${pct(POLL.other)}) and are rescaled; seats are the tracker's median projection${POLL.seatTotal !== 460 ? ` (medians, so ${POLL.seatTotal} in all)` : ""}.` : ""}${e === 1991 ? " With no national threshold, 29 lists won seats in 1991." : ""}</p>`;
  bindChart("fragbars");
  partyBars("fragbars", "fragbarsbox", rows);
  $("fragbars").dataset.title = `Vote (thick) and seat (thin) shares, ${eLabel(e)}`;
}

// ---- disproportionality ---------------------------------------------------------------------
function renderDisp() {
  const common = { pts: electionPts(), sel: S.e, H: 150, onPick: setElection, ml: 34 };
  lineChart("dispg", "dispbox", { ...common, title: "Gallagher index", ymin: 0, fmt: v => pts(v), fmtAxis: v => (100 * v).toFixed(0), series: [{ name: "Gallagher index", colour: ACCENT, get: k => val("gallagher", k) }] });
  lineChart("dispw", "dispbox", { ...common, title: "Wasted votes, % of valid votes", ymin: 0, fmt: v => pct(v), fmtAxis: v => (100 * v).toFixed(0), series: [{ name: "Wasted votes", colour: ACCENT, get: k => val("wasted_votes", k) }] });
  lineChart("dispb", "dispbox", { ...common, title: "Seat bonus of the largest list, points", ymin: 0, fmt: v => signedPts(v), fmtAxis: v => (100 * v).toFixed(0), series: [{ name: "Seat bonus", colour: ACCENT, get: k => val("seat_bonus", k) }] });
  renderDispPanel();
}
function renderDispPanel() {
  const e = S.e, rows = listsFor(e).filter(r => r.v >= 0.002 || r.s > 0);
  $("disppanel").innerHTML = `<h3>Votes and seats in <b>${esc(eLabel(e))}</b></h3>
    <div class="kpis"><span>Gallagher <b>${pts(val("gallagher", e))}</b></span><span>wasted votes <b>${pct(val("wasted_votes", e))}</b></span><span>seat bonus <b>${signedPts(val("seat_bonus", e))}</b> pts</span></div>
    <div class="chartbox plain" id="vsbox"><svg id="vs" role="img" aria-label="Each list's seat share against its vote share"></svg><div class="tip" hidden></div></div>
    <p class="note">Each dot is a list. Above the diagonal a list won more seats than votes, below it fewer; the Gallagher index is the root of half the summed squared gaps.${e === 1991 ? " There was no national threshold in 1991." : " The dashed line is the 5% threshold for parties (coalitions need more; the German minority is exempt)."}</p>`;
  bindChart("vs");
  const W = frameW("vsbox"), H = Math.round(clamp(W * 0.86, 260, 380)), m = { l: 38, r: 12, t: 10, b: 32 };
  const mx = Math.max(...rows.map(r => Math.max(r.v, r.s)), 0.1) * 1.06, [, top] = niceDomain(0, mx, 4);
  const X = v => m.l + v / top * (W - m.l - m.r), Y = v => H - m.b - v / top * (H - m.t - m.b);
  const o = [];
  ticks(0, top, 4).forEach(t => o.push(`<line x1="${X(t)}" x2="${X(t)}" y1="${m.t}" y2="${H - m.b}" stroke="${GRID}"/><line x1="${m.l}" x2="${W - m.r}" y1="${Y(t)}" y2="${Y(t)}" stroke="${GRID}"/><text x="${X(t)}" y="${H - m.b + 14}" text-anchor="middle" font-size="10.5" fill="${MUTED}">${Math.round(t * 100)}%</text><text x="${m.l - 5}" y="${Y(t) + 4}" text-anchor="end" font-size="10.5" fill="${MUTED}">${Math.round(t * 100)}%</text>`));
  o.push(`<text x="${(m.l + W - m.r) / 2}" y="${H - 3}" text-anchor="middle" font-size="11" fill="${MUTED}">Vote share</text><text x="${m.l + 4}" y="${m.t + 10}" font-size="11" fill="${MUTED}">Seat share</text>`);
  o.push(`<line x1="${X(0)}" y1="${Y(0)}" x2="${X(top)}" y2="${Y(top)}" stroke="#999"/>`);
  if (e !== 1991) o.push(`<line x1="${X(0.05)}" x2="${X(0.05)}" y1="${m.t}" y2="${H - m.b}" stroke="#999" stroke-dasharray="3 3"/>`);
  const labs = [];
  rows.forEach((r, i) => {
    o.push(`<circle data-i="${i}" cx="${X(r.v)}" cy="${Y(r.s)}" r="${r.s > 0 ? 5 : 3.5}" fill="${r.s > 0 ? r.col : "#fff"}" stroke="${r.col}" stroke-width="1.5" style="cursor:default"/>`);
    if (r.v >= 0.04 || r.s >= 0.04) labs.push({ text: r.list, x: X(r.v) + 7, y: Y(r.s) - 4 });
  });
  declutter(labs).forEach(l => o.push(`<text x="${l.x}" y="${l.y + 4}" font-size="11" font-weight="500" fill="${INK}" pointer-events="none">${esc(l.text)}</text>`));
  put("vs", W, H, o.join(""), `Seat share against vote share, ${eLabel(e)}`);
  CH.vs = {
    W, box: $("vsbox"),
    move(mx_, my, ev) {
      const c = ev.target.closest("[data-i]"); if (!c) return null;
      const r = rows[+c.dataset.i];
      return tipHead(r.list, r.name !== r.list ? esc(r.name) : "") + tipRow(r.col, "Vote", pct(r.v, 2)) + tipRow(null, "Seats", `${int(r.seats)} · ${pct(r.s, 1)}`) + tipRow(null, "Seats minus votes", signedPts(r.s - r.v) + " pts");
    }
  };
}

// ---- volatility -----------------------------------------------------------------------------
const VOL = {
  ab: { lab: "New vs continuing", parts: [["volatility_type_a", "Type A: new and withdrawn lists", ACCENT], ["volatility_type_b", "Type B: continuing lists", "#E3B9B9"]] },
  fam: { lab: "Between vs within families", parts: [["volatility_between_family", "Between party families", ACCENT], ["volatility_within_family", "Within party families", "#E3B9B9"]] },
  plain: { lab: "Families, plain", parts: [["volatility_between_family_plain", "Between party families", ACCENT], ["volatility_within_family_plain", "Within party families", "#E3B9B9"]], total: true },
  new: { lab: "Vote for new lists", parts: [["new_genuine", "Genuinely new", ACCENT], ["new_splinter", "Splinters", "#C0504D"], ["new_returning", "Returning", "#E8A9A9"], ["new_unidentified", "No party id", "#F3F3F1"]] }
};
const VOL_NOTES = {
  ab: "Type A is the vote won by new lists and lost by withdrawn ones (lists with no party id are always one or the other); Type B is the change among lists that continue (Powell and Tucker).",
  fam: "Between-family volatility is the Pedersen index across Party tracker's 14 party families, each list at the earlier election taking the family of the list it continues into; the rest is change within families.",
  plain: "The plain version keeps each list in its own family at both elections. Where a lineage changes family its vote counts as leaving one family and entering another, so between and within can add up to more than the total (the black tick), as in 2019.",
  new: "The vote for lists with no predecessor at the previous election: genuinely new lists, splinters (a party that stood last time on a list that continues elsewhere, or one born of a split), returning parties that sat out the previous election, and lists with no party id."
};
function stackedBars(svgId, boxId, cfg) { // cfg: cats [k], parts [[get, name, colour]], total(k), H, fmt, sel, onPick, pct100
  const W = frameW(boxId), H = cfg.H || Math.round(clamp(W * 0.42, 200, 300)), m = { l: 34, r: 8, t: 14, b: 24 };
  const cats = cfg.cats, n = cats.length, bw = (W - m.l - m.r) / n, barW = Math.min(46, bw * 0.66);
  const tot = k => sum(cfg.parts.map(p => p[0](k) || 0));
  const top = cfg.pct100 ? 1 : niceDomain(0, Math.max(...cats.map(k => Math.max(tot(k), cfg.total ? cfg.total(k) || 0 : 0))), 4)[1];
  const Y = v => H - m.b - v / top * (H - m.t - m.b), X = i => m.l + bw * i + bw / 2;
  const o = [];
  ticks(0, top, 4).forEach(t => o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${Y(t)}" y2="${Y(t)}" stroke="${GRID}"/><text x="${m.l - 5}" y="${Y(t) + 4}" text-anchor="end" font-size="10.5" fill="${MUTED}">${Math.round(t * 100)}</text>`));
  cats.forEach((k, i) => {
    const sel = k === cfg.sel, short = W < 560;
    if (sel) o.push(`<rect x="${X(i) - bw / 2 + 2}" y="${m.t - 6}" width="${bw - 4}" height="${H - m.t - m.b + 6}" rx="6" fill="#F6EBEB"/>`);
    let y0 = 0;
    cfg.parts.forEach(p => {
      const v = p[0](k) || 0; if (v <= 0) return;
      o.push(`<rect x="${X(i) - barW / 2}" y="${Y(y0 + v)}" width="${barW}" height="${Math.max(0.5, Y(y0) - Y(y0 + v))}" fill="${p[2]}" stroke="${p[2] === "#F3F3F1" ? "#BDBDBD" : "#fff"}" stroke-width=".6"/>`);
      y0 += v;
    });
    if (cfg.total) { const t = cfg.total(k); if (isNum(t)) o.push(`<line x1="${X(i) - barW / 2 - 4}" x2="${X(i) + barW / 2 + 4}" y1="${Y(t)}" y2="${Y(t)}" stroke="${INK}" stroke-width="2"/>`); }
    if (!cfg.pct100 && barW >= 22) o.push(`<text x="${X(i)}" y="${Y(cfg.total ? Math.max(y0, cfg.total(k) || 0) : y0) - 4}" text-anchor="middle" font-size="10.5" fill="${INK}">${(100 * (cfg.total ? cfg.total(k) : y0)).toFixed(0)}</text>`);
    o.push(`<text x="${X(i)}" y="${H - 8}" text-anchor="middle" font-size="11" fill="${sel ? ACCENT : MUTED}" font-weight="${sel ? 600 : 400}">${short ? "’" + String(k).slice(2) : k}</text>`);
  });
  put(svgId, W, H, o.join(""), cfg.title);
  CH[svgId] = {
    W, box: $(boxId),
    cat(mx) { const i = Math.floor((mx - m.l) / bw); return i >= 0 && i < n ? cats[i] : null; },
    move(mx) {
      const k = this.cat(mx); if (k == null) return null;
      const rows = cfg.parts.map(p => ({ p, v: p[0](k) })).filter(r => isNum(r.v) && (r.v > 0 || !cfg.pct100));
      if (cfg.sortTip) rows.sort((a, b) => b.v - a.v);
      return tipHead(cfg.tipTitle ? cfg.tipTitle(k) : `${k} election`) + rows.map(r => tipRow(r.p[2], r.p[1], cfg.fmt(r.v))).join("") + (cfg.tipExtra ? cfg.tipExtra(k) : "");
    },
    click(mx) { const k = this.cat(mx); if (k != null && cfg.onPick) cfg.onPick(k); }
  };
}
function renderVol() {
  $("volm").innerHTML = Object.entries(VOL).map(([k, v]) => `<button type="button" data-k="${k}" aria-pressed="${S.vol === k}">${v.lab}</button>`).join("");
  const V = VOL[S.vol];
  stackedBars("vol", "volbox", {
    cats: YEARS.slice(1), sel: S.e, fmt: v => S.vol === "new" ? pct(v) : pts(v), onPick: setElection,
    title: `${V.lab}: ${V.parts.map(p => p[1]).join(", ")}${V.total ? "; black tick: total volatility" : ""}`,
    parts: V.parts.map(p => [k => val(p[0], k), p[1], p[2]]),
    total: V.total ? k => val("volatility_total", k) : null,
    tipExtra: k => tipRow(null, S.vol === "new" ? "All new lists" : "Total volatility", S.vol === "new" ? pct(val("new_list_vshare", k)) : pts(val("volatility_total", k)))
  });
  $("volnote").innerHTML = `${V.parts.map(p => `<span style="display:inline-flex;align-items:center;gap:5px;margin-right:12px"><span class="sw" style="background:${p[2]};${p[2] === "#F3F3F1" ? "border:1px solid #BDBDBD" : ""}"></span>${esc(p[1])}</span>`).join("")}${V.total ? `<span style="display:inline-flex;align-items:center;gap:5px"><span style="width:14px;height:2px;background:${INK};display:inline-block"></span>Total volatility</span>` : ""}<br>${S.vol === "new" ? "Percent of valid votes." : "Pedersen index, points of the vote."} ${esc(VOL_NOTES[S.vol])}`;
  const fams = D.families.map(f => f.id);
  const share = (y, f) => { const r = D.familyShares.find(x => x.year === y && x.family === f); return r ? r.v : 0; };
  stackedBars("fam", "fambox", {
    cats: YEARS, sel: S.e, H: 190, pct100: true, title: "Vote by party family, %", fmt: v => pct(v), onPick: setElection, sortTip: true,
    parts: fams.map(f => [y => share(y, f), famLab(f), famCol(f)])
  });
  renderVolPanel();
}
function renderVolPanel() {
  const e = S.e, p = $("volpanel");
  if (e === "polls" || e === YEARS[0]) {
    p.innerHTML = `<h3>Lineages into <b>${esc(eLabel(e))}</b></h3><div class="empty">${e === "polls" ? "Volatility needs results at both ends, so there is none for the polls. Pick an election from 1993 on." : "1991 is the first election in the series, so volatility starts with 1993."}</div>`;
    return;
  }
  const yp = prevYear(e), E = EBY[e];
  p.innerHTML = `<h3>Lineages from <b>${yp}</b> into <b>${e}</b></h3>
    <div class="kpis"><span>Total <b>${pts(E.volatility_total)}</b></span><span>Type A <b>${pts(E.volatility_type_a)}</b></span><span>Type B <b>${pts(E.volatility_type_b)}</b></span><span>new lists <b>${pct(E.new_list_vshare)}</b></span></div>
    <div class="chartbox plain" id="linbox"><svg id="lin" role="img" aria-label="How the lists of one election continue into the next"></svg><div class="tip" hidden></div></div>
    <p class="note">Bands join each list to the list it continues into, at its vote share; lists with no band withdrew (left) or are new (right). These are lineages of lists, not flows of voters. Lists under 1% that withdrew or are new are pooled. <a href="../../pages/polish-political-analyses/party-genealogy.html" target="_top">The party genealogy</a> traces the parties themselves.</p>`;
  bindChart("lin");
  lineageChart(e, yp);
}
function lineageChart(y, yp) {
  const W = frameW("linbox"), right = D.lineages.filter(r => r.year === y && r.at === "t");
  const leftAll = (LISTS_BY_YEAR[yp] || []).map(l => ({ ...l }));
  const succ = {}; right.forEach(r => r.pred.forEach(id => { succ[id] = r.id; }));
  const STATUS = { "new: genuinely new": "genuinely new", "new: splinter": "splinter", "new: returning": "returning", "new: no party id": "no party id", "continuing (second list)": "second list" };
  // right nodes: continuing lists, then new; minor new lists pooled by kind
  let R = right.map(r => ({ ...r, col: listCol({ label: (LISTS_BY_YEAR[y] || []).find(l => l.id === r.id)?.label, family: r.family }), isNew: r.status.startsWith("new") }));
  const pool = (arr, test, mk) => { const small = arr.filter(test); if (small.length < 2) return arr; return [...arr.filter(x => !test(x)), mk(small)]; };
  ["new: genuinely new", "new: splinter", "new: returning", "new: no party id"].forEach(st => {
    R = pool(R, r => r.status === st && r.v < 0.01, sm => ({ id: "pool:" + st, list: `${sm.length} small ${STATUS[st]}`, name: sm.map(x => x.list).join(", "), v: sum(sm.map(x => x.v)), status: st, pred: [], col: "#C9C9C9", isNew: true, pooled: sm.length }));
  });
  R.sort((a, b) => (a.isNew - b.isNew) || (b.v - a.v));
  const rIdx = Object.fromEntries(R.map((r, i) => [r.id, i]));
  let L = leftAll.map(l => ({ ...l, to: succ[l.id] ?? null, col: listCol(l) }));
  L = pool(L, l => l.to == null && l.v < 0.01, sm => ({ id: "pool:withdrawn", list: `${sm.length} small lists`, name: sm.map(x => x.list).join(", "), v: sum(sm.map(x => x.v)), to: null, col: "#C9C9C9", pooled: sm.length }));
  L.sort((a, b) => { const ra = a.to != null ? rIdx[a.to] : 1e6, rb = b.to != null ? rIdx[b.to] : 1e6; return (ra - rb) || (b.v - a.v); });
  const H = Math.round(clamp(W * 1.05, 380, 560)), lab = W < 420 ? 64 : 92, nodeW = 12, m = { t: 22, b: 8 };
  const xl = lab, xr = W - lab - nodeW, gap = 2.2;
  const tl = sum(L.map(l => l.v)), tr = sum(R.map(r => r.v)), k = (H - m.t - m.b - gap * Math.max(L.length, R.length)) / Math.max(tl, tr);
  let yy = m.t; L.forEach(l => { l.y = yy; l.h = Math.max(1, l.v * k); yy += l.h + gap; });
  yy = m.t; R.forEach(r => { r.y = yy; r.h = Math.max(1, r.v * k); yy += r.h + gap; });
  const o = [];
  o.push(`<text x="${xl + nodeW / 2}" y="12" text-anchor="middle" font-size="11" font-weight="600" fill="${MUTED}">${yp}</text><text x="${xr + nodeW / 2}" y="12" text-anchor="middle" font-size="11" font-weight="600" fill="${MUTED}">${y}</text>`);
  // bands: each right node is split among its predecessors in proportion to their earlier shares
  const LBY = Object.fromEntries(L.map(l => [l.id, l]));
  R.forEach(r => {
    const preds = (r.pred || []).map(id => LBY[id]).filter(Boolean), tot = sum(preds.map(p => p.v));
    let ry = r.y;
    preds.forEach(p => {
      const rh = tot > 0 ? r.h * p.v / tot : 0, x0 = xl + nodeW, x1 = xr, cx = (x0 + x1) / 2;
      o.push(`<path data-b="${p.id}|${r.id}" d="M${x0},${p.y}C${cx},${p.y} ${cx},${ry} ${x1},${ry}L${x1},${ry + rh}C${cx},${ry + rh} ${cx},${p.y + p.h} ${x0},${p.y + p.h}Z" fill="${r.col}" fill-opacity=".28" stroke="${r.col}" stroke-opacity=".2"/>`);
      ry += rh;
    });
  });
  const node = (n, x, side) => {
    const out = n.to === null && side === "l", isNew = side === "r" && n.isNew;
    o.push(`<rect data-n="${side}:${n.id}" x="${x}" y="${n.y}" width="${nodeW}" height="${n.h}" rx="2" fill="${out || isNew ? "#fff" : n.col}" stroke="${n.col}" stroke-width="${out || isNew ? 1.4 : 0}" ${out || isNew ? 'stroke-dasharray="3 2"' : ""} style="cursor:default"/>`);
    if (n.h >= 9) {
      const tx = side === "l" ? x - 6 : x + nodeW + 6, anchor = side === "l" ? "end" : "start", nm = n.pooled ? n.list : shortList(n.list);
      o.push(`<text x="${tx}" y="${n.y + n.h / 2 + 4}" text-anchor="${anchor}" font-size="11" fill="${INK}" pointer-events="none">${esc(nm.length > 12 && lab < 80 ? nm.slice(0, 10) + "…" : nm)} <tspan fill="${MUTED}">${(100 * n.v).toFixed(n.v < 0.1 ? 1 : 0)}</tspan></text>`);
    }
  };
  L.forEach(l => node(l, xl, "l"));
  R.forEach(r => node(r, xr, "r"));
  put("lin", W, H, o.join(""), `List lineages from ${yp} into ${y}`);
  const RBY = Object.fromEntries(R.map(r => [r.id, r]));
  CH.lin = {
    W, box: $("linbox"),
    move(mx, my, ev) {
      const t = ev.target.closest("[data-n],[data-b]"); if (!t) return null;
      if (t.dataset.b) {
        const [a, b] = t.dataset.b.split("|"), l = LBY[a], r = RBY[b];
        return tipHead(`${l.list} → ${r.list}`) + tipRow(l.col, `${yp}`, pct(l.v)) + tipRow(r.col, `${y} (whole list)`, pct(r.v));
      }
      const [side, id] = [t.dataset.n.slice(0, 1), t.dataset.n.slice(2)];
      if (side === "l") {
        const l = LBY[id], r = l.to != null ? RBY[l.to] : null;
        return tipHead(l.pooled ? l.list : shortList(l.list), esc(l.name)) + tipRow(l.col, `Vote in ${yp}`, pct(l.v, 2)) + (r ? tipRow(r.col, `Continues into ${r.list}`, pct(r.v, 2)) : `<div class="sub">${l.pooled ? "Lists that did not stand again" : "Did not continue into any list in " + y}</div>`);
      }
      const r = RBY[id], preds = (r.pred || []).map(i => LBY[i]).filter(Boolean), was = sum(preds.map(p => p.v));
      return tipHead(r.pooled ? r.list : shortList(r.list), esc(r.name)) + tipRow(r.col, `Vote in ${y}`, pct(r.v, 2)) +
        (preds.length ? tipRow(null, `Its predecessors in ${yp}`, pct(was, 2)) + `<div class="sub">${esc(preds.map(p => p.list).join(", "))}</div>` + (r.status === "continuing" ? tipRow(null, "Change", signedPts(r.v - was)) : "") : `<div class="sub">${esc(r.pooled ? "Pooled new lists" : (r.status === "continuing (second list)" ? "A second list of a continuing party" : r.status.replace("new: ", "New: ")))}</div>`);
    }
  };
}

// ---- nationalisation ------------------------------------------------------------------------
function natLineages() { // the lineages of the figure: lists with 5% or more and a palette label
  const lv = S.nat, by = {};
  D.nationalisation.forEach(n => {
    if (n.level !== lv || !n.l || !n.l.label || n.l.v < 0.05 || !isNum(n.spns)) return;
    (by[n.l.label] = by[n.l.label] || {})[n.year] = { spns: n.spns, l: n.l };
  });
  return Object.entries(by).map(([label, ys]) => ({ label, ys, colour: colOf(label) || "#808080", first: Math.min(...Object.keys(ys).map(Number)) })).sort((a, b) => a.first - b.first);
}
function renderNat() {
  $("natm").innerHTML = [["constituency", "Constituencies"], ["gmina", "Gminas"]].map(([k, l]) => `<button type="button" data-k="${k}" aria-pressed="${S.nat === k}">${l}</button>`).join("");
  const lins = natLineages();
  if (S.natHl && !lins.some(l => l.label === S.natHl)) S.natHl = null;
  $("natchips").innerHTML = `<button type="button" class="chip" data-hl="" aria-pressed="${!S.natHl}"><span class="sw" style="background:${INK}"></span>All seat-winning lists</button>` +
    lins.map(l => `<button type="button" class="chip" data-hl="${esc(l.label)}" aria-pressed="${S.natHl === l.label}"><span class="dot" style="background:${l.colour}"></span>${esc(l.label)}</button>`).join("");
  renderNatMain(S.natHl);
  lineChart("infl", "inflbox", {
    pts: electionPts(false), H: 190, sel: S.e, ymin: 0, fmt: f2, padX: 1.2, title: "ENP inflation: national party system against the average constituency",
    series: [
      { name: "National", colour: ACCENT, get: k => val("enp_national", k) },
      { name: "Average constituency", short: "Constituency", colour: INK, marker: "tri", open: true, dash: "4 3", get: k => val("enp_district_mean", k) }
    ],
    endLabels: true, area: [0, 1],
    tipExtra: k => tipRow(null, "Inflation", pct(val("enp_inflation", k))) + (k === 1991 ? `<div class="sub">1991: only the lists in the KBW constituency files</div>` : ""),
    onPick: setElection
  });
  renderNatPanel();
}
function renderNatMain(hl) { // the system score and the main lineages; hl brings one line forward
  const lins = natLineages(), sysKey = "psns_" + S.nat, sysName = "All seat-winning lists";
  const series = [{ name: sysName, colour: INK, dash: "5 3", width: 2, marker: "sq", get: k => val(sysKey, k) },
    ...lins.map(l => ({ name: l.label, colour: l.colour, get: k => l.ys[k] ? l.ys[k].spns : null }))];
  lineChart("nat", "natbox", {
    pts: electionPts(false), series, fmt: f3, sel: S.e, hl: hl || null, padX: 1.2, tipSort: true,
    yLabel: `Standardised party nationalisation score, ${S.nat === "gmina" ? "gminas" : "constituencies"}`,
    title: `Party nationalisation by ${S.nat === "gmina" ? "gmina" : "constituency"}: all seat-winning lists (dashed) and the main lineages`,
    tipExtra: k => k === 1991 ? `<div class="sub">1991: only the lists in the KBW files</div>` : "",
    onPick: setElection
  });
}
function renderNatPanel() {
  const e = S.e, p = $("natpanel"), lv = S.nat;
  if (e === "polls") { p.innerHTML = `<h3>Nationalisation in <b>the polls</b></h3><div class="empty">Polls give national shares only, so there are no nationalisation scores for them. Pick an election.</div>`; return; }
  const rows = D.nationalisation.filter(n => n.year === e && n.level === lv && isNum(n.spns) && n.l).map(n => ({ ...n, col: listCol(n.l), v: n.l.v, list: n.l.list, name: n.l.name, seats: n.l.seats }));
  const E = EBY[e];
  p.innerHTML = `<h3>Each list in <b>${e}</b></h3>
    <div class="kpis"><span>System score <b>${f3(E["psns_" + lv])}</b></span><span>${lv === "gmina" ? "gminas" : "constituencies"} <b>${int(E["units_" + lv])}</b></span><span>ENP inflation <b>${pct(E.enp_inflation)}</b></span></div>
    <div class="chartbox plain" id="nsbox"><svg id="ns" role="img" aria-label="Each list's nationalisation score against its vote share"></svg><div class="tip" hidden></div></div>
    <p class="note">Filled dots won seats; the dashed line is the system score, the vote-weighted mean of those lists. The vote axis is logarithmic. Regional lists, above all the German minority, sit at the bottom.${e === 1991 ? " The KBW files for 1991 omit 64 small lists (3% of the vote), so 1991 covers only the lists in them." : ""} Click a list to map its vote on <a href="../../pages/polish-political-analyses/election-results.html#e=sejm-${e}" target="_top">Election results</a>.</p>`;
  bindChart("ns");
  const W = frameW("nsbox"), H = Math.round(clamp(W * 0.8, 250, 360)), m = { l: 36, r: 12, t: 10, b: 32 };
  const vmin = 0.0005, vmax = 0.6, L = v => Math.log10(Math.max(v, vmin));
  const X = v => m.l + (L(v) - L(vmin)) / (L(vmax) - L(vmin)) * (W - m.l - m.r), Y = v => H - m.b - v * (H - m.t - m.b);
  const o = [];
  [0, 0.2, 0.4, 0.6, 0.8, 1].forEach(t => o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${Y(t)}" y2="${Y(t)}" stroke="${GRID}"/><text x="${m.l - 5}" y="${Y(t) + 4}" text-anchor="end" font-size="10.5" fill="${MUTED}">${t.toFixed(1)}</text>`));
  [0.001, 0.01, 0.05, 0.1, 0.5].forEach(t => o.push(`<line x1="${X(t)}" x2="${X(t)}" y1="${m.t}" y2="${H - m.b}" stroke="${GRID}"/><text x="${X(t)}" y="${H - m.b + 14}" text-anchor="middle" font-size="10.5" fill="${MUTED}">${t < 0.01 ? (t * 100).toFixed(1) : Math.round(t * 100)}%</text>`));
  o.push(`<text x="${(m.l + W - m.r) / 2}" y="${H - 3}" text-anchor="middle" font-size="11" fill="${MUTED}">Vote share</text>`);
  const sys = E["psns_" + lv];
  if (isNum(sys)) o.push(`<line x1="${m.l}" x2="${W - m.r}" y1="${Y(sys)}" y2="${Y(sys)}" stroke="${INK}" stroke-dasharray="5 3"/>`);
  rows.sort((a, b) => a.seats - b.seats);
  const named = new Set([...rows].filter(r => r.v >= 0.05).sort((a, b) => b.v - a.v).slice(0, 6).map(r => r.id));
  const labs = [];
  rows.forEach((r, i) => {
    o.push(`<circle data-i="${i}" cx="${X(r.v)}" cy="${Y(r.spns)}" r="${r.seats > 0 ? 5 : 3.5}" fill="${r.seats > 0 ? r.col : "#fff"}" stroke="${r.col}" stroke-width="1.4" style="cursor:pointer"/>`);
    if (named.has(r.id) || (r.seats > 0 && r.spns < 0.5)) labs.push({ text: shortList(r.list), x: X(r.v) + 7, y: Y(r.spns) });
  });
  declutter(labs).forEach(l => o.push(`<text x="${Math.min(l.x, W - l.w - 2)}" y="${l.y + 4}" font-size="11" font-weight="500" fill="${INK}" pointer-events="none">${esc(l.text)}</text>`));
  put("ns", W, H, o.join(""), `Each list's nationalisation score against its vote share, ${e}`);
  CH.ns = {
    W, box: $("nsbox"),
    move(mx, my, ev) {
      const c = ev.target.closest("[data-i]"); if (!c) return null;
      const r = rows[+c.dataset.i];
      return tipHead(r.list, r.name !== r.list ? esc(r.name) : "") + tipRow(r.col, "Nationalisation", f3(r.spns)) + tipRow(null, "Vote", pct(r.v, 2)) + tipRow(null, "Seats", int(r.seats)) + (r.coverage < 0.999 ? `<div class="sub">Files hold ${pct(r.coverage)} of its vote</div>` : "") + `<div class="sub">Click to map it on Election results</div>`;
    },
    // Election results maps the list by its committee id (it=), by gmina or constituency as here
    click(mx, my, ev) {
      const c = ev.target.closest("[data-i]"); if (!c) return;
      const r = rows[+c.dataset.i];
      hashTarget().location.href = `../../pages/polish-political-analyses/election-results.html#e=sejm-${e}&m=share&it=${encodeURIComponent(r.id)}${lv === "constituency" ? "&l=c" : ""}`;
    }
  };
}

// ---- polarisation ---------------------------------------------------------------------------
const DIMS = { galtan: { lab: "GAL–TAN", short: "GAL–TAN", col: ACCENT, marker: "dot", lo: "GAL", hi: "TAN" }, lrgen: { lab: "General left–right", short: "Left–right", col: INK, marker: "tri", lo: "Left", hi: "Right" }, lrecon: { lab: "Economic left–right", short: "Economic", col: "#8C8C8C", marker: "sq", lo: "Left", hi: "Right" } };
function renderPol() {
  $("polm").innerHTML = [...Object.entries(DIMS).map(([k, d]) => [k, d.lab]), ["map", "Map"]].map(([k, l]) => `<button type="button" data-k="${k}" aria-pressed="${S.pol === k}">${l}</button>`).join("");
  const W0 = D.ches.index, wf = waveFor(S.e);
  lineChart("pol", "polbox", {
    pts: W0.map(c => ({ k: c.wave, x: c.wave, lab: String(c.wave) })), padX: 1.5, sel: wf.c.wave, ymin: 0, fmt: f2, endLabels: true,
    hl: S.pol === "map" ? null : DIMS[S.pol].lab,
    series: Object.entries(DIMS).map(([k, d]) => ({ name: d.lab, short: d.short, colour: d.col, marker: d.marker, get: w => { const c = W0.find(x => x.wave === w); return c ? c[k] : null; } })),
    yLabel: "Dalton polarisation index (0–10)", tipHead: w => `CHES ${w}`, title: "Dalton polarisation index by CHES wave",
    tipSub: w => { const c = W0.find(x => x.wave === w); return `Weighted by the ${c.ref} election · ${c.lists} lists, ${pct(c.covered, 0)} of the vote`; },
    onPick: w => setElection(W0.find(x => x.wave === w).ref)
  });
  renderPolPanel();
}
function renderPolPanel() {
  const e = S.e, wf = waveFor(e), c = wf.c, rows = D.ches.lists.filter(r => r.wave === c.wave).map(r => {
    const l = (LISTS_BY_YEAR[r.ref] || []).find(x => x.id === r.id);
    return { ...r, col: l ? listCol(l) : "#808080", name: l ? l.name : r.list };
  });
  const dim = S.pol === "map" ? null : S.pol;
  const pnote = wf.exact ? "" : `<p class="note" style="color:var(--accent)">No CHES wave is weighted by ${esc(eLabel(e))}; this is ${wf.why} (${c.wave}, weighted by ${c.ref}).</p>`;
  $("polpanel").innerHTML = `<h3>Party positions, <b>CHES ${c.wave}</b></h3>
    <div class="kpis"><span>Weighted by <b>${c.ref}</b></span>${Object.entries(DIMS).map(([k, d]) => `<span>${d.lab} <b>${f2(c[k])}</b></span>`).join("")}</div>${pnote}
    <div class="chartbox plain" id="posbox"><svg id="pos" role="img" aria-label="Party positions from the Chapel Hill Expert Survey"></svg><div class="tip" hidden></div></div>
    <p class="note">${dim ? `Circles are sized by vote share at the ${c.ref} election; the triangle marks the vote-weighted mean. The index is ten times the square root of the vote-weighted mean squared distance from it, over 5.` : "Economic left–right across, GAL–TAN up (TAN at the top); circles sized by vote share."} A joint list takes the mean position of its parties.</p>`;
  bindChart("pos");
  const W = frameW("posbox"), o = [];
  const rad = v => 4 + Math.sqrt(v) * 26;
  if (dim) {
    const m = { l: 14, r: 14 }, X = p => m.l + p / 10 * (W - m.l - m.r);
    const sorted = [...rows].sort((a, b) => b.v - a.v), lanes = [];
    sorted.forEach(r => { // stack circles so they do not overlap
      const x = X(r[dim]), rr = rad(r.v);
      let lane = 0; while ((lanes[lane] || []).some(q => Math.abs(q.x - x) < q.r + rr + 2)) lane++;
      (lanes[lane] = lanes[lane] || []).push({ x, r: rr }); r.lane = lane; r.x = x; r.rr = rr;
    });
    const laneH = 54, H = 46 + lanes.length * laneH, base = 26;
    const mean = sum(rows.map(r => r.v * r[dim])) / sum(rows.map(r => r.v));
    o.push(`<line x1="${X(0)}" x2="${X(10)}" y1="${H - 22}" y2="${H - 22}" stroke="${MUTED}"/>`);
    [0, 2, 4, 6, 8, 10].forEach(t => o.push(`<line x1="${X(t)}" x2="${X(t)}" y1="${H - 25}" y2="${H - 19}" stroke="${MUTED}"/><text x="${X(t)}" y="${H - 6}" text-anchor="middle" font-size="10.5" fill="${MUTED}">${t}</text>`));
    o.push(`<text x="${X(0)}" y="12" font-size="11" fill="${MUTED}">${DIMS[dim].lo}</text><text x="${X(10)}" y="12" text-anchor="end" font-size="11" fill="${MUTED}">${DIMS[dim].hi}</text>`);
    o.push(`<line x1="${X(mean)}" x2="${X(mean)}" y1="${base - 6}" y2="${H - 22}" stroke="${INK}" stroke-dasharray="3 3"/><path d="M${X(mean)},${H - 24}l-6,-9h12z" fill="${INK}"/>`);
    const labs = [];
    rows.forEach((r, i) => {
      const cy = base + r.lane * laneH + 22;
      o.push(`<circle data-i="${i}" cx="${r.x}" cy="${cy}" r="${r.rr}" fill="${r.col}" fill-opacity=".85" stroke="#fff" stroke-width="1.2" style="cursor:default"/>`);
      labs.push({ text: r.list, x: r.x - r.list.length * 3.1, y: cy + r.rr + 8 });
    });
    declutter(labs, 12).forEach(l => o.push(`<text x="${clamp(l.x, 0, W - l.w)}" y="${l.y + 4}" font-size="11" font-weight="500" fill="${INK}" pointer-events="none">${esc(l.text)}</text>`));
    put("pos", W, H, o.join(""), `Party positions on ${DIMS[dim].lab}, CHES ${c.wave}`);
  } else {
    const H = Math.round(clamp(W * 0.85, 260, 380)), m = { l: 28, r: 12, t: 12, b: 30 };
    const X = p => m.l + p / 10 * (W - m.l - m.r), Y = p => H - m.b - p / 10 * (H - m.t - m.b);
    [0, 2, 4, 6, 8, 10].forEach(t => o.push(`<line x1="${X(t)}" x2="${X(t)}" y1="${m.t}" y2="${H - m.b}" stroke="${GRID}"/><line x1="${m.l}" x2="${W - m.r}" y1="${Y(t)}" y2="${Y(t)}" stroke="${GRID}"/><text x="${X(t)}" y="${H - m.b + 14}" text-anchor="middle" font-size="10.5" fill="${MUTED}">${t}</text><text x="${m.l - 5}" y="${Y(t) + 4}" text-anchor="end" font-size="10.5" fill="${MUTED}">${t}</text>`));
    o.push(`<text x="${(m.l + W - m.r) / 2}" y="${H - 3}" text-anchor="middle" font-size="11" fill="${MUTED}">Economic left–right</text><text x="${m.l + 4}" y="${m.t + 10}" font-size="11" fill="${MUTED}">TAN ↑</text><text x="${m.l + 4}" y="${H - m.b - 5}" font-size="11" fill="${MUTED}">GAL</text>`);
    const labs = [];
    [...rows].sort((a, b) => b.v - a.v).forEach(r => {
      const i = rows.indexOf(r), rr = rad(r.v);
      o.push(`<circle data-i="${i}" cx="${X(r.lrecon)}" cy="${Y(r.galtan)}" r="${rr}" fill="${r.col}" fill-opacity=".8" stroke="#fff" stroke-width="1.2" style="cursor:default"/>`);
      labs.push({ text: r.list, x: X(r.lrecon) - r.list.length * 3.1, y: Y(r.galtan) + rr + 8 });
    });
    declutter(labs, 12).forEach(l => o.push(`<text x="${clamp(l.x, 0, W - l.w)}" y="${l.y + 4}" font-size="11" font-weight="500" fill="${INK}" pointer-events="none">${esc(l.text)}</text>`));
    put("pos", W, H, o.join(""), `Party positions, economic left–right and GAL–TAN, CHES ${c.wave}`);
  }
  CH.pos = {
    W, box: $("posbox"),
    move(mx, my, ev) {
      const t = ev.target.closest("[data-i]"); if (!t) return null;
      const r = rows[+t.dataset.i];
      return tipHead(r.list, esc(r.parties && r.parties !== r.list ? "CHES: " + r.parties : "")) + tipRow(r.col, `Vote, ${r.ref}`, pct(r.v)) + Object.entries(DIMS).map(([k, d]) => tipRow(null, d.lab, f2(r[k]))).join("");
    }
  };
}

// ---- government -----------------------------------------------------------------------------
const CODES = [["alternation", "Alternation"], ["formula", "Formula"], ["access", "Access"]];
const closedOf = g => g.coded ? [g.alternation !== "partial", g.familiar, g.access === "closed"] : null;
function renderGov() {
  $("govm").innerHTML = [["n", "Changes of government"], ["d", "Days in office"]].map(([k, l]) => `<button type="button" data-k="${k}" aria-pressed="${S.gov === k}">${l}</button>`).join("");
  const G = D.governments, W = frameW("govbox"), short = W < 560;
  const nameW = short ? 82 : 104, codeW = 54, m = { l: nameW, r: codeW + 8, t: 26, b: 22 }, rh = short ? 20 : 23, H = m.t + G.length * rh + m.b;
  const d0 = G[0].start - 200, d1 = D.meta.asOf + 120, X = d => m.l + (d - d0) / (d1 - d0) * (W - m.l - m.r);
  const hot = new Set(cabinetsAfter(S.e).map(g => g.id));
  const o = [], ey = YEARS.filter(y => EBY[y].date > d0), eg = ey.slice(1).map((y, i) => X(EBY[y].date) - X(EBY[ey[i]].date)), shortY = eg.length && Math.min(...eg) < 32;
  G.forEach((g, i) => { if (hot.has(g.id)) o.push(`<rect x="2" y="${m.t + i * rh + 1}" width="${W - 4}" height="${rh - 2}" rx="5" fill="#F6EBEB"/>`); });
  const shownX = YEARS.includes(S.e) ? [X(EBY[S.e].date)] : [], minD = shortY ? 22 : 32;
  ey.forEach(y => {
    const sel = y === S.e, px = X(EBY[y].date), lab = !sel && shownX.some(q => Math.abs(q - px) < minD) ? "" : (shortY ? "’" + String(y).slice(2) : String(y));
    if (lab && !sel) shownX.push(px);
    o.push(`<line x1="${px}" x2="${px}" y1="${m.t - 6}" y2="${H - m.b}" stroke="${sel ? ACCENT : "#CCC"}" stroke-dasharray="3 3"/>${lab ? `<text x="${px}" y="${m.t - 10}" text-anchor="middle" font-size="10.5" fill="${sel ? ACCENT : MUTED}" font-weight="${sel ? 600 : 400}">${lab}</text>` : ""}`);
  });
  o.push(`<text x="${W - codeW / 2 - 4}" y="${m.t - 10}" text-anchor="middle" font-size="10" fill="${MUTED}">A · F · Acc</text>`);
  G.forEach((g, i) => {
    const y = m.t + i * rh, col = famCol(g.pmFamily);
    o.push(`<g data-i="${i}" style="cursor:pointer"><rect x="0" y="${y}" width="${W}" height="${rh}" fill="transparent"/>`);
    o.push(`<text x="${nameW - 8}" y="${y + rh / 2 + 4}" text-anchor="end" font-size="${short ? 11 : 12}" font-weight="${hot.has(g.id) ? 600 : 400}" fill="${INK}">${esc(g.name)}</text>`);
    o.push(`<rect x="${X(g.start)}" y="${y + 4}" width="${Math.max(2, X(g.end) - X(g.start))}" height="${rh - 8}" rx="3" fill="${col}"${g.majority === "minority" ? ` fill-opacity=".55"` : ""}/>`);
    if (g.ongoing) o.push(`<path d="M${X(g.end)},${y + 4}l6,${(rh - 8) / 2}l-6,${(rh - 8) / 2}z" fill="${col}"/>`);
    const c = closedOf(g);
    CODES.forEach((cd, j) => {
      const cx = W - codeW + j * 17;
      o.push(c ? `<rect x="${cx}" y="${y + rh / 2 - 5}" width="10" height="10" rx="2" fill="${c[j] ? ACCENT : "#fff"}" stroke="${ACCENT}" stroke-width="1.4"/>` : `<rect x="${cx}" y="${y + rh / 2 - 5}" width="10" height="10" rx="2" fill="#fff" stroke="#D5D5D5" stroke-width="1.2"/>`);
    });
    o.push(`</g>`);
  });
  // x axis: years every 5
  const yStep = X(Math.round(Date.UTC(2000, 0, 1) / 864e5)) - X(Math.round(Date.UTC(1995, 0, 1) / 864e5)) < 40 ? 10 : 5;
  for (let yr = yStep === 10 ? 2000 : 1995; yr <= 2025; yr += yStep) { const d = Math.round(Date.UTC(yr, 0, 1) / 864e5); o.push(`<text x="${X(d)}" y="${H - 6}" text-anchor="middle" font-size="10.5" fill="${MUTED}">${yr}</text>`); }
  put("gov", W, H, o.join(""), "Cabinets since 1991; squares: alternation, formula and access, filled when closed");
  CH.gov = {
    W, box: $("govbox"),
    move(mx, my, ev) {
      const t = ev.target.closest("[data-i]"); if (!t) return null;
      const g = G[+t.dataset.i], c = closedOf(g);
      return tipHead(g.name, `${esc(g.full)} · ${esc(g.pm)} (${esc(g.pmParty)})`) +
        tipRow(famCol(g.pmFamily), "Parties", esc(g.parties.join(", "))) +
        tipRow(null, "In office", `${dstr(g.start)} – ${g.ongoing ? "now" : dstr(g.end)}`) +
        tipRow(null, "Days", int(g.days) + (g.ongoing ? " so far" : "")) +
        tipRow(null, "Support", g.majority === "minority" ? "minority" : "majority") +
        (c ? `<div style="margin-top:4px">${tipRow(null, "Alternation", `${esc(g.alternation)} · ${c[0] ? "closed" : "open"}`)}${tipRow(null, "Formula", g.familiar ? "familiar · closed" : "innovative · open")}${tipRow(null, "Access", g.access === "open" ? `open · ${esc(g.newParties.join(", "))}` : "closed")}</div>` : `<div class="sub">The baseline: changes are coded from Suchocka on</div>`);
    },
    click(mx, my, ev) { const t = ev.target.closest("[data-i]"); if (t) setElection(electionBefore(G[+t.dataset.i].start - 1)); }
  };
  renderGovPanel();
}
function renderGovPanel() {
  const ci = D.closureIndex.find(c => c.weighting === (S.gov === "d" ? "Days in office" : "Changes of government"));
  const cabs = cabinetsAfter(S.e), coded = D.governments.filter(g => g.coded);
  $("govpanel").innerHTML = `<h3>Closure of government competition</h3>
    <div style="display:flex;align-items:baseline;gap:10px"><span class="bigno">${(100 * ci.index).toFixed(1)}</span><span class="muted">of 100, ${coded.length} changes of government since ${dstr(coded[0].start).split(" ").slice(1).join(" ")}${S.gov === "d" ? ", each weighted by its days in office" : ""}</span></div>
    <div class="bars">${CODES.map(([k, l]) => `<div class="bar"><span>${l === "Alternation" ? "Alternation closed" : l === "Formula" ? "Familiar formula" : "Access closed"}</span><span class="track"><i style="width:${(100 * ci[k]).toFixed(1)}%"></i></span><span class="val">${(100 * ci[k]).toFixed(1)}</span></div>`).join("")}</div>
    <p class="note">The index is the mean of the three shares. Squares in the chart: <span class="codes"><i class="c"></i></span> closed, <span class="codes"><i></i></span> open, for alternation (A), formula (F) and access (Acc). Pale bars are minority cabinets; colours are the prime minister's party family.</p>
    <div style="border-top:1px solid var(--rule);padding-top:8px;margin-top:2px">
      <span class="label">${S.e === "polls" ? "Governing now" : `After the ${S.e} election`}</span>
      ${cabs.length ? cabs.map(g => `<div style="margin-top:6px;font-size:13px"><b>${esc(g.name)}</b> <span class="muted">${dstr(g.start)}</span><br>${esc(g.parties.join(", "))}${g.coded ? `<br><span class="muted">${esc(g.alternation)} alternation · ${g.familiar ? "familiar" : "innovative"} formula · ${g.access} access${g.newParties.length ? " (" + esc(g.newParties.join(", ")) + " new to government)" : ""}</span>` : `<br><span class="muted">The baseline cabinet</span>`}</div>`).join("") : `<div class="note" style="margin-top:6px">No cabinet that won a confidence vote was formed in this term.</div>`}
      <p class="note" style="margin-top:6px">Cabinets that never won a confidence vote (Pawlak 1992, Belka 2004, Morawiecki 2023) are left out.</p>
    </div>`;
}

// ---- rendering ------------------------------------------------------------------------------
function header() {
  const live = POLL.live;
  $("asof").innerHTML = `Sejm elections ${YEARS[0]}–${YEARS[YEARS.length - 1]} from PKW results, with party lineages, families and cabinets from my Polish party tracker (to ${dlong(D.meta.asOf)}). ` +
    (live ? `The polls column is the Polling tracker's estimate for ${dlong(POLL.date)}.` : `The polls column is the snapshot of ${dlong(POLL.date)}; the Polling tracker's latest estimate could not be loaded.`) + ` ${esc(D.meta.credit)}.`;
}
function renderAll() {
  renderStrip(); renderOverview(); renderFrag(); renderDisp(); renderVol(); renderNat(); renderPol(); renderGov();
  saveHash();
}
function setElection(e) {
  if (e == null || e === S.e) return;
  S.e = e; renderAll();
}

// ---- downloads ------------------------------------------------------------------------------
function download(name, blob) {
  const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = name;
  document.body.appendChild(a); a.click(); setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 500);
}
function csv() {
  const keys = Object.keys(D.codebook).filter(k => !["election", "date", "source"].includes(k));
  const head = ["election", "date", "source", ...keys, "polarisation_lrgen", "polarisation_galtan", "polarisation_lrecon", "ches_wave"];
  const q = v => v == null || (typeof v === "number" && !isFinite(v)) ? "" : typeof v === "string" && /[",]/.test(v) ? `"${v.replace(/"/g, '""')}"` : (typeof v === "number" ? +v.toFixed(6) : v);
  const rows = [head];
  YEARS.forEach(y => { const r = EBY[y], c = cheFor(y); rows.push([y, iso(r.date), "PKW results", ...keys.map(k => r[k]), c ? c.lrgen : "", c ? c.galtan : "", c ? c.lrecon : "", c ? c.wave : ""]); });
  rows.push(["polls", iso(POLL.date), POLL.live ? "Polling tracker (live)" : "Polling tracker (snapshot)", ...keys.map(k => POLL_KEYS.has(k) ? POLL.stats[k] : null), "", "", "", ""]);
  download("party-system-indicators.csv", new Blob([rows.map(r => r.map(q).join(",")).join("\n") + "\n"], { type: "text/csv" }));
}
const SEC_TITLES = { fragmentation: "Fragmentation", disproportionality: "Disproportionality", volatility: "Volatility", nationalisation: "Nationalisation", polarisation: "Polarisation", government: "Government" };
async function png(sec) {
  const svgs = [...$(sec).querySelectorAll("svg[id]")].filter(s => s.viewBox && s.viewBox.baseVal && s.viewBox.baseVal.width && s.closest(".chartbox"));
  // ChartExport (../chart-export.js) keeps the labels in Jost in the images.
  const imgs = await Promise.all(svgs.map(async svg => {
    const vb = svg.viewBox.baseVal, w = vb.width, h = vb.height, clone = await ChartExport.clone(svg);
    clone.setAttribute("xmlns", "http://www.w3.org/2000/svg"); clone.setAttribute("width", w); clone.setAttribute("height", h);
    clone.querySelectorAll(".hov").forEach(n => n.remove());
    return new Promise(res => {
      const img = new Image(); img.onload = () => res({ img, w, h }); img.onerror = () => res(null);
      img.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(clone));
    });
  }));
  await ChartExport.fonts();
  imgs.forEach((im, i) => { if (im) im.title = svgs[i].dataset.title || ""; });
  const ok = imgs.filter(Boolean), pad = 24, head = 62, foot = 34, th = 22;
  const W = Math.max(...ok.map(i => i.w)) + 2 * pad, H = head + foot + sum(ok.map(i => i.h + (i.title ? th : 0))) + pad * (ok.length - 1);
  const cv = document.createElement("canvas"), sc = 2; cv.width = W * sc; cv.height = H * sc;
  const g = cv.getContext("2d"); g.scale(sc, sc); g.fillStyle = "#fff"; g.fillRect(0, 0, W, H);
  g.fillStyle = ACCENT; g.font = ChartExport.TITLE; g.fillText(`Party system indicators · ${SEC_TITLES[sec]}`, pad, 32);
  g.fillStyle = "#4B4B4B"; g.font = ChartExport.SUBTITLE; g.fillText(`Sejm elections ${YEARS[0]}–${YEARS[YEARS.length - 1]}; selected: ${S.e === "polls" ? "polls, " + dstr(POLL.date) : S.e}`, pad, 52);
  let yy = head;
  ok.forEach(i => {
    if (i.title) { g.fillStyle = INK; g.font = "500 13px Jost, sans-serif"; g.fillText(i.title, pad, yy + 14); yy += th; }
    g.drawImage(i.img, pad, yy, i.w, i.h); yy += i.h + pad;
  });
  g.fillStyle = MUTED; g.font = ChartExport.CREDIT; g.fillText(`${D.meta.credit} · benstanley.eu`, pad, H - 12);
  cv.toBlob(b => download(`party-system-${sec}-${S.e}.png`, b), "image/png");
}

// ---- events ---------------------------------------------------------------------------------
function bind() {
  ["frag", "count", "dispg", "dispw", "dispb", "vol", "fam", "nat", "infl", "pol", "gov"].forEach(bindChart);
  $("elections").addEventListener("click", e => { const b = e.target.closest("button[data-e]"); if (b) setElection(b.dataset.e === "polls" ? "polls" : +b.dataset.e); });
  const seg = (id, key) => $(id).addEventListener("click", e => { const b = e.target.closest("button[data-k]"); if (b && S[key] !== b.dataset.k) { S[key] = b.dataset.k; renderAll(); } });
  seg("fragm", "frag"); seg("volm", "vol"); seg("natm", "nat"); seg("polm", "pol"); seg("govm", "gov");
  $("natchips").addEventListener("click", e => { const b = e.target.closest("button[data-hl]"); if (!b) return; const v = b.dataset.hl || null; S.natHl = S.natHl === v ? null : v; renderNat(); });
  let hov = null;
  $("natchips").addEventListener("mouseover", e => { const b = e.target.closest("button[data-hl]"); if (!b || S.natHl) return; const v = b.dataset.hl || null; if (v !== hov) { hov = v; renderNatMain(v); } });
  $("natchips").addEventListener("mouseleave", () => { hov = null; if (!S.natHl) renderNatMain(null); });
  $("scorecard").addEventListener("click", e => {
    const c = e.target.closest("[data-e]");
    if (c) { setElection(c.dataset.e === "polls" ? "polls" : +c.dataset.e); return; }
    const r = e.target.closest("[data-sec]");
    if (r) {
      const g = GROUPS.find(gg => gg.sec === r.dataset.sec), row = g && g.rows.find(x => x.k === r.dataset.row);
      if (row && row.set) { Object.assign(S, row.set); renderAll(); }
      if (r.dataset.sec !== "overview") jumpTo(r.dataset.sec);
    }
  });
  document.querySelectorAll("[data-png]").forEach(b => b.addEventListener("click", () => png(b.dataset.png)));
  $("jump").addEventListener("click", e => { const a = e.target.closest("a"); if (!a) return; e.preventDefault(); jumpTo(a.getAttribute("href").slice(1)); });
  $("copylink").addEventListener("click", () => {
    const u = shareUrl();
    (navigator.clipboard ? navigator.clipboard.writeText(u) : Promise.reject()).then(() => toast("Link copied")).catch(() => { window.prompt("Copy this link:", u); });
  });
  $("csvall").addEventListener("click", csv);
  window.addEventListener("scroll", debounce(markJump, 60), { passive: true });
  let lastW = window.innerWidth;
  window.addEventListener("resize", debounce(() => { if (window.innerWidth !== lastW) { lastW = window.innerWidth; renderAll(); } }, 150));
}
function jumpTo(id) {
  const el = $(id); if (!el) return;
  window.scrollTo({ top: el.getBoundingClientRect().top + window.scrollY - 54, behavior: "smooth" });
}
function markJump() {
  const cur = currentSection();
  document.querySelectorAll("#jump a").forEach(a => a.classList.toggle("on", a.getAttribute("href") === "#" + cur));
}

// ---- load -----------------------------------------------------------------------------------
Promise.all([
  fetch("party_system.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }),
  fetch("../polling-tracker/polls.json").then(r => r.ok ? r.json() : null).catch(() => null)
]).then(([d, pj]) => {
  D = d; prep();
  try { if (pj) { PJ = pj; POLL = livePolls(pj); } } catch (err) { console.warn("Polling tracker data unusable:", err); PJ = null; }
  if (!POLL) POLL = snapshotPolls();
  const sec = restoreHash();
  header(); bind(); renderAll();
  if (sec && $(sec)) setTimeout(() => jumpTo(sec), 120);
}).catch(err => { $("asof").textContent = "Could not load the party system data."; console.error(err); });
