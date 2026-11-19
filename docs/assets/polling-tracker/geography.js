/* geography.js: the Polling tracker's Geography address (Election 2027/PLAN.md, decision 18). It took over the Constituencies page
 * (removed on 18 November 2026): what a vote is worth in each Sejm constituency, 1991-2023, a profile of each constituency, and
 * what the polls would give each party if the seats followed population. For the current term the Map view also shows where the
 * tracker's projected seats fall and how many are in doubt.
 *
 * Data: ../constituencies/constituencies.json, geometry.json, mps.json and scenarios.json, written by
 * "Constituencies/analysis/90_export.R" in the Polish political analyses repo and copied in by scripts/sync-analyses.py; and the
 * tracker's own (app.js, loaded first: S, CUR, renderMap, renderConstCard, hemiSim, seatByConstituency, png and the rest are its
 * globals). The term selector picks an election and its constituencies; "current" is the 2023 constituencies with the projection.
 * Views: Map (#v-geomap), What a vote is worth (#v-worth), If seats followed population (#v-population). No libraries. */
(function () {
"use strict";
const qs = s => document.querySelector(s);
const h = s => String(s == null ? "" : s).replace(/[&<>"]/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
const num = x => (x == null ? "n/a" : Number(x).toLocaleString("en-GB"));
const pc = (x, d = 1) => (x == null ? "n/a" : (100 * x).toFixed(d) + "%");
const f2 = x => (x == null ? "n/a" : x.toFixed(2));
// a {columns, rows} table as objects; an array (already converted) or a missing table as it is
const tab = t => (!t || Array.isArray(t) ? t || [] : t.rows.map(r => Object.fromEntries(t.columns.map((c, i) => [c, r[i]]))));
const cache = {};
const fetchJSON = f => cache[f] || (cache[f] = fetch("../constituencies/" + f).then(r => { if (!r.ok) throw new Error(f + " " + r.status); return r.json(); }));
const PAGES = "../../pages/polish-political-analyses/";
const MEASURES = {
  proj: { label: "Projected seats", current: true, help: "One dot for each seat the tracker projects, in the colour of its list: this week's estimates, or the scenario in the link." },
  doubt: { label: "Seats in doubt", current: true, kind: "seq", fmt: x => String(x), help: "Seats that are not certain: the projected list wins them in under 97.5% of the tracker's simulations. Hover for each one." },
  vvi_electors: { label: "Electors per seat", kind: "div", help: "Vote value: 1 is the national average. Above 1, a registered elector counts for more than average in choosing the Sejm; below 1, for less." },
  vvi_electors_dom: { label: "Electors at home per seat", kind: "div", help: "As electors per seat, leaving out electors registered abroad or on ships, whom PKW counts in Warsaw (constituency 19): 637,000 of them in 2023." },
  vvi_votes: { label: "Votes per seat", kind: "div", help: "Vote value counting the votes actually cast, so a constituency with low turnout gets more seats per vote." },
  vvi_residents: { label: "Residents per seat", kind: "div", help: "Vote value counting residents, the basis the Electoral Code uses (2001 onwards)." },
  seats: { label: "Seats", kind: "seq", fmt: x => String(x), help: "The number of deputies the constituency elects (district magnitude)." },
  turnout: { label: "Turnout", kind: "seq", fmt: x => pc(x, 0), help: "Votes cast as a share of registered electors." },
  wasted: { label: "Votes that elected no one", kind: "seq", fmt: x => pc(x, 0), help: "Votes for lists that won no seat in the constituency, including lists below the national threshold." },
  largest_bonus: { label: "Bonus to the largest list", kind: "seq", fmt: x => pc(x, 0), help: "The largest list's share of the seats minus its share of the votes, in points." }
};
let CC = null, GG = null, MP = null, SCN = null, loading = null;
// term: "current" or an election year; measure, mode (geo or circles) on the Map; wm on What a vote is worth; sc, pa on If seats followed population
const GS = { term: "current", measure: "proj", mode: "geo", wm: "vvi_electors", sc: "annex", pa: "" };
const yearOf = () => (GS.term === "current" ? 2023 : +GS.term);
const colOf = l => (CC.palette && CC.palette[l]) || "#7f7f7f";
const setOf = y => CC.elections.find(e => e.year === y).set;
const rowsOf = y => CC.constituencies.filter(r => r.year === y);
const valueOf = (r, m) => (m === "wasted" ? r.wasted_lost_locally + r.wasted_below_threshold : r[m]);
const cur = () => GS.term === "current";
function load() {
  return loading || (loading = Promise.all([fetchJSON("constituencies.json"), fetchJSON("geometry.json")]).then(([c, g]) => {
    c.constituencies = tab(c.constituencies); c.elections = tab(c.elections); c.list_results = tab(c.list_results); c.list_costs = tab(c.list_costs);
    c.national_lists = tab(c.national_lists); CC = c; GG = g;
    if (!cur() && !CC.elections.some(e => e.year === +GS.term && e.year !== 2023)) GS.term = "current";
  }));
}
// "2019–23": the term an election opened (the tracker's past terms are named the same way)
function termLabel(y) {
  const ys = CC.elections.map(e => e.year).sort((a, b) => a - b), i = ys.indexOf(y), nx = ys[i + 1];
  return nx ? `${y}–${String(nx).slice(0, 2) === String(y).slice(0, 2) ? String(nx).slice(2) : nx}` : `${y}–`;
}
// The current term is the 2023 constituencies (and the 2023 election's figures) with the tracker's projection
function termSelect() {
  const ys = CC.elections.map(e => e.year).filter(y => y !== 2023).sort((a, b) => b - a);
  return `<label class="termsel">Sejm term <select id="gterm" aria-label="Sejm term">${`<option value="current"${cur() ? " selected" : ""}>${termLabel(2023)} (current)</option>`}${ys.map(y => `<option value="${y}"${!cur() && +GS.term === y ? " selected" : ""}>${termLabel(y)}</option>`).join("")}</select></label>`;
}
function setTerm(v) {
  const before = setOf(yearOf());
  GS.term = v;
  if (setOf(yearOf()) !== before) S.constSel = null;
  if (!cur() && MEASURES[GS.measure].current) GS.measure = "vvi_electors";
  if (GS.measure === "vvi_residents" && yearOf() < 2001) GS.measure = "vvi_electors";
  renderView(); saveHash();
}
// the tracker's tooltip box (placeTip) needs a container; the Constituencies charts use a floating one of their own
let tipEl = null;
function tip() {
  if (tipEl) return tipEl;
  tipEl = document.createElement("div"); tipEl.className = "gtip"; document.body.appendChild(tipEl);
  document.addEventListener("mousemove", e => {
    const t = e.target.closest && e.target.closest(".geo [data-tip]");
    if (!t) { tipEl.style.display = "none"; return; }
    tipEl.innerHTML = t.dataset.tip; tipEl.style.display = "block";
    tipEl.style.left = Math.min(innerWidth - tipEl.offsetWidth - 8, e.clientX + 12) + "px"; tipEl.style.top = (e.clientY + 14) + "px";
  });
  return tipEl;
}

/* ---------- colour scales ---------- */
const lerp = (a, b, t) => a.map((v, i) => Math.round(v + (b[i] - v) * t));
const hex = c => "rgb(" + c.join(",") + ")";
const RED = [139, 0, 0], LIGHT = [246, 235, 235], SLATE = [62, 92, 118], MID = [245, 245, 243];
function scale(vals, m) {
  const def = MEASURES[m];
  if (def.kind === "div") { const lim = Math.max(0.12, Math.max(...vals.map(v => Math.abs(v - 1)))); return { f: v => (v >= 1 ? hex(lerp(MID, RED, Math.min(1, (v - 1) / lim))) : hex(lerp(MID, SLATE, Math.min(1, (1 - v) / lim)))), lo: 1 - lim, hi: 1 + lim, lc: hex(SLATE), hc: hex(RED), mid: true }; }
  const lo = Math.min(...vals), hi = Math.max(...vals);
  return { f: v => hex(lerp(LIGHT, RED, hi > lo ? (v - lo) / (hi - lo) : 0.5)), lo, hi, lc: hex(LIGHT), hc: hex(RED) };
}

/* ---------- seats in doubt (current term) ---------- */
function doubtByOkreg() {
  const sim = hemiSim(); if (!sim) return null;
  const out = {}; seatByConstituency(sim).forEach(k => { out[k.okreg] = k.projected.filter(s => s.cat !== "certain"); });
  return out;
}

/* ---------- Map ---------- */
function viewMap() {
  tip();
  const y = yearOf(), rows = rowsOf(y), set = GG.sets[setOf(y)];
  const ms = Object.keys(MEASURES).filter(m => (!MEASURES[m].current || cur()) && (m !== "vvi_residents" || y >= 2001));
  if (!ms.includes(GS.measure)) GS.measure = cur() ? "proj" : "vvi_electors";
  if (S.constSel != null && !rowsOf(y).some(r => r.okreg === S.constSel)) S.constSel = null;
  const m = GS.measure, def = MEASURES[m], proj = m === "proj";
  qs("#geotb").innerHTML = termSelect() +
    `<label class="termsel">Colour by <select id="gmeasure">${ms.map(k => `<option value="${k}"${k === m ? " selected" : ""}>${h(MEASURES[k].label)}</option>`).join("")}</select></label>` +
    (proj ? "" : `<div class="seg small" role="group" aria-label="Map type"><button type="button" data-gmode="geo" aria-pressed="${GS.mode === "geo"}">Map</button><button type="button" data-gmode="circles" aria-pressed="${GS.mode === "circles"}">Cartogram</button></div>`) +
    (proj ? "" : `<span class="btnrow" style="margin-left:auto"><button type="button" class="btn2 sm" id="gpng">${(S.fmt || "png").toUpperCase()}</button></span>`);
  qs("#projmap").hidden = !proj; qs("#measmap").hidden = proj;
  qs("#constcard").hidden = !cur();
  if (proj) renderMap();
  else {
    let vals, by = Object.fromEntries(rows.map(r => [r.okreg, r])), dq = null;
    if (m === "doubt") {
      dq = doubtByOkreg();
      if (!dq) { qs("#measmap").innerHTML = `<p class="help">${drawsState === "missing" ? "The tracker's simulations could not be loaded." : "Loading the tracker's simulations…"}</p>`; loadDraws(); profile(); return; }
      vals = rows.map(r => (dq[r.okreg] || []).length);
    } else vals = rows.map(r => valueOf(r, m));
    const sc = scale(vals, m), val = r => (m === "doubt" ? (dq[r.okreg] || []).length : valueOf(r, m));
    const fmt = v => (def.fmt ? def.fmt(v) : f2(v));
    const [vx, vy, vw, vh] = GS.mode === "circles" ? set.circle_view : [0, 0, GG.meta.view[0], GG.meta.view[1]];
    const kh = vh * 0.07, ky = vy + vh + kh * 0.6;
    let s = `<svg id="geo" viewBox="${vx} ${vy} ${vw} ${vh + kh * 2.6}" role="img" aria-label="${h(def.label)} by constituency">`;
    s += `<defs><linearGradient id="ggrad"><stop offset="0" stop-color="${sc.lc}"/>${sc.mid ? `<stop offset=".5" stop-color="rgb(245,245,243)"/>` : ""}<stop offset="1" stop-color="${sc.hc}"/></linearGradient></defs>`;
    set.okreg.forEach((o, i) => {
      const r = by[o]; if (!r) return;
      const v = val(r), extra = m === "doubt" && dq[o] && dq[o].length ? "<br>" + dq[o].map(st => `${h(st.id)}'s ${ord(st.j)} seat: ${prob(st.p)} (${st.cat})${st.next ? `, else ${h(st.next.id)} ${prob(st.next.p)}` : ""}`).join("<br>") : "";
      const t = `<b>${h(r.name)}</b> (${r.seats} seats)<br>${h(def.label)}: ${fmt(v)}${extra}`;
      if (GS.mode === "geo") s += `<path class="region ${S.constSel === o ? "sel" : ""}" data-o="${o}" d="${set.d[i]}" fill="${sc.f(v)}" data-tip="${h(t)}"/>`;
      else { const c = set.circles[i]; s += `<circle class="cir ${S.constSel === o ? "sel" : ""}" data-o="${o}" cx="${c[0]}" cy="${c[1]}" r="${c[2]}" fill="${sc.f(v)}" data-tip="${h(t)}"/>`; }
    });
    const kw = vw * 0.5, kx = vx + (vw - kw) / 2, fs = kh * 0.42;
    s += `<rect x="${kx}" y="${ky}" width="${kw}" height="${kh * 0.45}" rx="${kh * 0.2}" fill="url(#ggrad)"/>` +
      `<text x="${kx}" y="${ky + kh * 1.2}" style="font-size:${fs.toFixed(1)}px" fill="#767676">${h(fmt(sc.lo))}</text>` +
      (sc.mid ? `<text x="${kx + kw / 2}" y="${ky + kh * 1.2}" style="font-size:${fs.toFixed(1)}px" fill="#767676" text-anchor="middle">1 (average)</text>` : "") +
      `<text x="${kx + kw}" y="${ky + kh * 1.2}" style="font-size:${fs.toFixed(1)}px" fill="#767676" text-anchor="end">${h(fmt(sc.hi))}</text></svg>`;
    qs("#measmap").innerHTML = `<div style="display:flex;justify-content:space-between;align-items:center"><span class="label">${h(def.label)}, ${cur() ? "2023 constituencies" : y + " election"}</span></div>${s}
      <p class="help" style="margin-top:6px">${h(def.help)}${setOf(y) !== "sejm2001" ? " This election used the " + h(set.label.replace("Sejm constituencies", "Sejm constituencies of").replace("of,", "of")) + "." : ""}</p>`;
    qs("#geo").setAttribute("font-family", "Jost, 'Avenir Next', -apple-system, sans-serif");
  }
  profile();
  foot("#geofoot");
}
// The profile: for the current term the tracker's card (projected seats) comes first, then the constituency at the 2023 election
function profile() {
  const box = qs("#geoprof"), y = yearOf(), rows = rowsOf(y);
  if (cur()) renderConstCard();
  if (S.constSel == null) { box.innerHTML = `<span class="label">Constituency</span><p class="help" style="margin:0">Select a constituency on the map for ${cur() ? "its projected seats, " : ""}what its seats cost, how its lists did and, from 2001, who represents it.</p>`; box.hidden = cur(); return; }
  box.hidden = false;
  const r = rows.find(x => x.okreg === S.constSel); if (!r) { box.innerHTML = ""; return; }
  const set = setOf(y), hist = CC.constituencies.filter(x => x.okreg === S.constSel && CC.elections.find(e => e.year === x.year).set === set).sort((a, b) => a.year - b.year);
  const L = CC.list_results.filter(x => x.year === y && x.okreg === S.constSel).sort((a, b) => b.votes - a.votes), tot = L.reduce((a, x) => a + x.votes, 0), shown = L.slice(0, 7), rest = L.slice(7);
  box.innerHTML = `<span class="label">${cur() ? "At the 2023 election" : "Constituency " + S.constSel + ", " + y}</span>
   <div style="font-size:20px;font-weight:300">${h(r.name)}${cur() ? "" : ` <span class="muted" style="font-size:13px">${r.seats} seats</span>`}</div>
   <p class="help" style="margin:0"><a href="${PAGES}election-results.html#e=sejm-${y}" target="_top">Results on Election results</a>${set === "sejm2001" ? ` · <a href="${PAGES}election-results.html#u=c${S.constSel}" target="_top">its history since 1990</a>` : ""}</p>
   <dl class="kv"><dt>Seats</dt><dd>${r.seats}${r.legal_seats != null && r.legal_seats !== r.seats ? ` (the norm of representation gives ${r.legal_seats})` : ""}</dd>
   <dt>Registered electors per seat</dt><dd>${num(r.electors_per_seat)} · vote value ${f2(r.vvi_electors)}</dd>
   ${r.electors_abroad > 5000 ? `<dt>Of them registered abroad or on ships</dt><dd>${num(r.electors_abroad)} · without them ${f2(r.vvi_electors_dom)}</dd>` : ""}
   <dt>Votes cast per seat</dt><dd>${num(r.votes_per_seat)} · ${f2(r.vvi_votes)}</dd>
   ${r.residents_per_seat != null ? `<dt>Residents per seat</dt><dd>${num(r.residents_per_seat)} · ${f2(r.vvi_residents)}</dd>` : ""}
   <dt>Turnout</dt><dd>${pc(r.turnout, 1)}</dd>
   ${r.last_seat_votes != null ? `<dt>Votes behind the last seat won</dt><dd>${num(r.last_seat_votes)}</dd>` : ""}
   ${r.guaranteed_share != null ? `<dt>Share that guarantees a seat</dt><dd>${pc(r.guaranteed_share, 1)} of valid votes</dd>` : ""}
   <dt>Votes for lists with no seat</dt><dd>${pc(r.wasted_lost_locally + r.wasted_below_threshold, 1)}</dd></dl>
   <h3>Lists, ${y}</h3><div class="tbl"><table><thead><tr><th>List</th><th class="n">Votes</th><th class="n">Share</th><th class="n">Seats</th><th class="n">Votes per seat</th></tr></thead><tbody>${shown.map(x => `<tr><td><span class="sw sq" style="background:${colOf(x.label)};margin-right:6px"></span>${h(x.short)}</td><td class="n">${num(x.votes)}</td><td class="n">${pc(x.votes / tot, 1)}</td><td class="n">${x.seats}</td><td class="n">${x.seats ? num(Math.round(x.votes / x.seats)) : ""}</td></tr>`).join("")}${rest.length ? `<tr><td>${rest.length} other lists</td><td class="n">${num(rest.reduce((a, x) => a + x.votes, 0))}</td><td class="n">${pc(rest.reduce((a, x) => a + x.votes, 0) / tot, 1)}</td><td class="n">${rest.reduce((a, x) => a + x.seats, 0)}</td><td></td></tr>` : ""}</tbody></table></div>
   ${hist.length > 1 ? `<h3 style="margin-top:12px">Vote value over time</h3><div id="ghs"></div>` : ""}
   ${set === "sejm2001" ? `<div id="gmpl"></div><div id="gcv"></div>` : ""}`;
  if (hist.length > 1) {
    const W = 360, H = 120, m = { l: 30, r: 10, t: 8, b: 20 }, ys = hist.map(x => x.year), vs = hist.map(x => x.vvi_electors), lo = Math.min(0.85, ...vs), hi = Math.max(1.15, ...vs);
    const X = v => m.l + (v - ys[0]) / Math.max(1, ys[ys.length - 1] - ys[0]) * (W - m.l - m.r), Y = v => m.t + (hi - v) / (hi - lo) * (H - m.t - m.b);
    let g = `<svg style="max-width:${Math.round(W * 1.2)}px" viewBox="0 0 ${W} ${H}" role="img" aria-label="Vote value over time"><line x1="${m.l}" x2="${W - m.r}" y1="${Y(1)}" y2="${Y(1)}" stroke="#D5D5D5" stroke-dasharray="3 3"/><text x="${m.l - 4}" y="${Y(1) + 4}" text-anchor="end">1</text>
      <polyline fill="none" stroke="#8B0000" stroke-width="2" points="${hist.map(x => X(x.year) + "," + Y(x.vvi_electors)).join(" ")}"/>`;
    hist.forEach(x => { g += `<circle cx="${X(x.year)}" cy="${Y(x.vvi_electors)}" r="${x.year === y ? 5 : 3}" fill="#8B0000" data-tip="${x.year}: ${f2(x.vvi_electors)} (${x.seats} seats, ${num(x.electors_per_seat)} electors per seat)"/><text x="${X(x.year)}" y="${H - 4}" text-anchor="middle">${x.year}</text>`; });
    qs("#ghs").innerHTML = g + "</svg>";
  }
  if (set === "sejm2001") { covBlock(); mpBlock(y); }
}
function covBlock() {
  const cv = CC.covariates, vals = Object.fromEntries(cv.values.rows.filter(r => r[0] === S.constSel).map(r => [r[1], r[2]]));
  const f = (v, u) => (v == null ? "n/a" : u.startsWith("%") ? v.toFixed(1) + "%" : num(Math.round(v)));
  qs("#gcv").innerHTML = `<h3 style="margin-top:12px">The constituency against Poland</h3><div class="tbl"><table><tbody>${cv.vars.map(v => `<tr><td>${h(v.label)} <span class="help">${h(v.year)}</span></td><td class="n">${f(vals[v.var], v.unit)}</td><td class="n help">${v.national == null ? "" : f(v.national, v.unit) + " Poland"}</td></tr>`).join("")}</tbody></table></div>`;
}
async function mpBlock(y) {
  if (y < 2001) return;
  const j = await fetchJSON("mps.json"); MP = MP || tab(j.mps);
  const box = qs("#gmpl"); if (!box) return;
  const ms = MP.filter(x => x.year === y && x.okreg === S.constSel), t = CC.elections.findIndex(e => e.year === y) + 1;
  box.innerHTML = `<h3 style="margin-top:12px">Members elected here, ${y} (${ms.length}, replacements included)</h3><p class="help">${ms.map(x => `<span style="color:${colOf(x.family)}">&#9632;</span> <a href="${PAGES}legislative-activity.html#view=mps&t=${t}&mp=${x.mp_id}" target="_top" style="color:inherit">${h(x.name)}</a> <span style="color:${colOf(x.family)}">${h(x.club || "")}</span>`).join("<br>")}</p><p class="help">Each name opens the member's record in <a href="${PAGES}legislative-activity.html#view=mps&t=${t}" target="_top">Legislative activity</a>.</p>`;
}
function foot(sel) {
  const y = yearOf(), el = CC.elections.find(e => e.year === y), nl = CC.national_lists.filter(x => x.year === y);
  qs(sel).innerHTML = `${y}: ${el.constituencies} constituencies, ${el.seats_total} constituency seats${el.national_list_seats ? ` and ${el.national_list_seats} from national lists (not counted here${nl.length ? ": " + nl.map(x => h(x.committee) + " " + x.seats).join("; ") : ""})` : ""}. ${h(CC.meta.caveats[3])}`;
}

/* ---------- What a vote is worth ---------- */
function viewWorth() {
  tip();
  const y = yearOf(), rows = rowsOf(y), wm = (GS.wm === "vvi_residents" && y < 2001) ? "vvi_electors" : GS.wm, el = CC.elections.find(e => e.year === y);
  const box = qs("#v-worth");
  box.innerHTML = `<div class="toolbar">${termSelect()}</div><div class="geo">
   <p class="lead">Every Sejm election since 2001 used the same 41 constituencies, with nearly the same seat numbers, while the population moved. ${y >= 2001 ? `In ${y}, residents per seat ranged ${f2(el.ratio_residents)} to one between the constituencies with the most and the fewest people for each seat` : `In ${y}, registered electors per seat ranged ${f2(el.ratio_electors)} to one between the constituencies`}; counting votes cast, the range was ${f2(el.ratio_votes)} to one.</p>
   <div class="cols"><div class="chartbox"><h3>How far the vote's value is from equal, by election</h3><div id="gmal"></div><p class="help">Half the sum, over constituencies, of the absolute difference between a constituency's share of the seats and its share of the electors (or residents). Zero would be perfectly equal; 0.04 means 4% of the seats sit in the wrong constituencies.</p></div>
   <div class="chartbox"><h3>Magnitude and the largest list's bonus</h3><div id="gbon"></div><p class="help">Each dot is one constituency in one election, 2005 to 2023: the largest list's seat share minus its vote share, by the constituency's number of seats. The curve is what D'Hondt's rule guarantees a list at the margin, 1 / (seats + 1) of the votes.</p></div></div>
   <div class="chartbox"><div class="toolbar"><h3 style="margin:0">Vote value by constituency, ${y}</h3><div class="seg small" role="group" aria-label="Measure"><button type="button" data-wm="vvi_electors" aria-pressed="${wm === "vvi_electors"}">Electors per seat</button><button type="button" data-wm="vvi_electors_dom" aria-pressed="${wm === "vvi_electors_dom"}">Electors at home</button><button type="button" data-wm="vvi_votes" aria-pressed="${wm === "vvi_votes"}">Votes per seat</button>${y >= 2001 ? `<button type="button" data-wm="vvi_residents" aria-pressed="${wm === "vvi_residents"}">Residents per seat</button>` : ""}</div></div><div id="gdot"></div>
   <p class="help">Each dot is a constituency; the size shows its seats. Left of 1, a vote counts for less than the national average, right of it, for more. ${y >= 2001 ? "" : "Residents are not available on this basis before 2001."}</p></div>
   <div class="cols"><div class="chartbox"><h3>What a seat costs each list, ${y}</h3><div class="tbl" id="gcost"></div><p class="help">Votes per seat won in the constituencies, and the list's share of seats minus its share of votes. ${el.national_list_seats ? "The " + el.national_list_seats + " national-list seats are not counted." : ""}</p></div>
   <div class="chartbox"><h3>Seats the Electoral Code's norm gives, against the seats the constituency has</h3><div id="glegal"></div><p class="help">${y >= 2001 ? "Residents divided by seats, rounded as art. 202 says, with at least 7 seats. A bar to the right means the constituency has more seats than its population gives it." : "Residents are not available on this basis before 2001."}</p></div></div>
   <p class="help" id="gworthfoot"></p></div>`;
  const E = CC.elections.filter(e => e.year >= 1991), W = 520, H = 190, m = { l: 40, r: 14, t: 10, b: 22 }, mx = Math.max(...E.map(e => e.mal_electors), 0.045);
  const X = i => m.l + i / (E.length - 1) * (W - m.l - m.r), Y = v => m.t + (1 - v / mx) * (H - m.t - m.b);
  let s = `<svg style="max-width:${Math.round(W * 1.2)}px" viewBox="0 0 ${W} ${H}" role="img" aria-label="Malapportionment by election">`;
  for (let v = 0; v <= mx + 1e-9; v += 0.01) s += `<line x1="${m.l}" x2="${W - m.r}" y1="${Y(v)}" y2="${Y(v)}" stroke="#ECECEC"/><text x="${m.l - 5}" y="${Y(v) + 4}" text-anchor="end">${(v * 100).toFixed(0)}%</text>`;
  const line = (key, col, dash) => { const pts = E.map((e, i) => [i, e[key]]).filter(p => p[1] != null); return `<polyline fill="none" stroke="${col}" stroke-width="2" ${dash ? 'stroke-dasharray="4 3"' : ""} points="${pts.map(p => X(p[0]) + "," + Y(p[1])).join(" ")}"/>` + pts.map(p => `<circle cx="${X(p[0])}" cy="${Y(p[1])}" r="${E[p[0]].year === y ? 5 : 3}" fill="${col}" data-tip="${E[p[0]].year}: ${(p[1] * 100).toFixed(1)}%"/>`).join(""); };
  E.forEach((e, i) => { s += `<text x="${X(i)}" y="${H - 5}" text-anchor="middle">${e.year}</text>`; });
  s += line("mal_electors", "#8B0000", false) + line("mal_electors_dom", "#B8860B", true) + line("mal_residents", "#3E5C76", true) + `<text class="t" x="${m.l + 6}" y="${m.t + 6}" style="fill:#8B0000">electors</text><text class="t" x="${m.l + 6}" y="${m.t + 20}" style="fill:#B8860B">electors at home</text><text class="t" x="${m.l + 6}" y="${m.t + 34}" style="fill:#3E5C76">residents</text>`;
  qs("#gmal").innerHTML = s + "</svg>";
  const P = CC.constituencies.filter(r => r.year >= 2005), W2 = 520, H2 = 190, m2 = { l: 40, r: 14, t: 10, b: 24 }, Xm = v => m2.l + (v - 6) / 15 * (W2 - m2.l - m2.r), Yb = v => m2.t + (1 - v / 0.24) * (H2 - m2.t - m2.b);
  let b = `<svg style="max-width:${Math.round(W2 * 1.2)}px" viewBox="0 0 ${W2} ${H2}" role="img" aria-label="Bonus against magnitude">`;
  for (let v = 0; v <= 0.24; v += 0.08) b += `<line x1="${m2.l}" x2="${W2 - m2.r}" y1="${Yb(v)}" y2="${Yb(v)}" stroke="#ECECEC"/><text x="${m2.l - 5}" y="${Yb(v) + 4}" text-anchor="end">${Math.round(v * 100)}</text>`;
  for (let M = 7; M <= 20; M += 3) b += `<text x="${Xm(M)}" y="${H2 - 6}" text-anchor="middle">${M}</text>`;
  P.forEach(r => { b += `<circle cx="${Xm(r.seats) + ((r.year - 2005) % 5 - 2) * 1.8}" cy="${Yb(Math.max(0, r.largest_bonus))}" r="3" fill="#8B0000" fill-opacity=".35" data-tip="<b>${h(r.name)} ${r.year}</b><br>${r.seats} seats: ${h(r.largest_list)} ${pc(r.largest_vote_share, 0)} of votes, ${pc(r.largest_seat_share, 0)} of seats"/>`; });
  b += `<polyline fill="none" stroke="#3E5C76" stroke-width="1.6" points="${[7, 8, 9, 10, 12, 14, 17, 20].map(M => Xm(M) + "," + Yb(1 / (M + 1))).join(" ")}"/>`;
  qs("#gbon").innerHTML = b + `<text x="${W2 - m2.r}" y="${m2.t + 8}" text-anchor="end" style="fill:#3E5C76">1 / (seats + 1)</text><text x="${m2.l + 8}" y="${m2.t + 6}">points</text></svg>`;
  const sorted = rows.filter(r => r[wm] != null).sort((a, c) => a[wm] - c[wm]), lim = Math.max(0.15, ...sorted.map(r => Math.abs(r[wm] - 1))) * 1.05, rowH = 15, W3 = 520, H3 = sorted.length * rowH + 26, ml = 100;
  const X3 = v => ml + (v - (1 - lim)) / (2 * lim) * (W3 - ml - 14);
  let d = `<svg style="max-width:${Math.round(W3 * 1.2)}px" viewBox="0 0 ${W3} ${H3}" role="img" aria-label="Vote value by constituency"><line x1="${X3(1)}" x2="${X3(1)}" y1="4" y2="${H3 - 20}" stroke="#141414" stroke-dasharray="3 3"/>`;
  [1 - lim, 1, 1 + lim].forEach(v => { d += `<text x="${X3(v)}" y="${H3 - 6}" text-anchor="middle">${f2(v)}</text>`; });
  sorted.forEach((r, i) => { const yy = 10 + i * rowH; d += `<text class="t" x="${ml - 6}" y="${yy + 4}" text-anchor="end" style="font-size:10.5px">${h(r.name)}</text><circle cx="${X3(r[wm])}" cy="${yy}" r="${2.5 + r.seats / 4}" fill="${r[wm] >= 1 ? "#8B0000" : "#3E5C76"}" fill-opacity=".85" data-tip="<b>${h(r.name)}</b> (${r.seats} seats)<br>${f2(r[wm])}; ${num(r.electors_per_seat)} electors per seat"/>`; });
  qs("#gdot").innerHTML = d + "</svg>";
  const co = CC.list_costs.filter(x => x.year === y).sort((a, c) => c.votes - a.votes).slice(0, 8);
  qs("#gcost").innerHTML = `<table><thead><tr><th>List</th><th class="n">Votes per seat</th><th class="n">Seat share minus vote share</th></tr></thead><tbody>${co.map(x => `<tr><td><span class="sw sq" style="background:${colOf(x.label)};margin-right:6px"></span>${h(x.short)}</td><td class="n">${x.votes_per_seat == null ? "no seat" : num(x.votes_per_seat)}</td><td class="n">${x.bonus >= 0 ? "+" : ""}${(x.bonus * 100).toFixed(1)}</td></tr>`).join("")}</tbody></table>`;
  const lg = rows.filter(r => r.legal_seats != null).sort((a, c) => (c.seats - c.legal_seats) - (a.seats - a.legal_seats)).filter(r => r.seats !== r.legal_seats);
  qs("#glegal").innerHTML = lg.length ? `<div class="tbl"><table><tbody>${lg.map(r => `<tr><td>${h(r.name)}</td><td class="n">${r.seats} against ${r.legal_seats}</td><td class="n">${r.seats - r.legal_seats > 0 ? "+" : ""}${r.seats - r.legal_seats}</td></tr>`).join("")}</tbody></table></div>` : `<p class="help">${y >= 2001 ? "No constituency departs from the norm." : "Not available."}</p>`;
  foot("#gworthfoot");
}

/* ---------- If seats followed population: the latest polls on four distributions of the 460 seats ---------- */
const SC_KEYS = ["annex", "extended", "halfway", "norm"], SC_LET = { annex: "a", extended: "b", halfway: "c", norm: "d" };
const SC_SHORT = { annex: "As it is", extended: "Continuation of the trend", halfway: "Partial correction", norm: "Full correction" };
let scnP = null;
function loadScn() {
  return scnP || (scnP = fetchJSON("scenarios.json").then(d => {
    d.constituencies = tab(d.constituencies); d.measures = tab(d.measures); d.parties = tab(d.parties); d.gallagher = tab(d.gallagher);
    d.by_constituency = tab(d.by_constituency); d.additional_national = tab(d.additional_national); d.additional_constituency = tab(d.additional_constituency);
    SCN = d; return d;
  }));
}
function hemiPts(n, rows = 11) {
  const radii = Array.from({ length: rows }, (_, i) => 1 + i / (rows - 1)), rs = radii.reduce((a, b) => a + b, 0), counts = radii.map(r => Math.floor(n * r / rs));
  let rem = n - counts.reduce((a, b) => a + b, 0); const order = radii.map((r, i) => i).sort((p, q) => radii[q] - radii[p]);
  for (let k = 0; rem > 0; k++, rem--) counts[order[k % rows]]++;
  const pts = []; counts.forEach((nc, ri) => { for (let j = 0; j < nc; j++) { const ang = nc === 1 ? Math.PI / 2 : Math.PI - j * Math.PI / (nc - 1); pts.push({ x: radii[ri] * Math.cos(ang), y: radii[ri] * Math.sin(ang), ang, r: radii[ri] }); } });
  return pts.sort((p, q) => q.ang - p.ang || p.r - q.r);
}
const wholeSeats = k => { const o = SCN.meta.hemicycle_order; return Object.entries(SCN.hemi[k]).map(([p, n]) => ({ p, n })).sort((a, b) => o.indexOf(a.p) - o.indexOf(b.p)); };
const pcol = p => (SCN.palette && SCN.palette[p]) || "#7f7f7f";
function hemiSvg(k) {
  const blocks = wholeSeats(k), pts = hemiPts(460); let i = 0, o = "";
  blocks.forEach(b => { for (let j = 0; j < b.n; j++, i++) { const pt = pts[i]; o += `<circle cx="${pt.x.toFixed(3)}" cy="${(-pt.y).toFixed(3)}" r="0.046" fill="${pcol(b.p)}" data-tip="${h(b.p)}: ${b.n} seats"/>`; } });
  o += `<line x1="0" x2="0" y1="-0.86" y2="-2.12" stroke="#141414" stroke-width="0.012" stroke-dasharray="0.04 0.04"/><text x="0" y="-2.16" text-anchor="middle" style="font-size:0.085px">231</text>`;
  return `<svg viewBox="-2.2 -2.28 4.4 2.36" role="img" aria-label="Hemicycle: ${h(SC_SHORT[k])}">${o}</svg>`;
}
const sgn = x => (x > 0 ? "+" + x : x < 0 ? "−" + Math.abs(x) : "0");
function viewPopulation() {
  tip();
  const box = qs("#v-population");
  box.innerHTML = `<p class="help">Loading…</p>`;
  loadScn().then(d => {
    if (S.view !== "population") return;
    const k = GS.sc, m = d.meta, par = d.parties, set = GG.sets.sejm2001;
    const med = (kk, p) => (par.find(r => r.scenario === kk && r.party === p) || { median: 0 }).median;
    const names = [...new Set(par.filter(r => r.median > 0).map(r => r.party))].sort((a, b) => med("annex", b) - med("annex", a) || a.localeCompare(b));
    const ms = Object.fromEntries(d.measures.map(r => [r.scenario, r])), gal = Object.fromEntries(d.gallagher.map(r => [r.scenario, r.gallagher]));
    const cell = (p, kk) => { const r = par.find(x => x.scenario === kk && x.party === p); if (!r) return "<td></td>"; return `<td class="n" data-tip="80% range ${Math.round(r.lo)}–${Math.round(r.hi)}${r.p_majority > 0.005 ? "; majority in " + pc(r.p_majority, 0) + " of simulations" : ""}">${Math.round(r.median)}</td>`; };
    const rowsT = names.map(p => `<tr><td><span class="sw sq" style="background:${pcol(p)};margin-right:6px"></span>${h(p)}</td>${SC_KEYS.map(kk => cell(p, kk)).join("")}</tr>`).join("");
    const cons = d.constituencies.slice().sort((a, b) => (b[k] - b.annex) - (a[k] - a.annex) || a.okreg - b.okreg);
    const by = Object.fromEntries(d.constituencies.map(r => [r.okreg, r])), pa = names.includes(GS.pa) ? GS.pa : "";
    const gc = pa ? d.additional_constituency.filter(x => x.scenario === k && x.party === pa) : [];
    const gp = (o, dir) => gc.filter(x => x.okreg === o && x.dir === dir).sort((u, v) => u.k - v.k).map(x => x.p);
    const net = o => (gp(o, "gain")[0] || 0) - (gp(o, "lose")[0] || 0);
    const dOf = r => (pa ? net(r.okreg) : r[k] - r.annex);
    const lim = pa ? 1 : Math.max(1, ...d.constituencies.map(r => Math.abs(r[k] - r.annex)));
    const plist = (ps, what) => ps.map((p, j) => `${ord(j + 1)} ${what}: ${pc(p, 0)}`).join("; ");
    const fillOf = dv => (dv === 0 ? "rgb(245,245,243)" : dv > 0 ? hex(lerp(MID, SLATE, Math.min(1, dv / lim))) : hex(lerp(MID, RED, Math.min(1, -dv / lim))));
    let map = `<svg viewBox="0 0 ${GG.meta.view[0]} ${GG.meta.view[1]}" role="img" aria-label="Seats gained or lost by constituency">`;
    set.okreg.forEach((o, i) => { const r = by[o]; if (!r) return; const dv = dOf(r); map += `<path class="region" d="${set.d[i]}" fill="${fillOf(dv)}" data-tip="<b>${h(r.name)}</b><br>${pa ? `${h(pa)}: ${gp(r.okreg, "gain").length ? "chance of additional seats, " + plist(gp(r.okreg, "gain"), "extra seat") : gp(r.okreg, "lose").length ? "chance of fewer seats, " + plist(gp(r.okreg, "lose"), "seat lost") : "same seats in every simulation"}<br>` : ""}${r.annex} seats in the constituency now; ${r[k]} here (${sgn(r[k] - r.annex)})<br>${num(r.residents)} residents"/>`; });
    map += "</svg>";
    const constTbl = `<div class="chartbox"><h3>Seats by constituency</h3><div class="tbl"><table><tr><th>Constituency</th><th class="n">Residents</th><th class="n">(a) As it is</th><th class="n">(b) Continuation</th><th class="n">(c) Partial</th><th class="n">(d) Full</th></tr>
       ${cons.map(r => `<tr><td>${h(r.name)}</td><td class="n">${num(r.residents)}</td>${SC_KEYS.map(kk => `<td class="n"${kk === k ? ' style="font-weight:600"' : ""}>${r[kk]}${kk !== "annex" && r[kk] !== r.annex ? ` <span style="color:var(--muted)">${sgn(r[kk] - r.annex)}</span>` : ""}</td>`).join("")}</tr>`).join("")}</table></div></div>`;
    const nat = pa ? d.additional_national.filter(x => x.scenario === k && x.party === pa) : [], npv = dir => nat.filter(x => x.dir === dir).sort((u, v) => u.k - v.k).map(x => x.p);
    const natSame = (nat.find(x => x.dir === "same") || {}).p;
    const prs = pa ? d.constituencies.map(c => ({ c, g: gp(c.okreg, "gain"), l: gp(c.okreg, "lose") })).filter(r => r.g.length || r.l.length).sort((u, v) => (v.g[0] || 0) - (u.g[0] || 0) || (v.l[0] || 0) - (u.l[0] || 0) || u.c.okreg - v.c.okreg) : [];
    const cellP = ps => (ps.length ? ps.map((p, j) => `<span style="white-space:nowrap">${ord(j + 1)} ${pc(p, 0)}</span>`).join(", ") : '<span style="color:var(--muted)">none</span>');
    const partyTbl = pa ? `<div class="chartbox"><h3>${h(pa)}: chance of more or fewer seats than under the annex</h3>${k === "annex" ? `<p class="help">Under (a) nothing changes. Choose another distribution.</p>` : `
      <p class="lead" style="margin:0 0 8px"><b>Nationally.</b> ${npv("gain").length ? `More seats than under the annex: ${npv("gain").map((p, j) => `at least ${j + 1}, ${pc(p, 0)}`).join("; ")}.` : "It never wins more seats than under the annex."} ${npv("lose").length ? `Fewer seats: ${npv("lose").map((p, j) => `at least ${j + 1}, ${pc(p, 0)}`).join("; ")}.` : "It never wins fewer."} The same number: ${natSame == null ? "0%" : pc(natSame, 0)}.</p>
      ${prs.length ? `<div class="tbl"><table><tr><th>Constituency</th><th class="n">Seats, (a)</th><th class="n">Seats here</th><th>Chance of each additional seat</th><th>Chance of each seat lost</th></tr>
      ${prs.map(r => `<tr><td>${h(r.c.name)}</td><td class="n">${r.c.annex}</td><td class="n">${r.c[k]}</td><td>${cellP(r.g)}</td><td>${cellP(r.l)}</td></tr>`).join("")}</table></div>` : `<p class="help">No constituency has a different number of seats for ${h(pa)} in any simulation.</p>`}
      <p class="help" style="margin-top:6px">The chance of the 1st additional seat is the share of the ${m.draws} simulated parliaments in which the list wins at least one more seat in the constituency than it does under the annex with the same votes; the 2nd, at least two more, and so on. A constituency only gains seats if it is given more, and only loses them if it is given fewer. With ${m.draws} simulations, each chance is good to about three percentage points.</p>`}</div>` : "";
    box.innerHTML = `<div class="geo"><p class="lead">The Sejm's 460 seats are shared out between the constituencies by the annex to the Electoral Code, not by today's population, and the two have drifted apart. This view asks what the poll projection would give each party if the seats between constituencies were (a) left as they are, (b) moved further from the population norm in keeping with the trend since 2005, (c) moved halfway to the norm, or (d) set exactly by the norm (art. 202, residents at 31 December ${m.residents_year}). Only the number of seats in each constituency changes; each constituency's vote shares are the tracker's projection for ${h(m.week)}.</p>
     <div class="toolbar"><div class="seg small" role="group" aria-label="Distribution of seats">${SC_KEYS.map(kk => `<button type="button" data-sc="${kk}" aria-pressed="${k === kk}">(${SC_LET[kk]}) ${SC_SHORT[kk]}</button>`).join("")}</div>
       <label class="termsel">Show a party <select id="gpa"><option value="">All seats</option>${names.slice().sort((a, b) => a.localeCompare(b, "pl")).map(p => `<option value="${h(p)}"${p === pa ? " selected" : ""}>${h(p)}</option>`).join("")}</select></label>
       <span class="btnrow" style="margin-left:auto"><button class="btn2 sm" id="gscpng" type="button">PNG of all four</button></span></div>
     <div class="cols"><div class="chartbox"><h3>(${SC_LET[k]}) ${h(SC_SHORT[k])}</h3>${hemiSvg(k)}<p class="help" style="margin-top:6px">${h(m.scenarios[k])} Dots are seats in one typical simulated parliament (the one closest to the median seats); the dashed line marks the 231 needed for a majority.</p>
        <div class="legend">${wholeSeats(k).map(b => `<span><i class="sw sq" style="background:${pcol(b.p)}"></i>${h(b.p)} ${b.n}</span>`).join("")}</div></div>
      <div class="chartbox"><h3>${pa ? h(pa) + ": chance of gaining or losing seats, against (a)" : "Seats moved between constituencies"}</h3>${map}<div class="grad" style="background:linear-gradient(90deg,${hex(RED)},rgb(245,245,243),${hex(SLATE)})"></div>
        <div style="display:flex;justify-content:space-between;font-size:12px;color:var(--muted)">${pa ? "<span>Likely to lose a seat</span><span>no change</span><span>Likely to gain a seat</span>" : `<span>−${lim} seats</span><span>no change</span><span>+${lim}</span>`}</div></div></div>
     <div class="chartbox"><h3>Median seats by party, with 80% ranges on hover</h3><div class="tbl"><table><tr><th>List</th>${SC_KEYS.map(kk => `<th class="n">(${SC_LET[kk]}) ${SC_SHORT[kk]}</th>`).join("")}</tr>${rowsT}
       <tr><td>Seats moved between constituencies</td>${SC_KEYS.map(kk => `<td class="n">${ms[kk].seats_moved}</td>`).join("")}</tr>
       <tr><td>Constituencies with a different number</td>${SC_KEYS.map(kk => `<td class="n">${ms[kk].constituencies_changed}</td>`).join("")}</tr>
       <tr><td>Residents per seat, largest ÷ smallest</td>${SC_KEYS.map(kk => `<td class="n">${f2(ms[kk].max_min_ratio)}</td>`).join("")}</tr>
       <tr><td>Seats against residents (Loosemore–Hanby, points)</td>${SC_KEYS.map(kk => `<td class="n">${(100 * ms[kk].mal_residents).toFixed(1)}</td>`).join("")}</tr>
       <tr><td>Seats against votes (Gallagher, points)</td>${SC_KEYS.map(kk => `<td class="n">${f2(gal[kk])}</td>`).join("")}</tr></table></div>
       <p class="help" style="margin-top:6px">${m.caveats.map(h).join(" ")} ${m.draws} simulated parliaments.</p></div>
     ${pa ? partyTbl : constTbl}</div>`;
  }).catch(e => { box.innerHTML = `<p class="notice">Could not load the scenarios (${h(e.message)}).</p>`; });
}
// one PNG with the four hemicycles, drawn on a canvas, with the title and the citation of the other exports
async function scenarioPng() {
  const d = await loadScn(), m = d.meta, W = 1800, H = 1500, cv = document.createElement("canvas"); cv.width = W; cv.height = H;
  await ChartExport.fonts();
  const g = cv.getContext("2d"), F = "Jost, 'Avenir Next', Helvetica, Arial, sans-serif";
  g.fillStyle = "#fff"; g.fillRect(0, 0, W, H); g.textBaseline = "alphabetic";
  g.fillStyle = "#8B0000"; g.font = `300 44px ${F}`; g.fillText("Polling tracker · Sejm seats if the constituencies' seats followed population", 60, 80);
  g.font = `400 26px ${F}`; g.fillStyle = "#4B4B4B"; g.fillText(`The tracker's projection for the week of ${m.week}; only the number of seats in each constituency changes.`, 60, 122);
  const pw = 840, ox = [60, 900], oy = [160, 770];
  SC_KEYS.forEach((k, idx) => {
    const x0 = ox[idx % 2], y0 = oy[Math.floor(idx / 2)], cx = x0 + pw / 2, R = 160, pts = hemiPts(460), blocks = wholeSeats(k), baseY = y0 + 100 + 2.15 * R;
    g.fillStyle = "#141414"; g.font = `500 30px ${F}`; g.textAlign = "left"; g.fillText(`(${SC_LET[k]}) ${SC_SHORT[k]}`, x0, y0 + 36);
    g.font = `400 20px ${F}`; g.fillStyle = "#767676"; g.fillText(k === "annex" ? "Seats per constituency as in the Electoral Code's annex" : `${d.measures.find(r => r.scenario === k).seats_moved} seats moved between ${d.measures.find(r => r.scenario === k).constituencies_changed} constituencies`, x0, y0 + 66);
    let i = 0; blocks.forEach(b => { g.fillStyle = pcol(b.p); for (let j = 0; j < b.n; j++, i++) { const p = pts[i]; g.beginPath(); g.arc(cx + p.x * R, baseY - p.y * R, 0.046 * R, 0, 2 * Math.PI); g.fill(); } });
    g.strokeStyle = "#141414"; g.lineWidth = 1.5; g.setLineDash([5, 5]); g.beginPath(); g.moveTo(cx, baseY - 0.86 * R); g.lineTo(cx, baseY - 2.12 * R); g.stroke(); g.setLineDash([]);
    g.fillStyle = "#4B4B4B"; g.font = `400 18px ${F}`; g.textAlign = "center"; g.fillText("231", cx, baseY - 2.16 * R);
    g.font = `500 22px ${F}`; let lx = x0, ly = baseY + 52; const rs = blocks.slice().sort((a, b) => b.n - a.n);
    rs.forEach(b => { const t = `${b.p} ${b.n}`, w = g.measureText(t).width + 40; if (lx + w > x0 + pw) { lx = x0; ly += 32; } g.fillStyle = pcol(b.p); g.fillRect(lx, ly - 15, 15, 15); g.fillStyle = "#141414"; g.textAlign = "left"; g.fillText(t, lx + 22, ly); lx += w; });
  });
  g.textAlign = "left"; g.fillStyle = "#767676"; g.font = `400 19px ${F}`;
  const footTxt = [`Distributions: (a) annex to the Electoral Code; (b) the gap between the annex and the norm of representation (residents, art. 202) extended at its 2005–${m.residents_year} trend to ${m.horizon}; (c) halfway to the norm; (d) the norm on residents at 31 December ${m.residents_year}.`,
    `Seats are those of one typical simulated parliament (closest to the median seats) of ${m.draws}; each constituency's vote shares, the 5% threshold and the regional swing are the same in all four. Sources: PKW, GUS BDL.`];
  const wrap = t => { const out = []; let line = ""; t.split(" ").forEach(w => { if (g.measureText(line + " " + w).width > W - 120 && line) { out.push(line); line = w; } else line = line ? line + " " + w : w; }); out.push(line); return out; };
  const fl = footTxt.flatMap(wrap); fl.forEach((t, i) => g.fillText(t, 60, H - 70 - (fl.length - 1 - i) * 27));
  if (ChartExport.drawCite) ChartExport.drawCite(g, PAGE, 60, H - 28, W - 120);
  const blob = await new Promise(res => cv.toBlob(res, "image/png"));
  download("polling-tracker-seats-if-seats-followed-population.png", blob);
}

/* ---------- the module ---------- */
function bindOnce() {
  if (bindOnce.done) return; bindOnce.done = true;
  const onChange = e => {
    const t = e.target;
    if (t.id === "gterm") return setTerm(t.value);
    if (t.id === "gmeasure") { GS.measure = t.value; renderView(); saveHash(); return; }
    if (t.id === "gpa") { GS.pa = t.value; renderView(); saveHash(); }
  };
  const onClick = e => {
    const t = e.target, b = t.closest("button");
    const o = t.closest && t.closest("#measmap [data-o]");
    if (o) { S.constSel = S.constSel === +o.dataset.o ? null : +o.dataset.o; renderView(); saveHash(); return; }
    if (!b) return;
    if (b.dataset.gmode) { GS.mode = b.dataset.gmode; renderView(); saveHash(); return; }
    if (b.id === "gpng") { png("geo").catch(err => toast("Could not make the image: " + err.message)); return; }
    if (b.dataset.wm) { GS.wm = b.dataset.wm; renderView(); saveHash(); return; }
    if (b.dataset.sc) { GS.sc = b.dataset.sc; renderView(); saveHash(); return; }
    if (b.id === "gscpng") { scenarioPng().catch(err => toast("Could not make the PNG: " + err.message)); }
  };
  ["v-geomap", "v-worth", "v-population"].forEach(id => { const el = qs("#" + id); el.addEventListener("change", onChange); el.addEventListener("click", onClick); });
}
window.Geography = {
  readState(q) {
    const y = q.get("y"); if (y) GS.term = y === "c" ? "current" : y;
    if (q.get("m") && MEASURES[q.get("m")]) GS.measure = q.get("m");
    else if (y && y !== "c") GS.measure = "vvi_electors";
    if (q.get("md") === "circles") GS.mode = "circles";
    if (q.get("c") && isFinite(+q.get("c"))) S.constSel = +q.get("c");
    if (q.get("gw") && MEASURES[q.get("gw")]) GS.wm = q.get("gw");
    if (SC_KEYS.includes(q.get("sc"))) GS.sc = q.get("sc");
    GS.pa = q.get("pa") || "";
  },
  writeState(q) {
    if (!cur()) q.set("y", GS.term);
    if (S.view === "map") { if (GS.measure !== (cur() ? "proj" : "vvi_electors")) q.set("m", GS.measure); if (GS.mode !== "geo" && GS.measure !== "proj") q.set("md", GS.mode); if (S.constSel != null) q.set("c", S.constSel); }
    if (S.view === "worth" && GS.wm !== "vvi_electors") q.set("gw", GS.wm);
    if (S.view === "population") { if (GS.sc !== "annex") q.set("sc", GS.sc); if (GS.pa) q.set("pa", GS.pa); }
  },
  render(view) {
    bindOnce();
    const box = view === "map" ? qs("#measmap") : qs("#v-" + view);
    if (!CC) { if (view !== "map") box.innerHTML = `<p class="help">Loading the constituencies…</p>`; else qs("#geotb").innerHTML = `<span class="help">Loading the constituencies…</span>`; load().then(() => { if (S.view === view) Geography.render(view); }).catch(e => { qs("#geotb").innerHTML = `<p class="notice">Could not load the constituencies (${h(e.message)}).</p>`; }); return; }
    if (view === "map") viewMap(); else if (view === "worth") viewWorth(); else viewPopulation();
  },
  exportTitle() { return `${MEASURES[GS.measure].label} by constituency`; },
  exportSub() { const y = yearOf(); return `${cur() ? "The 2023 constituencies" : "The " + y + " election"}: ${MEASURES[GS.measure].help}`; },
  exportName() { return `constituencies-${GS.measure}-${yearOf()}`; }
};
})();
