/* Constituencies: what a vote is worth in each Sejm constituency, 1991-2023, and a profile of each. Data: constituencies.json, geometry.json, mps.json. No libraries. */
(function () {
"use strict";
const $ = s => document.querySelector(s);
const esc = s => String(s == null ? "" : s).replace(/[&<>"]/g, c => ({"&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;"}[c]));
const num = x => x == null ? "n/a" : Number(x).toLocaleString("en-GB");
const pct = (x, d = 1) => x == null ? "n/a" : (100 * x).toFixed(d) + "%";
const f2 = x => x == null ? "n/a" : x.toFixed(2);
const tab = t => t.rows.map(r => Object.fromEntries(t.columns.map((c, i) => [c, r[i]])));
const cache = {}; const fetchJSON = f => cache[f] || (cache[f] = fetch(f).then(r => { if (!r.ok) throw new Error(f + " " + r.status); return r.json(); }));
const VIEWS = [["map", "Map"], ["worth", "What a vote is worth"], ["method", "How it is measured"]];
const MEASURES = {
  vvi_electors: {label: "Vote value: electors per seat", kind: "div", help: "1 is the national average. Above 1, a registered elector counts for more than average in choosing the Sejm; below 1, for less."},
  vvi_electors_dom: {label: "Vote value: electors at home per seat", kind: "div", help: "As the first measure, leaving out electors registered abroad or on ships, whom PKW counts in Warsaw (constituency 19): 637,000 of them in 2023."},
  vvi_votes: {label: "Vote value: votes cast per seat", kind: "div", help: "As above, but counting the votes actually cast, so a constituency with low turnout gets more seats per vote."},
  vvi_residents: {label: "Vote value: residents per seat", kind: "div", help: "As above, counting residents, the basis the Electoral Code uses (2001 onwards)."},
  seats: {label: "Seats (district magnitude)", kind: "seq", fmt: x => String(x), help: "The number of deputies the constituency elects."},
  turnout: {label: "Turnout", kind: "seq", fmt: x => pct(x, 0), help: "Votes cast as a share of registered electors."},
  wasted: {label: "Votes that elected no one", kind: "seq", fmt: x => pct(x, 0), help: "Votes for lists that won no seat in the constituency, including lists below the national threshold."},
  largest_bonus: {label: "Bonus to the largest list", kind: "seq", fmt: x => pct(x, 0), help: "The largest list's share of the seats minus its share of the votes, in points."}
};
let C, G, MP = null;
const S = {view: "map", year: 2023, measure: "vvi_electors", mode: "geo", sel: null, wm: "vvi_electors"};
const colOf = l => (C.palette && C.palette[l]) || "#7f7f7f";
const setOf = y => C.elections.find(e => e.year === y).set;
const rowsOf = y => C.constituencies.filter(r => r.year === y);
const valueOf = (r, m) => m === "wasted" ? r.wasted_lost_locally + r.wasted_below_threshold : r[m];

function hash() {
  const p = new URLSearchParams(location.hash.slice(1));
  if (VIEWS.some(v => v[0] === p.get("view"))) S.view = p.get("view");
  if (C.elections.some(e => e.year === +p.get("y"))) S.year = +p.get("y");
  if (p.get("c")) S.sel = +p.get("c");
}
function save() { const p = new URLSearchParams({view: S.view, y: S.year}); if (S.sel) p.set("c", S.sel); history.replaceState(null, "", "#" + p.toString()); }
const tip = $("#tip");
document.addEventListener("mousemove", e => { const t = e.target.closest && e.target.closest("[data-tip]"); if (!t) { tip.style.display = "none"; return; }
  tip.innerHTML = t.dataset.tip; tip.style.display = "block"; tip.style.left = Math.min(innerWidth - tip.offsetWidth - 8, e.clientX + 12) + "px"; tip.style.top = (e.clientY + 14) + "px"; });

function toolbar() {
  $("#tb").innerHTML = `<div class="seg" role="group" aria-label="View">${VIEWS.map(v => `<button data-v="${v[0]}" aria-pressed="${S.view === v[0]}">${v[1]}</button>`).join("")}</div>
   <label class="ctl">Election <span class="seg small wrap" role="group" aria-label="Election">${C.elections.map(e => `<button data-y="${e.year}" aria-pressed="${S.year === e.year}">${e.year}</button>`).join("")}</span></label>`;
  $("#tb").onclick = e => { const b = e.target.closest("button"); if (!b) return;
    if (b.dataset.v) S.view = b.dataset.v; else { S.year = +b.dataset.y; if (S.measure === "vvi_residents" && S.year < 2001) S.measure = "vvi_electors"; if (S.sel && !rowsOf(S.year).some(r => r.okreg === S.sel)) S.sel = null; }
    render(); };
}
function render() {
  save(); toolbar();
  const el = C.elections.find(e => e.year === S.year);
  ({map: viewMap, worth: viewWorth, method: viewMethod})[S.view]();
  $("#foot").innerHTML = `${S.year}: ${el.constituencies} constituencies, ${el.seats_total} constituency seats${el.national_list_seats ? ` and ${el.national_list_seats} from national lists (not counted here)` : ""}. ${esc(C.meta.caveats[3])}`;
}

/* ---------- colour scales ---------- */
const lerp = (a, b, t) => a.map((v, i) => Math.round(v + (b[i] - v) * t));
const hex = c => "rgb(" + c.join(",") + ")";
const RED = [139, 0, 0], LIGHT = [246, 235, 235], SLATE = [62, 92, 118], MID = [245, 245, 243];
function scale(vals, m) {
  const def = MEASURES[m];
  if (def.kind === "div") { const lim = Math.max(0.12, Math.max(...vals.map(v => Math.abs(v - 1)))); return {f: v => v >= 1 ? hex(lerp(MID, RED, Math.min(1, (v - 1) / lim))) : hex(lerp(MID, SLATE, Math.min(1, (1 - v) / lim))), lo: 1 - lim, hi: 1 + lim, lc: hex(SLATE), hc: hex(RED), mid: true}; }
  const lo = Math.min(...vals), hi = Math.max(...vals); return {f: v => hex(lerp(LIGHT, RED, hi > lo ? (v - lo) / (hi - lo) : 0.5)), lo, hi, lc: hex(LIGHT), hc: hex(RED)};
}

/* ---------- map and profile ---------- */
function viewMap() {
  const rows = rowsOf(S.year), set = G.sets[setOf(S.year)];
  if (MEASURES[S.measure] === undefined || (S.measure === "vvi_residents" && S.year < 2001)) S.measure = "vvi_electors";
  const ms = Object.keys(MEASURES).filter(m => m !== "vvi_residents" || S.year >= 2001);
  const m = S.measure, def = MEASURES[m], vals = rows.map(r => valueOf(r, m)), sc = scale(vals, m);
  const by = Object.fromEntries(rows.map(r => [r.okreg, r]));
  $("#main").innerHTML = `<p class="lead">What a vote is worth depends on where it is cast. Seats follow residents, at least seven to a constituency, so a registered elector, or a vote cast, counts for more in some constituencies than others. Choose a measure and a constituency.</p>
   <div class="toolbar"><label class="ctl" for="ms">Colour by <select class="sel" id="ms">${ms.map(k => `<option value="${k}" ${k === m ? "selected" : ""}>${MEASURES[k].label}</option>`).join("")}</select></label>
   <div class="seg small" role="group" aria-label="Map type"><button data-mode="geo" aria-pressed="${S.mode === "geo"}">Map</button><button data-mode="circles" aria-pressed="${S.mode === "circles"}">Circles by electors</button></div></div>
   <div class="mapwrap"><div class="chartbox"><div id="mp"></div><div class="grad" style="background:linear-gradient(90deg,${sc.lc},${sc.mid ? "rgb(245,245,243)," : ""}${sc.hc})"></div>
     <div style="display:flex;justify-content:space-between;font-size:12px;color:var(--muted)"><span>${def.fmt ? def.fmt(sc.lo) : f2(sc.lo)}</span>${sc.mid ? "<span>1 (average)</span>" : ""}<span>${def.fmt ? def.fmt(sc.hi) : f2(sc.hi)}</span></div>
     <p class="help" style="margin-top:8px">${esc(def.help)}${setOf(S.year) !== "sejm2001" ? " This election used the " + set.label.replace("Sejm constituencies", "Sejm constituencies of") .replace("of,", "of") + "." : ""}</p></div>
   <div class="chartbox" id="prof"></div></div>`;
  $("#ms").onchange = e => { S.measure = e.target.value; render(); };
  document.querySelectorAll("[data-mode]").forEach(b => b.onclick = () => { S.mode = b.dataset.mode; render(); });
  const [vx, vy, vw, vh] = S.mode === "circles" ? set.circle_view : [0, 0, G.meta.view[0], G.meta.view[1]];
  let s = `<svg viewBox="${vx} ${vy} ${vw} ${vh}" role="img" aria-label="Map of the constituencies">`;
  set.okreg.forEach((o, i) => { const r = by[o]; if (!r) return; const v = valueOf(r, m), tipText = `<b>${esc(r.name)}</b> (${r.seats} seats)<br>${def.label}: ${def.fmt ? def.fmt(v) : f2(v)}`;
    if (S.mode === "geo") s += `<path class="region ${S.sel === o ? "sel" : ""}" data-o="${o}" d="${set.d[i]}" fill="${sc.f(v)}" data-tip="${tipText}"/>`;
    else { const c = set.circles[i]; s += `<circle class="cir ${S.sel === o ? "sel" : ""}" data-o="${o}" cx="${c[0]}" cy="${c[1]}" r="${c[2]}" fill="${sc.f(v)}" data-tip="${tipText}"/>`; } });
  $("#mp").innerHTML = s + "</svg>";
  $("#mp").onclick = e => { const t = e.target.closest("[data-o]"); if (t) { S.sel = +t.dataset.o; save(); viewMap(); } };
  profile();
}
function profile() {
  const box = $("#prof"); const rows = rowsOf(S.year);
  if (!S.sel) { box.innerHTML = `<h3>Constituency</h3><p class="help">Select a constituency on the map to see what its seats cost, how its lists did and, from 2001, who represents it.</p>`; return; }
  const r = rows.find(x => x.okreg === S.sel); if (!r) { box.innerHTML = ""; return; }
  const set = setOf(S.year), hist = C.constituencies.filter(x => x.okreg === S.sel && C.elections.find(e => e.year === x.year).set === set).sort((a, b) => a.year - b.year);
  const L = C.list_results.filter(x => x.year === S.year && x.okreg === S.sel).sort((a, b) => b.votes - a.votes), tot = L.reduce((a, x) => a + x.votes, 0), shown = L.slice(0, 7), rest = L.slice(7);
  const el = C.elections.find(e => e.year === S.year);
  const proj = set === "sejm2001" && S.year === 2023 ? C.projection.rows.filter(p => p.okreg === S.sel) : [];
  box.innerHTML = `<h2>${esc(r.name)}</h2><p class="help">Constituency ${S.sel}, ${S.year}. <a href="../election-results.html#e=sejm-${S.year}">Results on Election results</a>${set === "sejm2001" ? ` · <a href="../election-results.html#u=c${S.sel}">its history</a>` : ""}</p>
   <dl class="kv"><dt>Seats</dt><dd>${r.seats}${r.legal_seats != null && r.legal_seats !== r.seats ? ` (the norm of representation gives ${r.legal_seats})` : ""}</dd>
   <dt>Registered electors per seat</dt><dd>${num(r.electors_per_seat)} · vote value ${f2(r.vvi_electors)}</dd>
   ${r.electors_abroad > 5000 ? `<dt>Of them registered abroad or on ships</dt><dd>${num(r.electors_abroad)} · without them ${f2(r.vvi_electors_dom)}</dd>` : ""}
   <dt>Votes cast per seat</dt><dd>${num(r.votes_per_seat)} · ${f2(r.vvi_votes)}</dd>
   ${r.residents_per_seat != null ? `<dt>Residents per seat</dt><dd>${num(r.residents_per_seat)} · ${f2(r.vvi_residents)}</dd>` : ""}
   <dt>Turnout</dt><dd>${pct(r.turnout, 1)}</dd>
   ${r.last_seat_votes != null ? `<dt>Votes behind the last seat won</dt><dd>${num(r.last_seat_votes)}</dd>` : ""}
   ${r.guaranteed_share != null ? `<dt>Share that guarantees a seat</dt><dd>${pct(r.guaranteed_share, 1)} of valid votes</dd>` : ""}
   <dt>Votes for lists with no seat</dt><dd>${pct(r.wasted_lost_locally + r.wasted_below_threshold, 1)}</dd></dl>
   <h3>Lists</h3><div class="tbl"><table><thead><tr><th>List</th><th class="n">Votes</th><th class="n">Share</th><th class="n">Seats</th><th class="n">Votes per seat</th></tr></thead><tbody>${shown.map(x => `<tr><td><span class="sw sq" style="background:${colOf(x.label)};margin-right:6px"></span>${esc(x.short)}</td><td class="n">${num(x.votes)}</td><td class="n">${pct(x.votes / tot, 1)}</td><td class="n">${x.seats}</td><td class="n">${x.seats ? num(Math.round(x.votes / x.seats)) : ""}</td></tr>`).join("")}${rest.length ? `<tr><td>${rest.length} other lists</td><td class="n">${num(rest.reduce((a, x) => a + x.votes, 0))}</td><td class="n">${pct(rest.reduce((a, x) => a + x.votes, 0) / tot, 1)}</td><td class="n">${rest.reduce((a, x) => a + x.seats, 0)}</td><td></td></tr>` : ""}</tbody></table></div>
   ${hist.length > 1 ? `<h3 style="margin-top:12px">Vote value over time</h3><div id="hs"></div>` : ""}
   ${proj.length ? `<h3 style="margin-top:12px">The Polling tracker's projection (${esc(C.projection.date)})</h3><p class="help">${proj.map(p => `<span style="color:${colOf(p.party)}">&#9632;</span> ${esc(p.party)} ${p.seats}`).join(" · ")}</p>` : ""}
   ${set === "sejm2001" ? `<div id="mpl"></div><div id="cv"></div>` : ""}`;
  if (hist.length > 1) {
    const W = 360, H = 120, m = {l: 30, r: 10, t: 8, b: 20}, ys = hist.map(h => h.year), vs = hist.flatMap(h => [h.vvi_electors]), lo = Math.min(0.85, ...vs), hi = Math.max(1.15, ...vs);
    const X = y => m.l + (y - ys[0]) / Math.max(1, ys[ys.length - 1] - ys[0]) * (W - m.l - m.r), Y = v => m.t + (hi - v) / (hi - lo) * (H - m.t - m.b);
    let g = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Vote value over time"><line x1="${m.l}" x2="${W - m.r}" y1="${Y(1)}" y2="${Y(1)}" stroke="#D5D5D5" stroke-dasharray="3 3"/><text x="${m.l - 4}" y="${Y(1) + 4}" text-anchor="end">1</text>
      <polyline fill="none" stroke="#8B0000" stroke-width="2" points="${hist.map(h => X(h.year) + "," + Y(h.vvi_electors)).join(" ")}"/>`;
    hist.forEach(h => g += `<circle cx="${X(h.year)}" cy="${Y(h.vvi_electors)}" r="${h.year === S.year ? 5 : 3}" fill="#8B0000" data-tip="${h.year}: ${f2(h.vvi_electors)} (${h.seats} seats, ${num(h.electors_per_seat)} electors per seat)"/><text x="${X(h.year)}" y="${H - 4}" text-anchor="middle">${h.year}</text>`);
    $("#hs").innerHTML = g + "</svg>";
  }
  if (set === "sejm2001") { covBlock(); mpBlock(); }
}
function covBlock() {
  const cv = C.covariates, vals = Object.fromEntries(cv.values.rows.filter(r => r[0] === S.sel).map(r => [r[1], r[2]]));
  $("#cv").innerHTML = `<h3 style="margin-top:12px">The constituency against Poland</h3><div class="tbl"><table><tbody>${cv.vars.map(v => `<tr><td>${esc(v.label)} <span class="help">${esc(v.year)}</span></td><td class="n">${vals[v.var] == null ? "n/a" : (v.unit.startsWith("%") ? vals[v.var].toFixed(1) + "%" : num(Math.round(vals[v.var])))}</td><td class="n help">${v.national == null ? "" : (v.unit.startsWith("%") ? v.national.toFixed(1) + "%" : num(Math.round(v.national)))} Poland</td></tr>`).join("")}</tbody></table></div>`;
}
async function mpBlock() {
  if (S.year < 2001) return;
  const j = await fetchJSON("mps.json"); MP = MP || tab(j.mps);
  const box = $("#mpl"); if (!box) return;
  const ms = MP.filter(x => x.year === S.year && x.okreg === S.sel);
  box.innerHTML = `<h3 style="margin-top:12px">MPs elected here, ${S.year} (${ms.length}, replacements included)</h3><p class="help">${ms.map(x => `<span style="color:${colOf(x.family)}">&#9632;</span> ${esc(x.name)} <span style="color:var(--muted)">${esc(x.club || "")}</span>`).join("<br>")}</p><p class="help">Each MP's record is on the <a href="../legislative-activity.html#view=mps&t=${C.elections.findIndex(e => e.year === S.year) + 1}&mp=${ms[0] ? ms[0].mp_id : ""}">Legislative activity</a> page.</p>`;
}

/* ---------- what a vote is worth ---------- */
function viewWorth() {
  const rows = rowsOf(S.year), wm = (S.wm === "vvi_residents" && S.year < 2001) ? "vvi_electors" : S.wm;
  const el = C.elections.find(e => e.year === S.year);
  $("#main").innerHTML = `<p class="lead">Every Sejm election since 2001 used the same 41 constituencies, with nearly the same seat numbers, while the population moved. ${S.year >= 2001 ? `In ${S.year}, residents per seat ranged ${f2(el.ratio_residents)} to one between the constituencies with the most and the fewest people for each seat` : `In ${S.year}, registered electors per seat ranged ${f2(el.ratio_electors)} to one between the constituencies`}; counting votes cast, the range was ${f2(el.ratio_votes)} to one.</p>
   <div class="cols"><div class="chartbox"><h3>How far the vote's value is from equal, by election</h3><div id="mal"></div>${`<p class="help">Half the sum, over constituencies, of the absolute difference between a constituency's share of the seats and its share of the electors (or residents). Zero would be perfectly equal; 0.04 means 4% of the seats sit in the wrong constituencies.</p>`}</div>
   <div class="chartbox"><h3>Magnitude and the largest list's bonus</h3><div id="bon"></div><p class="help">Each dot is one constituency in one election, 2005 to 2023: the largest list's seat share minus its vote share, by the constituency's number of seats. The curve is what D'Hondt's rule guarantees a list at the margin, 1 / (seats + 1) of the votes.</p></div></div>
   <div class="chartbox"><div class="toolbar"><h3 style="margin:0">Vote value by constituency, ${S.year}</h3><div class="seg small" role="group" aria-label="Measure"><button data-wm="vvi_electors" aria-pressed="${wm === "vvi_electors"}">Electors per seat</button><button data-wm="vvi_electors_dom" aria-pressed="${wm === "vvi_electors_dom"}">Electors at home</button><button data-wm="vvi_votes" aria-pressed="${wm === "vvi_votes"}">Votes per seat</button>${S.year >= 2001 ? `<button data-wm="vvi_residents" aria-pressed="${wm === "vvi_residents"}">Residents per seat</button>` : ""}</div></div><div id="dot"></div>
   <p class="help">Each dot is a constituency; the size shows its seats. Left of 1, a vote counts for less than the national average, right of it, for more. ${S.year >= 2001 ? "" : "Residents are not available on this basis before 2001."}</p></div>
   <div class="cols"><div class="chartbox"><h3>What a seat costs each list, ${S.year}</h3><div class="tbl" id="cost"></div><p class="help">Votes per seat won in the constituencies, and the list's share of seats minus its share of votes. ${el.national_list_seats ? "The " + el.national_list_seats + " national-list seats are not counted." : ""}</p></div>
   <div class="chartbox"><h3>Seats the Electoral Code's norm gives, against the seats the constituency has</h3><div id="legal"></div><p class="help">${S.year >= 2001 ? "Residents divided by seats, rounded as art. 202 says, with at least 7 seats. A bar to the right means the constituency has more seats than its population gives it." : "Residents are not available on this basis before 2001."}</p></div></div>`;
  document.querySelectorAll("[data-wm]").forEach(b => b.onclick = () => { S.wm = b.dataset.wm; viewWorth(); });
  // malapportionment over time
  const E = C.elections.filter(e => e.year >= 1991), W = 520, H = 190, m = {l: 40, r: 14, t: 10, b: 22}, mx = Math.max(...E.map(e => e.mal_electors), 0.045);
  const X = i => m.l + i / (E.length - 1) * (W - m.l - m.r), Y = v => m.t + (1 - v / mx) * (H - m.t - m.b);
  let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Malapportionment by election">`;
  for (let v = 0; v <= mx + 1e-9; v += 0.01) s += `<line x1="${m.l}" x2="${W - m.r}" y1="${Y(v)}" y2="${Y(v)}" stroke="#ECECEC"/><text x="${m.l - 5}" y="${Y(v) + 4}" text-anchor="end">${(v * 100).toFixed(0)}%</text>`;
  const line = (key, col, dash) => { const pts = E.map((e, i) => [i, e[key]]).filter(p => p[1] != null); return `<polyline fill="none" stroke="${col}" stroke-width="2" ${dash ? 'stroke-dasharray="4 3"' : ""} points="${pts.map(p => X(p[0]) + "," + Y(p[1])).join(" ")}"/>` + pts.map(p => `<circle cx="${X(p[0])}" cy="${Y(p[1])}" r="${E[p[0]].year === S.year ? 5 : 3}" fill="${col}" data-tip="${E[p[0]].year}: ${(p[1] * 100).toFixed(1)}%"/>`).join(""); };
  E.forEach((e, i) => s += `<text x="${X(i)}" y="${H - 5}" text-anchor="middle">${e.year}</text>`);
  s += line("mal_electors", "#8B0000", false) + line("mal_electors_dom", "#B8860B", true) + line("mal_residents", "#3E5C76", true) + `<text class="t" x="${m.l + 6}" y="${m.t + 6}" style="fill:#8B0000">electors</text><text class="t" x="${m.l + 6}" y="${m.t + 20}" style="fill:#B8860B">electors at home</text><text class="t" x="${m.l + 6}" y="${m.t + 34}" style="fill:#3E5C76">residents</text>`;
  $("#mal").innerHTML = s + "</svg>";
  // bonus against magnitude
  const P = C.constituencies.filter(r => r.year >= 2005), W2 = 520, H2 = 190, m2 = {l: 40, r: 14, t: 10, b: 24}, Xm = v => m2.l + (v - 6) / 15 * (W2 - m2.l - m2.r), Yb = v => m2.t + (1 - v / 0.24) * (H2 - m2.t - m2.b);
  let b = `<svg viewBox="0 0 ${W2} ${H2}" role="img" aria-label="Bonus against magnitude">`;
  for (let v = 0; v <= 0.24; v += 0.08) b += `<line x1="${m2.l}" x2="${W2 - m2.r}" y1="${Yb(v)}" y2="${Yb(v)}" stroke="#ECECEC"/><text x="${m2.l - 5}" y="${Yb(v) + 4}" text-anchor="end">${Math.round(v * 100)}</text>`;
  for (let M = 7; M <= 20; M += 3) b += `<text x="${Xm(M)}" y="${H2 - 6}" text-anchor="middle">${M}</text>`;
  P.forEach(r => b += `<circle cx="${Xm(r.seats) + ((r.year - 2005) % 5 - 2) * 1.8}" cy="${Yb(Math.max(0, r.largest_bonus))}" r="3" fill="#8B0000" fill-opacity=".35" data-tip="<b>${esc(r.name)} ${r.year}</b><br>${r.seats} seats: ${esc(r.largest_list)} ${pct(r.largest_vote_share, 0)} of votes, ${pct(r.largest_seat_share, 0)} of seats"/>`);
  b += `<polyline fill="none" stroke="#3E5C76" stroke-width="1.6" points="${[7, 8, 9, 10, 12, 14, 17, 20].map(M => Xm(M) + "," + Yb(1 / (M + 1))).join(" ")}"/>`;
  $("#bon").innerHTML = b + `<text x="${W2 - m2.r}" y="${m2.t + 8}" text-anchor="end" style="fill:#3E5C76">1 / (seats + 1)</text><text x="${m2.l + 8}" y="${m2.t + 6}">points</text></svg>`;
  // dot plot of the index
  const sorted = rows.filter(r => r[wm] != null).sort((a, c) => a[wm] - c[wm]), lim = Math.max(0.15, ...sorted.map(r => Math.abs(r[wm] - 1))) * 1.05, rowH = 15, W3 = 520, H3 = sorted.length * rowH + 26, ml = 100;
  const X3 = v => ml + (v - (1 - lim)) / (2 * lim) * (W3 - ml - 14);
  let d = `<svg viewBox="0 0 ${W3} ${H3}" role="img" aria-label="Vote value by constituency"><line x1="${X3(1)}" x2="${X3(1)}" y1="4" y2="${H3 - 20}" stroke="#141414" stroke-dasharray="3 3"/>`;
  [1 - lim, 1, 1 + lim].forEach(v => d += `<text x="${X3(v)}" y="${H3 - 6}" text-anchor="middle">${f2(v)}</text>`);
  sorted.forEach((r, i) => { const y = 10 + i * rowH; d += `<text class="t" x="${ml - 6}" y="${y + 4}" text-anchor="end" style="font-size:10.5px">${esc(r.name)}</text><circle cx="${X3(r[wm])}" cy="${y}" r="${2.5 + r.seats / 4}" fill="${r[wm] >= 1 ? "#8B0000" : "#3E5C76"}" fill-opacity=".85" data-tip="<b>${esc(r.name)}</b> (${r.seats} seats)<br>${f2(r[wm])}; ${num(r.electors_per_seat)} electors per seat"/>`; });
  $("#dot").innerHTML = d + "</svg>";
  // costs and legal seats
  const co = C.list_costs.filter(x => x.year === S.year).sort((a, c) => c.votes - a.votes).slice(0, 8);
  $("#cost").innerHTML = `<table><thead><tr><th>List</th><th class="n">Votes per seat</th><th class="n">Seat share minus vote share</th></tr></thead><tbody>${co.map(x => `<tr><td><span class="sw sq" style="background:${colOf(x.label)};margin-right:6px"></span>${esc(x.short)}</td><td class="n">${x.votes_per_seat == null ? "no seat" : num(x.votes_per_seat)}</td><td class="n">${x.bonus >= 0 ? "+" : ""}${(x.bonus * 100).toFixed(1)}</td></tr>`).join("")}</tbody></table>`;
  const lg = rows.filter(r => r.legal_seats != null).sort((a, c) => (c.seats - c.legal_seats) - (a.seats - a.legal_seats)).filter(r => r.seats !== r.legal_seats);
  $("#legal").innerHTML = lg.length ? `<div class="tbl"><table><tbody>${lg.map(r => `<tr><td>${esc(r.name)}</td><td class="n">${r.seats} against ${r.legal_seats}</td><td class="n">${r.seats - r.legal_seats > 0 ? "+" : ""}${r.seats - r.legal_seats}</td></tr>`).join("")}</tbody></table></div>` : `<p class="help">${S.year >= 2001 ? "No constituency departs from the norm." : "Not available."}</p>`;
}

/* ---------- method ---------- */
function viewMethod() {
  $("#main").innerHTML = `<div class="chartbox"><h3>The law</h3><p class="lead">${esc(C.meta.legal.norm)}</p><h3 style="margin-top:12px">What the numbers are</h3><ul class="help">${C.meta.caveats.map(c => `<li>${esc(c)}</li>`).join("")}</ul>
   <h3 style="margin-top:12px">How seats go to lists</h3><p class="help">${Object.entries(C.meta.legal.formulas).map(([y, f]) => y + ": " + ({hare_niemeyer: "largest remainder (Hare–Niemeyer)", dhondt: "D'Hondt", "sainte_lague_1.4": "modified Sainte-Laguë (first divisor 1.4)"})[f]).join("; ")}.</p>
   <h3 style="margin-top:12px">National lists, 1991–97</h3><p class="help">${C.national_lists.filter(x => x.year === S.year).length ? C.national_lists.filter(x => x.year === S.year).map(x => `${esc(x.committee)} ${x.seats}`).join("; ") : "None this election."}</p>
   <h3 style="margin-top:12px">Sources</h3><ul class="help">${C.meta.sources.map(c => `<li>${esc(c)}</li>`).join("")}</ul></div>`;
}

Promise.all([fetchJSON("constituencies.json"), fetchJSON("geometry.json")]).then(([c, g]) => { C = c; G = g; C.constituencies = tab(C.constituencies); C.elections = tab(C.elections); C.list_results = tab(C.list_results); C.list_costs = tab(C.list_costs); C.projection.rows = tab(C.projection.rows); C.covariates.values.rows = C.covariates.values.rows;
  hash(); render(); }).catch(e => { $("#main").innerHTML = `<p class="notice">Could not load the data (${esc(e.message)}).</p>`; });
window.addEventListener("hashchange", () => { if (C) { hash(); render(); } });
})();
