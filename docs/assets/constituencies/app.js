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
const VIEWS = [["map", "Map"], ["worth", "What a vote is worth"], ["seats", "If seats followed population"], ["method", "How it is measured"]];
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
const S = {view: "map", year: 2023, measure: "vvi_electors", mode: "geo", sel: null, wm: "vvi_electors", sc: "annex", pa: ""};
const colOf = l => (C.palette && C.palette[l]) || "#7f7f7f";
const setOf = y => C.elections.find(e => e.year === y).set;
const rowsOf = y => C.constituencies.filter(r => r.year === y);
const valueOf = (r, m) => m === "wasted" ? r.wasted_lost_locally + r.wasted_below_threshold : r[m];

function hash() {
  const p = new URLSearchParams(location.hash.slice(1));
  if (VIEWS.some(v => v[0] === p.get("view"))) S.view = p.get("view");
  if (C.elections.some(e => e.year === +p.get("y"))) S.year = +p.get("y");
  if (p.get("c")) S.sel = +p.get("c");
  if (["annex", "extended", "halfway", "norm"].includes(p.get("sc"))) S.sc = p.get("sc");
  S.pa = p.get("pa") || "";
}
function save() { const p = new URLSearchParams({view: S.view, y: S.year}); if (S.sel) p.set("c", S.sel); if (S.view === "seats") { p.set("sc", S.sc); if (S.pa) p.set("pa", S.pa); } history.replaceState(null, "", "#" + p.toString()); }
const tip = $("#tip");
document.addEventListener("mousemove", e => { const t = e.target.closest && e.target.closest("[data-tip]"); if (!t) { tip.style.display = "none"; return; }
  tip.innerHTML = t.dataset.tip; tip.style.display = "block"; tip.style.left = Math.min(innerWidth - tip.offsetWidth - 8, e.clientX + 12) + "px"; tip.style.top = (e.clientY + 14) + "px"; });

function toolbar() {
  $("#tb").innerHTML = `<div class="seg" role="group" aria-label="View">${VIEWS.map(v => `<button data-v="${v[0]}" aria-pressed="${S.view === v[0]}">${v[1]}</button>`).join("")}</div>
   <label class="ctl" ${S.view === "seats" ? "hidden" : ""}>Election <span class="seg small wrap" role="group" aria-label="Election">${C.elections.map(e => `<button data-y="${e.year}" aria-pressed="${S.year === e.year}">${e.year}</button>`).join("")}</span></label>`;
  $("#tb").onclick = e => { const b = e.target.closest("button"); if (!b) return;
    if (b.dataset.v) S.view = b.dataset.v; else { S.year = +b.dataset.y; if (S.measure === "vvi_residents" && S.year < 2001) S.measure = "vvi_electors"; if (S.sel && !rowsOf(S.year).some(r => r.okreg === S.sel)) S.sel = null; }
    render(); };
}
function render() {
  save(); toolbar();
  const el = C.elections.find(e => e.year === S.year);
  ({map: viewMap, worth: viewWorth, seats: viewSeats, method: viewMethod})[S.view]();
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
    let g = `<svg style="max-width:${Math.round(W * 1.2)}px" viewBox="0 0 ${W} ${H}" role="img" aria-label="Vote value over time"><line x1="${m.l}" x2="${W - m.r}" y1="${Y(1)}" y2="${Y(1)}" stroke="#D5D5D5" stroke-dasharray="3 3"/><text x="${m.l - 4}" y="${Y(1) + 4}" text-anchor="end">1</text>
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
  let s = `<svg style="max-width:${Math.round(W * 1.2)}px" viewBox="0 0 ${W} ${H}" role="img" aria-label="Malapportionment by election">`;
  for (let v = 0; v <= mx + 1e-9; v += 0.01) s += `<line x1="${m.l}" x2="${W - m.r}" y1="${Y(v)}" y2="${Y(v)}" stroke="#ECECEC"/><text x="${m.l - 5}" y="${Y(v) + 4}" text-anchor="end">${(v * 100).toFixed(0)}%</text>`;
  const line = (key, col, dash) => { const pts = E.map((e, i) => [i, e[key]]).filter(p => p[1] != null); return `<polyline fill="none" stroke="${col}" stroke-width="2" ${dash ? 'stroke-dasharray="4 3"' : ""} points="${pts.map(p => X(p[0]) + "," + Y(p[1])).join(" ")}"/>` + pts.map(p => `<circle cx="${X(p[0])}" cy="${Y(p[1])}" r="${E[p[0]].year === S.year ? 5 : 3}" fill="${col}" data-tip="${E[p[0]].year}: ${(p[1] * 100).toFixed(1)}%"/>`).join(""); };
  E.forEach((e, i) => s += `<text x="${X(i)}" y="${H - 5}" text-anchor="middle">${e.year}</text>`);
  s += line("mal_electors", "#8B0000", false) + line("mal_electors_dom", "#B8860B", true) + line("mal_residents", "#3E5C76", true) + `<text class="t" x="${m.l + 6}" y="${m.t + 6}" style="fill:#8B0000">electors</text><text class="t" x="${m.l + 6}" y="${m.t + 20}" style="fill:#B8860B">electors at home</text><text class="t" x="${m.l + 6}" y="${m.t + 34}" style="fill:#3E5C76">residents</text>`;
  $("#mal").innerHTML = s + "</svg>";
  // bonus against magnitude
  const P = C.constituencies.filter(r => r.year >= 2005), W2 = 520, H2 = 190, m2 = {l: 40, r: 14, t: 10, b: 24}, Xm = v => m2.l + (v - 6) / 15 * (W2 - m2.l - m2.r), Yb = v => m2.t + (1 - v / 0.24) * (H2 - m2.t - m2.b);
  let b = `<svg style="max-width:${Math.round(W2 * 1.2)}px" viewBox="0 0 ${W2} ${H2}" role="img" aria-label="Bonus against magnitude">`;
  for (let v = 0; v <= 0.24; v += 0.08) b += `<line x1="${m2.l}" x2="${W2 - m2.r}" y1="${Yb(v)}" y2="${Yb(v)}" stroke="#ECECEC"/><text x="${m2.l - 5}" y="${Yb(v) + 4}" text-anchor="end">${Math.round(v * 100)}</text>`;
  for (let M = 7; M <= 20; M += 3) b += `<text x="${Xm(M)}" y="${H2 - 6}" text-anchor="middle">${M}</text>`;
  P.forEach(r => b += `<circle cx="${Xm(r.seats) + ((r.year - 2005) % 5 - 2) * 1.8}" cy="${Yb(Math.max(0, r.largest_bonus))}" r="3" fill="#8B0000" fill-opacity=".35" data-tip="<b>${esc(r.name)} ${r.year}</b><br>${r.seats} seats: ${esc(r.largest_list)} ${pct(r.largest_vote_share, 0)} of votes, ${pct(r.largest_seat_share, 0)} of seats"/>`);
  b += `<polyline fill="none" stroke="#3E5C76" stroke-width="1.6" points="${[7, 8, 9, 10, 12, 14, 17, 20].map(M => Xm(M) + "," + Yb(1 / (M + 1))).join(" ")}"/>`;
  $("#bon").innerHTML = b + `<text x="${W2 - m2.r}" y="${m2.t + 8}" text-anchor="end" style="fill:#3E5C76">1 / (seats + 1)</text><text x="${m2.l + 8}" y="${m2.t + 6}">points</text></svg>`;
  // dot plot of the index
  const sorted = rows.filter(r => r[wm] != null).sort((a, c) => a[wm] - c[wm]), lim = Math.max(0.15, ...sorted.map(r => Math.abs(r[wm] - 1))) * 1.05, rowH = 15, W3 = 520, H3 = sorted.length * rowH + 26, ml = 100;
  const X3 = v => ml + (v - (1 - lim)) / (2 * lim) * (W3 - ml - 14);
  let d = `<svg style="max-width:${Math.round(W3 * 1.2)}px" viewBox="0 0 ${W3} ${H3}" role="img" aria-label="Vote value by constituency"><line x1="${X3(1)}" x2="${X3(1)}" y1="4" y2="${H3 - 20}" stroke="#141414" stroke-dasharray="3 3"/>`;
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

/* ---------- if seats followed population: the latest polls on four distributions of the 460 seats ---------- */
let SCN = null;
const SC_KEYS = ["annex", "extended", "halfway", "norm"], SC_LET = {annex: "a", extended: "b", halfway: "c", norm: "d"};
const SC_SHORT = {annex: "As it is", extended: "Continuation of the trend", halfway: "Partial correction", norm: "Full correction"};
function loadScn() { return SCN ? Promise.resolve(SCN) : fetchJSON("scenarios.json").then(d => { SCN = d; d.constituencies = tab(d.constituencies); d.measures = tab(d.measures); d.parties = tab(d.parties); d.gallagher = tab(d.gallagher); d.by_constituency = tab(d.by_constituency); d.additional_national = tab(d.additional_national); d.additional_constituency = tab(d.additional_constituency); return d; }); }
function hemiLayout(n, rows = 11) {
  const radii = Array.from({length: rows}, (_, i) => 1 + i / (rows - 1)), rs = radii.reduce((a, b) => a + b, 0), counts = radii.map(r => Math.floor(n * r / rs));
  let rem = n - counts.reduce((a, b) => a + b, 0); const order = radii.map((r, i) => i).sort((p, q) => radii[q] - radii[p]);
  for (let k = 0; rem > 0; k++, rem--) counts[order[k % rows]]++;
  const pts = []; counts.forEach((nc, ri) => { for (let j = 0; j < nc; j++) { const ang = nc === 1 ? Math.PI / 2 : Math.PI - j * Math.PI / (nc - 1); pts.push({x: radii[ri] * Math.cos(ang), y: radii[ri] * Math.sin(ang), ang, r: radii[ri]}); } });
  return pts.sort((p, q) => q.ang - p.ang || p.r - q.r);
}
function wholeSeats(k) {   // one simulated parliament, the draw whose national seats lie closest to the medians: whole seats that sum to 460
  const o = SCN.meta.hemicycle_order; return Object.entries(SCN.hemi[k]).map(([p, n]) => ({p, n})).sort((a, b) => o.indexOf(a.p) - o.indexOf(b.p));
}
const pcol = p => (SCN.palette && SCN.palette[p]) || "#7f7f7f";
function hemiSvg(k) {
  const blocks = wholeSeats(k), pts = hemiLayout(460); let i = 0, o = "";
  blocks.forEach(b => { for (let j = 0; j < b.n; j++, i++) { const pt = pts[i]; o += `<circle cx="${pt.x.toFixed(3)}" cy="${(-pt.y).toFixed(3)}" r="0.046" fill="${pcol(b.p)}" data-tip="${esc(b.p)}: ${b.n} seats"/>`; } });
  o += `<line x1="0" x2="0" y1="-0.86" y2="-2.12" stroke="#141414" stroke-width="0.012" stroke-dasharray="0.04 0.04"/><text x="0" y="-2.16" text-anchor="middle" style="font-size:0.085px">231</text>`;
  return `<svg viewBox="-2.2 -2.28 4.4 2.36" role="img" aria-label="Hemicycle: ${esc(SC_SHORT[k])}">${o}</svg>`;
}
const sgn = x => x > 0 ? "+" + x : x < 0 ? "−" + Math.abs(x) : "0";
function viewSeats() {
  $("#main").innerHTML = `<div class="loading">Loading…</div>`;
  loadScn().then(d => {
    const k = S.sc, m = d.meta, par = d.parties, byK = kk => par.filter(r => r.scenario === kk && r.median != null);
    const med = (kk, p) => (par.find(r => r.scenario === kk && r.party === p) || {median: 0}).median;
    const names = [...new Set(par.filter(r => r.median > 0).map(r => r.party))].sort((a, b) => med("annex", b) - med("annex", a) || a.localeCompare(b));
    const ms = Object.fromEntries(d.measures.map(r => [r.scenario, r])), gal = Object.fromEntries(d.gallagher.map(r => [r.scenario, r.gallagher]));
    const cell = (p, kk) => { const r = par.find(x => x.scenario === kk && x.party === p), a = par.find(x => x.scenario === "annex" && x.party === p); if (!r) return "<td></td>"; const dif = ""; return `<td class="n" data-tip="80% range ${Math.round(r.lo)}–${Math.round(r.hi)}${r.p_majority > 0.005 ? "; majority in " + pct(r.p_majority, 0) + " of simulations" : ""}">${Math.round(r.median)}${dif}</td>`; };
    const rowsT = names.map(p => `<tr><td><span class="sw sq" style="background:${pcol(p)};margin-right:6px"></span>${esc(p)}</td>${SC_KEYS.map(kk => cell(p, kk)).join("")}</tr>`).join("");
    const cons = d.constituencies.slice().sort((a, b) => (b[k] - b.annex) - (a[k] - a.annex) || a.okreg - b.okreg);
    const set = G.sets.sejm2001, by = Object.fromEntries(d.constituencies.map(r => [r.okreg, r])), pa = names.includes(S.pa) ? S.pa : "";
    const gc = pa ? d.additional_constituency.filter(x => x.scenario === k && x.party === pa) : [];
    const gp = (o, dir) => gc.filter(x => x.okreg === o && x.dir === dir).sort((u, v) => u.k - v.k).map(x => x.p);
    const net = o => (gp(o, "gain")[0] || 0) - (gp(o, "lose")[0] || 0);
    const dOf = r => pa ? net(r.okreg) : r[k] - r.annex;
    const lim = pa ? 1 : Math.max(1, ...d.constituencies.map(r => Math.abs(r[k] - r.annex)));
    const ordn = n => n + (["th", "st", "nd", "rd"][(n % 100 - 20) % 10] || ["th", "st", "nd", "rd"][n % 100] || "th");
    const plist = (ps, what) => ps.map((p, j) => `${ordn(j + 1)} ${what}: ${pct(p, 0)}`).join("; ");
    const fillOf = dv => dv === 0 ? "rgb(245,245,243)" : dv > 0 ? hex(lerp(MID, SLATE, Math.min(1, dv / lim))) : hex(lerp(MID, RED, Math.min(1, -dv / lim)));
    let map = `<svg viewBox="0 0 ${G.meta.view[0]} ${G.meta.view[1]}" role="img" aria-label="Seats gained or lost by constituency">`;
    set.okreg.forEach((o, i) => { const r = by[o]; if (!r) return; const dv = dOf(r); map += `<path class="region" d="${set.d[i]}" fill="${fillOf(dv)}" data-tip="<b>${esc(r.name)}</b><br>${pa ? `${esc(pa)}: ${gp(r.okreg, "gain").length ? "chance of additional seats, " + plist(gp(r.okreg, "gain"), "extra seat") : gp(r.okreg, "lose").length ? "chance of fewer seats, " + plist(gp(r.okreg, "lose"), "seat lost") : "same seats in every simulation"}<br>` : ""}${r.annex} seats in the constituency now; ${r[k]} here (${sgn(r[k] - r.annex)})<br>${num(r.residents)} residents"/>`; });
    map += "</svg>";
    const constTbl = `<div class="chartbox"><h3>Seats by constituency</h3><div class="tbl"><table><tr><th>Constituency</th><th class="n">Residents</th><th class="n">(a) As it is</th><th class="n">(b) Continuation</th><th class="n">(c) Partial</th><th class="n">(d) Full</th></tr>
       ${cons.map(r => `<tr><td>${esc(r.name)}</td><td class="n">${num(r.residents)}</td>${["annex", "extended", "halfway", "norm"].map(kk => `<td class="n"${kk === k ? ' style="font-weight:600"' : ""}>${r[kk]}${kk !== "annex" && r[kk] !== r.annex ? ` <span style="color:var(--muted)">${sgn(r[kk] - r.annex)}</span>` : ""}</td>`).join("")}</tr>`).join("")}</table></div></div>`;
    const nat = pa ? d.additional_national.filter(x => x.scenario === k && x.party === pa) : [], npv = dir => nat.filter(x => x.dir === dir).sort((u, v) => u.k - v.k).map(x => x.p);
    const natSame = (nat.find(x => x.dir === "same") || {}).p;
    const prs = pa ? d.constituencies.map(c => ({c, g: gp(c.okreg, "gain"), l: gp(c.okreg, "lose")})).filter(r => r.g.length || r.l.length).sort((u, v) => (v.g[0] || 0) - (u.g[0] || 0) || (v.l[0] || 0) - (u.l[0] || 0) || u.c.okreg - v.c.okreg) : [];
    const cellP = ps => ps.length ? ps.map((p, j) => `<span style="white-space:nowrap">${ordn(j + 1)} ${pct(p, 0)}</span>`).join(", ") : '<span style="color:var(--muted)">none</span>';
    const partyTbl = pa ? `<div class="chartbox"><h3>${esc(pa)}: chance of more or fewer seats than under the annex</h3>${k === "annex" ? `<p class="help">Under (a) nothing changes. Choose another distribution.</p>` : `
      <p class="lead" style="margin:0 0 8px"><b>Nationally.</b> ${npv("gain").length ? `More seats than under the annex: ${npv("gain").map((p, j) => `at least ${j + 1}, ${pct(p, 0)}`).join("; ")}.` : "It never wins more seats than under the annex."} ${npv("lose").length ? `Fewer seats: ${npv("lose").map((p, j) => `at least ${j + 1}, ${pct(p, 0)}`).join("; ")}.` : "It never wins fewer."} The same number: ${natSame == null ? "0%" : pct(natSame, 0)}.</p>
      ${prs.length ? `<div class="tbl"><table><tr><th>Constituency</th><th class="n">Seats, (a)</th><th class="n">Seats here</th><th>Chance of each additional seat</th><th>Chance of each seat lost</th></tr>
      ${prs.map(r => `<tr><td>${esc(r.c.name)}</td><td class="n">${r.c.annex}</td><td class="n">${r.c[k]}</td><td>${cellP(r.g)}</td><td>${cellP(r.l)}</td></tr>`).join("")}</table></div>` : `<p class="help">No constituency has a different number of seats for ${esc(pa)} in any simulation.</p>`}
      <p class="help" style="margin-top:6px">The chance of the 1st additional seat is the share of the ${m.draws} simulated parliaments in which the list wins at least one more seat in the constituency than it does under the annex with the same votes; the 2nd, at least two more, and so on. A constituency only gains seats if it is given more, and only loses them if it is given fewer. With ${m.draws} simulations, each chance is good to about three percentage points.</p>`}</div>` : "";
    $("#main").innerHTML = `<p class="lead">The Sejm's 460 seats are shared out between the constituencies by the annex to the Electoral Code, not by today's population, and the two have drifted apart. This view asks what the latest poll projection would give each party if the seats between constituencies were (a) left as they are, (b) moved further from the population norm in keeping with the trend since 2005, (c) moved halfway to the norm, or (d) set exactly by the norm (art. 202, residents at 31 December ${m.residents_year}). Only the number of seats in each constituency changes; each constituency's vote shares are the Polling tracker's projection for ${esc(m.week)}.</p>
     <div class="toolbar"><div class="seg" role="group" aria-label="Distribution of seats">${SC_KEYS.map(kk => `<button data-sc="${kk}" aria-pressed="${k === kk}">(${SC_LET[kk]}) ${SC_SHORT[kk]}</button>`).join("")}</div>
       <label class="ctl" for="pa">Show a party <select class="sel" id="pa"><option value="">All seats</option>${names.slice().sort((a, b) => a.localeCompare(b, "pl")).map(p => `<option value="${esc(p)}" ${p === pa ? "selected" : ""}>${esc(p)}</option>`).join("")}</select></label>
       <button class="btn2" id="scpng" type="button">Download PNG of all four</button></div>
     <div class="cols"><div class="chartbox"><h3>(${SC_LET[k]}) ${esc(SC_SHORT[k])}</h3>${hemiSvg(k)}<p class="help" style="margin-top:6px">${esc(m.scenarios[k])} Dots are seats in one typical simulated parliament (the one closest to the median seats); the dashed line marks the 231 needed for a majority.</p>
        <div class="legend">${wholeSeats(k).map(b => `<span><i class="sw sq" style="background:${pcol(b.p)}"></i>${esc(b.p)} ${b.n}</span>`).join("")}</div></div>
      <div class="chartbox"><h3>${pa ? esc(pa) + ": chance of gaining or losing seats, against (a)" : "Seats moved between constituencies"}</h3>${map}<div class="grad" style="background:linear-gradient(90deg,${hex(RED)},rgb(245,245,243),${hex(SLATE)})"></div>
        <div style="display:flex;justify-content:space-between;font-size:12px;color:var(--muted)">${pa ? "<span>Likely to lose a seat</span><span>no change</span><span>Likely to gain a seat</span>" : `<span>−${lim} seats</span><span>no change</span><span>+${lim}</span>`}</div></div></div>
     <div class="chartbox"><h3>Median seats by party, with 80% ranges on hover</h3><div class="tbl"><table><tr><th>List</th>${SC_KEYS.map(kk => `<th class="n">(${SC_LET[kk]}) ${SC_SHORT[kk]}</th>`).join("")}</tr>${rowsT}
       <tr><td>Seats moved between constituencies</td>${SC_KEYS.map(kk => `<td class="n">${ms[kk].seats_moved}</td>`).join("")}</tr>
       <tr><td>Constituencies with a different number</td>${SC_KEYS.map(kk => `<td class="n">${ms[kk].constituencies_changed}</td>`).join("")}</tr>
       <tr><td>Residents per seat, largest ÷ smallest</td>${SC_KEYS.map(kk => `<td class="n">${f2(ms[kk].max_min_ratio)}</td>`).join("")}</tr>
       <tr><td>Seats against residents (Loosemore–Hanby, points)</td>${SC_KEYS.map(kk => `<td class="n">${(100 * ms[kk].mal_residents).toFixed(1)}</td>`).join("")}</tr>
       <tr><td>Seats against votes (Gallagher, points)</td>${SC_KEYS.map(kk => `<td class="n">${f2(gal[kk])}</td>`).join("")}</tr></table></div>
       <p class="help" style="margin-top:6px">${m.caveats.map(esc).join(" ")} ${m.draws} simulated parliaments.</p></div>
     ${pa ? partyTbl : constTbl}`;
    $("#pa").onchange = e => { S.pa = e.target.value; render(); };
    document.querySelectorAll("[data-sc]").forEach(b => b.onclick = () => { S.sc = b.dataset.sc; render(); });
    $("#scpng").onclick = () => { scenarioPng().catch(e => { $("#scpng").textContent = "Could not make the PNG: " + e.message; }); };
  }).catch(e => { $("#main").innerHTML = `<p class="notice">Could not load the scenarios (${esc(e.message)}).</p>`; });
}
// one PNG with the four hemicycles, drawn on a canvas
async function scenarioPng() {
  const d = await loadScn(), m = d.meta, W = 1800, H = 1470, cv = document.createElement("canvas"); cv.width = W; cv.height = H;
  const g = cv.getContext("2d"), F = "Jost, 'Avenir Next', Helvetica, Arial, sans-serif"; try { await Promise.race([document.fonts.load("500 20px Jost"), new Promise(r => setTimeout(r, 1500))]); } catch (e) { }
  g.fillStyle = "#fff"; g.fillRect(0, 0, W, H); g.fillStyle = "#141414"; g.textBaseline = "alphabetic";
  g.font = `600 44px ${F}`; g.fillText("Sejm seats if constituencies' seats followed population", 60, 80);
  g.font = `400 26px ${F}`; g.fillStyle = "#4B4B4B"; g.fillText(`Polling tracker's projection for the week of ${m.week}; only the number of seats in each constituency changes.`, 60, 122);
  const pw = 840, ph = 560, ox = [60, 900], oy = [160, 770];
  SC_KEYS.forEach((k, idx) => {
    const x0 = ox[idx % 2], y0 = oy[Math.floor(idx / 2)], cx = x0 + pw / 2, R = 160, pts = hemiLayout(460), blocks = wholeSeats(k), baseY = y0 + 100 + 2.15 * R;
    g.fillStyle = "#141414"; g.font = `600 30px ${F}`; g.textAlign = "left"; g.fillText(`(${SC_LET[k]}) ${SC_SHORT[k]}`, x0, y0 + 36);
    g.font = `400 20px ${F}`; g.fillStyle = "#767676"; g.fillText(k === "annex" ? "Seats per constituency as in the Electoral Code's annex" : `${d.measures.find(r => r.scenario === k).seats_moved} seats moved between ${d.measures.find(r => r.scenario === k).constituencies_changed} constituencies`, x0, y0 + 66);
    let i = 0; blocks.forEach(b => { g.fillStyle = pcol(b.p); for (let j = 0; j < b.n; j++, i++) { const p = pts[i]; g.beginPath(); g.arc(cx + p.x * R, baseY - p.y * R, 0.046 * R, 0, 2 * Math.PI); g.fill(); } });
    g.strokeStyle = "#141414"; g.lineWidth = 1.5; g.setLineDash([5, 5]); g.beginPath(); g.moveTo(cx, baseY - 0.86 * R); g.lineTo(cx, baseY - 2.12 * R); g.stroke(); g.setLineDash([]);
    g.fillStyle = "#4B4B4B"; g.font = `400 18px ${F}`; g.textAlign = "center"; g.fillText("231", cx, baseY - 2.16 * R);
    // party counts under the arc, wrapped into rows
    g.font = `500 22px ${F}`; let lx = x0, ly = baseY + 52; const rowsOf = blocks.slice().sort((a, b) => b.n - a.n);
    rowsOf.forEach(b => { const t = `${b.p} ${b.n}`, w = g.measureText(t).width + 40; if (lx + w > x0 + pw) { lx = x0; ly += 32; } g.fillStyle = pcol(b.p); g.fillRect(lx, ly - 15, 15, 15); g.fillStyle = "#141414"; g.textAlign = "left"; g.fillText(t, lx + 22, ly); lx += w; });
  });
  g.textAlign = "left"; g.fillStyle = "#767676"; g.font = `400 19px ${F}`;
  const foot = [`Distributions: (a) annex to the Electoral Code; (b) the gap between the annex and the norm of representation (residents, art. 202) extended at its 2005–${m.residents_year} trend to ${m.horizon}; (c) halfway to the norm; (d) the norm on residents at 31 December ${m.residents_year}.`,
    `Seats are those of one typical simulated parliament (closest to the median seats) of ${m.draws}; each constituency's vote shares, the 5% threshold and the regional swing are the same in all four. Source: Ben Stanley, benstanley.eu (Pooling the Poles, GUS BDL, PKW).`];
  const wrap = t => { const out = []; let line = ""; t.split(" ").forEach(w => { if (g.measureText(line + " " + w).width > W - 120 && line) { out.push(line); line = w; } else line = line ? line + " " + w : w; }); out.push(line); return out; };
  const fl = foot.flatMap(wrap); fl.forEach((t, i) => g.fillText(t, 60, H - 30 - (fl.length - 1 - i) * 27));
  const blob = await new Promise(res => cv.toBlob(res, "image/png"));
  const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = "sejm-seats-by-constituency-scenario.png"; document.body.appendChild(a); a.click(); setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 500);
}

Promise.all([fetchJSON("constituencies.json"), fetchJSON("geometry.json")]).then(([c, g]) => { C = c; G = g; C.constituencies = tab(C.constituencies); C.elections = tab(C.elections); C.list_results = tab(C.list_results); C.list_costs = tab(C.list_costs); C.projection.rows = tab(C.projection.rows); C.covariates.values.rows = C.covariates.values.rows;
  hash(); render(); }).catch(e => { $("#main").innerHTML = `<p class="notice">Could not load the data (${esc(e.message)}).</p>`; });
window.addEventListener("hashchange", () => { if (C) { hash(); render(); } });
})();
