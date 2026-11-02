/* Legislative activity: clubs, MPs, positions, questions and legislation in the Sejm, terms 3-10.
   Data: legislative.json (clubs, series, topics) plus per-term files loaded on demand. No libraries. */
(function () {
"use strict";
const $ = s => document.querySelector(s);
const esc = s => String(s == null ? "" : s).replace(/[&<>"]/g, c => ({"&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;"}[c]));
const pct = (x, d = 1) => x == null ? "n/a" : (100 * x).toFixed(d) + "%";
const num = x => x == null ? "n/a" : Number(x).toLocaleString("en-GB");
const VIEWS = [["clubs", "Clubs"], ["mps", "MPs"], ["spread", "All MPs"], ["positions", "Positions"], ["questions", "Questions"], ["legislation", "Legislation"]];
const cache = {};
const fetchJSON = f => cache[f] || (cache[f] = fetch(f).then(r => { if (!r.ok) throw new Error(f + " " + r.status); return r.json(); }));
let L, S = {view: "clubs", term: 10, mp: null, q: "", club: "", page: 0, ext: false};
const TD = {};   // term -> mps json
const colOf = f => (L.family_colours[f]) || "#7f7f7f";
const tab = t => t.rows.map(r => Object.fromEntries(t.columns.map((c, i) => [c, r[i]])));

function readHash() {
  const p = new URLSearchParams(location.hash.slice(1));
  if (p.get("view") && VIEWS.some(v => v[0] === p.get("view"))) S.view = p.get("view");
  if (p.get("t") && L.terms.some(t => t.term === +p.get("t"))) S.term = +p.get("t");
  if (p.get("mp")) S.mp = +p.get("mp");
}
function writeHash() {
  const p = new URLSearchParams({view: S.view, t: S.term}); if (S.mp && (S.view === "mps")) p.set("mp", S.mp);
  history.replaceState(null, "", "#" + p.toString());
}
const tip = $("#tip");
document.addEventListener("mousemove", e => {
  const t = e.target.closest && e.target.closest("[data-tip]");
  if (!t) { tip.style.display = "none"; return; }
  tip.innerHTML = t.dataset.tip; tip.style.display = "block";
  tip.style.left = Math.min(innerWidth - tip.offsetWidth - 8, e.clientX + 12) + "px"; tip.style.top = (e.clientY + 14) + "px";
});

function toolbar() {
  const T = L.terms;
  $("#tb").innerHTML = `<div class="seg" role="group" aria-label="View">${VIEWS.map(v => `<button data-v="${v[0]}" aria-pressed="${S.view === v[0]}">${v[1]}</button>`).join("")}</div>
   <label class="ctl">Sejm term <span class="seg small" role="group" aria-label="Term">${T.map(t => `<button data-t="${t.term}" aria-pressed="${S.term === t.term}" ${S.view === "legislation" ? "disabled" : ""} title="${t.first_vote.slice(0, 4)}–${t.term === 10 ? "" : t.term_end.slice(0, 4)}">${t.term}</button>`).join("")}</span></label>`;
  $("#tb").onclick = e => { const b = e.target.closest("button"); if (!b || b.disabled) return;
    if (b.dataset.v) { S.view = b.dataset.v; } else { S.term = +b.dataset.t; S.mp = null; S.club = ""; S.q = ""; S.page = 0; }
    render(); };
}
async function render() {
  writeHash(); toolbar();
  const ti = L.terms.find(t => t.term === S.term);
  $("#main").innerHTML = `<div class="loading">Loading…</div>`;
  try { if (S.view !== "legislation" && S.view !== "questions") TD[S.term] = TD[S.term] || await fetchJSON(`mps-${S.term}.json`);
    await ({clubs: viewClubs, mps: viewMps, spread: viewSpread, positions: viewPositions, questions: viewQuestions, legislation: viewLegislation})[S.view]();
  } catch (e) { $("#main").innerHTML = `<p class="notice">Could not load the data (${esc(e.message)}).</p>`; }
  const cav = L.meta.caveats;
  $("#foot").innerHTML = `<b>Term ${S.term}</b>: ${num(ti.n_mps)} MPs, ${num(ti.n_roll_calls)} roll calls, ${esc(ti.first_vote)} to ${esc(ti.last_vote)}. Sources and their cut-off dates: ${L.meta.sources.filter(s => s.term === S.term).map(s => esc(s.source) + " " + esc(s.last)).join("; ")}.<ul>${[2, 3, 4, 5, 6, 10].map(i => `<li>${esc(cav[i])}</li>`).join("")}</ul>`;
}
const hyx = h => +h.slice(0, 4) + (h.endsWith("H1") ? 0.25 : 0.75);
function note(html) { return `<p class="help">${html}</p>`; }

/* ---------- Clubs ---------- */
async function viewClubs() {
  const cl = L.clubs.filter(c => c.term === S.term && c.max_size >= 8);
  const rows = tab(L.cohesion).filter(r => r.term === S.term && r.n_votes >= 20);
  const clubs = cl.filter(c => rows.some(r => r.club === c.club));
  $("#main").innerHTML = `<p class="lead">How united is each club, and how often do government and opposition take opposite sides? Cohesion is the agreement index on contested votes: 1 means every MP voted with the club's majority, 0 an even three-way split.</p>
   <div class="cols"><div class="chartbox"><h3>Cohesion by half-year</h3><div id="cc"></div><div class="legend" id="cl"></div></div>
   <div class="chartbox"><h3>Government against opposition</h3><div id="go"></div>${note("Share of contested votes on which the two sides' majorities voted YES against NO.")}</div></div>
   <div class="chartbox"><h3>Clubs this term</h3><div class="tbl"><table><thead><tr><th>Club</th><th>Family</th><th class="n">MPs (largest)</th><th>Composite</th></tr></thead><tbody>${cl.map(c => `<tr><td><span class="sw" style="border-color:${colOf(c.family)};margin-right:8px"></span>${esc(c.club)}<br><span class="help">${esc(c.name || "")}</span></td><td>${esc(c.family)}</td><td class="n">${Math.round(c.max_size)}</td><td>${c.composite ? "yes: " + esc(c.parties) : ""}</td></tr>`).join("")}</tbody></table></div>
   ${note("Composite clubs hold more than one party (Party tracker), so rebellion inside them often marks a party faction rather than individual dissent. Dotted lines mark them.")}</div>
   <div class="chartbox"><h3>Agreement between party families, term ${S.term}</h3><div id="ag"></div>${note("Hix–Noury–Roland agreement index of two families' pooled votes on contested votes (1 = identical majority position, lower = further apart). Families of at least three MPs.")}</div>`;
  if (!rows.length) { $("#cc").innerHTML = `<p class="help">No club has enough contested votes.</p>`; return; }
  const hs = [...new Set(rows.map(r => r.half_year))].sort(), x0 = hyx(hs[0]), x1 = hyx(hs[hs.length - 1]);
  const W = 640, H = 320, m = {l: 40, r: 64, t: 8, b: 24}, vmin = Math.floor(Math.min(...rows.map(r => r.cohesion)) * 20) / 20;
  const X = h => m.l + (hyx(h) - x0) / Math.max(.5, x1 - x0) * (W - m.l - m.r), Y = v => m.t + (1 - v) / (1 - vmin) * (H - m.t - m.b);
  let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Club cohesion by half-year">`;
  for (let v = Math.ceil(vmin * 10) / 10; v <= 1.0001; v += .1) s += `<line x1="${m.l}" x2="${W - m.r}" y1="${Y(v)}" y2="${Y(v)}" stroke="#ECECEC"/><text x="${m.l - 6}" y="${Y(v) + 4}" text-anchor="end">${v.toFixed(1)}</text>`;
  [...new Set(hs.map(h => h.slice(0, 4)))].forEach(y => s += `<text x="${X(y + "H1")}" y="${H - 6}" text-anchor="middle">${y}</text>`);
  const ends = [];
  clubs.forEach(c => { const pts = rows.filter(r => r.club === c.club).sort((a, b) => a.half_year < b.half_year ? -1 : 1); if (pts.length < 2) return;
    s += `<polyline fill="none" stroke="${colOf(c.family)}" stroke-width="2" ${c.composite ? 'stroke-dasharray="2 3"' : ""} points="${pts.map(p => X(p.half_year) + "," + Y(p.cohesion)).join(" ")}"/>`;
    pts.forEach(p => s += `<circle cx="${X(p.half_year)}" cy="${Y(p.cohesion)}" r="7" fill="transparent" data-tip="<b>${esc(c.club)}</b><br>${p.half_year}: ${p.cohesion.toFixed(3)} (${p.n_votes} contested votes)"/>`);
    const l = pts[pts.length - 1]; ends.push({y: Y(l.cohesion), club: c.club, col: colOf(c.family), x: X(l.half_year)}); });
  ends.sort((a, b) => a.y - b.y); for (let i = 1; i < ends.length; i++) if (ends[i].y - ends[i - 1].y < 12) ends[i].y = ends[i - 1].y + 12;
  ends.forEach(e => s += `<circle cx="${e.x}" cy="${e.y}" r="3" fill="${e.col}"/><text class="t" x="${e.x + 7}" y="${e.y + 4}">${esc(e.club)}</text>`);
  $("#cc").innerHTML = s + "</svg>";
  $("#cl").innerHTML = clubs.map(c => `<span><i class="sw ${c.composite ? "dots" : ""}" style="border-color:${colOf(c.family)}"></i>${esc(c.club)}</span>`).join("");
  const g = tab(L.government_opposition).filter(r => r.term === S.term), W2 = 420, H2 = 320, bw = Math.max(8, (W2 - 50) / Math.max(1, g.length) - 8);
  let s2 = `<svg viewBox="0 0 ${W2} ${H2}" role="img" aria-label="Opposed-line rate by half-year">`;
  for (let v = 0; v <= 1.0001; v += .25) { const y = 8 + (1 - v) * (H2 - 34); s2 += `<line x1="36" x2="${W2}" y1="${y}" y2="${y}" stroke="#ECECEC"/><text x="30" y="${y + 4}" text-anchor="end">${Math.round(v * 100)}%</text>`; }
  g.forEach((r, i) => { const x = 44 + i * ((W2 - 50) / g.length), h = (r.opposed_rate || 0) * (H2 - 34);
    s2 += `<rect x="${x}" y="${8 + (H2 - 34) - h}" width="${bw}" height="${h}" rx="3" fill="#8B0000" data-tip="${r.half_year}: ${pct(r.opposed_rate)} of ${r.n_both_lines} contested votes"/><text x="${x + bw / 2}" y="${H2 - 6}" text-anchor="middle">${r.half_year.slice(2)}</text>`; });
  $("#go").innerHTML = s2 + "</svg>";
  const ag = tab(L.agreement_between_families).filter(r => r.term === S.term), fams = [...new Set(ag.map(r => r.family_a))];
  const get = (a, b) => (ag.find(r => r.family_a === a && r.family_b === b) || {}).hnr_ai;
  $("#ag").innerHTML = `<div class="tbl"><table class="heat"><thead><tr><th></th>${fams.map(f => `<th style="text-align:center">${esc(f)}</th>`).join("")}</tr></thead><tbody>${fams.map(a => `<tr><td><b>${esc(a)}</b></td>${fams.map(b => { const v = get(a, b); return v == null ? "<td></td>" : `<td style="background:rgba(139,0,0,${Math.max(0, (v - .3) / .7 * .75).toFixed(2)});color:${v > .8 ? "#fff" : "#141414"}">${v.toFixed(2)}</td>`; }).join("")}</tr>`).join("")}</tbody></table></div>`;
}

/* ---------- MPs ---------- */
async function viewMps() {
  const mps = TD[S.term].mps;
  if (!S.mp || !mps.some(m => m.mp_id === S.mp)) S.mp = (mps.find(m => m.n_cast >= 500 && !m.office_holder) || mps[0]).mp_id;
  const clubs = [...new Set(mps.map(m => m.modal_club).filter(Boolean))].sort();
  $("#main").innerHTML = `<p class="lead">Every MP of the term can be found by name. There are no ranked lists: the charts and profiles show where an MP stands among colleagues, not who is "best" or "worst".</p>
   <div class="ledger"><div class="find"><label class="ctl" for="q">Find an MP</label><input id="q" type="search" placeholder="Name" value="${esc(S.q)}" autocomplete="off">
   <label class="ctl" for="cf">Club</label><select class="sel" id="cf"><option value="">All clubs</option>${clubs.map(c => `<option ${c === S.club ? "selected" : ""}>${esc(c)}</option>`).join("")}</select>
   <div class="list" id="list" role="listbox" aria-label="MPs"></div></div><div class="chartbox" id="prof"></div></div>`;
  $("#q").oninput = e => { S.q = e.target.value; drawList(); };
  $("#cf").onchange = e => { S.club = e.target.value; drawList(); };
  drawList(); await drawProfile();
}
function drawList() {
  const q = S.q.trim().toLowerCase();
  const Ls = TD[S.term].mps.filter(m => (!q || m.name.toLowerCase().includes(q)) && (!S.club || m.modal_club === S.club)).sort((a, b) => (a.last_name || "").localeCompare(b.last_name || "", "pl"));
  $("#list").innerHTML = Ls.map(m => `<div class="row" role="option" tabindex="0" data-id="${m.mp_id}" aria-selected="${m.mp_id === S.mp}"><i class="bar" style="background:${colOf(m.modal_family)}"></i><div>${esc(m.name)}<small>${esc(m.modal_club || "no club")}${m.marshal ? " · Marshal" : m.office_holder ? " · office holder" : ""}</small></div><div class="r">${m.attendance_rollcalls == null ? "" : pct(m.attendance_rollcalls, 0)}</div></div>`).join("") || `<div class="row"><div>No MP matches.</div></div>`;
  const pick = el => { const r = el.closest(".row"); if (!r || !r.dataset.id) return; S.mp = +r.dataset.id; S.page = 0; writeHash(); drawList(); drawProfile(); };
  $("#list").onclick = e => pick(e.target); $("#list").onkeydown = e => { if (e.key === "Enter" || e.key === " ") { e.preventDefault(); pick(e.target); } };
}
async function rebelsOf(m) {
  const td = TD[S.term]; if (!m.rebels_file) return [];
  const rf = await fetchJSON(`rebels-${S.term}-${m.rebels_file}.json`), lst = rf.mps[m.mp_id] || [], firsts = [].concat(td.vote_chunk_first_index);
  const chunkOf = i => { let k = firsts.length - 1; while (firsts[k] > i) k--; return k; };
  const need = [...new Set(lst.map(x => chunkOf(x[0])))];
  const vf = {}; await Promise.all(need.map(async k => vf[k] = await fetchJSON(`votes-${S.term}-${k + 1}.json`)));
  const NAME = ["", "YES", "NO", "ABSTAIN"];
  return lst.map(([i, v, l]) => { const k = chunkOf(i), r = vf[k].votes.rows[i - firsts[k]]; return {date: r[1], label: r[2], vote: NAME[v], line: NAME[l], c: r[4]}; }).sort((a, b) => a.date < b.date ? 1 : -1);
}
async function drawProfile() {
  const mps = TD[S.term].mps, m = mps.find(x => x.mp_id === S.mp); if (!m) return;
  document.querySelectorAll("#list .row").forEach(r => r.setAttribute("aria-selected", +r.dataset.id === S.mp));
  const box = $("#prof"); box.innerHTML = `<div class="loading">Loading…</div>`;
  const [rolesF, rb] = await Promise.all([fetchJSON(`roles-${S.term}.json`), rebelsOf(m)]);
  if (S.mp !== m.mp_id) return;
  const det = rolesF.mps[m.mp_id] || {}, hist = det.club_history || [], roles = det.roles || [], rec = det.top_recipients || [];
  const peers = mps.filter(x => x.modal_club === m.modal_club && !x.office_holder && x.n_cast >= 500 && x.rebellion_rate != null).map(x => x.rebellion_rate).sort((a, b) => a - b);
  const med = peers.length ? peers[Math.floor(peers.length / 2)] : null, per = 12, pages = Math.max(1, Math.ceil(rb.length / per)); S.page = Math.min(S.page, pages - 1);
  const ti = L.terms.find(t => t.term === S.term);
  box.innerHTML = `<h2 style="font-size:24px">${esc(m.name)}</h2>
   <div class="chips">${hist.map(h => `<span class="chip"><i class="sw sq" style="background:${colOf(h.family)}"></i>${esc(h.club)} <span style="color:var(--muted)">${esc(h.from)} to ${esc(h.to)}</span></span>`).join("")}
    ${m.modal_club_composite ? `<span class="chip flag">composite club</span>` : ""}${m.marshal ? `<span class="chip flag">Marshal of the Sejm: left out of medians</span>` : m.office_holder ? `<span class="chip flag" data-tip="${esc(m.offices || "")}">minister or secretary of state: left out of medians</span>` : ""}
    <span class="chip">${esc(m.district_name || "")}${m.constituency_41 ? " · constituency " + m.constituency_41 : ""}</span>${m.mandate_end ? `<span class="chip">mandate ended ${esc(m.mandate_end)}</span>` : ""}</div>
   <div class="stats">
    <div class="stat"><b>${pct(m.attendance_rollcalls)}</b><span>of ${num(m.n_held)} roll calls voted in</span></div>
    <div class="stat"><b>${pct(m.rebellion_rate, 2)}</b><span>votes against the club${med != null ? `; club median ${pct(med, 2)}` : ""}</span></div>
    <div class="stat"><b>${m.days_missed_some_unexcused == null ? "n/a" : m.days_missed_some_unexcused}</b><span>sitting days with missed votes, unexcused (${m.days_missed_some_excused || 0} excused of ${m.sitting_days || 0} days)</span></div>
    ${ti.questions ? `<div class="stat"><b>${(m.n_interpellations_first || 0) + (m.n_written_questions_first || 0)}</b><span>questions first-signed, ${(m.n_interpellations_co || 0) + (m.n_written_questions_co || 0)} co-signed${m.median_days_to_reply != null ? `; median reply ${m.median_days_to_reply} days` : ""}</span></div>` : ""}
    ${ti.speech ? `<div class="stat"><b>${num(m.n_words_spoken)}</b><span>words in ${num(m.n_statements)} statements${m.n_unspoken_statements ? `, ${m.n_unspoken_statements} written` : ""}</span></div>` : ""}
    ${m.n_rapporteur_reports ? `<div class="stat"><b>${m.n_rapporteur_reports}</b><span>committee reports as rapporteur</span></div>` : ""}</div>
   <div class="two"><div><h3>Position against the club</h3>${m.ideal_point != null ? ideal(m) : `<p class="help">Ideal points exist for terms 5 to 10.</p>`}</div>
    <div><h3>Roles</h3>${roles.length ? `<div class="tbl"><table><tbody>${roles.slice(0, 8).map(r => `<tr><td>${esc(r.body)}<br><span class="help">${esc(r.kind)}</span></td><td>${esc(r.role)}</td><td class="n">${esc((r.from || "").slice(0, 7))}</td></tr>`).join("")}</tbody></table></div>${roles.length > 8 ? `<p class="help">and ${roles.length - 8} more</p>` : ""}` : `<p class="help">No committee, club or group roles recorded.</p>`}
    ${rec.length ? `<h3 style="margin-top:12px">Most questions addressed to</h3><p class="help">${rec.map(r => esc(r.recipient) + " (" + r.n + ")").join("; ")}</p>` : ""}</div></div>
   <h3>Votes against the club line (${num(rb.length)})</h3>
   <div class="tbl"><table><thead><tr><th>Date</th><th>Roll call</th><th>MP</th><th>Club</th></tr></thead><tbody>${rb.slice(S.page * per, (S.page + 1) * per).map(r => `<tr><td style="white-space:nowrap">${esc(r.date)}</td><td>${esc(r.label)}${r.c ? "" : ' <span class="help">(uncontested)</span>'}</td><td><span class="vote ${r.vote}">${r.vote}</span></td><td><span class="vote ${r.line}">${r.line}</span></td></tr>`).join("") || `<tr><td colspan="4">No votes against the club line.</td></tr>`}</tbody></table></div>
   <div class="pg"><button class="btn2" id="pp" ${S.page === 0 ? "disabled" : ""}>Newer</button><span>${S.page + 1} of ${pages}</span><button class="btn2" id="pn" ${S.page >= pages - 1 ? "disabled" : ""}>Older</button></div>
   <p class="help" style="margin-top:8px">Attendance is the share of roll calls the MP voted in. Days with missed votes come from the API's sitting-day statistics, which also count personnel-list ballots, so the two sources count slightly differently.</p>`;
  $("#pp").onclick = () => { S.page--; drawProfile(); }; $("#pn").onclick = () => { S.page++; drawProfile(); };
}
function ideal(m) {
  const xs = TD[S.term].mps.map(x => x.ideal_point).filter(v => v != null), lo = Math.min(...xs), hi = Math.max(...xs), W = 360, p = v => 10 + (v - lo) / (hi - lo) * (W - 20);
  const dots = TD[S.term].mps.filter(x => x.ideal_point != null).map((x, i) => `<circle cx="${p(x.ideal_point)}" cy="${34 + ((i * 37) % 24) - 12}" r="2" fill="${colOf(x.modal_family)}" opacity=".5"/>`).join("");
  return `<svg viewBox="0 0 ${W} 100" role="img" aria-label="Ideal point against club mean"><line x1="10" x2="${W - 10}" y1="34" y2="34" stroke="#ECECEC"/>${dots}
   <line x1="${p(m.ideal_point_club)}" x2="${p(m.ideal_point_club)}" y1="8" y2="62" stroke="#141414" stroke-dasharray="3 3"/><text x="${p(m.ideal_point_club)}" y="76" text-anchor="middle">club ${m.ideal_point_club.toFixed(2)}</text>
   <circle cx="${p(m.ideal_point)}" cy="34" r="6" fill="${colOf(m.modal_family)}" stroke="#fff" stroke-width="2"/><text class="t" x="${p(m.ideal_point)}" y="92" text-anchor="middle">this MP ${m.ideal_point.toFixed(2)}</text>
   <text x="10" y="20">PO/KO side</text><text x="${W - 10}" y="20" text-anchor="end">PiS side</text></svg>`;
}

/* ---------- scatter helpers ---------- */
function openMp(id) { S.mp = id; S.view = "mps"; S.page = 0; S.q = ""; S.club = ""; render(); }
function legendFor(ms) { return [...new Set(ms.map(x => x.modal_family))].map(f => `<span><i class="sw sq" style="background:${colOf(f)}"></i>${esc(f)}</span>`).join(""); }
async function viewSpread() {
  const ms = TD[S.term].mps.filter(m => m.n_cast >= 500 && m.rebellion_rate != null && m.attendance_rollcalls != null);
  $("#main").innerHTML = `<p class="lead">Every MP with at least 500 votes cast: how often they voted (right) and how often they voted against their club's line (up). Hollow dots are ministers, secretaries of state and Marshals, who are left out of medians. Select a dot to open the MP's profile.</p>
   <div class="chartbox"><div id="sp"></div><div class="legend" id="spl"></div></div>${S.term === 3 || S.term === 4 ? note("In terms 3 and 4 many MPs sat in composite clubs, and term 3's vote types are unreliable, so high rebellion rates there often mark a party faction.") : ""}`;
  const W = 900, H = 420, m = {l: 52, r: 16, t: 10, b: 40}, xmin = Math.min(.5, Math.floor(Math.min(...ms.map(x => x.attendance_rollcalls)) * 10) / 10), ymax = Math.min(.2, Math.max(...ms.map(x => x.rebellion_rate)) * 1.05);
  const X = v => m.l + (v - xmin) / (1 - xmin) * (W - m.l - m.r), Y = v => m.t + (1 - Math.min(v, ymax) / ymax) * (H - m.t - m.b);
  let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Rebellion against attendance">`;
  const step = ymax > .1 ? .05 : .025;
  for (let v = 0; v <= ymax + 1e-9; v += step) s += `<line x1="${m.l}" x2="${W - m.r}" y1="${Y(v)}" y2="${Y(v)}" stroke="#ECECEC"/><text x="${m.l - 6}" y="${Y(v) + 4}" text-anchor="end">${(v * 100).toFixed(1)}%</text>`;
  for (let v = Math.ceil(xmin * 10) / 10; v <= 1.0001; v += .1) s += `<text x="${X(v)}" y="${H - 18}" text-anchor="middle">${Math.round(v * 100)}%</text>`;
  s += `<text x="${(W + m.l) / 2}" y="${H - 2}" text-anchor="middle">roll calls voted in</text><text x="4" y="${m.t + 8}">votes against club</text>`;
  ms.forEach(x => { const c = colOf(x.modal_family); s += `<circle class="dot" data-id="${x.mp_id}" cx="${X(x.attendance_rollcalls)}" cy="${Y(x.rebellion_rate)}" r="4" fill="${x.office_holder ? "none" : c}" fill-opacity=".7" stroke="${c}" stroke-width="1.3" style="cursor:pointer" data-tip="<b>${esc(x.name)}</b><br>${esc(x.modal_club || "")}<br>voted in ${pct(x.attendance_rollcalls)}, against club ${pct(x.rebellion_rate, 2)}"/>`; });
  $("#sp").innerHTML = s + "</svg>"; $("#spl").innerHTML = legendFor(ms);
  $("#sp").onclick = e => { const d = e.target.closest(".dot"); if (d) openMp(+d.dataset.id); };
}

/* ---------- Positions ---------- */
async function viewPositions() {
  const ti = L.terms.find(t => t.term === S.term);
  if (!ti.ideal_points) { $("#main").innerHTML = `<p class="notice">Positions are estimated from roll calls for terms 5 to 10. Choose one of those terms.</p>`; return; }
  const WN = L.wnominate, pkv = (tab(WN.indices_halfyear).filter(r => r.term === S.term && r.index === "d_PiS_KO").map(r => r.value)), fit = tab(WN.fit).find(r => r.term === S.term), ms = TD[S.term].mps.filter(m => m.wn_dim1 != null);
  $("#main").innerHTML = `<p class="lead">Each dot is one MP, placed by how they voted over the whole term (W-NOMINATE). The horizontal axis is the main line of division in the Sejm; the vertical axis picks up what is left. Select a dot to open the profile.</p>
   <div class="cols"><div class="chartbox"><div id="wn"></div><div class="legend" id="wnl"></div></div>
   <div class="chartbox"><h3>Fit, term ${S.term}</h3><div class="stats" style="grid-template-columns:1fr 1fr"><div class="stat"><b>${fit.correctclass1D.toFixed(1)}%</b><span>votes correctly classified, one dimension</span></div><div class="stat"><b>${fit.correctclass2D.toFixed(1)}%</b><span>two dimensions</span></div></div>
   ${note(`${num(fit.n_votes_scaled)} roll calls scaled for ${num(fit.n_legislators)} MPs. Dimension 1 has ${esc(fit.polarity_dim1)} on the positive side; dimension 2 is anchored on ${esc(fit.polarity_dim2)}. ${esc(WN.note)}`)}
${pkv.length ? `<h3 style="margin-top:14px">Distance between PiS and KO</h3><p class="lead"><b style="font-size:22px">${(pkv.reduce((a, b) => a + b, 0) / pkv.length).toFixed(2)}</b> on dimension 1, the gap between the two parties' average positions over the term (the scale runs from about -1 to 1).</p>` : ""}</div></div>`;
  const W = 640, H = 460, m = {l: 40, r: 14, t: 10, b: 34}, X = v => m.l + (v + 1) / 2 * (W - m.l - m.r), Y = v => m.t + (1 - (v + 1) / 2) * (H - m.t - m.b);
  let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="W-NOMINATE map">`;
  [-1, -.5, 0, .5, 1].forEach(v => s += `<line x1="${X(v)}" x2="${X(v)}" y1="${m.t}" y2="${H - m.b}" stroke="#ECECEC"/><line x1="${m.l}" x2="${W - m.r}" y1="${Y(v)}" y2="${Y(v)}" stroke="#ECECEC"/><text x="${X(v)}" y="${H - 18}" text-anchor="middle">${v}</text><text x="${m.l - 6}" y="${Y(v) + 4}" text-anchor="end">${v}</text>`);
  s += `<text x="${(W + m.l) / 2}" y="${H - 2}" text-anchor="middle">dimension 1 (positive: PiS side)</text><text x="${W - m.r}" y="${m.t + 8}" text-anchor="end">dimension 2</text>`;
  ms.forEach(x => { const c = colOf(x.modal_family); s += `<circle class="dot" data-id="${x.mp_id}" cx="${X(x.wn_dim1)}" cy="${Y(x.wn_dim2)}" r="4" fill="${x.office_holder ? "none" : c}" fill-opacity=".7" stroke="${c}" stroke-width="1.3" style="cursor:pointer" data-tip="<b>${esc(x.name)}</b><br>${esc(x.modal_club || "")}<br>dim 1 ${x.wn_dim1.toFixed(2)}, dim 2 ${x.wn_dim2.toFixed(2)}"/>`; });
  $("#wn").innerHTML = s + "</svg>"; $("#wnl").innerHTML = legendFor(ms);
  $("#wn").onclick = e => { const d = e.target.closest(".dot"); if (d) openMp(+d.dataset.id); };
}

/* ---------- Questions ---------- */
async function viewQuestions() {
  const all = tab(L.ministry_delays), terms = [...new Set(all.map(r => r.term))].sort();
  if (!terms.includes(S.term)) { $("#main").innerHTML = `<p class="notice">Reply times can be measured for terms ${terms.join(", ")} only: term 7's replies carry no dates, and questions began in term 7.</p>`; return; }
  const key = S.ext ? "share_late_extended" : "share_late";
  const rows = all.filter(r => r.term === S.term && r.n_answered >= 100).sort((a, b) => b[key] - a[key]);
  $("#main").innerHTML = `<p class="lead">How late do ministries answer MPs' interpellations and written questions? A reply is late if the first answer (not a request for more time) arrives more than 21 days after the question was sent. Questions with one recipient only; unanswered questions are left out of the shares.</p>
   <label class="switch"><input type="checkbox" id="ext" ${S.ext ? "checked" : ""}> Allow extra time when more time was requested (assumes 14 days; the API does not give the length)</label>
   <div class="chartbox"><div id="mn"></div></div>${note("Ministries with at least 100 answered questions this term; dot area shows how many. Recipients are named as the API gives them, so a renamed ministry appears under each name.")}`;
  $("#ext").onchange = e => { S.ext = e.target.checked; viewQuestions(); };
  const W = 720, rowH = 22, H = rows.length * rowH + 30, m = {l: 250, r: 20, t: 6}, X = v => m.l + v * (W - m.l - m.r);
  let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Share of replies later than 21 days by ministry">`;
  [0, .25, .5, .75, 1].forEach(v => s += `<line x1="${X(v)}" x2="${X(v)}" y1="${m.t}" y2="${H - 22}" stroke="#ECECEC"/><text x="${X(v)}" y="${H - 6}" text-anchor="middle">${Math.round(v * 100)}%</text>`);
  rows.forEach((r, i) => { const y = m.t + i * rowH + rowH / 2; s += `<text class="t" x="${m.l - 8}" y="${y + 4}" text-anchor="end">${esc(r.recipient.length > 38 ? r.recipient.slice(0, 37) + "…" : r.recipient)}</text><circle cx="${X(r[key])}" cy="${y}" r="${Math.max(3, Math.min(8, Math.sqrt(r.n_answered) / 4))}" fill="#8B0000" fill-opacity=".8" data-tip="<b>${esc(r.recipient)}</b><br>${pct(r[key])} late; median ${r.median_days_to_reply} days<br>${num(r.n_answered)} answered, ${num(r.n_unanswered)} unanswered"/>`; });
  $("#mn").innerHTML = s + "</svg>";
}

/* ---------- Legislation ---------- */
async function viewLegislation() {
  const T = L.topics, per = T.periods;
  const bp = tab(T.by_period).filter(r => !r.incl_adjudicated), topics = [...new Set(bp.map(r => r.topic))];
  const val = (t, p) => (bp.find(r => r.topic === t && r.period_passed === p) || {}).share || 0, n = p => (bp.find(r => r.period_passed === p) || {}).period_total;
  const pr = tab(T.passage_rate_terms_5_9).filter(r => !r.incl_adjudicated && r.initiator !== "All" && r.bills >= 15);
  const sp = tab(T.speed_by_topic).filter(r => !r.incl_adjudicated && r.passed_bills >= 20).sort((a, b) => a.median_days - b.median_days);
  const gm = ["Government", "MPs"], prt = [...new Set(pr.map(r => r.topic))];
  $("#main").innerHTML = `<p class="lead">What kind of laws does the Sejm pass? Every bill since 2005 has a topic from the Comparative Agendas Project's list, coded from its title by two language models. There is no hand-coded accuracy figure; the check made is that a bill's topic fits the committee it was referred to (85.3% of bills the two models agreed on, against 12.2% by chance).</p>
   <div class="chartbox"><h3>Topics of bills passed, by cabinet period (per cent of the period's bills)</h3><div class="tbl"><table class="heat"><thead><tr><th>Topic</th>${per.map(p => `<th style="text-align:center">${esc(p)}<br><span style="font-weight:400">n = ${num(n(p))}</span></th>`).join("")}</tr></thead><tbody>${topics.map(t => `<tr><td>${esc(t)}</td>${per.map(p => { const v = val(t, p); return `<td style="background:rgba(139,0,0,${Math.min(.8, v / 14).toFixed(2)});color:${v > 9 ? "#fff" : "#141414"}">${v ? v.toFixed(1) : ""}</td>`; }).join("")}</tr>`).join("")}</tbody></table></div>${note("Bills on whose topic the two coders agreed (93%); the 7% they disputed are uncertain and left out here. The 2023– period is still running.")}</div>
   <div class="cols"><div class="chartbox"><h3>Passage rate by topic and initiator, terms 5 to 9</h3><div id="pr"></div>${note("Bills passed as a share of bills received; topics with at least 15 bills from the initiator. Bills withdrawn or undecided at the end of a term count as not passed. The API names no individual MP sponsors, so bills show government against MPs' bills only.")}</div>
   <div class="chartbox"><h3>Median days from receipt to passage</h3><div id="sd"></div>${note("Passed bills, topics with at least 20. Topics are model-coded from titles; the only accuracy check made is agreement with the committee a bill was referred to.")}</div></div>`;
  const W = 520, rowH = 20, H = prt.length * rowH + 30, m = {l: 150, r: 14, t: 4}, X = v => m.l + v / 100 * (W - m.l - m.r);
  let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Passage rate by topic">`;
  [0, 25, 50, 75, 100].forEach(v => s += `<line x1="${X(v)}" x2="${X(v)}" y1="${m.t}" y2="${H - 22}" stroke="#ECECEC"/><text x="${X(v)}" y="${H - 6}" text-anchor="middle">${v}%</text>`);
  prt.forEach((t, i) => { const y = m.t + i * rowH + rowH / 2; s += `<text class="t" x="${m.l - 6}" y="${y + 4}" text-anchor="end">${esc(t)}</text>`;
    pr.filter(r => r.topic === t).forEach(r => { s += r.initiator === "Government" ? `<circle cx="${X(r.rate)}" cy="${y}" r="4.5" fill="#141414" data-tip="Government, ${esc(t)}: ${r.rate}% of ${r.bills}"/>` : `<rect x="${X(r.rate) - 4}" y="${y - 4}" width="8" height="8" fill="#8B0000" data-tip="MPs, ${esc(t)}: ${r.rate}% of ${r.bills}"/>`; }); });
  $("#pr").innerHTML = s + "</svg><div class=\"legend\"><span><i class=\"sw sq\" style=\"background:#141414;border-radius:50%\"></i>Government</span><span><i class=\"sw sq\" style=\"background:#8B0000\"></i>MPs</span></div>";
  const W2 = 520, H2 = sp.length * rowH + 30, mx = Math.max(...sp.map(r => r.median_days)), X2 = v => 150 + v / mx * (W2 - 150 - 60);
  let s2 = `<svg viewBox="0 0 ${W2} ${H2}" role="img" aria-label="Median days to passage by topic">`;
  sp.forEach((r, i) => { const y = 4 + i * rowH + rowH / 2; s2 += `<text class="t" x="144" y="${y + 4}" text-anchor="end">${esc(r.topic)}</text><rect x="150" y="${y - 6}" width="${X2(r.median_days) - 150}" height="12" rx="3" fill="#8B0000" fill-opacity=".85" data-tip="${esc(r.topic)}: ${r.median_days} days; ${r.urgent_pct}% urgent; ${r.passed_bills} bills"/><text x="${X2(r.median_days) + 5}" y="${y + 4}">${r.median_days}</text>`; });
  $("#sd").innerHTML = s2 + "</svg>";
}

fetchJSON("legislative.json").then(j => { L = j; readHash(); if (!L.terms.some(t => t.term === S.term)) S.term = 10; render(); })
  .catch(e => { $("#main").innerHTML = `<p class="notice">Could not load the data (${esc(e.message)}).</p>`; });
window.addEventListener("hashchange", () => { if (L) { readHash(); render(); } });
})();
