/* Governments and ministers — who sat in each Polish government, 1988–2026 (draft).
 *
 * Reads governments.json: the 23 cabinets of Party tracker, every post (minister, deputy prime minister,
 * minister without portfolio, head of a central office, committee chair) with its dates and the official
 * acts that confirm them, the ministry lineages (proposed) and statistics. Written by
 * "Governments/analysis/90_export.R" in the Polish political analyses repo.
 * Views: Cabinets (a timeline of posts), Ministries (lineages), Statistics.
 */
"use strict";
const $ = id => document.getElementById(id);
const esc = s => String(s == null ? "" : s).replace(/[&<>"']/g, c => ({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}[c]));
const isNum = v => typeof v === "number" && isFinite(v);
const NS = "http://www.w3.org/2000/svg";
const INK = "#141414", SOFT = "#4B4B4B", MUTED = "#767676", GRID = "#ECECEC", BASE = "#D5D5D5";
const HALO = {stroke: "#fff", "stroke-width": 3, "paint-order": "stroke"};
const TYPE_COL = {"prime minister": "#8B0000", "deputy prime minister": "#C25A5A", "minister": "#4B6A9B", "minister without portfolio": "#8FA3C2", "head of a central office": "#6B8F71", "committee chair": "#B8B8B8", "other": "#B8B8B8"};
const TYPE_LAB_PL = {"prime minister": "Prezes Rady Ministrów", "deputy prime minister": "Wiceprezesi Rady Ministrów", "minister": "Ministrowie kierujący resortami", "minister without portfolio": "Ministrowie-członkowie Rady Ministrów", "head of a central office": "Kierownicy urzędów centralnych", "committee chair": "Przewodniczący komitetów", "other": "Inne"};
const TYPE_ORDER = ["prime minister", "deputy prime minister", "minister", "minister without portfolio", "head of a central office", "committee chair", "other"];
const TYPE_LAB = {"prime minister": "Prime minister", "deputy prime minister": "Deputy prime minister", "minister": "Minister heading a ministry", "minister without portfolio": "Minister without portfolio", "head of a central office": "Head of a central office", "committee chair": "Committee chair", "other": "Other"};
let D, S = {v: "cabinet", cab: null, colour: "party", post: null, type: "all", lang: "en", lin: null, person: null};
const day = s => s ? Date.parse(s + "T00:00:00Z") / 864e5 : null;
const MON = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
const dstr = s => { if (!s) return "present"; const d = new Date(s + "T00:00:00Z"); return d.getUTCDate() + " " + MON[d.getUTCMonth()] + " " + d.getUTCFullYear(); };
function el(tag, a, p) { const e = document.createElementNS(NS, tag); for (const k in a) e.setAttribute(k, a[k]); if (p) p.appendChild(e); return e; }
function text(p, x, y, s, a = {}) { const t = el("text", Object.assign({x, y, "font-size": 11, fill: MUTED}, a), p); t.textContent = s; return t; }
const trunc = (s, n) => s && s.length > n ? s.slice(0, n - 1) + "…" : s;

function init(d) {
  D = d; D.posts.forEach(p => { p.s = day(p.start); p.e = day(p.end); const arr = x => Array.isArray(x) ? x : (x == null ? [] : [x]); p.segments = arr(p.segments); p.cabinets = arr(p.cabinets); p.reappointedOn = arr(p.reappointedOn); });
  $("cabsel").innerHTML = D.cabinets.map(c => `<option value="${c.id}">${esc(c.name)} (${c.start.slice(0, 4)}–${c.end ? c.end.slice(0, 4) : ""})</option>`).join("");
  S.cab = D.cabinets[D.cabinets.length - 1].id;
  const h = new URLSearchParams(location.hash.replace(/^#/, "")); if (h.get("c") && D.cabinets.some(c => c.id === h.get("c"))) S.cab = h.get("c"); if (["cabinet", "ministries", "people", "stats"].includes(h.get("v"))) S.v = h.get("v"); if (h.get("m")) S.lin = h.get("m"); if (h.get("q")) S.person = h.get("q");
  $("asof").innerHTML = `${esc(D.meta.coverage)}. ${D.meta.validation.confirmed.toLocaleString("en-GB")} of ${D.meta.validation.events.toLocaleString("en-GB")} appointments and departures are confirmed against Monitor Polski, the official gazette.`;
  $("cabsel").addEventListener("change", e => { S.cab = e.target.value; S.post = null; render(); });
  $("views").querySelectorAll("button").forEach(b => b.addEventListener("click", () => { S.v = b.dataset.v; render(); }));
  $("langsel").querySelectorAll("button").forEach(b => b.addEventListener("click", () => { S.lang = b.dataset.l; render(); }));
  if (h.get("l") === "pl") S.lang = "pl";
  $("colourby").querySelectorAll("button").forEach(b => b.addEventListener("click", () => { S.colour = b.dataset.c; render(); }));
  const types = [...new Set(D.ministries.map(m => m.type))].sort();
  $("mintype").innerHTML = `<option value="all">All</option>` + types.map(t => `<option value="${t}">${esc(t)}</option>`).join("");
  $("mintype").addEventListener("change", e => { S.type = e.target.value; render(); });
  const people = new Map(); D.posts.forEach(p => { if (p.qid && !people.has(p.qid)) people.set(p.qid, p.person); });
  D.people = [...people.entries()].map(([qid, name]) => ({qid, name})).sort((x, y) => x.name.localeCompare(y.name, "pl"));
  $("plist").innerHTML = D.people.map(p => `<option value="${esc(p.name)}"></option>`).join("");
  $("psearch").addEventListener("change", e => { const m = D.people.find(p => p.name.toLowerCase() === e.target.value.trim().toLowerCase()); if (m) { S.person = m.qid; render(); } });
  $("credit").innerHTML = "<p><b>Sources:</b> " + D.meta.sources.map(s => `<a href="${esc(s.url)}" target="_top">${esc(s.name)}</a> (${esc(s.note)})`).join("; ") + ". Gender, birth dates and party memberships are from Wikidata (CC0); cabinet tables are from Polish Wikipedia (CC BY-SA 4.0). Every post links to the official acts that confirm its dates where Monitor Polski has them.</p>";
  window.addEventListener("resize", () => { clearTimeout(init.rz); init.rz = setTimeout(() => { if (S.v === "cabinet") drawGantt(); else if (S.v === "stats") drawStats(); else if (S.v === "people") drawPeople(); else if (S.v === "ministries") drawHolders(); }, 150); });
  render();
}
function render() {
  $("views").querySelectorAll("button").forEach(b => b.setAttribute("aria-pressed", b.dataset.v === S.v));
  ["cabinet", "ministries", "people", "stats"].forEach(v => { $("v-" + v).hidden = S.v !== v; });
  $("colourby").querySelectorAll("button").forEach(b => b.setAttribute("aria-pressed", b.dataset.c === S.colour));
  $("langsel").querySelectorAll("button").forEach(b => b.setAttribute("aria-pressed", b.dataset.l === S.lang));
  $("cabsel").value = S.cab;
  try { history.replaceState(null, "", "#v=" + S.v + "&c=" + S.cab + (S.lang === "pl" ? "&l=pl" : "") + (S.lin ? "&m=" + S.lin : "") + (S.person ? "&q=" + S.person : "")); } catch (e) {}
  if (S.v === "cabinet") drawCabinet(); else if (S.v === "ministries") drawMinistries(); else if (S.v === "people") drawPeople(); else drawStats();
}

// ---- Cabinets -------------------------------------------------------------------------------------
const cabPm = id => { const c = D.cabinets.find(x => x.id === id); return esc(c ? c.pm + " " + c.start.slice(0, 4) : id); };
const cabPosts = id => D.posts.filter(p => p.cabinets.includes(id)).filter(p => p.type !== "other");
// The post as it was in this cabinet: the last segment of the tenure inside it
const segIn = (p, id) => { const s = p.segments.filter(x => x.cabinet === id); return s.length ? s[s.length - 1] : p.segments[p.segments.length - 1]; };
const roleTxt = seg => S.lang === "en" ? (seg.roleEn || seg.role) : seg.role;
const typeLab = t => S.lang === "en" ? TYPE_LAB[t] : TYPE_LAB_PL[t];
const movedTxt = (p, k) => S.lang === "en" ? (p[k + "En"] || p[k]) : p[k];
const portfolioOf = role => /^minister\b/i.test(role) ? role.replace(/^minister\s*[-–,]?\s*/i, "").replace(/^członek rady ministrów,?\s*/i, "") || "member of the Council" : role;
function postOrder(a, b) {
  const t = TYPE_ORDER.indexOf(a.type) - TYPE_ORDER.indexOf(b.type);
  if (t) return t;
  return (a.lineageLabel || a.portfolio || "").localeCompare(b.lineageLabel || b.portfolio || "") || a.s - b.s;
}
function drawCabinet() {
  const c = D.cabinets.find(x => x.id === S.cab), v = c.validation, st = c.stats, t = c.turnover;
  const ok = v ? Math.round(100 * v.share) : null;
  $("cabhead").innerHTML =
    `<div class="card"><h3>${esc(c.name)}</h3><div class="kv"><span class="k">Prime minister</span><span>${esc(c.pm)}</span><span class="k">In office</span><span>${dstr(c.start)} to ${dstr(c.end)}</span>` +
    `<span class="k">Dates checked</span><span>${v ? `<span class="badge ${ok >= 95 ? "ok" : "warn"}">${v.confirmed} of ${v.events} confirmed (${ok}%)</span>` : "–"}</span>` +
    `<span class="k">Source</span><span>${c.wikipedia ? `<a href="${esc(c.wikipedia.url)}" target="_top">Wikipedia: ${esc(c.wikipedia.title)}</a>` : "–"}</span></div></div>` +
    `<div class="card"><h3>The cabinet in numbers</h3><div class="kv">` +
    (st ? `<span class="k">Members over the term</span><span>${st.people}, of whom ${st.women} women (${Math.round(100 * st.women / st.people)}%)</span><span class="k">Average age at appointment</span><span>${st.meanAge} years</span><span class="k">Non-party</span><span>${st.nonParty} recorded</span>` : "") +
    (t ? `<span class="k">Changes after the first month</span><span>${t.changeDates} dates: ${t.joiners} joined, ${t.departures} left, ${t.moves} moved post (${t.perYear} dates a year)</span>` : "") + `</div></div>`;
  drawGantt(); drawChanges(c); drawDetail();
}
function drawGantt() {
  const c = D.cabinets.find(x => x.id === S.cab), svg = $("gantt"), tip = $("tip"); svg.innerHTML = "";
  const posts = cabPosts(S.cab).sort(postOrder);
  const nHead = new Set(posts.map(p => p.type)).size;
  const x0 = day(c.start) - 10, x1 = (c.end ? day(c.end) : day("2026-10-01")) + 10;
  const narrow = svg.parentNode.clientWidth < 560, W = Math.max(narrow ? 320 : 520, svg.parentNode.clientWidth - 12), L = narrow ? 120 : 330, R = 16, rh = 17, T = 22, H = T + (posts.length + nHead) * rh + 22;
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  const X = d => L + (d - x0) / (x1 - x0) * (W - L - R);
  $("gtitle").textContent = `${posts.length} posts, grouped by type and portfolio. Each bar is one person\u2019s continuous service in one post; an open end (\u25C2 \u25B8) means the minister carried on from, or into, the neighbouring government.`;
  // year grid
  const y0 = new Date(x0 * 864e5).getUTCFullYear(), y1 = new Date(x1 * 864e5).getUTCFullYear(), span = y1 - y0, step = span > 20 ? 5 : span > 8 ? 2 : 1;
  for (let y = Math.ceil(y0 / step) * step; y <= y1; y += step) { const x = X(day(y + "-01-01")); if (x < L || x > W - R) continue; el("line", {x1: x, x2: x, y1: T - 6, y2: H - 16, stroke: GRID}, svg); text(svg, x, H - 4, y, {"text-anchor": "middle"}); }
  el("line", {x1: L, x2: W - R, y1: H - 16, y2: H - 16, stroke: BASE}, svg);
  let lastType = null, row = 0;
  posts.forEach((p) => {
    if (p.type !== lastType) { text(svg, 4, T + row * rh + 12, typeLab(p.type).toUpperCase(), {"font-size": 9.5, fill: "#8B0000", "letter-spacing": ".06em", "font-weight": 500}); lastType = p.type; row++; }
    const y = T + row * rh; row++;
    const seg = segIn(p, S.cab);
    const label = roleTxt(seg);
    const pcol = S.colour === "party" ? p.colour : TYPE_COL[p.type];
    const cStart = day(c.start), cEnd = c.end ? day(c.end) : null;
    const xs = X(Math.max(p.s, cStart)), xe = X(Math.min(p.e == null ? x1 - 10 : p.e, cEnd == null ? x1 - 10 : cEnd));
    el("rect", {x: xs, y: y + 2, width: Math.max(3, xe - xs), height: rh - 5, rx: 2, fill: pcol, opacity: .88, style: "cursor:pointer"}, svg);
    // a post that carries on from the previous government, or into the next one, is drawn open at that end
    if (p.s < cStart - 3) el("path", {d: `M${xs},${y + 2}l-6,${(rh - 5) / 2}l6,${(rh - 5) / 2}`, fill: "none", stroke: pcol, "stroke-width": 1.6}, svg);
    if (cEnd != null && (p.e == null || p.e > cEnd + 3)) el("path", {d: `M${xe},${y + 2}l6,${(rh - 5) / 2}l-6,${(rh - 5) / 2}`, fill: "none", stroke: pcol, "stroke-width": 1.6}, svg);
    text(svg, xs + 4, y + 12, trunc(p.person, Math.max(8, Math.floor((xe - xs) / 5.6))), Object.assign({"font-size": 10, fill: "#fff"}, {}));
    const nl = text(svg, L - 6, y + 12, trunc(label, narrow ? 19 : 56), {"text-anchor": "end", fill: SOFT});
    const hit = el("rect", {x: 0, y: y, width: W, height: rh, fill: "transparent", style: "cursor:pointer"}, svg);
    hit.addEventListener("mousemove", ev => {
      const r = svg.parentNode.getBoundingClientRect();
      tip.hidden = false; tip.style.left = Math.min(ev.clientX - r.left + 14, r.width - 330) + "px"; tip.style.top = ev.clientY - r.top + 12 + "px";
      tip.innerHTML = `<b>${esc(p.person)}</b><br>${esc(roleTxt(seg))}<br>In this post ${dstr(p.start)} to ${dstr(p.end)}${p.reappointedOn.length ? `<br><span class="muted">re-appointed to the same post ${p.reappointedOn.length}×</span>` : ""}<br><span class="muted">${esc(p.party)}${p.ageAtStart ? " · " + Math.round(p.ageAtStart) + " at appointment" : ""}</span>`;
    });
    hit.addEventListener("mouseleave", () => { tip.hidden = true; });
    hit.addEventListener("click", () => { S.post = p.id; drawDetail(); $("detail").scrollIntoView({block: "nearest"}); });
    if (p.id === S.post) el("rect", {x: xs - 1, y: y + 1, width: Math.max(5, xe - xs + 2), height: rh - 3, rx: 3, fill: "none", stroke: INK, "stroke-width": 1.4}, svg);
  });
  const seen = new Map(); posts.forEach(p => seen.set(S.colour === "party" ? p.party : typeLab(p.type), S.colour === "party" ? p.colour : TYPE_COL[p.type]));
  $("glegend").innerHTML = [...seen.entries()].map(([k, col]) => `<span><span class="dot" style="background:${col}"></span>${esc(k)}</span>`).join("");
}
function statusBadge(status, act, url) {
  if (!status) return `<span class="badge">not checked</span>`;
  const ok = status.startsWith("confirmed");
  if (status.startsWith("not checked person by person")) return `<span class="badge">ended with the government</span>`;
  return `<span class="badge ${ok ? "ok" : "warn"}">${esc(status)}</span>` + (act ? ` <a href="${esc(url || "#")}" target="_top">${esc(act)}</a>` : "");
}
function drawDetail() {
  const p = D.posts.find(x => x.id === S.post), el_ = $("detail");
  if (!p) { el_.innerHTML = `<span class="muted">Click a bar for the minister's details and the official acts that confirm their dates.</span>`; return; }
  const others = D.posts.filter(x => x.qid === p.qid && x.id !== p.id).sort((a, b) => a.s - b.s);
  el_.innerHTML = `<h3>${esc(p.person)}</h3><div class="kv"><span class="k">Post</span><span>${esc(roleTxt(segIn(p, S.cab)))}${S.lang === "en" ? `<br><span class="muted">${esc(segIn(p, S.cab).role)}</span>` : ""}${p.portfolio && p.portfolio.includes(" > ") ? `<br><span class="muted">the ministry was renamed: ${esc(p.portfolio)}</span>` : ""}</span>` +
    `<span class="k">Held</span><span>${dstr(p.start)} to ${dstr(p.end)} <span class="muted">(${esc(p.startEvent)}; ${esc(p.endEvent)}${p.movedTo ? ": " + esc(movedTxt(p, "movedTo")) : ""})</span></span>` +
    (p.segments.length > 1 ? `<span class="k">Appointments</span><span>${p.segments.map(s => `${dstr(s.start)} (${esc(s.cabinet)}${s.startAct ? ", " + (s.startActUrl ? `<a href="${esc(s.startActUrl)}" target="_top">${esc(s.startAct)}</a>` : esc(s.startAct)) : ""})`).join("<br>")}<br><span class="muted">One continuous tenure: the minister was re-appointed to the same post each time, so these are not counted as leaving and returning.</span></span>` : "") +
    `<span class="k">Start confirmed by</span><span>${statusBadge(p.startStatus, p.startAct, p.startActUrl)}${p.startNote ? `<br><span class="muted">Wikipedia cites: ${esc(p.startNote)}</span>` : ""}</span>` +
    `<span class="k">End confirmed by</span><span>${p.end ? statusBadge(p.endStatus, p.endAct, p.endActUrl) + (p.endNote ? `<br><span class="muted">Wikipedia cites: ${esc(p.endNote)}</span>` : "") : "still in office"}</span>` +
    `<span class="k">Party</span><span>${esc(p.party)} <span class="muted">(${esc(p.partySource)})</span></span>` +
    `<span class="k">Born</span><span>${p.birth ? dstr(p.birth) : "–"}${p.ageAtStart ? `, ${Math.round(p.ageAtStart)} at appointment` : ""}${p.gender ? ` · ${esc(p.gender)}` : ""}</span>` +
    `<span class="k">Records</span><span>${p.qid ? `<a href="https://www.wikidata.org/wiki/${esc(p.qid)}" target="_top">Wikidata ${esc(p.qid)}</a>` : "–"}</span>` +
    `<span class="k">Career</span><span><a href="#" data-q="${esc(p.qid)}">Open this minister\u2019s whole career</a></span>` + (others.length ? `<span class="k">Also in these governments</span><span>${others.map(o => `${esc(roleTxt(o.segments[o.segments.length - 1]))} (${dstr(o.start).slice(-4)}–${o.end ? o.end.slice(0, 4) : ""})`).join("; ")}</span>` : "") + `</div>`;
  el_.querySelectorAll("a[data-q]").forEach(l => l.addEventListener("click", ev => { ev.preventDefault(); S.person = l.dataset.q; S.v = "people"; render(); }));
}
function drawChanges(c) {
  const cs = day(c.start), ce = c.end ? day(c.end) : 1e9;
  const mid = d => d != null && d > cs + 31 && d < ce - 3;
  const ev = [], posts = cabPosts(c.id); let reapp = 0;
  posts.forEach(p => {
    if (mid(p.s)) {
      if (p.startEvent === "joined government") ev.push({d: p.start, k: "joins", p, txt: `<b>${esc(p.person)}</b> joins as ${esc(portfolioLabel(p))}`});
      else if (p.startEvent === "moved from another post") ev.push({d: p.start, k: "moves", p, txt: `<b>${esc(p.person)}</b> moves from ${esc(movedTxt(p, "movedFrom"))} to ${esc(portfolioLabel(p))}`});
      else ev.push({d: p.start, k: "adds", p, txt: `<b>${esc(p.person)}</b> takes on ${esc(portfolioLabel(p))} as well`});
    }
    if (p.e && mid(p.e) && p.endEvent === "left government") ev.push({d: p.end, k: "leaves", p, txt: `<b>${esc(p.person)}</b> leaves the government <span class="muted">(was ${esc(portfolioLabel(p))})</span>`});
    reapp += (p.reappointedOn || []).filter(d => mid(day(d))).length;
  });
  ev.sort((a, b) => a.d.localeCompare(b.d) || a.k.localeCompare(b.k));
  const byDate = new Map(); ev.forEach(e => { if (!byDate.has(e.d)) byDate.set(e.d, []); byDate.get(e.d).push(e); });
  $("changes").innerHTML = `<h3>Changes during the term</h3>` + (byDate.size ? `<table class="t"><tr><th>Date</th><th>Changes</th></tr>` +
    [...byDate.entries()].map(([d, es]) => `<tr><td style="white-space:nowrap">${dstr(d)}</td><td>${es.map(e => e.txt).join("<br>")}</td></tr>`).join("") + `</table>` : `<p class="help">No changes after the first month.</p>`) +
    `<p class="help">Ministers who carried on, including those re-appointed to the same post or whose ministry was renamed${reapp ? ` (${reapp} such re-appointments in this term)` : ""}, are not listed as leaving or joining.</p>`;
}
const portfolioLabel = p => roleTxt(segIn(p, S.cab));

// ---- Ministries -----------------------------------------------------------------------------------
function drawMinistries() {
  const ms = D.ministries.filter(m => S.type === "all" || m.type === S.type).sort((a, b) => a.label.localeCompare(b.label));
  $("lineages").innerHTML = ms.map(m => {
    const names = m.names.slice().sort((a, b) => a.first.localeCompare(b.first));
    return `<div class="lin${S.lin === m.id ? " on" : ""}"><h4><button type="button" data-l="${esc(m.id)}">${esc(m.label)}</button> <span class="badge">${esc(m.type)}</span> <span class="badge warn">proposed</span></h4><div class="names">` +
      names.map(n => `<div><span class="yr">${n.first.slice(0, 4)}–${n.last >= "2026-09" ? "" : n.last.slice(0, 4)}</span><span>${esc(S.lang === "en" ? (n.nameEn || n.name) : "Minister " + n.name)}${n.roleType !== "minister" ? ` <span class="muted">(${esc(n.roleType)})</span>` : ""}` +
        `${n.alsoFrom ? `<br><span class="muted">draws on: ${esc(n.alsoFrom.split(";").map(a => (D.ministries.find(x => x.id === a) || {label: a}).label).join(", "))}</span>` : ""}${n.note ? `<br><span class="muted">${esc(n.note)}</span>` : ""}</span></div>`).join("") + `</div></div>`;
  }).join("");
  $("lineages").querySelectorAll("h4 button").forEach(b => b.addEventListener("click", () => { S.lin = S.lin === b.dataset.l ? null : b.dataset.l; render(); }));
  drawHolders();
}
function drawHolders() {
  const box = $("holders");
  const m = D.ministries.find(x => x.id === S.lin);
  if (!m) { box.hidden = true; return; }
  box.hidden = false;
  const ts = D.posts.filter(p => p.lineage === m.id && ["minister", "head of a central office"].includes(p.type)).sort((a, b) => a.s - b.s);
  const x0 = day("1988-09-01"), x1 = day("2026-10-15"), W = Math.max(520, box.clientWidth - 32), L = 6, R = 6, X = d => L + (d - x0) / (x1 - x0) * (W - L - R);
  let svg = `<svg viewBox="0 0 ${W} 54" style="width:100%;height:auto;display:block">`;
  for (let y = 1990; y <= 2025; y += 5) svg += `<line x1="${X(day(y + "-01-01"))}" x2="${X(day(y + "-01-01"))}" y1="2" y2="36" stroke="${GRID}"/><text x="${X(day(y + "-01-01"))}" y="50" font-size="10.5" fill="${MUTED}" text-anchor="middle">${y}</text>`;
  ts.forEach((p, i) => { const xs = X(p.s), xe = X(p.e == null ? x1 : p.e); svg += `<rect x="${xs}" y="${i % 2 ? 20 : 4}" width="${Math.max(2, xe - xs)}" height="14" rx="2" fill="${p.colour}" opacity=".88"><title>${esc(p.person)}: ${esc(roleTxt(p.segments[0]))}, ${dstr(p.start)} to ${dstr(p.end)}</title></rect>`; });
  svg += `</svg>`;
  box.innerHTML = `<h3>${esc(m.label)}: who held it</h3><p class="help">Every minister of this lineage, in order. Alternate rows only keep neighbouring bars apart. Proposed lineage: the names are joined by my reading of their dates, not yet confirmed.</p>${svg}` +
    `<table class="t"><tr><th>From</th><th>To</th><th>Minister</th><th>Ministry as named</th><th>Party</th><th>Government(s)</th></tr>` +
    ts.map(p => `<tr><td style="white-space:nowrap">${dstr(p.start)}</td><td style="white-space:nowrap">${dstr(p.end)}</td><td><a href="#" data-q="${esc(p.qid)}">${esc(p.person)}</a></td><td>${esc(S.lang === "en" ? p.segments.map(s => s.roleEn).filter((v, i, a) => a.indexOf(v) === i).join(" > ") : p.portfolio)}</td><td><span class="dot" style="background:${p.colour}"></span> ${esc(p.party)}</td><td>${p.cabinets.map(cabPm).join(", ")}</td></tr>`).join("") + `</table>`;
  box.querySelectorAll("a[data-q]").forEach(l => l.addEventListener("click", ev => { ev.preventDefault(); S.person = l.dataset.q; S.v = "people"; render(); }));
}

// ---- People -------------------------------------------------------------------------------------
function drawPeople() {
  const pc = $("pcard"), pt = $("ptable"), svg = $("pgantt"); svg.innerHTML = "";
  const ts = D.posts.filter(p => p.qid === S.person).sort((a, b) => a.s - b.s);
  const none = !ts.length; svg.parentNode.hidden = none; pt.hidden = none;
  if (none) { pc.innerHTML = `<div class="card"><span class="muted">Choose a minister: type a name above, or open one from a cabinet's timeline or a ministry's holders.</span></div>`; pt.innerHTML = ""; $("ptitle").textContent = ""; return; }
  const p0 = ts[0];
  $("psearch").value = p0.person;
  const gov = []; ts.forEach(t => { const last = gov[gov.length - 1]; if (last && t.s <= last.e + 3) last.e = Math.max(last.e, t.e == null ? 1e9 : t.e); else gov.push({s: t.s, e: t.e == null ? 1e9 : t.e}); });
  const yrs = gov.reduce((a, g) => a + ((Math.min(g.e, day("2026-10-01")) - g.s) / 365.25), 0);
  const cabs = [...new Set(ts.flatMap(t => t.cabinets))].map(id => (D.cabinets.find(c => c.id === id) || {name: id}).name);
  pc.innerHTML = `<div class="card"><h3>${esc(p0.person)}</h3><div class="kv"><span class="k">Born</span><span>${p0.birth ? dstr(p0.birth) : "–"}${p0.gender ? " · " + esc(p0.gender) : ""}</span>` +
    `<span class="k">First appointed</span><span>${dstr(p0.start)}${p0.ageAtStart ? ` (aged ${Math.round(p0.ageAtStart)})` : ""}</span>` +
    `<span class="k">Time in government</span><span>${yrs.toFixed(1)} years${gov.length > 1 ? ` in ${gov.length} spells` : ""}</span>` +
    `<span class="k">Party</span><span>${[...new Set(ts.map(t => t.party))].map(esc).join(" → ")} <span class="muted">(${esc(p0.partySource)})</span></span>` +
    `<span class="k">Records</span><span><a href="https://www.wikidata.org/wiki/${esc(p0.qid)}" target="_top">Wikidata ${esc(p0.qid)}</a></span></div></div>` +
    `<div class="card"><h3>Governments served in</h3><div class="kv"><span class="k">Cabinets</span><span>${cabs.map(esc).join("<br>")}</span></div></div>`;
  $("ptitle").textContent = `${ts.length} posts; each bar is one continuous tenure`;
  const x0 = ts[0].s - 200, x1 = Math.max(...ts.map(t => t.e == null ? day("2026-10-01") : t.e)) + 200, nw = svg.parentNode.clientWidth < 560, W = Math.max(nw ? 320 : 520, svg.parentNode.clientWidth - 12), Lm = nw ? 120 : 330, R = 16, rh = 22, T = 8, H = T + ts.length * rh + 22;
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  const X = d => Lm + (d - x0) / (x1 - x0) * (W - Lm - R);
  const y0 = new Date(x0 * 864e5).getUTCFullYear(), y1 = new Date(x1 * 864e5).getUTCFullYear(), step = (y1 - y0) > 20 ? 5 : (y1 - y0) > 8 ? 2 : 1;
  for (let y = Math.ceil(y0 / step) * step; y <= y1; y += step) { const x = X(day(y + "-01-01")); if (x < Lm || x > W - R) continue; el("line", {x1: x, x2: x, y1: T, y2: H - 16, stroke: GRID}, svg); text(svg, x, H - 4, y, {"text-anchor": "middle"}); }
  ts.forEach((t, i) => {
    const y = T + i * rh, xs = X(t.s), xe = X(t.e == null ? day("2026-10-01") : t.e);
    text(svg, Lm - 6, y + 14, trunc(roleTxt(t.segments[t.segments.length - 1]), nw ? 19 : 56), {"text-anchor": "end", fill: SOFT});
    el("rect", {x: xs, y: y + 2, width: Math.max(3, xe - xs), height: rh - 6, rx: 2, fill: t.colour, opacity: .88}, svg);
  });
  pt.innerHTML = `<h3>Posts and sources</h3><div style="overflow-x:auto"><table class="t"><tr><th>From</th><th>To</th><th>Post</th><th>Government(s)</th><th>Confirmed by</th></tr>` +
    ts.map(t => `<tr><td style="white-space:nowrap">${dstr(t.start)}</td><td style="white-space:nowrap">${dstr(t.end)}</td><td>${esc(roleTxt(t.segments[t.segments.length - 1]))}<br><span class="muted">${esc(t.startEvent)}${t.endEvent && t.end ? "; " + esc(t.endEvent) : ""}</span></td><td>${t.cabinets.map(cabPm).join(", ")}</td>` +
      `<td>${statusBadge(t.startStatus, t.startAct, t.startActUrl)}${t.end ? "<br>" + statusBadge(t.endStatus, t.endAct, t.endActUrl) : ""}</td></tr>`).join("") + `</table></div>`;
}

// ---- Statistics ---------------------------------------------------------------------------------
function barChart(svgId, rows, o) {
  const svg = $(svgId); svg.innerHTML = "";
  const W = Math.max(320, svg.parentNode.clientWidth - 12), L = 150, R = 40, rh = 20, T = 6, H = T + rows.length * rh + 6;
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  const mx = o.max || Math.max(...rows.map(r => r.v)) * 1.05, X = v => L + (W - L - R) * v / mx;
  rows.forEach((r, i) => {
    const y = T + i * rh;
    text(svg, L - 6, y + 13, trunc(r.label, 26), {"text-anchor": "end", fill: SOFT});
    el("rect", {x: L, y: y + 3, width: Math.max(0, X(r.v) - L), height: rh - 7, fill: o.col, opacity: .85}, svg);
    text(svg, X(r.v) + 5, y + 13, o.fmt(r.v), {fill: SOFT});
  });
}
function drawStats() {
  const cs = D.cabinets.filter(c => c.stats && isNum(c.stats.meanAge));
  const short = c => c.pm.split(" ").slice(-1)[0] + " " + c.start.slice(0, 4);
  barChart("s-women", cs.map(c => ({label: short(c), v: c.stats.women / c.stats.people})), {col: "#8B0000", fmt: v => Math.round(100 * v) + "%", max: 0.6});
  barChart("s-age", cs.map(c => ({label: short(c), v: c.stats.meanAge})), {col: "#4B6A9B", fmt: v => v.toFixed(1), max: 60});
  barChart("s-turn", cs.filter(c => c.turnover && isNum(c.turnover.perYear)).map(c => ({label: short(c), v: c.turnover.perYear})), {col: "#6B8F71", fmt: v => v.toFixed(1)});
  const parties = {}; D.stats.partyPortfolios.forEach(r => { (parties[r.party_group] = parties[r.party_group] || {})[r.portfolio_type] = r.posts; });
  const types = [...new Set(D.stats.partyPortfolios.map(r => r.portfolio_type))].sort();
  const rows = Object.entries(parties).map(([p, o]) => ({p, o, tot: Object.values(o).reduce((a, b) => a + b, 0)})).sort((a, b) => b.tot - a.tot).slice(0, 14);
  $("s-party").innerHTML = `<div style="overflow-x:auto"><table class="t"><tr><th>Party</th>${types.map(t => `<th class="n">${esc(t)}</th>`).join("")}<th class="n">All</th></tr>` +
    rows.map(r => `<tr><td>${esc(r.p)}</td>${types.map(t => `<td class="n">${r.o[t] || ""}</td>`).join("")}<td class="n">${r.tot}</td></tr>`).join("") + `</table></div>`;
}

fetch("governments.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).then(init)
  .catch(e => { $("asof").innerHTML = '<div class="loading">The data could not be loaded (' + esc(e.message) + ').</div>'; });
