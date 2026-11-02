/* Governments — who sat in each Polish government, 1988–2026 .
 *
 * Reads governments.json: the 23 cabinets of Party tracker, every post (minister, deputy prime minister,
 * minister without portfolio, head of a central office, committee chair) with its dates and the official
 * acts that confirm them, the ministry lineages with their legal references and statistics. Written by
 * "Governments/analysis/90_export.R" in the Polish political analyses repo.
 * Views: Cabinets (a timeline of posts), Ministries (lineages), People, Coalitions (how cabinets last and end), Statistics.
 */
"use strict";
const $ = id => document.getElementById(id);
const esc = s => String(s == null ? "" : s).replace(/[&<>"']/g, c => ({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}[c]));
const isNum = v => typeof v === "number" && isFinite(v);
const NS = "http://www.w3.org/2000/svg";
const INK = "#141414", SOFT = "#4B4B4B", MUTED = "#767676", GRID = "#ECECEC", BASE = "#D5D5D5";
const HALO = {stroke: "#fff", "stroke-width": 3, "paint-order": "stroke"};
const TYPE_LAB_PL = {"prime minister": "Prezes Rady Ministrów", "deputy prime minister": "Wiceprezesi Rady Ministrów", "minister": "Ministrowie kierujący resortami", "minister without portfolio": "Ministrowie-członkowie Rady Ministrów", "head of a central office": "Kierownicy urzędów centralnych", "committee chair": "Przewodniczący komitetów", "other": "Inne"};
const TYPE_ORDER = ["prime minister", "deputy prime minister", "minister", "minister without portfolio", "head of a central office", "committee chair", "other"];
const TYPE_LAB = {"prime minister": "Prime minister", "deputy prime minister": "Deputy prime minister", "minister": "Minister heading a ministry", "minister without portfolio": "Minister without portfolio", "head of a central office": "Head of a central office", "committee chair": "Committee chair", "other": "Other"};
let D, S = {v: "cabinet", cab: null, post: null, type: "all", lang: "en", lin: null, person: null};
const day = s => s ? Date.parse(s + "T00:00:00Z") / 864e5 : null;
const MON = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
const dstr = s => { if (!s) return "present"; const d = new Date(s + "T00:00:00Z"); return d.getUTCDate() + " " + MON[d.getUTCMonth()] + " " + d.getUTCFullYear(); };
function el(tag, a, p) { const e = document.createElementNS(NS, tag); for (const k in a) e.setAttribute(k, a[k]); if (p) p.appendChild(e); return e; }
function text(p, x, y, s, a = {}) { const t = el("text", Object.assign({x, y, "font-size": 11, fill: MUTED}, a), p); t.textContent = s; return t; }
const trunc = (s, n) => s && s.length > n ? s.slice(0, n - 1) + "…" : s;

function init(d) {
  D = d; D.posts.forEach(p => { p.s = day(p.start); p.e = day(p.end); const arr = x => Array.isArray(x) ? x : (x == null ? [] : [x]); p.segments = arr(p.segments); p.cabinets = arr(p.cabinets); p.reappointedOn = arr(p.reappointedOn); });
  S.cab = D.cabinets[D.cabinets.length - 1].id;
  const h = new URLSearchParams(location.hash.replace(/^#/, "")); if (h.get("c") && D.cabinets.some(c => c.id === h.get("c"))) S.cab = h.get("c"); if (["cabinet", "ministries", "people", "coalitions", "stats"].includes(h.get("v"))) S.v = h.get("v"); if (h.get("m")) S.lin = h.get("m"); if (h.get("q")) S.person = h.get("q");
  $("asof").innerHTML = `${esc(D.meta.coverage)}. ${D.meta.validation.confirmed.toLocaleString("en-GB")} of ${D.meta.validation.events.toLocaleString("en-GB")} appointments and departures are confirmed against Monitor Polski, the official gazette.`;
  const step = k => { const i = D.cabinets.findIndex(c => c.id === S.cab) + k; if (i >= 0 && i < D.cabinets.length) { S.cab = D.cabinets[i].id; S.post = null; render(); } };
  $("cabprev").addEventListener("click", () => step(-1)); $("cabnext").addEventListener("click", () => step(1));
  $("views").querySelectorAll("button").forEach(b => b.addEventListener("click", () => { S.v = b.dataset.v; render(); }));
  $("langsel").querySelectorAll("button").forEach(b => b.addEventListener("click", () => { S.lang = b.dataset.l; render(); }));
  if (h.get("l") === "pl") S.lang = "pl";
  const types = [...new Set(D.ministries.map(m => m.type))].sort();
  $("mintype").innerHTML = `<option value="all">All</option>` + types.map(t => `<option value="${t}">${esc(t)}</option>`).join("");
  $("mintype").addEventListener("change", e => { S.type = e.target.value; render(); });
  const people = new Map(); D.posts.forEach(p => { if (p.qid && !people.has(p.qid)) people.set(p.qid, p.person); });
  D.people = [...people.entries()].map(([qid, name]) => ({qid, name})).sort((x, y) => x.name.localeCompare(y.name, "pl"));
  $("plist").innerHTML = D.people.map(p => `<option value="${esc(p.name)}"></option>`).join("");
  $("psearch").addEventListener("change", e => { const m = D.people.find(p => p.name.toLowerCase() === e.target.value.trim().toLowerCase()); if (m) { S.person = m.qid; render(); } });
  $("credit").innerHTML = "<p><b>Sources:</b> " + D.meta.sources.map(s => `<a href="${esc(s.url)}" target="_top">${esc(s.name)}</a> (${esc(s.note)})`).join("; ") + ". Every post links to the official acts that confirm its dates where Monitor Polski has them.</p>";
  window.addEventListener("resize", () => { clearTimeout(init.rz); init.rz = setTimeout(() => { if (S.v === "cabinet") { drawStrip(); drawGantt(); } else if (S.v === "stats") drawStats(); else if (S.v === "coalitions") drawCoalitions(); else if (S.v === "people") drawPeople(); else if (S.v === "ministries") drawHolders(); }, 150); });
  render();
}
function render() {
  $("views").querySelectorAll("button").forEach(b => b.setAttribute("aria-pressed", b.dataset.v === S.v));
  ["cabinet", "ministries", "people", "coalitions", "stats"].forEach(v => { $("v-" + v).hidden = S.v !== v; });
  $("langsel").querySelectorAll("button").forEach(b => b.setAttribute("aria-pressed", b.dataset.l === S.lang));
  try { history.replaceState(null, "", "#v=" + S.v + "&c=" + S.cab + (S.lang === "pl" ? "&l=pl" : "") + (S.lin ? "&m=" + S.lin : "") + (S.person ? "&q=" + S.person : "")); } catch (e) {}
  if (S.v === "cabinet") { drawStrip(); drawCabinet(); } else if (S.v === "ministries") drawMinistries(); else if (S.v === "people") drawPeople(); else if (S.v === "coalitions") drawCoalitions(); else drawStats();
}


// A strip of all cabinets, each as wide as its time in office, coloured by its prime minister's party
const inkOn = col => { const m = /^#?([0-9a-f]{6})$/i.exec(col || ""); if (!m) return "#fff"; const n = parseInt(m[1], 16), l = (0.299 * (n >> 16) + 0.587 * ((n >> 8) & 255) + 0.114 * (n & 255)) / 255; return l > 0.62 ? INK : "#fff"; };
const surname = s => String(s).split(" ").slice(-1)[0];
function drawStrip() {
  const svg = $("strip"); svg.innerHTML = "";
  const W = Math.max(300, svg.parentNode.clientWidth - 12), H = 52, L = 3, R = 3, x0 = day("1988-09-01"), x1 = day("2026-10-15"), X = d => L + (d - x0) / (x1 - x0) * (W - L - R);
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  for (let y = 1990; y <= 2025; y += 5) { const x = X(day(y + "-01-01")); text(svg, x, H - 4, y, {"text-anchor": "middle", "font-size": 10.5}); }
  D.cabinets.forEach(c => {
    const xs = X(day(c.start)), xe = X(c.end ? day(c.end) : x1), w = Math.max(3, xe - xs - 1);
    const pm = D.posts.find(p => p.type === "prime minister" && p.cabinets.includes(c.id)), col = pm ? pm.colour : "#B8B8B8";
    const g = el("g", {tabindex: 0, role: "button", "aria-label": `${c.name}, ${dstr(c.start)} to ${dstr(c.end)}`, style: "cursor:pointer;outline:none"}, svg);
    const t = el("title", {}, g); t.textContent = `${c.name} (${dstr(c.start)} to ${dstr(c.end)})`;
    el("rect", {x: xs + .5, y: 4, width: w, height: 24, rx: 2, fill: col, opacity: c.failed ? .3 : .92}, g);
    const sn = surname(c.pm);
    if (!c.failed && w >= sn.length * 5.6 + 6) text(g, xs + 4, 20, sn, {"font-size": 10, fill: inkOn(col)});
    if (c.id === S.cab) el("rect", {x: xs - 1, y: 2.5, width: w + 3, height: 27, rx: 3, fill: "none", stroke: INK, "stroke-width": 1.6}, g);
    const pick = () => { S.cab = c.id; S.post = null; render(); };
    g.addEventListener("click", pick); g.addEventListener("keydown", e => { if (e.key === "Enter" || e.key === " ") { e.preventDefault(); pick(); } });
  });
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
  const c = D.cabinets.find(x => x.id === S.cab), has = o => o && Object.keys(o).length > 0, v = has(c.validation) ? c.validation : null, st = has(c.stats) ? c.stats : null, t = has(c.turnover) ? c.turnover : null;
  const ok = v ? Math.round(100 * v.share) : null;
  $("cabhead").innerHTML =
    `<div class="card"><h3>${esc(c.name)}</h3><div class="kv"><span class="k">Prime minister</span><span>${esc(c.pm)}</span><span class="k">In office</span><span>${dstr(c.start)} to ${dstr(c.end)}</span>` +
    `${c.durability && !c.failed ? `<span class="k">Time in office</span><span>${c.durability.days.toLocaleString("en-GB")} days${c.durability.shareOfTerm != null ? `, ${Math.round(100 * c.durability.shareOfTerm)}% of the Sejm term it began in` : ""}</span><span class="k">Coalition</span><span>${[].concat(c.durability.parties).map(esc).join(", ")} (${esc(c.durability.majority)}${c.durability.seatsStart ? `, ${c.durability.seatsStart} seats at appointment` : ""})</span><span class="k">How it ended</span><span>${esc(c.durability.endLabel)}</span>` : ""}<span class="k">Dates checked</span><span>${v ? `<span class="badge ${ok >= 95 ? "ok" : "warn"}">${v.confirmed} of ${v.events} confirmed (${ok}%)</span>` : "–"}</span>` +
    `<span class="k">Source</span><span><a href="https://monitorpolski.gov.pl" target="_top">Monitor Polski</a>${v && v.confirmed < v.events && c.wikipedia ? `; the ${v.events - v.confirmed} unconfirmed dates are from <a href="${esc(c.wikipedia.url)}" target="_top">Polish Wikipedia</a>` : (!v && c.wikipedia ? `; <a href="${esc(c.wikipedia.url)}" target="_top">Polish Wikipedia</a>` : "")}</span></div></div>` +
    (c.failed ? `<div class="card"><h3>A government that was never formed</h3><p class="help" style="margin:0">The Sejm appointed Waldemar Pawlak prime minister on 5 June 1992, after dismissing Jan Olszewski\u2019s government, and asked him to propose a Council of Ministers (M.P. 1992 nr 17 poz. 126). He could not form one, resigned and was dismissed on 10 July, and Hanna Suchocka was appointed in his place. The Olszewski ministers stayed in office meanwhile, so the timeline shows them, and they are counted under the Olszewski government in the statistics.</p></div>` : `<div class="card"><h3>The cabinet in numbers</h3><div class="kv">` +
    (st ? `<span class="k">Members over the term</span><span>${st.people}, of whom ${st.women} women (${Math.round(100 * st.women / st.people)}%)</span><span class="k">Average age at appointment</span><span>${st.meanAge} years</span><span class="k">Non-party</span><span>${st.nonParty} recorded</span>` : "") +
    (t ? `<span class="k">Changes after the first month</span><span>${t.changeDates} dates: ${t.joiners} joined, ${t.departures} left, ${t.moves} moved post (${t.perYear} dates a year)</span>` : "") + `</div></div>`);
  drawGantt(); drawChanges(c); drawDetail();
}
// One row per post slot: successive holders of the same post share a row, concurrent ones (deputy prime ministers) do not
function laneRows(posts) {
  const groups = new Map();
  posts.forEach(p => {
    const seg = segIn(p, S.cab), byLineage = p.type === "minister" || p.type === "head of a central office";
    const key = p.type + "|" + (byLineage ? (p.lineageLabel || p.portfolio || roleTxt(seg)) : roleTxt(seg));
    if (!groups.has(key)) groups.set(key, []); groups.get(key).push(p);
  });
  const rows = [];
  groups.forEach(ps => {
    ps.sort((x, y) => x.s - y.s);
    const lanes = [];
    ps.forEach(p => { const lane = lanes.find(l => l.end <= p.s + 3); const e = p.e == null ? 1e9 : p.e; if (lane) { lane.items.push(p); lane.end = e; } else lanes.push({items: [p], end: e}); });
    lanes.forEach(l => rows.push({type: ps[0].type, sort: ps[0].lineageLabel || ps[0].portfolio || "", items: l.items}));
  });
  return rows.sort((x, y) => TYPE_ORDER.indexOf(x.type) - TYPE_ORDER.indexOf(y.type) || x.sort.localeCompare(y.sort) || x.items[0].s - y.items[0].s);
}
const wrap2 = (s, n) => { if (s.length <= n) return [s]; const w = s.split(" "); let a1 = ""; while (w.length && (a1 + " " + w[0]).trim().length <= n) a1 = (a1 + " " + w.shift()).trim(); return [a1 || w.shift(), trunc(w.join(" "), n)].filter(Boolean); };
let tipTimer;
function drawGantt() {
  const c = D.cabinets.find(x => x.id === S.cab), svg = $("gantt"), tip = $("tip"); svg.innerHTML = "";
  const posts = cabPosts(S.cab), rows = laneRows(posts);
  const nHead = new Set(rows.map(r => r.type)).size;
  const x0 = day(c.start) - 10, x1 = (c.end ? day(c.end) : day("2026-10-01")) + 10;
  const narrow = svg.parentNode.clientWidth < 560, W = Math.max(narrow ? 320 : 520, svg.parentNode.clientWidth - 12), L = narrow ? 128 : 330, R = 16, rh = narrow ? 28 : 18, T = 30, H = T + (rows.length + nHead) * rh + 22;
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  const X = d => L + (d - x0) / (x1 - x0) * (W - L - R);
  $("gtitle").textContent = `${posts.length} posts in ${rows.length} rows. Each bar is one person’s continuous service in one post, and successive holders of a post share a row; an open end (◂ ▸) means the minister carried on from, or into, the neighbouring government. Marks at the top are dates of changes.`;
  const y0 = new Date(x0 * 864e5).getUTCFullYear(), y1 = new Date(x1 * 864e5).getUTCFullYear(), span = y1 - y0, step = span > 20 ? 5 : span > 8 ? 2 : 1;
  for (let y = Math.ceil(y0 / step) * step; y <= y1; y += step) { const x = X(day(y + "-01-01")); if (x < L || x > W - R) continue; el("line", {x1: x, x2: x, y1: T - 6, y2: H - 16, stroke: GRID}, svg); text(svg, x, H - 4, y, {"text-anchor": "middle"}); }
  el("line", {x1: L, x2: W - R, y1: H - 16, y2: H - 16, stroke: BASE}, svg);
  // dates of changes: a faint line down the chart and a mark at the top that leads to the changes table
  const cds = [...new Set(changeEvents(c).map(e => e.d))];
  cds.forEach(d => {
    const x = X(day(d)); if (x < L || x > W - R) return;
    el("line", {x1: x, x2: x, y1: T - 6, y2: H - 16, stroke: "#8B0000", "stroke-opacity": .22, "stroke-dasharray": "2 3"}, svg);
    const m = el("path", {d: `M${x - 4},${T - 20}h8l-4,8z`, fill: "#8B0000", opacity: .75, style: "cursor:pointer"}, svg);
    const t = el("title", {}, m); t.textContent = "Changes on " + dstr(d);
    m.addEventListener("click", () => { const r = $("chg-" + d); if (r) { r.scrollIntoView({block: "center", behavior: "smooth"}); r.classList.add("flash"); setTimeout(() => r.classList.remove("flash"), 1800); } });
  });
  const showTip = (ev, p, seg) => {
    const r = svg.parentNode.getBoundingClientRect();
    tip.hidden = false; tip.style.left = Math.max(0, Math.min(ev.clientX - r.left + 14, r.width - 330)) + "px"; tip.style.top = ev.clientY - r.top + 12 + "px";
    tip.innerHTML = `<b>${esc(p.person)}</b><br>${esc(roleTxt(seg))}<br>In this post ${dstr(p.start)} to ${dstr(p.end)}${p.reappointedOn.length ? `<br><span class="muted">re-appointed to the same post ${p.reappointedOn.length}×</span>` : ""}<br><span class="muted">${esc(p.party)}${p.ageAtStart ? " · " + Math.round(p.ageAtStart) + " at appointment" : ""}</span>`;
  };
  let lastType = null, row = 0;
  const cStart = day(c.start), cEnd = c.end ? day(c.end) : null, CW = 5.6;
  rows.forEach(rw => {
    if (rw.type !== lastType) { text(svg, 4, T + row * rh + 12, typeLab(rw.type).toUpperCase(), {"font-size": 9.5, fill: "#8B0000", "letter-spacing": ".06em", "font-weight": 500}); lastType = rw.type; row++; }
    const y = T + row * rh; row++;
    const label = roleTxt(segIn(rw.items[0], S.cab));
    const lines = narrow ? wrap2(label, 21) : [trunc(label, 56)];
    lines.forEach((ln, k) => text(svg, L - 6, y + (lines.length > 1 ? 10 + 11 * k : rh / 2 + 4), ln, {"text-anchor": "end", fill: SOFT, "font-size": lines.length > 1 ? 10 : 11}));
    const bh = rh - 5;
    rw.items.forEach((p, k) => {
      const seg = segIn(p, S.cab), pcol = p.colour;
      const prev = rw.items[k - 1], next = rw.items[k + 1];
      const touches = (u, v) => u && v && Math.abs(v.s - (u.e == null ? 1e9 : u.e)) <= 6;
      const gapL = touches(prev, p) ? 1 : 0, gapR = touches(p, next) ? 1 : 0;
      // a 2 px gap between neighbouring holders, with rounded ends, so that a hand-over shows
      const xs = X(Math.max(p.s, cStart)), xe = X(Math.min(p.e == null ? x1 - 10 : p.e, cEnd == null ? x1 - 10 : cEnd)), w = xe - xs;
      el("rect", {x: xs + gapL, y: y + 2, width: Math.max(3, w - gapL - gapR), height: bh, rx: 2.5, fill: pcol, opacity: .88}, svg);
      // a post that carries on from the previous government, or into the next one, is drawn open at that end
      if (p.s < cStart - 3) el("path", {d: `M${xs},${y + 2}l-6,${bh / 2}l6,${bh / 2}`, fill: "none", stroke: pcol, "stroke-width": 1.6}, svg);
      if (cEnd != null && (p.e == null || p.e > cEnd + 3)) el("path", {d: `M${xe},${y + 2}l6,${bh / 2}l-6,${bh / 2}`, fill: "none", stroke: pcol, "stroke-width": 1.6}, svg);
      // name: in full if it fits, else the surname; else beside the bar when nothing is in the way
      const sn = surname(p.person), full = narrow ? sn : p.person, ty = y + 2 + bh / 2 + 3.6;
      if (w >= full.length * CW + 8) text(svg, xs + 4 + gapL, ty, full, {"font-size": 10, fill: inkOn(pcol)});
      else if (w >= sn.length * CW + 8) text(svg, xs + 4 + gapL, ty, sn, {"font-size": 10, fill: inkOn(pcol)});
      else { const nx = rw.items[k + 1] ? X(Math.max(rw.items[k + 1].s, cStart)) : W; if (xe + 4 + sn.length * CW < Math.min(nx, W - R)) text(svg, xe + 4, ty, sn, {"font-size": 10, fill: SOFT}); }
      const hit = el("rect", {x: xs, y: y, width: Math.max(10, w), height: rh, fill: "transparent", style: "cursor:pointer"}, svg);
      hit.addEventListener("mousemove", ev => { if (ev.pointerType !== "touch") showTip(ev, p, seg); });
      hit.addEventListener("mouseleave", () => { tip.hidden = true; });
      hit.addEventListener("click", ev => {
        S.post = p.id; drawDetail();
        if (narrow) { showTip(ev, p, seg); clearTimeout(tipTimer); tipTimer = setTimeout(() => { tip.hidden = true; }, 3500); }
        else $("detail").scrollIntoView({block: "nearest"});
      });
      if (p.id === S.post) el("rect", {x: xs - 1, y: y + 1, width: Math.max(5, w + 2), height: bh + 2, rx: 3, fill: "none", stroke: INK, "stroke-width": 1.4}, svg);
    });
  });
  // legend: parties that share a colour are one swatch
  const byCol = new Map(); posts.forEach(p => { if (!byCol.has(p.colour)) byCol.set(p.colour, new Set()); byCol.get(p.colour).add(p.party); });
  $("glegend").innerHTML = [...byCol.entries()].map(([col, ps]) => `<span><span class="dot" style="background:${col}"></span>${[...ps].map(esc).join(" / ")}</span>`).join("");
}
// The ELI link to a gazette act is built from its reference ("M.P. 2023 nr 5 poz. 12")
const actUrl = act => { const m = /M\.P\. (\d{4}) (?:nr \d+ )?poz\. (\d+)/.exec(act || ""); return m ? `https://api.sejm.gov.pl/eli/acts/MP/${m[1]}/${m[2]}/text.pdf` : null; };
function statusBadge(status, act, url) {
  if (!status) return `<span class="badge">not checked</span>`;
  const ok = status.startsWith("confirmed");
  if (status.startsWith("not checked person by person")) return `<span class="badge">ended with the government</span>`;
  return `<span class="badge ${ok ? "ok" : "warn"}">${esc(status)}</span>` + (act ? ` <a href="${esc(url || actUrl(act) || "#")}" target="_top">${esc(act)}</a>` : "");
}
function drawDetail() {
  const p = D.posts.find(x => x.id === S.post), el_ = $("detail");
  if (!p) { el_.innerHTML = `<span class="muted">Click a bar for the minister's details and the official acts that confirm their dates.</span>`; return; }
  const others = D.posts.filter(x => x.qid === p.qid && x.id !== p.id).sort((a, b) => a.s - b.s);
  el_.innerHTML = `<h3>${esc(p.person)}</h3><div class="kv"><span class="k">Post</span><span>${esc(roleTxt(segIn(p, S.cab)))}${S.lang === "en" ? `<br><span class="muted">${esc(segIn(p, S.cab).role)}</span>` : ""}${p.portfolio && p.portfolio.includes(" > ") ? `<br><span class="muted">the ministry was renamed: ${esc(p.portfolio)}</span>` : ""}</span>` +
    `<span class="k">Held</span><span>${dstr(p.start)} to ${dstr(p.end)} <span class="muted">(${esc(p.startEvent)}; ${esc(p.endEvent)}${p.movedTo ? ": " + esc(movedTxt(p, "movedTo")) : ""})</span></span>` +
    (p.segments.length > 1 ? `<span class="k">Appointments</span><span>${p.segments.map(s => `${dstr(s.start)} (${esc(s.cabinet)}${s.startAct ? ", " + (actUrl(s.startAct) ? `<a href="${esc(actUrl(s.startAct))}" target="_top">${esc(s.startAct)}</a>` : esc(s.startAct)) : ""})`).join("<br>")}<br><span class="muted">One continuous tenure: the minister was re-appointed to the same post each time, so these are not counted as leaving and returning.</span></span>` : "") +
    `<span class="k">Start confirmed by</span><span>${statusBadge(p.startStatus, p.startAct)}${p.startNote && !String(p.startStatus || "").startsWith("confirmed") ? `<br><span class="muted">Polish Wikipedia cites: ${esc(p.startNote)}</span>` : ""}</span>` +
    `<span class="k">End confirmed by</span><span>${p.end ? statusBadge(p.endStatus, p.endAct) + (p.endNote && !String(p.endStatus || "").startsWith("confirmed") ? `<br><span class="muted">Polish Wikipedia cites: ${esc(p.endNote)}</span>` : "") : "still in office"}</span>` +
    `<span class="k">Party</span><span>${esc(p.party)} <span class="muted">(${esc(p.partySource)})</span></span>` +
    `<span class="k">Born</span><span>${p.birth ? dstr(p.birth) : "–"}${p.ageAtStart ? `, ${Math.round(p.ageAtStart)} at appointment` : ""}${p.gender ? ` · ${esc(p.gender)}` : ""}</span>` +
    `<span class="k">Records</span><span>${p.qid ? `<a href="https://www.wikidata.org/wiki/${esc(p.qid)}" target="_top">Wikidata ${esc(p.qid)}</a>` : "–"}</span>` +
    `<span class="k">Career</span><span><a href="#" data-q="${esc(p.qid)}">Open this minister\u2019s whole career</a></span>` + (others.length ? `<span class="k">Also in these governments</span><span>${others.map(o => `${esc(roleTxt(o.segments[o.segments.length - 1]))} (${dstr(o.start).slice(-4)}–${o.end ? o.end.slice(0, 4) : ""})`).join("; ")}</span>` : "") + `</div>`;
  el_.querySelectorAll("a[data-q]").forEach(l => l.addEventListener("click", ev => { ev.preventDefault(); S.person = l.dataset.q; S.v = "people"; render(); }));
}
function changeEvents(c) {
  const cs = day(c.start), ce = c.end ? day(c.end) : 1e9;
  const mid = d => d != null && d > cs + 31 && d < ce - 3;
  const ev = []; let reapp = 0;
  cabPosts(c.id).forEach(p => {
    if (mid(p.s)) {
      if (p.startEvent === "joined government") ev.push({d: p.start, k: "joins", p, txt: `<b>${esc(p.person)}</b> joins as ${esc(portfolioLabel(p))}`});
      else if (p.startEvent === "moved from another post") ev.push({d: p.start, k: "moves", p, txt: `<b>${esc(p.person)}</b> moves from ${esc(movedTxt(p, "movedFrom"))} to ${esc(portfolioLabel(p))}`});
      else ev.push({d: p.start, k: "adds", p, txt: `<b>${esc(p.person)}</b> takes on ${esc(portfolioLabel(p))} as well`});
    }
    if (p.e && mid(p.e) && p.endEvent === "left government") ev.push({d: p.end, k: "leaves", p, txt: `<b>${esc(p.person)}</b> leaves the government <span class="muted">(was ${esc(portfolioLabel(p))})</span>`});
    reapp += (p.reappointedOn || []).filter(d => mid(day(d))).length;
  });
  ev.sort((a, b) => a.d.localeCompare(b.d) || a.k.localeCompare(b.k));
  ev.reapp = reapp;
  return ev;
}
function drawChanges(c) {
  const ev = changeEvents(c), reapp = ev.reapp;
  const byDate = new Map(); ev.forEach(e => { if (!byDate.has(e.d)) byDate.set(e.d, []); byDate.get(e.d).push(e); });
  $("changes").innerHTML = `<h3>Changes during the term</h3>` + (byDate.size ? `<table class="t"><tr><th>Date</th><th>Changes</th></tr>` +
    [...byDate.entries()].map(([d, es]) => `<tr id="chg-${d}"><td style="white-space:nowrap">${dstr(d)}</td><td>${es.map(e => e.txt).join("<br>")}</td></tr>`).join("") + `</table>` : `<p class="help">No changes after the first month.</p>`) +
    `<p class="help">Ministers who carried on, including those re-appointed to the same post or whose ministry was renamed${reapp ? ` (${reapp} such re-appointments in this term)` : ""}, are not listed as leaving or joining.</p>`;
}
const portfolioLabel = p => roleTxt(segIn(p, S.cab));

// ---- Ministries -----------------------------------------------------------------------------------
function legalTxt(n) {
  const l = n.legal; if (!l) return "";
  const arr = x => Array.isArray(x) ? x : (x == null ? [] : [x]);
  const link = a => a.url ? `<a href="${esc(a.url)}" target="_blank" rel="noopener" title="${esc(a.title || "")}">${esc(a.act)}</a>` : esc(a.act);
  const sp = arr(l.spans), many = sp.length > 1;
  const main = sp.map(s => `<br><span>${many ? s.from.slice(0, 4) + ": " : ""}${s.act ? `Scope regulation${s.status.startsWith("only") ? " (amending only)" : ""}: ${link(s)}, ${dstr(s.date)}` : "No scope regulation found in the Sejm's legal database"}</span>`).join("");
  const am = arr(l.amendments).slice(0, 4).map(x => `${link(x)} (${dstr(x.date)})`).join("; ");
  return main + (am ? `<br><span>Amendments to the act on departments (within two years) that name it: ${am}</span>` : "");
}
const spansOf = n => Array.isArray(n.spans) ? n.spans : (n.spans ? [n.spans] : [{from: n.first, to: n.last >= "2026-09" ? null : n.last}]);
const nameOf = n => S.lang === "en" ? (n.nameEn || n.name) : "Minister " + n.name;
function legalBlock(names) {
  const arr = x => Array.isArray(x) ? x : (x == null ? [] : [x]);
  let regs = 0, ams = 0;
  names.forEach(n => { arr(n.legal && n.legal.spans).forEach(s => { if (s.act) regs++; }); ams += arr(n.legal && n.legal.amendments).length; });
  const body = names.map(n => `<div class="lg"><b>${esc(nameOf(n))}</b>${legalTxt(n)}</div>`).join("");
  return `<details class="legal"><summary>Legal references${regs || ams ? ` (${regs} regulation${regs === 1 ? "" : "s"}${ams ? `, ${ams} amendment${ams === 1 ? "" : "s"}` : ""})` : ": none found in the database"}</summary>${body}</details>`;
}
function drawMinistries() {
  const ms = D.ministries.filter(m => S.type === "all" || m.type === S.type).sort((a, b) => a.label.localeCompare(b.label));
  $("lineages").innerHTML = ms.map(m => {
    const names = m.names.slice().sort((a, b) => a.first.localeCompare(b.first));
    const from = n => n.alsoFrom ? n.alsoFrom.split(";").map(a => (D.ministries.find(x => x.id === a) || {label: a}).label).join(", ") : "";
    return `<div class="lin${S.lin === m.id ? " on" : ""}"><h4><button type="button" data-l="${esc(m.id)}">${esc(m.label)}</button> <span class="badge">${esc(m.type)}</span></h4><div class="names">` +
      names.map(n => `<div><span class="yr">${spansOf(n).map(s => s.from.slice(0, 4) + "–" + (s.to ? s.to.slice(0, 4) : "")).join(", ")}</span><span>${esc(nameOf(n))}${n.roleType !== "minister" ? ` <span class="muted">(${esc(n.roleType)})</span>` : ""}` +
        `${n.alsoFrom ? ` <span class="muted">· draws on ${esc(from(n))}</span>` : ""}${n.note ? `<br><span class="muted">${esc(n.note)}</span>` : ""}</span></div>`).join("") + `</div>` + legalBlock(names) + `</div>`;
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
  box.innerHTML = `<h3>${esc(m.label)}: who held it</h3><p class="help">Every minister of this lineage, in order. Alternate rows only keep neighbouring bars apart. The names are joined into one lineage by my reading of the changes, confirmed by Ben Stanley. Legal references are the Prime Minister\u2019s or Council of Ministers\u2019 regulation on the ministry\u2019s scope of action and amendments to the act on departments of government administration found in the Sejm\u2019s database nearest in date; they show the legal setting, not that each act created the ministry.</p>${svg}` +
    `<table class="t"><tr><th>From</th><th>To</th><th>Minister</th><th>Ministry as named</th><th>Party</th><th>Government(s)</th></tr>` +
    ts.map(p => `<tr><td style="white-space:nowrap">${dstr(p.start)}</td><td style="white-space:nowrap">${dstr(p.end)}</td><td><a href="#" data-q="${esc(p.qid)}">${esc(p.person)}</a></td><td>${esc(S.lang === "en" ? p.segments.map(s => s.roleEn).filter((v, i, a) => a.indexOf(v) === i).join(" > ") : p.portfolio)}</td><td><span class="dot" style="background:${p.colour}"></span> ${esc(p.party)}</td><td>${p.cabinets.map(cabPm).join(", ")}</td></tr>`).join("") + `</table>`;
  box.querySelectorAll("a[data-q]").forEach(l => l.addEventListener("click", ev => { ev.preventDefault(); S.person = l.dataset.q; S.v = "people"; render(); }));
}

// Time in government (spells joined when the gap is three days or less) and first-appointment age, per person
function personStats() {
  if (D.pstats) return D.pstats;
  const m = new Map(); D.posts.forEach(p => { if (p.qid) { if (!m.has(p.qid)) m.set(p.qid, []); m.get(p.qid).push(p); } });
  return D.pstats = [...m.entries()].map(([qid, ts]) => {
    ts.sort((x, y) => x.s - y.s);
    const gov = []; ts.forEach(t => { const last = gov[gov.length - 1]; if (last && t.s <= last.e + 3) last.e = Math.max(last.e, t.e == null ? 1e9 : t.e); else gov.push({s: t.s, e: t.e == null ? 1e9 : t.e}); });
    return {qid, name: ts[0].person, years: gov.reduce((a, g) => a + (Math.min(g.e, day("2026-10-01")) - g.s) / 365.25, 0), posts: ts.length, age: ts[0].ageAtStart, first: ts[0].start};
  });
}
function leaderboard() {
  const ps = personStats(), top = (arr, f, n = 8) => `<ol>${arr.slice(0, n).map(p => `<li><a href="#" data-q="${esc(p.qid)}">${esc(p.name)}</a> <span>${f(p)}</span></li>`).join("")}</ol>`;
  const aged = ps.filter(p => isNum(p.age) && p.age > 0);
  return `<div class="card" style="grid-column:1/-1"><h3>Where to start</h3><p class="help">Type a name above, open a minister from a cabinet or a ministry, or pick one of these.</p><div class="lb">` +
    `<div><h4>Longest in government</h4>${top(ps.slice().sort((x, y) => y.years - x.years), p => p.years.toFixed(1) + " years")}</div>` +
    `<div><h4>Most posts held</h4>${top(ps.slice().sort((x, y) => y.posts - x.posts || y.years - x.years), p => p.posts + " posts")}</div>` +
    `<div><h4>Youngest at first appointment</h4>${top(aged.slice().sort((x, y) => x.age - y.age), p => Math.round(p.age) + " (" + p.first.slice(0, 4) + ")")}</div>` +
    `<div><h4>Oldest at first appointment</h4>${top(aged.slice().sort((x, y) => y.age - x.age), p => Math.round(p.age) + " (" + p.first.slice(0, 4) + ")")}</div></div></div>`;
}

// ---- People -------------------------------------------------------------------------------------
function drawPeople() {
  const pc = $("pcard"), pt = $("ptable"), svg = $("pgantt"); svg.innerHTML = "";
  const ts = D.posts.filter(p => p.qid === S.person).sort((a, b) => a.s - b.s);
  const none = !ts.length; svg.parentNode.hidden = none; pt.hidden = none;
  if (none) { pc.innerHTML = leaderboard(); pc.querySelectorAll("a[data-q]").forEach(l => l.addEventListener("click", ev => { ev.preventDefault(); S.person = l.dataset.q; render(); })); pt.innerHTML = ""; $("ptitle").textContent = ""; return; }
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
      `<td>${statusBadge(t.startStatus, t.startAct)}${t.end ? "<br>" + statusBadge(t.endStatus, t.endAct) : ""}</td></tr>`).join("") + `</table></div>`;
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
function multiChart(cs, short) {
  const svg = $("s-multi"); svg.innerHTML = "";
  const W = Math.max(600, svg.parentNode.clientWidth - 12), L = 128, gap = 26, T = 34, rh = 17, H = T + cs.length * rh + 8, pw = (W - L - 2 * gap - 8) / 3;
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  const panels = [
    {t: "Women", f: c => c.stats.women / c.stats.people, fmt: v => Math.round(100 * v) + "%", max: .6, col: "#8B0000"},
    {t: "Average age at appointment", f: c => c.stats.meanAge, fmt: v => v.toFixed(1), max: 60, col: "#4B6A9B"},
    {t: "Dates of change a year", f: c => c.turnover && isNum(c.turnover.perYear) ? c.turnover.perYear : null, fmt: v => v.toFixed(1), max: Math.max(...cs.map(c => c.turnover && isNum(c.turnover.perYear) ? c.turnover.perYear : 0)) * 1.05, col: "#6B8F71"}];
  cs.forEach((c, i) => text(svg, L - 6, T + i * rh + 12, short(c), {"text-anchor": "end", fill: SOFT}));
  panels.forEach((p, k) => {
    const x0 = L + k * (pw + gap), X = v => x0 + (pw - 34) * v / p.max;
    text(svg, x0, 16, p.t, {fill: INK, "font-size": 12, "font-weight": 500});
    el("line", {x1: x0, x2: x0, y1: T - 4, y2: H - 6, stroke: BASE}, svg);
    cs.forEach((c, i) => { const v = p.f(c); if (v == null) return; const y = T + i * rh; el("rect", {x: x0, y: y + 2, width: Math.max(1, X(v) - x0), height: rh - 6, fill: p.col, opacity: .85}, svg); text(svg, X(v) + 4, y + 12, p.fmt(v), {fill: SOFT, "font-size": 10.5}); });
  });
}
const END_COL = {"after an election": "#4B6A9B", "resignation": "#B8860B", "forced out by the Sejm": "#8B0000", "never formed": "#B0B0B0", "still in office": "#2F7D4F"};
const cabShort = c => { const n = surname(c.pm), same = D.cabinets.filter(x => surname(x.pm) === n); return same.length > 1 ? n + " " + (same.indexOf(c) + 1) : n; };
function drawCoalitions() {
  const cs = D.cabinets.filter(c => c.durability), formed = cs.filter(c => !c.failed), med = a => { const v = [...a].sort((x, y) => x - y), m = v.length >> 1; return v.length % 2 ? v[m] : (v[m - 1] + v[m]) / 2; };
  const days = formed.map(c => c.durability.days), out = formed.filter(c => c.durability.outlastedTerm === true).length, known = formed.filter(c => c.durability.outlastedTerm != null).length;
  const forced = formed.filter(c => c.durability.endGroup === "forced out by the Sejm").length, minority = formed.filter(c => c.durability.majority === "minority").length;
  const ended = formed.filter(c => !c.durability.ongoing);
  $("co-lead").textContent = D.coalitions.note;
  $("co-sum").innerHTML = [[formed.length, "governments that took office, September 1988 to 2026"], [Math.round(med(days)).toLocaleString("en-GB") + " days", "median time in office (about " + (med(days) / 365.25).toFixed(1) + " years)"],
    [forced, "were forced out by the Sejm: a vote of no confidence or a lost confidence vote"], [ended.filter(c => c.durability.endGroup === "resignation").length, "ended by the government's or the prime minister's resignation"],
    [minority, "had no majority of seats when appointed"], [out + " of " + known, "were still in office when the Sejm term they began in ended"]].map(([a, b]) => `<div><b>${a}</b><span>${esc(b)}</span></div>`).join("");
  $("co-title").textContent = "Each bar runs from a cabinet's appointment to its end. Colour shows how it ended; stripes mark a cabinet without a majority of seats. Shaded bands are Sejm terms. Click a row to open the cabinet.";
  const svg = $("co-tl"); svg.innerHTML = "";
  const narrow = svg.parentNode.clientWidth < 560, W = Math.max(narrow ? 320 : 520, svg.parentNode.clientWidth - 12), L = narrow ? 78 : 96, R = narrow ? 92 : 190, rh = 22, T = 26, H = T + cs.length * rh + 24;
  svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
  const AS = day("2026-10-01"), x0 = day("1988-09-01"), x1 = AS, X = d => L + (d - x0) / (x1 - x0) * (W - L - R);
  const defs = el("defs", {}, svg), pat = el("pattern", {id: "co-mi", width: 5, height: 5, patternUnits: "userSpaceOnUse", patternTransform: "rotate(45)"}, defs); el("rect", {width: 2, height: 5, fill: "#fff", opacity: .75}, pat);
  D.coalitions.terms.forEach((t, i) => { const a = X(Math.max(x0, day(t.from))), b = X(t.to ? day(t.to) : AS); if (i % 2 === 0) el("rect", {x: a, y: T - 6, width: Math.max(0, b - a), height: cs.length * rh + 6, fill: "#F6F6F4"}, svg); if (b - a > 14) text(svg, (a + b) / 2, T - 10, t.term === 0 ? "Contract" : t.term, {"text-anchor": "middle"}); });
  for (let y = 1990; y <= 2025; y += 5) { const x = X(day(y + "-01-01")); el("line", {x1: x, x2: x, y1: T - 6, y2: T + cs.length * rh, stroke: GRID}, svg); text(svg, x, H - 8, y, {"text-anchor": "middle"}); }
  cs.forEach((c, i) => { const u = c.durability, y = T + i * rh, a = X(day(c.start)), b = X(c.end ? day(c.end) : AS), col = END_COL[u.endGroup] || "#999", w = Math.max(2.5, b - a);
    const g = el("g", {style: "cursor:pointer"}, svg); g.addEventListener("click", () => { S.cab = c.id; S.v = "cabinet"; S.post = null; render(); });
    el("rect", {x: L - 4 - 0, y, width: W - L - 4, height: rh, fill: "transparent"}, g);
    text(g, L - 6, y + 14, cabShort(c), {"text-anchor": "end", fill: INK});
    el("rect", {x: a, y: y + 4, width: w, height: rh - 8, rx: 3, fill: col}, g);
    if (u.majority === "minority") el("rect", {x: a, y: y + 4, width: w, height: rh - 8, rx: 3, fill: "url(#co-mi)"}, g);
    const lab = c.failed ? "never formed" : u.days.toLocaleString("en-GB") + " days" + (!narrow && u.shareOfTerm != null ? ` · ${Math.round(100 * u.shareOfTerm)}% of term` : "");
    text(g, Math.min(b + 5, W - R + 6), y + 14, lab, {fill: SOFT, "font-size": 10.5});
    g.appendChild(Object.assign(document.createElementNS(NS, "title"), {textContent: `${c.name}: ${dstr(c.start)} to ${dstr(c.end)}, ${u.days} days; ${u.endLabel}; ${u.majority}`})); });
  $("co-legend").innerHTML = D.coalitions.endGroups.filter(g => cs.some(c => c.durability.endGroup === g.id)).map(g => `<span><i style="display:inline-block;width:14px;height:10px;border-radius:2px;background:${END_COL[g.id]}"></i>${esc(g.label)}</span>`).join("") + `<span><i style="display:inline-block;width:14px;height:10px;border-radius:2px;background:repeating-linear-gradient(45deg,#8a8a8a 0 2px,#d9d9d9 2px 4px)"></i>no majority of seats</span>`;
  $("co-table").innerHTML = `<table><thead><tr><th>Cabinet</th><th>Parties</th><th class="n">Seats at appointment</th><th class="n">Days with a majority</th><th class="n">Fewest seats</th><th class="n">Days</th><th class="n">Share of term</th><th>How it ended</th></tr></thead><tbody>${cs.map(c => { const u = c.durability; return `<tr data-c="${esc(c.id)}"><td>${esc(c.pm)}<br><span class="muted">${dstr(c.start)} to ${dstr(c.end)}</span></td><td>${[].concat(u.parties).map(esc).join(", ")}</td><td class="n">${u.seatsStart == null ? "" : u.seatsStart}${u.majority === "minority" ? " (minority)" : ""}</td><td class="n">${c.failed || !c.majority || c.majority.share == null ? "" : Math.round(100 * c.majority.share) + "%"}</td><td class="n">${c.failed || !c.majority || c.majority.share == null ? "" : c.majority.seatsMin}</td><td class="n">${c.failed ? "" : u.days.toLocaleString("en-GB")}</td><td class="n">${c.failed || u.shareOfTerm == null ? "" : Math.round(100 * u.shareOfTerm) + "%"}</td><td>${esc(u.endLabel)}</td></tr>`; }).join("")}</tbody></table>`;
  $("co-table").querySelectorAll("tr[data-c]").forEach(r => r.addEventListener("click", () => { S.cab = r.dataset.c; S.v = "cabinet"; S.post = null; render(); }));
  drawMajority(); drawPartners(); drawSupport();
}
// the width, margins and time scale shared by the three charts below the table
function frame(svg, L, R) { svg.innerHTML = ""; const W = Math.max(320, svg.parentNode.clientWidth - 12); return {W, L, R}; }
const surnameOf = c => cabShort(c);
function bandsFor(svg, X, T, H, narrow) {
  D.cabinets.filter(c => c.durability && !c.failed).forEach((c, i) => { const a = X(day(c.start)), b = X(c.end ? day(c.end) : day("2026-10-01")); if (i % 2 === 0) el("rect", {x: a, y: T, width: Math.max(0, b - a), height: H - T - 22, fill: "#F6F6F4"}, svg);
    if (!narrow && b - a > 46) text(svg, (a + b) / 2, T - 4, surnameOf(c), {"text-anchor": "middle", "font-size": 9.5}); });
}
function drawMajority() {
  const svg = $("mj-svg"), rows = D.coalitions.majority.series.rows, f = frame(svg, 44, 12), narrow = f.W < 560, T = 20, H = 270; svg.setAttribute("viewBox", `0 0 ${f.W} ${H}`);
  const x0 = day("1989-06-01"), x1 = day("2026-10-01"), X = d => f.L + (d - x0) / (x1 - x0) * (f.W - f.L - f.R), maj = D.coalitions.majority.threshold, lo = -190, hi = 210, Y = m => T + (hi - m) / (hi - lo) * (H - T - 22);
  $("mj-lead").textContent = "How far the governing parties' seats were above or below the 231 that make a majority of the Sejm's 460, on each day. " + D.coalitions.majority.note;
  bandsFor(svg, X, T, H, narrow);
  [-150, -100, -50, 0, 50, 100, 150, 200].forEach(v => { el("line", {x1: f.L, x2: f.W - f.R, y1: Y(v), y2: Y(v), stroke: v === 0 ? INK : GRID, "stroke-dasharray": v === 0 ? "4 3" : null}, svg); text(svg, f.L - 5, Y(v) + 4, (v > 0 ? "+" : "") + v, {"text-anchor": "end"}); });
  for (let y = 1990; y <= 2025; y += 5) text(svg, X(day(y + "-01-01")), H - 6, y, {"text-anchor": "middle"});
  rows.forEach((r, i) => { const d0 = day(r[0]), d1 = i + 1 < rows.length ? day(rows[i + 1][0]) : x1, m = r[1] - maj, y = Y(Math.max(m, 0)), h = Math.abs(Y(m) - Y(0));
    el("rect", {x: X(d0), y, width: Math.max(1, X(d1) - X(d0)), height: Math.max(0.6, h), fill: m >= 0 ? "#2F7D4F" : "#8B0000", opacity: .85}, svg).appendChild(Object.assign(document.createElementNS(NS, "title"), {textContent: `${dstr(r[0])}: ${r[1]} seats, ${m >= 0 ? m + " above" : -m + " below"} a majority (${r[2]})`})); });
  text(svg, f.L + 6, Y(0) - 4, "231 seats: a majority", {fill: INK});
}
function drawPartners() {
  const sel = $("pa-sel"); if (!sel.options.length) { sel.innerHTML = D.cabinets.filter(c => c.durability && !c.failed).map(c => `<option value="${esc(c.id)}">${esc(cabShort(c))}: ${esc(c.name)}</option>`).join(""); sel.addEventListener("change", () => { S.cab = sel.value; drawPartners(); }); }
  if (!D.cabinets.find(c => c.id === S.cab && !c.failed)) { sel.value = D.cabinets[D.cabinets.length - 1].id; } else sel.value = S.cab;
  const c = D.cabinets.find(x => x.id === sel.value), ps = (c.partners || []).slice().sort((a, b) => a.from.localeCompare(b.from) || b.to.localeCompare(a.to));
  const svg = $("pa-svg"), f = frame(svg, 110, 140), rh = 24, T = 22, H = T + ps.length * rh + 24; svg.setAttribute("viewBox", `0 0 ${f.W} ${H}`);
  const x0 = day(ps.length ? ps.reduce((m, p) => p.from < m ? p.from : m, ps[0].from) : c.start), x1 = Math.max(day(ps.length ? ps.reduce((m, p) => p.to > m ? p.to : m, ps[0].to) : c.start) , x0 + 30), X = d => f.L + (d - x0) / (x1 - x0) * (f.W - f.L - f.R);
  $("pa-title").textContent = `${c.name}: ${dstr(c.start)} to ${dstr(c.end)}. Each bar is a governing party's time in the coalition; a grey bar is a party giving external support. A marker ▸ means the party joined after the cabinet took office, ◂ that it left before it ended; a rename or merger (one party's bar ending as another's begins) is not a departure.`;
  const span = (x1 - x0) / 365.25, step = span > 8 ? 2 : 1;
  for (let y = Math.ceil(new Date(x0 * 864e5).getUTCFullYear() / step) * step; y <= new Date(x1 * 864e5).getUTCFullYear(); y += step) { const x = X(day(y + "-01-01")); if (x < f.L || x > f.W - f.R) continue; el("line", {x1: x, x2: x, y1: T - 4, y2: H - 20, stroke: GRID}, svg); text(svg, x, H - 6, y, {"text-anchor": "middle"}); }
  if (span <= 1) { [x0, x1].forEach((d, i) => text(svg, X(d), H - 6, dstr(new Date(d * 864e5).toISOString().slice(0, 10)), {"text-anchor": i ? "end" : "start"})); }
  ps.forEach((p, i) => { const y = T + i * rh, a = X(day(p.from)), b = X(day(p.to));
    text(svg, f.L - 6, y + 15, trunc(p.party, 16), {"text-anchor": "end", fill: INK}); el("rect", {x: a, y: y + 4, width: Math.max(3, b - a), height: rh - 9, rx: 3, fill: p.status === "external_support" ? "#B0B0B0" : "#4B6A9B"}, svg).appendChild(Object.assign(document.createElementNS(NS, "title"), {textContent: `${p.name || p.party}: ${dstr(p.from)} to ${dstr(p.to)}`}));
    if (p.joinedLate) text(svg, a - 3, y + 15, "▸", {"text-anchor": "end", fill: "#8B0000", "font-size": 13}); if (p.leftEarly) text(svg, b + 3, y + 15, "◂", {fill: "#8B0000", "font-size": 13});
    text(svg, Math.min(b + (p.leftEarly ? 16 : 5), f.W - 90), y + 15, `${dstr(p.from)} – ${dstr(p.to)}`, {fill: SOFT, "font-size": 10}); });
  const m = c.majority, mb = (c.ministers || []).map(x => `${esc(x.party)} ${x.people}`).join("; ");
  $("pa-card").innerHTML = `<div class="kv"><span class="k">Parties</span><span>${ps.length ? [...new Set(ps.map(p => p.party))].map(esc).join(", ") : "–"}</span>` +
    (m ? `<span class="k">Seats</span><span>${m.seatsMin} to ${m.seatsMax} over the cabinet's life (the lowest ${m.seatsMin >= D.coalitions.majority.threshold ? "still a majority" : "below the 231 for a majority"}); a majority on ${Math.round(100 * m.share)}% of days${m.firstBelow && c.durability.majority === "majority" ? `; first fell below 231 on ${dstr(m.firstBelow)}` : ""}</span>` : "") +
    (mb ? `<span class="k">People who held a post, by party at appointment</span><span>${mb}</span>` : "") + `</div>`;
}
function drawSupport() {
  const svg = $("su-svg"), rows = D.coalitions.support.series.rows, f = frame(svg, 40, 12), narrow = f.W < 560, T = 24, H = 290; svg.setAttribute("viewBox", `0 0 ${f.W} ${H}`);
  $("su-lead").textContent = D.coalitions.support.note;
  const x0 = day("1990-01-01"), x1 = day("2026-10-01"), lo = 1, hi = 9, X = d => f.L + (d - x0) / (x1 - x0) * (f.W - f.L - f.R), Y = v => T + (hi - v) / (hi - lo) * (H - T - 22);
  bandsFor(svg, X, T, H, narrow);
  for (let v = 2; v <= 8; v += 2) { el("line", {x1: f.L, x2: f.W - f.R, y1: Y(v), y2: Y(v), stroke: GRID}, svg); text(svg, f.L - 5, Y(v) + 4, v, {"text-anchor": "end"}); }
  for (let y = 1990; y <= 2025; y += 5) text(svg, X(day(y + "-01-01")), H - 6, y, {"text-anchor": "middle"});
  const pts = k => rows.filter(r => r[k] != null).map(r => X(day(r[0])).toFixed(1) + "," + Y(r[k]).toFixed(1)).join(" ");
  const band = rows.map(r => X(day(r[0])).toFixed(1) + "," + Y(r[3]).toFixed(1)).concat(rows.slice().reverse().map(r => X(day(r[0])).toFixed(1) + "," + Y(r[2]).toFixed(1))).join(" ");
  el("polygon", {points: band, fill: "#141414", opacity: .09}, svg);
  el("polyline", {points: pts(5), fill: "none", stroke: "#4B6A9B", "stroke-width": 1.6, "stroke-dasharray": "4 3"}, svg);
  el("polyline", {points: pts(4), fill: "none", stroke: "#8B0000", "stroke-width": 1.6}, svg);
  el("polyline", {points: pts(1), fill: "none", stroke: INK, "stroke-width": 2.2}, svg);
  $("su-leg").innerHTML = `<span><i style="display:inline-block;width:18px;border-top:2.4px solid ${INK}"></i>All Poles (80% range shaded)</span><span><i style="display:inline-block;width:18px;border-top:2px solid #8B0000"></i>Voters of the governing parties</span><span><i style="display:inline-block;width:18px;border-top:2px dashed #4B6A9B"></i>Voters of the other parties</span>`;
  const cs = D.cabinets.filter(c => c.support && c.support.startLevel != null);
  const sg = v => v == null ? "" : Math.abs(v) < 0.05 ? "0.0" : (v > 0 ? "+" : "") + v.toFixed(1);
  $("su-table").innerHTML = `<table><thead><tr><th>Cabinet</th><th class="n">Months</th><th class="n">First three months</th><th class="n">Last three months</th><th class="n">Change</th><th class="n">Governing parties' voters, first to last</th><th class="n">Others' voters, first to last</th></tr></thead><tbody>${cs.map(c => { const s = c.support; return `<tr data-c="${esc(c.id)}"><td>${esc(c.pm)}<br><span class="muted">${dstr(c.start)} to ${dstr(c.end)}</span></td><td class="n">${s.months}</td><td class="n">${s.startLevel.toFixed(1)}</td><td class="n">${s.endLevel.toFixed(1)}</td><td class="n">${sg(s.change)}</td><td class="n">${s.govVotersStart == null ? "" : s.govVotersStart.toFixed(1) + " to " + s.govVotersEnd.toFixed(1)}</td><td class="n">${s.otherVotersStart == null ? "" : s.otherVotersStart.toFixed(1) + " to " + s.otherVotersEnd.toFixed(1)}</td></tr>`; }).join("")}</tbody></table>`;
  $("su-table").querySelectorAll("tr[data-c]").forEach(r => r.addEventListener("click", () => { S.cab = r.dataset.c; S.v = "cabinet"; S.post = null; render(); }));
}
function drawStats() {
  const cs = D.cabinets.filter(c => c.stats && isNum(c.stats.meanAge));
  const ROM = ["", "I", "II", "III"], short = c => { const k = /_(\d)$/.exec(c.id); return c.pm.split(" ").slice(-1)[0] + (k ? " " + ROM[+k[1]] : "") + " " + c.start.slice(0, 4); };
  const wide = $("v-stats").clientWidth >= 640;
  $("s-multi-box").hidden = !wide; $("s-sep").hidden = wide; $("bx-turn").hidden = wide; $("s-sep2").style.gridTemplateColumns = wide ? "minmax(0,1fr)" : "";
  if (wide) multiChart(cs, short);
  else {
    barChart("s-women", cs.map(c => ({label: short(c), v: c.stats.women / c.stats.people})), {col: "#8B0000", fmt: v => Math.round(100 * v) + "%", max: 0.6});
    barChart("s-age", cs.map(c => ({label: short(c), v: c.stats.meanAge})), {col: "#4B6A9B", fmt: v => v.toFixed(1), max: 60});
    barChart("s-turn", cs.filter(c => c.turnover && isNum(c.turnover.perYear)).map(c => ({label: short(c), v: c.turnover.perYear})), {col: "#6B8F71", fmt: v => v.toFixed(1)});
  }
  // parties: the eight with most posts, then all other parties, then non-party and not recorded; shaded by share of the row
  const parties = {}; D.stats.partyPortfolios.forEach(r => { (parties[r.party_group] = parties[r.party_group] || {})[r.portfolio_type] = r.posts; });
  const types = [...new Set(D.stats.partyPortfolios.map(r => r.portfolio_type))].sort();
  const sum = o => Object.values(o).reduce((x, y) => x + y, 0), SPECIAL = ["non-party", "not recorded"];
  const named = Object.entries(parties).filter(([p]) => !SPECIAL.includes(p)).map(([p, o]) => ({p, o, tot: sum(o)})).sort((x, y) => y.tot - x.tot);
  const other = {}; named.slice(8).forEach(r => types.forEach(t => { other[t] = (other[t] || 0) + (r.o[t] || 0); }));
  const rows = [...named.slice(0, 8), ...(named.length > 8 ? [{p: `${named.length - 8} other parties`, o: other, tot: sum(other)}] : []), ...SPECIAL.filter(p => parties[p]).map(p => ({p: p === "non-party" ? "Non-party" : "Not recorded", o: parties[p], tot: sum(parties[p])}))];
  const mx = Math.max(...rows.flatMap(r => types.map(t => r.o[t] || 0)));
  $("s-party").innerHTML = `<div style="overflow-x:auto"><table class="t heat"><tr><th>Party</th>${types.map(t => `<th class="n">${esc(t[0].toUpperCase() + t.slice(1))}</th>`).join("")}<th class="n">All</th></tr>` +
    rows.map(r => `<tr><td>${esc(r.p)}</td>${types.map(t => { const v = r.o[t] || 0; return `<td class="n" style="${v ? `background:rgba(139,0,0,${(0.08 + 0.55 * v / mx).toFixed(2)});color:${v / mx > 0.55 ? "#fff" : INK}` : ""}">${v || ""}</td>`; }).join("")}<td class="n">${r.tot}</td></tr>`).join("") + `</table></div>`;
}

fetch("governments.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).then(init)
  .catch(e => { $("asof").innerHTML = '<div class="loading">The data could not be loaded (' + esc(e.message) + ').</div>'; });
