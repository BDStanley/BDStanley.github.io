// Presidents: Poland's presidents since 1989, their governments and Chancellery, vetoes, bills, applications to the
// Constitutional Tribunal, appointments and pardons, compared across presidencies. No libraries.
// Data: presidents.json beside this file (Presidents/analysis/90_export.R in the Polish political analyses repo).
(async function () {
  "use strict";
  const PAGE = "Presidents";
  const PAGES = "../../pages/polish-political-analyses/";
  let D;
  try {
    D = await fetch("presidents.json").then(r => { if (!r.ok) throw new Error(r.status); return r.json(); });
  } catch (e) {
    document.body.insertAdjacentHTML("afterbegin", '<p style="padding:16px;color:#8A5A00">The data could not be loaded. Please try again later.</p>');
    throw e;
  }

  // ---- helpers ----
  const $ = s => document.querySelector(s);
  const esc = s => String(s == null ? "" : s).replace(/[&<>"']/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
  const tab = x => x.rows.map(r => Object.fromEntries(x.columns.map((c, i) => [c, r[i]])));
  const MON = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
  const T = s => new Date(s + "T00:00:00Z").getTime();
  const fmtD = s => { if (!s) return ""; const d = new Date(T(s)); return `${d.getUTCDate()} ${MON[d.getUTCMonth()]} ${d.getUTCFullYear()}`; };
  const yearOf = s => +String(s).slice(0, 4);
  const num = (x, dp = 0) => x == null || isNaN(x) ? "–" : Number(x).toLocaleString("en-GB", { minimumFractionDigits: dp, maximumFractionDigits: dp });
  const pct = (x, dp = 0) => x == null || isNaN(x) ? "–" : (100 * x).toFixed(dp) + "%";
  const NS = "http://www.w3.org/2000/svg";
  const ext = (href, text) => `<a href="${esc(href)}" target="_blank" rel="noopener">${esc(text)}</a>`;
  const page = (p, hash, text) => `<a href="${PAGES}${p}.html${hash ? "#" + hash : ""}" target="_top">${esc(text)}</a>`;
  const AS_OF = D.meta.as_of, SEJM_END = D.meta.sejm_data_end;
  $("#asof").textContent = fmtD(AS_OF);
  $("#sejmend").textContent = fmtD(SEJM_END);

  const P = D.presidents;
  const PM = P.filter(p => !p.acting);
  const byId = Object.fromEntries(P.map(p => [p.id, p]));
  const nameOf = id => byId[id] ? byId[id].short : (id || "–");
  const yearsIn = p => ((p.end ? T(p.end) : T(AS_OF)) - T(p.start)) / (365.25 * 864e5);
  const span = p => `${yearOf(p.start)}–${p.end ? yearOf(p.end) : ""}`;

  const vetoes = tab(D.vetoes), bills = tab(D.bills), apps = tab(D.tribunal), gazette = tab(D.gazette), keyActs = tab(D.key_acts);
  const caps = Object.fromEntries((D.meta.caps || []).map(c => [c.code, c.en]));

  const REL = { "own party in government": "#4B6A88", cohabitation: "#B45309", "no party": "#C9C9C4" };
  const RELNAME = { "own party in government": "Own party in government", cohabitation: "Cohabitation", "no party": "No party of his own" };
  const VOUT = { sustained: "#8B0000", overridden: "#4B6A88", "not considered": "#9CA3AF", "pending or not considered": "#D5D5D5" };
  const VOUTNAME = { sustained: "Veto stood (Sejm failed to override)", overridden: "Overridden by the Sejm", "not considered": "Never voted on", "pending or not considered": "Not voted on yet (current term)" };
  const BOUT = { enacted: "#4B6A88", "passed by the Sejm, not in force": "#8A5A00", rejected: "#8B0000", withdrawn: "#B0B0B0", "lapsed at the end of the term": "#9CA3AF", pending: "#D5D5D5" };
  const TKIND = { "before signature": "#8B0000", "law in force": "#4B6A88", "other normative act": "#8A5A00", "dispute over powers": "#6B7280", "political party": "#B45309", other: "#C9C9C4" };
  const TRES = { unconstitutional: "#8B0000", "partly unconstitutional": "#C77C7C", constitutional: "#4B6A88", discontinued: "#9CA3AF", pending: "#D5D5D5", other: "#E5E5E5" };
  const ENDS = { term_shortened: "Term shortened by constitutional amendment", lost_election: "Lost his bid for re-election", term_limit: "Served the two-term limit", died: "Died in office", acting_ended: "Acting presidency ended" };
  const SPONSOR = { government: "Government", MPs: "MPs", president: "President", Senate: "Senate", committee: "Sejm committee", citizens: "Citizens" };
  // A source string as a link: its first URL, labelled by the Wikipedia article or the site
  const srcLink = src => {
    const m = String(src || "").match(/https?:\/\/[^\s,)]+/); if (!m) return esc(src);
    const u = m[0], wiki = u.match(/wikipedia\.org\/wiki\/([^#?]+)/);
    const lab = wiki ? "Wikipedia: " + decodeURIComponent(wiki[1]).replace(/_/g, " ") : u.replace(/^https?:\/\/(www\.)?/, "").split("/")[0];
    const rest = String(src).slice(String(src).indexOf(u) + u.length).replace(/https?:\/\/\S+/g, "").replace(/^[\s,;]+/, "");
    return ext(u, lab) + (rest ? ` <span class="muted">${esc(rest.slice(0, 120))}</span>` : "");
  };
  const cap1 = t => String(t).charAt(0).toUpperCase() + String(t).slice(1);
  const legendHtml = obj => Object.entries(obj).map(([k, c]) => `<span><i class="sw" style="background:${c}"></i>${esc(cap1(k))}</span>`).join("");

  // ---- state and address ----
  const S = { v: "pres", p: "nawrocki", m: "Vetoes", r: "total", k: "vetoes", lp: "", lo: "", lq: "", tp: "", tb: "kind", gk: "ambassadors", kk: "", kp: "", kq: "", pr: "total" };
  // The address lives on the page that embeds the app (same origin), as on the other analysis pages
  const hashTarget = () => { try { if (window.top !== window && window.top.location.host === location.host) return window.top; } catch (e) { /* cross-origin */ } return window; };
  (() => {
    let hs = ""; try { hs = hashTarget().location.hash.slice(1); } catch (e) { hs = location.hash.slice(1); }
    if (!hs) hs = location.hash.slice(1);
    const h = new URLSearchParams(hs);
    const views = ["pres", "cmp", "leg", "tk", "app", "par"];
    if (views.includes(h.get("v"))) S.v = h.get("v");
    if (h.get("p") && byId[h.get("p")]) { S.p = h.get("p"); S.lp = S.tp = S.kp = h.get("p"); }
    if (h.get("m")) S.m = h.get("m");
    if (h.get("r") === "year") S.r = "year";
    if (["vetoes", "bills"].includes(h.get("k"))) S.k = h.get("k");
    if (h.get("gk")) S.gk = h.get("gk");
  })();
  function writeHash() {
    const h = new URLSearchParams({ v: S.v });
    if (S.v === "pres") h.set("p", S.p);
    if (S.v === "cmp") { h.set("m", S.m); if (S.r === "year") h.set("r", "year"); }
    if (S.v === "leg") { h.set("k", S.k); if (S.lp) h.set("p", S.lp); }
    if (S.v === "tk" && S.tp) h.set("p", S.tp);
    if (S.v === "app") { h.set("gk", S.gk); }
    const t = hashTarget();
    try { t.history.replaceState(null, "", t.location.pathname + t.location.search + "#" + h.toString()); } catch (e) { /* ignore */ }
  }

  // ---- tooltips ----
  function tipFor(box, tip) {
    box.addEventListener("mousemove", e => {
      const t = e.target.closest && e.target.closest("[data-tip]");
      if (!t) { tip.hidden = true; return; }
      tip.innerHTML = t.getAttribute("data-tip"); tip.hidden = false;
      const r = box.getBoundingClientRect();
      let x = e.clientX - r.left + box.scrollLeft + 14, y = e.clientY - r.top + 14;
      if (x + tip.offsetWidth > box.scrollLeft + r.width - 4) x = e.clientX - r.left + box.scrollLeft - tip.offsetWidth - 14;
      tip.style.left = Math.max(4, x) + "px"; tip.style.top = y + "px";
    });
    box.addEventListener("mouseleave", () => { tip.hidden = true; });
  }
  document.querySelectorAll(".chartbox").forEach(b => { const t = b.querySelector(".tip"); if (t) tipFor(b, t); });

  // ---- shared chart pieces ----
  // Axis ticks on round steps (1, 2 or 5 times a power of ten), about four of them
  const niceStep = v => { const p = Math.pow(10, Math.floor(Math.log10(v))); const m = v / p; return (m <= 1 ? 1 : m <= 2 ? 2 : m <= 5 ? 5 : 10) * p; };
  const ticks = (max, n = 4) => { const st = niceStep(Math.max(max, 1e-9) / n); const k = Math.max(1, Math.ceil(max / st - 1e-9)); return Array.from({ length: k + 1 }, (_, i) => +(i * st).toFixed(6)); };
  const niceMax = v => { const t = ticks(v); return t[t.length - 1]; };
  const presAt = t => P.filter(p => !p.acting).find(p => T(p.start) <= t && (!p.end || T(p.end) > t));

  // Stacked bars per calendar year with the presidencies as bands behind them.
  // rows: [{year, key, n, tip}]; keys: order of stacking; shadeTo: years up to this one are shaded (not covered)
  function yearBars(svgEl, { y0, y1, rows, keys, colours, shadeTo = null, shadeLabel = "", height = 250, unit = "" }) {
    const W = 1000, padL = 40, padR = 10, padT = 30, padB = 24, H = height;
    const n = y1 - y0 + 1, bw = (W - padL - padR) / n;
    const totals = {}; rows.forEach(r => { totals[r.year] = (totals[r.year] || 0) + r.n; });
    const max = Math.max(1, ...Object.values(totals));
    const tk = ticks(max), top = tk[tk.length - 1];
    const y = v => H - padB - (H - padT - padB) * v / top;
    const xOf = yr => padL + (yr - y0) * bw;
    const xT = t => padL + ((t - T(y0 + "-01-01")) / (T((y1 + 1) + "-01-01") - T(y0 + "-01-01"))) * (W - padL - padR);
    let h = "", labels = "";
    // presidency bands, named above the plot
    PM.forEach((p, i) => {
      const a = Math.max(xT(T(p.start)), padL), b = Math.min(xT(p.end ? T(p.end) : T(AS_OF)), W - padR);
      if (b <= a) return;
      h += `<rect x="${a}" y="${padT - 18}" width="${b - a}" height="${H - padB - padT + 18}" fill="${i % 2 ? "#F7F7F5" : "#FFFFFF"}"/>`;
      h += `<line x1="${a}" x2="${a}" y1="${padT - 18}" y2="${H - padB}" stroke="#ECECEC"/>`;
      if (b - a > 34) labels += `<text x="${(a + b) / 2}" y="${padT - 6}" text-anchor="middle" class="ax" style="fill:#4B4B4B">${esc(b - a > 60 ? p.short : p.short.replace(/^L\. /, ""))}</text>`;
    });
    if (shadeTo != null && shadeTo >= y0) {
      const b = xOf(Math.min(shadeTo, y1) + 1);
      h += `<rect x="${padL}" y="${padT}" width="${b - padL}" height="${H - padB - padT}" fill="url(#shade)"/>`;
      if (b - padL > 60) h += `<text x="${(padL + b) / 2}" y="${(padT + H - padB) / 2}" text-anchor="middle" class="ax">${esc(shadeLabel)}</text>`;
    }
    h += labels;
    tk.forEach(v => { h += `<line x1="${padL}" x2="${W - padR}" y1="${y(v)}" y2="${y(v)}" stroke="#ECECEC"/><text x="${padL - 6}" y="${y(v) + 4}" text-anchor="end" class="ax">${num(v)}</text>`; });
    const byYear = {};
    rows.forEach(r => { (byYear[r.year] = byYear[r.year] || []).push(r); });
    Object.entries(byYear).forEach(([yr, rs]) => {
      let acc = 0;
      rs.sort((a, b) => keys.indexOf(a.key) - keys.indexOf(b.key)).forEach(r => {
        if (!r.n) return;
        const yt = y(acc + r.n), yb = y(acc); acc += r.n;
        h += `<rect x="${xOf(+yr) + bw * 0.12}" y="${yt}" width="${bw * 0.76}" height="${Math.max(0.5, yb - yt)}" fill="${colours[r.key] || "#9CA3AF"}" data-tip="${esc(r.tip || `<b>${yr}</b><br>${esc(r.key)}: ${num(r.n)}${unit}`)}"/>`;
      });
    });
    for (let yr = y0; yr <= y1; yr++) if ((yr - y0) % (n > 30 ? 5 : n > 15 ? 2 : 1) === 0 || yr === y1) h += `<text x="${xOf(yr) + bw / 2}" y="${H - 8}" text-anchor="middle" class="ax">${yr}</text>`;
    svgEl.setAttribute("viewBox", `0 0 ${W} ${H}`);
    svgEl.innerHTML = `<defs><pattern id="shade" width="6" height="6" patternUnits="userSpaceOnUse" patternTransform="rotate(45)"><rect width="6" height="6" fill="#FAFAFA"/><line x1="0" y1="0" x2="0" y2="6" stroke="#E2E2E2" stroke-width="2"/></pattern></defs>` + h;
  }

  function table(cols, rows, cls = "t") {
    return `<table class="${cls}"><thead><tr>${cols.map(c => `<th class="${c.n ? "n" : ""}">${esc(c.h)}</th>`).join("")}</tr></thead><tbody>${rows.map(r => `<tr${r._cls ? ` class="${r._cls}"` : ""}>${cols.map(c => `<td class="${c.n ? "n" : c.d ? "d" : ""}">${c.f ? c.f(r) : esc(r[c.k])}</td>`).join("")}</tr>`).join("")}</tbody></table>`;
  }
  const presOptions = (all = "All presidents") => `<option value="">${esc(all)}</option>` + PM.map(p => `<option value="${p.id}">${esc(p.short)} (${span(p)})</option>`).join("");
  const csvQ = v => { const s = v == null ? "" : String(v); return /[",\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s; };
  function download(name, text, type = "text/csv") {
    const a = document.createElement("a"); a.href = URL.createObjectURL(new Blob([text], { type })); a.download = name; a.click();
    setTimeout(() => URL.revokeObjectURL(a.href), 2000);
  }
  async function pngOf(svgEl, title, subtitle, source, file) {
    const vb = svgEl.viewBox.baseVal, w = 900, h = w * vb.height / vb.width;
    const clone = await ChartExport.clone(svgEl);
    clone.setAttribute("xmlns", NS); ChartExport.setSize(clone, w, h);
    const img = await new Promise(res => { const i = new Image(); i.onload = () => res(i); i.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(clone)); });
    await ChartExport.fonts();
    const pad = 24, head = 66, foot = 46, W = w + 2 * pad, H = h + head + foot, k = ChartExport.scale(W, H);
    const c = document.createElement("canvas"); c.width = W * k; c.height = H * k;
    const g = c.getContext("2d"); g.scale(k, k); g.fillStyle = "#fff"; g.fillRect(0, 0, W, H);
    g.fillStyle = "#8B0000"; g.font = ChartExport.TITLE; g.fillText(title, pad, 32);
    g.fillStyle = "#4B4B4B"; g.font = ChartExport.SUBTITLE; g.fillText(subtitle, pad, 52);
    g.drawImage(img, pad, head, w, h);
    g.fillStyle = "#767676"; g.font = ChartExport.CREDIT; g.fillText(source, pad, head + h + 18);
    ChartExport.drawCite(g, PAGE, pad, head + h + 36, W - 2 * pad);
    c.toBlob(b => { const a = document.createElement("a"); a.href = URL.createObjectURL(b); a.download = file; a.click(); });
  }

  // ---- views ----
  const RENDER = {};
  function show(v) {
    S.v = v;
    document.querySelectorAll("#views button").forEach(b => b.setAttribute("aria-pressed", b.dataset.v === v));
    document.querySelectorAll(".view").forEach(s => { s.hidden = s.id !== "v-" + v; });
    RENDER[v](); writeHash();
  }
  document.querySelectorAll("#views button").forEach(b => b.addEventListener("click", () => show(b.dataset.v)));
  const go = (v, patch) => { Object.assign(S, patch || {}); syncControls(); show(v); window.scrollTo(0, 0); };

  // ===== Presidents =====
  function timeline() {
    const svgEl = $("#timeline"), W = 1000, padL = 118, padR = 10;
    const t0 = T("1989-06-01"), t1 = T(AS_OF) + 120 * 864e5;
    const x = t => padL + (t - t0) / (t1 - t0) * (W - padL - padR);
    const rows = [["President", 40], ["Government", 18], ["Chancellery head", 20], ["Security Bureau head", 20]];
    let yy = 28; const Y = {}; rows.forEach(([k, hgt]) => { Y[k] = [yy, hgt]; yy += hgt + 10; });
    const H = yy + 6;
    let h = "";
    for (let yr = 1990; yr <= yearOf(AS_OF); yr++) {
      const xx = x(T(yr + "-01-01"));
      h += `<line x1="${xx}" x2="${xx}" y1="18" y2="${H - 6}" stroke="${yr % 5 ? "#F3F3F1" : "#E2E2E2"}"/>`;
      if (yr % 5 === 0) h += `<text x="${xx}" y="13" text-anchor="middle" class="ax">${yr}</text>`;
    }
    rows.forEach(([k, hgt]) => { h += `<text x="${padL - 8}" y="${Y[k][0] + hgt / 2 + 4}" text-anchor="end" class="ax" style="fill:#4B4B4B">${k}</text>`; });
    const end = p => p.end ? T(p.end) : T(AS_OF);
    P.forEach(p => {
      const a = x(T(p.start)), b = x(end(p)), [y0, hg] = Y.President, sel = p.id === S.p || (byId[S.p] && byId[S.p].person === p.person && p.acting);
      const fill = p.colour, ink = SiteUI.inkOn(fill);
      const tip = `<b>${esc(p.name)}</b>${p.acting ? " (acting, as " + esc(p.acting_office) + ")" : ""}<br>${fmtD(p.start)} – ${p.end ? fmtD(p.end) : "in office"}`;
      h += `<rect x="${a}" y="${y0}" width="${Math.max(1.5, b - a - 1)}" height="${hg}" rx="3" fill="${fill}" opacity="${p.acting ? 0.55 : 1}" stroke="${sel && !p.acting ? "#141414" : "none"}" stroke-width="2" data-tip="${esc(tip)}" data-p="${p.id}" style="cursor:pointer"/>`;
      if (!p.acting && b - a > 40) h += `<text x="${(a + b) / 2}" y="${y0 + hg / 2 + 4}" text-anchor="middle" font-size="12" style="fill:${ink};pointer-events:none" font-weight="${sel ? 600 : 400}">${esc(b - a > 70 ? p.short : p.short.replace(/^L\. /, ""))}</text>`;
    });
    D.governments.forEach(g => {
      const a = x(T(g.start)), b = x(g.end ? T(g.end) : T(AS_OF)), [y0, hg] = Y.Government;
      const tip = `<b>${esc(g.pm)}</b> (${esc(g.pm_party || "")})<br>${esc(g.cabinet_name)}<br>${fmtD(g.start)} – ${g.end ? fmtD(g.end) : "today"}<br>President: ${esc(nameOf(g.spell_id))}<br><i>${esc(RELNAME[g.relation])}</i>`;
      h += `<rect x="${a}" y="${y0}" width="${Math.max(1, b - a - 0.6)}" height="${hg}" fill="${REL[g.relation]}" data-tip="${esc(tip)}"/>`;
    });
    [["Chancellery of the President", "Chancellery head"], ["National Security Bureau", "Security Bureau head"]].forEach(([office, row]) => {
      D.heads.filter(hd => hd.office === office).forEach((hd, i) => {
        const a = x(T(hd.start)), b = x(hd.end ? T(hd.end) : T(AS_OF)), [y0, hg] = Y[row];
        const tip = `<b>${esc(hd.name)}</b>${hd.acting ? " (acting)" : ""}<br>${office}<br>${fmtD(hd.start)} – ${hd.end ? fmtD(hd.end) : "today"}${hd.party_at_appointment ? "<br>Party: " + esc(hd.party_at_appointment) : ""}`;
        h += `<rect x="${a}" y="${y0}" width="${Math.max(1, b - a - 0.6)}" height="${hg}" fill="${hd.acting ? "#D5D5D5" : i % 2 ? "#6B7280" : "#9CA3AF"}" data-tip="${esc(tip)}"/>`;
      });
    });
    svgEl.setAttribute("viewBox", `0 0 ${W} ${H}`); svgEl.style.minWidth = "720px";
    svgEl.innerHTML = h;
    svgEl.onclick = e => { const r = e.target.closest("[data-p]"); if (!r) return; const p = byId[r.dataset.p]; S.p = p.acting ? (PM.find(q => q.person === p.person) || p).id : p.id; RENDER.pres(); writeHash(); };
  }
  function presCard() {
    const p = byId[S.p];
    const sameRows = (arr, k = "spell_id") => arr.filter(r => r[k] === p.id);
    const govs = sameRows(D.governments);
    const cmp = Object.fromEntries(D.compare.filter(c => c.spell_id === p.id).map(c => [c.measure, c]));
    const val = m => cmp[m] ? cmp[m].value : null;
    const relDays = {}; govs.forEach(g => { relDays[g.relation] = (relDays[g.relation] || 0) + g.days; });
    const totDays = govs.reduce((s, g) => s + g.days, 0);
    const el = p.elections || [];
    const pardon = D.pardons.find(x => x.spell_id === p.id && x.measure === "pardons_granted" && x.shown === "TRUE");
    const vOff = val("Vetoes"), vSec = val("Vetoes (Chancellery or press total)");
    const vetoStat = p.id === "jaruzelski" || p.id === "walesa" || p.id === "kwasniewski" ? vSec : vOff;
    const stats = p.acting ? "" : `<div class="stats">
      <button data-go="leg" data-k="vetoes"><b>${num(vetoStat)}</b><span>vetoes${p.id === "kwasniewski" ? " (" + num(vOff) + " since Oct 1997 in the Sejm's records)" : p.id === "jaruzelski" || p.id === "walesa" ? " (published total)" : ""}</span></button>
      <button data-go="leg" data-k="bills"><b>${num(val("Bills submitted to the Sejm"))}</b><span>bills submitted${["jaruzelski", "walesa", "kwasniewski"].includes(p.id) ? " (since Oct 1997)" : ""}</span></button>
      <button data-go="tk"><b>${num(val("Applications to the Constitutional Tribunal"))}</b><span>applications to the Tribunal${["jaruzelski", "walesa", "kwasniewski"].includes(p.id) ? " (since 1997)" : ""}</span></button>
      <button data-go="par"><b>${pardon ? num(+pardon.value) : "–"}</b><span>people pardoned${p.id === "nawrocki" ? " (to Aug 2026)" : ""}</span></button>
      <button data-go="app"><b>${num(val("Prime ministers appointed"))}</b><span>prime ministers appointed</span></button>
      <button data-go="cmp"><b>${totDays ? pct((relDays.cohabitation || 0) / totDays) : "–"}</b><span>of the time in cohabitation</span></button>
    </div>`;
    const elTable = el.length ? table([
      { h: "Election", f: r => page("election-results", "e=" + r.election_id, String(r.year)) },
      { h: "Round 1", n: 1, f: r => pct(r.r1_pct, 1) }, { h: "Round 2", n: 1, f: r => r.r2_pct == null ? "–" : pct(r.r2_pct, 1) },
      { h: "Turnout", n: 1, f: r => pct(r.r2_turnout != null ? r.r2_turnout : r.r1_turnout, 1) },
      { h: "Party (PKW)", f: r => esc(r.membership || "") }], el) : `<p class="help">${p.id === "jaruzelski" ? "Elected by the National Assembly (the Sejm and the Senate) on 19 July 1989." : "Not elected: acting president as " + esc(p.acting_office) + "."}</p>`;
    const headTable = office => {
      const pe = p.end ? T(p.end) : T(AS_OF), day = 864e5;
      const rs = D.heads.filter(hd => hd.office === office && T(hd.start) < pe - day && (!hd.end || T(hd.end) > T(p.start) + day));
      return rs.length ? table([
        { h: "Name", f: r => esc(r.name) + (r.acting ? ' <span class="badge">acting</span>' : "") },
        { h: "From", d: 1, f: r => fmtD(r.start) }, { h: "To", d: 1, f: r => r.end ? fmtD(r.end) : "today" },
        ...(office.startsWith("Chancellery") ? [{ h: "Party when appointed", f: r => r.party_at_appointment ? `<span title="${esc(r.party_quote || "")}">${esc(r.party_at_appointment)}</span>` : '<span class="muted">not recorded</span>' }] : [])], rs) : '<p class="help">None.</p>';
    };
    const govTable = govs.length ? table([
      { h: "Prime minister", f: r => page("governments", "c=" + r.cabinet_id + "&v=cabinet", r.pm) + ` <span class="muted">${esc(r.pm_party || "")}</span>` },
      { h: "From", d: 1, f: r => fmtD(r.start) }, { h: "To", d: 1, f: r => r.end ? fmtD(r.end) : "today" },
      { h: "Relation", f: r => `<i class="sw" style="background:${REL[r.relation]}"></i>${esc(RELNAME[r.relation])}` }], govs) : "";
    const campLinks = (p.camp || []).filter(c => c.type === "party").map(c => page("party-genealogy", "pt=" + c.id, c.abbrev || c.id)).join(", ");
    $("#pres-card").innerHTML = `<div class="cards">
      <div class="card">
        <h3>${esc(p.name)}</h3>
        <div class="kv">
          <span class="k">In office</span><span>${fmtD(p.start)} – ${p.end ? fmtD(p.end) : "today"} (${num(yearsIn(p), 1)} years)${p.acting ? ", acting, as " + esc(p.acting_office) : ""}</span>
          ${p.end_reason ? `<span class="k">Left office</span><span>${esc(ENDS[p.end_reason] || p.end_reason)}</span>` : ""}
          ${p.born ? `<span class="k">Born</span><span>${fmtD(p.born)}${p.birthplace ? ", " + esc(p.birthplace) : ""}</span>` : ""}
          ${p.died ? `<span class="k">Died</span><span>${fmtD(p.died)}</span>` : ""}
          ${p.party_before ? `<span class="k">Party when elected</span><span>${esc(p.party_before)}</span>` : ""}
          ${p.membership_in_office ? `<span class="k">Party ties in office</span><span>${esc(p.membership_in_office)}${p.membership_quote ? `<br><span class="quote">„${esc(p.membership_quote)}”</span>` : ""}</span>` : ""}
          ${p.camp_basis ? `<span class="k">Political camp</span><span>${esc(p.camp_basis)}${campLinks ? "<br>" + campLinks : ""}</span>` : ""}
          ${p.note ? `<span class="k">Note</span><span>${esc(p.note)}</span>` : ""}
        </div>
        <h4>Elections</h4>${elTable}
      </div>
      <div class="card">${stats}
        <h4>Governments</h4><div class="tbox short">${govTable}</div>
      </div>
      <div class="card"><h4>Heads of the Chancellery of the President</h4><div class="tbox short">${headTable("Chancellery of the President")}</div>
        <h4>Heads of the National Security Bureau</h4><div class="tbox short">${headTable("National Security Bureau")}</div></div>
    </div>`;
    $("#pres-card").querySelectorAll("[data-go]").forEach(b => b.addEventListener("click", () => {
      const v = b.dataset.go, patch = { lp: p.id, tp: p.id, kp: p.id };
      if (b.dataset.k) patch.k = b.dataset.k;
      if (v === "cmp") patch.m = "Share of time in cohabitation";
      go(v, patch);
    }));
  }
  RENDER.pres = () => {
    $("#pres-legend").innerHTML = `<span>Government:</span>` + legendHtml(Object.fromEntries(Object.entries(REL).map(([k, c]) => [RELNAME[k], c])));
    $("#pres-chips").innerHTML = PM.map(p => `<button aria-pressed="${p.id === S.p}" data-p="${p.id}"><i class="sw" style="background:${p.colour}"></i>${esc(p.short)} <span class="muted">${span(p)}</span></button>`).join("");
    $("#pres-chips").querySelectorAll("button").forEach(b => b.addEventListener("click", () => { S.p = b.dataset.p; RENDER.pres(); writeHash(); }));
    timeline(); presCard();
  };

  // ===== Compare =====
  const GROUPS = { context: "The presidency", legislation: "Vetoes and bills", tribunal: "Constitutional Tribunal", appointments: "Appointments", pardons: "Pardons", chancellery: "Chancellery" };
  const measures = [...new Set(D.compare.map(c => c.measure))];
  const groupOf = m => D.compare.find(c => c.measure === m).group;
  const unitOf = m => D.compare.find(c => c.measure === m).unit;
  // For these, a president the official records cover only in part is shown with the published total instead
  const FALLBACK = { "Vetoes": "Vetoes (Chancellery or press total)", "Bills submitted to the Sejm": "Bills (Chancellery total)" };
  function cmpValue(id, m) {
    let c = D.compare.find(x => x.spell_id === id && x.measure === m), fb = false;
    if (c && !c.complete && FALLBACK[m]) {
      const f = D.compare.find(x => x.spell_id === id && x.measure === FALLBACK[m]);
      if (f && f.complete) { c = f; fb = true; }
    }
    return c ? { ...c, fallback: fb } : null;
  }
  const fmtVal = (c, rate) => !c || c.value == null ? "–" : c.unit === "share" ? pct(c.value) : c.unit === "years" ? num(c.value, 1) : rate === "year" ? num(c.per_year, 1) : num(c.value);
  RENDER.cmp = () => {
    const sel = $("#cmp-m");
    if (!sel.options.length) {
      sel.innerHTML = Object.entries(GROUPS).map(([g, lab]) => `<optgroup label="${esc(lab)}">${measures.filter(m => groupOf(m) === g && !Object.values(FALLBACK).includes(m)).map(m => `<option>${esc(m)}</option>`).join("")}</optgroup>`).join("");
      sel.addEventListener("change", () => { S.m = sel.value; RENDER.cmp(); writeHash(); });
    }
    if (!measures.includes(S.m)) S.m = "Vetoes";
    sel.value = S.m;
    document.querySelectorAll("#cmp-rate button").forEach(b => b.setAttribute("aria-pressed", b.dataset.r === S.r));
    const unit = unitOf(S.m), rate = unit === "count" ? S.r : "total";
    const vals = PM.map(p => ({ p, c: cmpValue(p.id, S.m) }));
    const v = x => !x.c || x.c.value == null ? null : rate === "year" ? x.c.per_year : x.c.value;
    const max = Math.max(1e-9, ...vals.map(v).filter(x => x != null));
    const W = 1000, H = 300, padL = 50, padB = 50, padT = 24, bw = (W - padL - 10) / PM.length;
    const top = unit === "share" ? Math.min(1, niceMax(max * 100) / 100) : niceMax(max), tk = ticks(top);
    const y = q => H - padB - (H - padB - padT) * q / top;
    let h = `<defs>${PM.map(p => `<pattern id="hatch-${p.id}" width="6" height="6" patternUnits="userSpaceOnUse" patternTransform="rotate(45)"><rect width="6" height="6" fill="#fff"/><line x1="0" y1="0" x2="0" y2="6" stroke="${p.colour}" stroke-width="3.2"/></pattern>`).join("")}</defs>`;
    tk.forEach(t => { h += `<line x1="${padL}" x2="${W - 10}" y1="${y(t)}" y2="${y(t)}" stroke="#ECECEC"/><text x="${padL - 6}" y="${y(t) + 4}" text-anchor="end" class="ax">${unit === "share" ? pct(t) : num(t, top < 5 && rate === "year" ? 1 : 0)}</text>`; });
    const notes = [];
    vals.forEach((x, i) => {
      const cx = padL + i * bw + bw / 2, val = v(x), c = x.c;
      const partial = c && !c.complete, sec = c && c.source_type === "secondary";
      if (c && partial && c.value != null) notes.push(`${x.p.short}: ${fmtD(c.covered_from)} to ${fmtD(c.covered_to)} only`);
      if (c && c.note && !/^https?:/.test(c.note) && !/wikipedia/i.test(c.note)) notes.push(`${x.p.short}: ${c.note}`);
      if (val != null) {
        const tip = `<b>${esc(x.p.name)}</b><br>${esc(c.measure)}: ${fmtVal(c, "total")}${unit === "count" ? ` (${num(c.per_year, 1)} a year over ${num(c.years_covered, 1)} years)` : ""}<br>${sec ? "Published total (secondary source)" : "Official records"}${partial ? `<br>Covers ${fmtD(c.covered_from)} – ${fmtD(c.covered_to)} only` : ""}`;
        h += `<rect x="${cx - bw * 0.3}" y="${y(val)}" width="${bw * 0.6}" height="${Math.max(0.5, y(0) - y(val))}" fill="${sec ? `url(#hatch-${x.p.id})` : x.p.colour}" stroke="${x.p.colour}" opacity="${partial ? 0.55 : 1}" data-tip="${esc(tip)}"/>`;
        h += `<text x="${cx}" y="${y(val) - 6}" text-anchor="middle" class="lab">${fmtVal(c, rate)}</text>`;
      } else h += `<text x="${cx}" y="${y(0) - 6}" text-anchor="middle" class="ax">no data</text>`;
      h += `<text x="${cx}" y="${H - padB + 18}" text-anchor="middle" font-size="12.5">${esc(x.p.short)}${partial ? "*" : ""}</text><text x="${cx}" y="${H - padB + 34}" text-anchor="middle" class="ax">${span(x.p)}</text>`;
    });
    const svgEl = $("#cmp-chart"); svgEl.setAttribute("viewBox", `0 0 ${W} ${H}`); svgEl.style.minWidth = "620px"; svgEl.innerHTML = h;
    const fbUsed = vals.filter(x => x.c && x.c.fallback).map(x => x.p.short);
    $("#cmp-note").innerHTML = [fbUsed.length ? `${fbUsed.join(", ")}: the Sejm's records start in October 1997, so ${fbUsed.length > 1 ? "their bars show" : "the bar shows"} the published total for the whole presidency (hatched).` : "",
      notes.length ? "* " + esc([...new Set(notes)].join("; ")) + "." : "", groupOf(S.m) === "appointments" && /acts/.test(S.m) ? "Counts of acts published in Monitor Polski; rates per year use only the years since the kind has been published regularly." : ""].filter(Boolean).join(" ");
    relChart(); cmpTable();
  };
  function relChart() {
    const rows = D.by_relation, ids = PM.map(p => p.id).filter(id => rows.some(r => r.spell_id === id));
    const W = 1000, H = 230, padT = 26, padB = 40, half = (W - 40) / 2;
    let h = "";
    [["vetoes_per_year", "Vetoes per year"], ["bills_per_year", "Bills submitted per year"]].forEach(([key, lab], k) => {
      const x0 = 30 + k * (half + 20), max = Math.max(1, ...rows.map(r => r[key])), top = niceMax(max), tk = ticks(top, 4);
      const y = q => H - padB - (H - padB - padT) * q / top, gw = (half - 30) / ids.length;
      h += `<text x="${x0}" y="14" class="lab" font-weight="500">${lab}</text>`;
      tk.forEach(t => { h += `<line x1="${x0 + 26}" x2="${x0 + half}" y1="${y(t)}" y2="${y(t)}" stroke="#ECECEC"/><text x="${x0 + 22}" y="${y(t) + 4}" text-anchor="end" class="ax">${num(t)}</text>`; });
      ids.forEach((id, i) => {
        const gx = x0 + 30 + i * gw;
        ["own party in government", "cohabitation"].forEach((rel, j) => {
          const r = rows.find(z => z.spell_id === id && z.relation === rel); if (!r) return;
          const bx = gx + gw * (0.12 + j * 0.38);
          h += `<rect x="${bx}" y="${y(r[key])}" width="${gw * 0.36}" height="${Math.max(0.5, y(0) - y(r[key]))}" fill="${REL[rel]}" data-tip="${esc(`<b>${nameOf(id)}</b>, ${RELNAME[rel].toLowerCase()}<br>${num(r[key.replace("_per_year", "")])} over ${num(r.years, 1)} years: ${num(r[key], 1)} a year`)}"/>`;
          h += `<text x="${bx + gw * 0.18}" y="${y(r[key]) - 4}" text-anchor="middle" class="ax">${num(r[key], 1)}</text>`;
        });
        h += `<text x="${gx + gw / 2}" y="${H - padB + 16}" text-anchor="middle" font-size="12">${esc(nameOf(id))}</text>`;
      });
    });
    h += `<g transform="translate(30,${H - 10})"><rect width="10" height="10" y="-9" fill="${REL["own party in government"]}"/><text x="14" y="0" class="ax">Own party in government</text><rect x="190" width="10" height="10" y="-9" fill="${REL.cohabitation}"/><text x="204" y="0" class="ax">Cohabitation</text></g>`;
    const svgEl = $("#rel-chart"); svgEl.setAttribute("viewBox", `0 0 ${W} ${H}`); svgEl.style.minWidth = "620px"; svgEl.innerHTML = h;
    tipFor(svgEl.parentElement, $("#tip-cmp"));
  }
  function cmpTable() {
    let rows = "";
    Object.entries(GROUPS).forEach(([g, lab]) => {
      rows += `<tr class="grp"><td colspan="${PM.length + 1}">${esc(lab)}</td></tr>`;
      measures.filter(m => groupOf(m) === g).forEach(m => {
        rows += `<tr><td class="m">${esc(m)}</td>${PM.map(p => {
          const c = D.compare.find(x => x.spell_id === p.id && x.measure === m);
          if (!c || c.value == null) return `<td class="n muted">–</td>`;
          const mark = (c.source_type === "secondary" ? "†" : "") + (!c.complete ? "*" : "");
          const tip = `${c.source_type === "secondary" ? "Secondary source. " : ""}${!c.complete ? `Covers ${fmtD(c.covered_from)} – ${fmtD(c.covered_to)}. ` : ""}${c.note && !/^https?:/.test(c.note) ? c.note : ""}`;
          return `<td class="n" title="${esc(tip)}">${fmtVal(c, "total")}${mark ? `<span class="flag">${mark}</span>` : ""}${c.unit === "count" && c.per_year != null ? `<br><span class="muted">${num(c.per_year, 1)}/yr</span>` : ""}</td>`;
        }).join("")}</tr>`;
      });
    });
    $("#cmp-table").innerHTML = `<table class="t cmp"><thead><tr><th>Measure</th>${PM.map(p => `<th class="n">${esc(p.short)}<br><span class="muted">${span(p)}</span></th>`).join("")}</tr></thead><tbody>${rows}</tbody></table><p class="help" style="padding:6px 8px">† A published total (Chancellery or press, as reported on Polish Wikipedia), not the official records. * The source covers only part of the presidency (hover for the dates). "/yr" is per year over the years covered.</p>`;
  }
  document.querySelectorAll("#cmp-rate button").forEach(b => b.addEventListener("click", () => { S.r = b.dataset.r; RENDER.cmp(); writeHash(); }));
  $("#cmp-csv").addEventListener("click", () => {
    const cols = ["spell_id", "measure", "group", "value", "unit", "per_year", "years_covered", "years_in_office", "complete", "covered_from", "covered_to", "source_type", "note"];
    download("presidents-comparison.csv", ChartExport.csvCite(PAGE) + cols.join(",") + "\n" + D.compare.map(r => cols.map(c => csvQ(r[c])).join(",")).join("\n") + "\n");
  });
  $("#cmp-png").addEventListener("click", () => pngOf($("#cmp-chart"), S.m + (S.r === "year" && unitOf(S.m) === "count" ? ", per year in office" : ""), "Polish presidents since 1989", "Sources: Sejm API, Constitutional Tribunal, Monitor Polski, Party tracker; published totals as reported on Polish Wikipedia.", "presidents-" + S.m.toLowerCase().replace(/[^a-z0-9]+/g, "-") + ".png"));

  // ===== Vetoes and bills =====
  function syncControls() {
    $("#leg-p").value = S.lp; $("#tk-p").value = S.tp; $("#key-p").value = S.kp;
    document.querySelectorAll("#leg-kind button").forEach(b => b.setAttribute("aria-pressed", b.dataset.k === S.k));
  }
  $("#leg-p").innerHTML = presOptions(); $("#tk-p").innerHTML = presOptions(); $("#key-p").innerHTML = presOptions();
  $("#leg-p").addEventListener("change", e => { S.lp = e.target.value; RENDER.leg(); writeHash(); });
  $("#leg-o").addEventListener("change", e => { S.lo = e.target.value; RENDER.leg(); });
  $("#leg-q").addEventListener("input", e => { S.lq = e.target.value; RENDER.leg(); });
  document.querySelectorAll("#leg-kind button").forEach(b => b.addEventListener("click", () => { S.k = b.dataset.k; S.lo = ""; RENDER.leg(); writeHash(); }));
  const fold = s => String(s || "").toLowerCase().normalize("NFD").replace(/[̀-ͯ]/g, "").replace(/ł/g, "l");
  const laLink = r => r.term >= 5 ? page("legislative-activity", `view=bills&t=${r.term}&bill=${r.term}:n${r.number}`, "Bill") : "";
  const actLink = r => r.eli ? ext(`https://api.sejm.gov.pl/eli/acts/${r.eli}/text.pdf`, "Act") : "";
  RENDER.leg = () => {
    syncControls();
    const isV = S.k === "vetoes", src = isV ? vetoes : bills, colours = isV ? VOUT : BOUT, dateK = isV ? "veto_date" : "submitted";
    const names = isV ? VOUTNAME : Object.fromEntries(Object.keys(BOUT).map(k => [k, k[0].toUpperCase() + k.slice(1)]));
    $("#leg-o").innerHTML = `<option value="">All outcomes</option>` + Object.keys(colours).map(k => `<option value="${esc(k)}">${esc(names[k])}</option>`).join("");
    $("#leg-o").value = S.lo;
    const q = fold(S.lq);
    const rows = src.filter(r => (!S.lp || r.spell_id === S.lp) && (!S.lo || r.outcome === S.lo) && (!q || fold(isV ? r.law_title : r.title_short).includes(q)));
    $("#leg-help").innerHTML = isV
      ? `Every law the president sent back to the Sejm since October 1997, found in the Sejm's records. The Sejm can override a veto by three fifths of the votes; a veto it never takes up kills the law at the end of the term. Before October 1997 only published totals exist (below).`
      : `Every bill the president submitted to the Sejm since October 1997. Bills of a term the Sejm did not finish lapse when the term ends. Before October 1997 only published totals exist (below).`;
    $("#leg-legend").innerHTML = legendHtml(Object.fromEntries(Object.entries(colours).map(([k, c]) => [names[k], c])));
    const agg = {};
    rows.forEach(r => { const yr = yearOf(r[dateK]); const k = yr + "|" + r.outcome; agg[k] = (agg[k] || 0) + 1; });
    yearBars($("#leg-chart"), { y0: 1997, y1: yearOf(AS_OF), keys: Object.keys(colours), colours,
      rows: Object.entries(agg).map(([k, n]) => { const [yr, o] = k.split("|"); return { year: +yr, key: o, n, tip: `<b>${yr}</b><br>${esc(names[o])}: ${n}` }; }) });
    const cnt = k => rows.filter(r => r.outcome === k).length;
    $("#leg-stats").innerHTML = `<button disabled><b>${num(rows.length)}</b><span>${isV ? "vetoes" : "bills"} shown</span></button>` +
      Object.keys(colours).filter(k => cnt(k)).map(k => `<button data-o="${esc(k)}"><b>${num(cnt(k))}</b><span><i class="sw" style="background:${colours[k]}"></i>${esc(names[k])}</span></button>`).join("");
    $("#leg-stats").querySelectorAll("[data-o]").forEach(b => b.addEventListener("click", () => { S.lo = S.lo === b.dataset.o ? "" : b.dataset.o; RENDER.leg(); }));
    const cols = isV ? [
      { h: "Vetoed", d: 1, f: r => fmtD(r.veto_date) + (r.date_basis === "sent to the president" ? '<span class="flag" title="No veto date in the records: the date the law was sent to the president">*</span>' : "") },
      { h: "President", f: r => esc(nameOf(r.spell_id)) },
      { h: "Law", f: r => esc(r.law_title) },
      { h: "Bill from", f: r => esc(SPONSOR[r.sponsor] || r.sponsor || "") },
      { h: "Outcome", f: r => `<i class="sw" style="background:${VOUT[r.outcome]}"></i>${esc(names[r.outcome])}${r.rc_yes != null ? `<br><span class="muted">${r.rc_yes} for, ${r.rc_no} against; ${r.rc_needed} needed</span>` : ""}` },
      { h: "Topic", f: r => esc(r.cap_label || "") },
      { h: "Links", f: r => [laLink(r), actLink(r)].filter(Boolean).join(" · ") }]
      : [
      { h: "Submitted", d: 1, f: r => fmtD(r.submitted) },
      { h: "President", f: r => esc(nameOf(r.spell_id)) },
      { h: "Bill", f: r => esc(r.title_short) + (r.constitutional ? ' <span class="badge">constitution</span>' : "") },
      { h: "Outcome", f: r => `<i class="sw" style="background:${BOUT[r.outcome]}"></i>${esc(names[r.outcome])}` },
      { h: "Topic", f: r => esc(r.cap_label || "") },
      { h: "Links", f: r => [laLink(r), actLink(r)].filter(Boolean).join(" · ") }];
    $("#leg-table").innerHTML = rows.length ? table(cols, rows.slice().sort((a, b) => a[dateK] < b[dateK] ? 1 : -1)) : '<p class="help" style="padding:10px">None match.</p>';
    // before October 1997
    const sec = D.secondary.filter(s => s.measure === (isV ? "vetoes" : "bills"));
    const off = id => src.filter(r => r.spell_id === id).length;
    const early = D.early.filter(e => e.kind === (isV ? "veto" : "bill"));
    const kw1 = sec.find(s => s.spell_id === "kwasniewski" && s.period_end === "2000-12-23");
    const kwOff1 = src.filter(r => r.spell_id === "kwasniewski" && r[dateK] <= "2000-12-23").length;
    $("#leg-early").innerHTML = `<h2 class="sec">Before October 1997 and published totals</h2>
      <p class="help">The Sejm's records begin with the term elected in September 1997. ${isV ? "For the earlier years, and as a check on the records, these are the totals the Chancellery or the press have published, as reported on Polish Wikipedia." : "For the earlier years these are the totals the Chancellery has published, as reported on Polish Wikipedia."}${kw1 ? ` Kwaśniewski's first term (1995–2000) had ${kw1.value} ${isV ? "vetoes" : "bills"} in all, of which ${kwOff1} came after October 1997 in the records, which leaves about ${Math.max(0, kw1.value - kwOff1)} before it.` : ""}</p>
      <div class="tbox short">${table([
        { h: "President", f: r => esc(nameOf(r.spell_id)) }, { h: "Period", d: 1, f: r => `${fmtD(r.period_start)} – ${fmtD(r.period_end)}` },
        { h: isV ? "Vetoes" : "Bills", n: 1, f: r => num(+r.value) },
        { h: "In the Sejm's records", n: 1, f: r => r.period_start >= "1997-10-20" || r.period_end > "1997-10-20" ? num(src.filter(x => x.spell_id === r.spell_id && x[dateK] >= r.period_start && x[dateK] <= r.period_end).length) + (r.period_start < "1997-10-20" ? '<span class="flag">*</span>' : "") : "–" },
        { h: "Source", f: r => srcLink(r.source) }], sec)}</div>
      <p class="help">* The records cover only the part of the period after 20 October 1997.</p>
      ${early.length ? `<h2 class="sec">${isV ? "Vetoes" : "Bills"} before 1997 named in published sources</h2><div class="tbox short">${table([
        { h: "Date", d: 1, f: r => r.date ? fmtD(r.date) : "–" }, { h: "President", f: r => esc(nameOf(r.spell_id)) },
        { h: "Title", f: r => esc(r.title) }, { h: "Outcome", f: r => esc(r.outcome) },
        { h: "Source", f: r => { const m = r.source.match(/https?:\/\/[^\s)]+/); return m ? ext(m[0], "Wikipedia") : esc(r.source); } }], early)}</div>` : ""}`;
  };

  // ===== Constitutional Tribunal =====
  $("#tk-p").addEventListener("change", e => { S.tp = e.target.value; RENDER.tk(); writeHash(); });
  document.querySelectorAll("#tk-by button").forEach(b => b.addEventListener("click", () => { S.tb = b.dataset.b; RENDER.tk(); }));
  RENDER.tk = () => {
    syncControls();
    document.querySelectorAll("#tk-by button").forEach(b => b.setAttribute("aria-pressed", b.dataset.b === S.tb));
    const rows = apps.filter(r => !S.tp || r.spell_id === S.tp), key = S.tb === "kind" ? "kind" : "result", colours = S.tb === "kind" ? TKIND : TRES;
    $("#tk-legend").innerHTML = legendHtml(colours);
    const agg = {}; rows.forEach(r => { const k = yearOf(r.filed) + "|" + r[key]; agg[k] = (agg[k] || 0) + 1; });
    yearBars($("#tk-chart"), { y0: 1997, y1: yearOf(AS_OF), keys: Object.keys(colours), colours,
      rows: Object.entries(agg).map(([k, n]) => { const [yr, o] = k.split("|"); return { year: +yr, key: o, n }; }) });
    const cnt = (k, v) => rows.filter(r => r[k] === v).length;
    const kw = D.secondary.find(s => s.measure === "tribunal_referrals");
    $("#tk-stats").innerHTML = `<button disabled><b>${rows.length}</b><span>applications</span></button>` +
      Object.keys(TKIND).filter(k => cnt("kind", k)).map(k => `<button disabled><b>${cnt("kind", k)}</b><span>${esc(k)}</span></button>`).join("") +
      `<button disabled><b>${cnt("result", "unconstitutional") + cnt("result", "partly unconstitutional")}</b><span>found wholly or partly unconstitutional</span></button>`;
    $("#tk-table").innerHTML = table([
      { h: "Filed", d: 1, f: r => fmtD(r.filed) },
      { h: "Case", d: 1, f: r => ext("https://ipo.trybunal.gov.pl/ipo/Sprawa?sygnatura=" + encodeURIComponent(r.signature), r.signature) },
      { h: "President", f: r => esc(nameOf(r.spell_id)) },
      { h: "Kind", f: r => `<i class="sw" style="background:${TKIND[r.kind]}"></i>${esc(r.kind)}` },
      { h: "Subject", f: r => esc(r.subject || "") + (r.sole_applicant ? "" : ' <span class="badge">joint application</span>') },
      { h: "Result", f: r => `<i class="sw" style="background:${TRES[r.result]}"></i>${esc(r.result)}` },
      { h: "Decided", d: 1, f: r => r.final_date ? fmtD(r.final_date) : "–" }], rows.slice().sort((a, b) => a.filed < b.filed ? 1 : -1)) +
      `<p class="help" style="padding:6px 8px">Results as the Tribunal's portal records them; see my ${page("constitutional-tribunal", "rul&app=President", "Constitutional Tribunal")} page for every ruling and the disputes over the Tribunal since 2015.${kw ? ` Kwaśniewski sent ${kw.value} laws to the Tribunal in his first term (1995–2000), by the Chancellery's count as reported on Polish Wikipedia; the portal's cases begin in 1997.` : ""}</p>`;
  };

  // ===== Appointments =====
  const kinds = D.meta.gazette_kinds.filter(k => k.kind !== "other");
  const KORDER = ["ambassadors", "judges", "military_ranks", "military_command", "court_heads", "bank_regulators", "government", "decorations", "forces_abroad", "professors", "elections", "chancellery"];
  kinds.sort((a, b) => KORDER.indexOf(a.kind) - KORDER.indexOf(b.kind));
  $("#app-k").innerHTML = kinds.map(k => `<option value="${k.kind}">${esc(k.label)}</option>`).join("");
  $("#app-k").addEventListener("change", e => { S.gk = e.target.value; RENDER.app(); writeHash(); });
  const keyKinds = ["court_heads", "bank_regulators", "military_command", "elections", "chancellery", "government"];
  $("#key-k").innerHTML = `<option value="">All</option>` + kinds.filter(k => keyKinds.includes(k.kind)).map(k => `<option value="${k.kind}">${esc(k.label)}</option>`).join("");
  $("#key-k").addEventListener("change", e => { S.kk = e.target.value; keyTable(); });
  $("#key-p").addEventListener("change", e => { S.kp = e.target.value; keyTable(); });
  $("#key-q").addEventListener("input", e => { S.kq = e.target.value; keyTable(); });
  function keyTable() {
    const q = fold(S.kq);
    const rows = keyActs.filter(r => (!S.kk || r.kind === S.kk) && (!S.kp || r.spell_id === S.kp) && (!q || fold(r.title).includes(q)));
    const lab = Object.fromEntries(kinds.map(k => [k.kind, k.label]));
    $("#key-table").innerHTML = rows.length ? table([
      { h: "Date", d: 1, f: r => fmtD(r.date) }, { h: "President", f: r => esc(nameOf(r.spell_id)) },
      { h: "Kind", f: r => esc(lab[r.kind] || r.kind) }, { h: "Decision", f: r => esc(r.title) },
      { h: "Monitor Polski", d: 1, f: r => ext(r.url, r.eli.replace(/^MP\//, "M.P. ").replace(/\//, " poz. ")) }], rows.slice().sort((a, b) => a.date < b.date ? 1 : -1)) : '<p class="help" style="padding:10px">None match.</p>';
  }
  RENDER.app = () => {
    if (!kinds.some(k => k.kind === S.gk)) S.gk = "ambassadors";
    $("#app-k").value = S.gk;
    const k = kinds.find(z => z.kind === S.gk), from = k.published_from;
    const rows = gazette.filter(r => r.kind === S.gk);
    yearBars($("#app-chart"), { y0: 1989, y1: yearOf(AS_OF), keys: PM.map(p => p.id).concat(P.filter(p => p.acting).map(p => p.id), [null]), colours: Object.fromEntries(P.map(p => [p.id, "#4B6A88"])),
      rows: rows.map(r => ({ year: r.year, key: r.spell_id, n: r.acts, tip: `<b>${r.year}</b>, ${esc(nameOf(r.spell_id))}<br>${num(r.acts)} acts` })),
      shadeTo: from - 1, shadeLabel: "not regularly published" });
    const per = PM.map(p => {
      const c = D.compare.find(x => x.spell_id === p.id && x.measure.startsWith(k.label.split(" ")[0]) && x.group === "appointments");
      const n = rows.filter(r => r.spell_id === p.id && r.year >= from).reduce((s, r) => s + r.acts, 0);
      const yrs = Math.max(0, ((p.end ? T(p.end) : T(AS_OF)) - Math.max(T(p.start), T(from + "-01-01"))) / (365.25 * 864e5));
      return { p, n, yrs };
    });
    $("#app-stats").innerHTML = per.map(x => x.yrs > 0.05 ? `<button disabled><b>${num(x.n)}</b><span>${esc(x.p.short)}: ${num(x.n / x.yrs, 1)} a year${x.yrs < yearsIn(x.p) - 0.05 ? ` (from ${from})` : ""}</span></button>` : `<button disabled><b>–</b><span>${esc(x.p.short)}: not published</span></button>`).join("");
    $("#app-pm").innerHTML = table([
      { h: "Prime minister", f: r => page("governments", "c=" + r.cabinet_id + "&v=cabinet", r.pm) + (r.formed ? "" : ' <span class="badge">never formed a government</span>') },
      { h: "Appointed", d: 1, f: r => fmtD(r.pm_appointed) }, { h: "President", f: r => esc(nameOf(r.spell_id)) }], D.prime_ministers.slice().reverse());
    $("#app-nbp").innerHTML = table([
      { h: "Appointed", d: 1, f: r => fmtD(r.resolution_date) }, { h: "President of the bank", f: r => esc(r.nominee) },
      { h: "Nominated by", f: r => esc(nameOf(r.spell_id)) + (r.note && /acting/.test(r.note) ? " (acting)" : "") },
      { h: "Resolution", d: 1, f: r => ext(`https://api.sejm.gov.pl/eli/acts/${r.resolution}/text.pdf`, r.resolution.replace(/^MP\//, "M.P. ").replace(/\//, " poz. ")) }], D.nbp.slice().reverse()) +
      '<p class="help" style="padding:6px 8px">From 1997. The 1992 appointment (Hanna Gronkiewicz-Waltz, on Wałęsa\'s motion) is not in the Sejm\'s legal database.</p>';
    keyTable();
  };

  // ===== Pardons =====
  document.querySelectorAll("#par-rate button").forEach(b => b.addEventListener("click", () => { S.pr = b.dataset.r; RENDER.par(); }));
  const PCOL = { granted: "#4B6A88", refused: "#C9C9C4" };
  RENDER.par = () => {
    document.querySelectorAll("#par-rate button").forEach(b => b.setAttribute("aria-pressed", b.dataset.r === S.pr));
    $("#par-legend").innerHTML = legendHtml({ "Pardoned": PCOL.granted, "Refused": PCOL.refused });
    const shown = D.pardons.filter(x => x.shown === "TRUE");
    const perYears = p => { const x = shown.find(z => z.spell_id === p.id); if (!x) return yearsIn(p); const e = x.period_end ? T(x.period_end.length === 7 ? x.period_end + "-28" : x.period_end) : (p.end ? T(p.end) : T(AS_OF)); return (e - T(x.period_start)) / (365.25 * 864e5); };
    const vals = PM.map(p => {
      const g = shown.find(x => x.spell_id === p.id && x.measure === "pardons_granted"), r = shown.find(x => x.spell_id === p.id && x.measure === "pardons_refused");
      const yrs = perYears(p), f = v => v == null ? null : S.pr === "year" ? v / yrs : v;
      return { p, g: g ? +g.value : null, r: r ? +r.value : null, gv: f(g ? +g.value : null), rv: f(r ? +r.value : null), yrs };
    });
    const max = Math.max(1, ...vals.flatMap(v => [v.gv || 0, v.rv || 0]));
    const W = 1000, H = 300, padL = 54, padB = 50, padT = 20, bw = (W - padL - 10) / PM.length, top = niceMax(max), tk = ticks(top);
    const y = q => H - padB - (H - padB - padT) * q / top;
    let h = "";
    tk.forEach(t => { h += `<line x1="${padL}" x2="${W - 10}" y1="${y(t)}" y2="${y(t)}" stroke="#ECECEC"/><text x="${padL - 6}" y="${y(t) + 4}" text-anchor="end" class="ax">${num(t)}</text>`; });
    vals.forEach((v, i) => {
      const cx = padL + i * bw + bw / 2;
      [["gv", "granted", -1], ["rv", "refused", 0]].forEach(([k, lab, side]) => {
        if (v[k] == null) return;
        const bx = cx + side * bw * 0.34 + (side ? 0 : 0.02 * bw);
        h += `<rect x="${bx}" y="${y(v[k])}" width="${bw * 0.32}" height="${Math.max(0.5, y(0) - y(v[k]))}" fill="${PCOL[lab]}" data-tip="${esc(`<b>${v.p.name}</b><br>${lab === "granted" ? "Pardoned" : "Refused"}: ${num(lab === "granted" ? v.g : v.r)} (${num((lab === "granted" ? v.g : v.r) / v.yrs, 1)} a year)`)}"/>`;
        h += `<text x="${bx + bw * 0.16}" y="${y(v[k]) - 5}" text-anchor="middle" class="ax" style="fill:#141414">${num(v[k], S.pr === "year" && v[k] < 10 ? 1 : 0)}</text>`;
      });
      const flag = v.p.id === "duda" || v.p.id === "nawrocki" ? "*" : "";
      h += `<text x="${cx}" y="${H - padB + 18}" text-anchor="middle" font-size="12.5">${esc(v.p.short)}${flag}</text><text x="${cx}" y="${H - padB + 34}" text-anchor="middle" class="ax">${span(v.p)}</text>`;
    });
    const svgEl = $("#par-chart"); svgEl.setAttribute("viewBox", `0 0 ${W} ${H}`); svgEl.style.minWidth = "620px"; svgEl.innerHTML = h;
    const per = s => !s ? "not stated" : s.length === 4 ? s : s.length === 7 ? MON[+s.slice(5) - 1] + " " + s.slice(0, 4) : fmtD(s);
    const g0 = m => id => D.pardons.find(x => x.spell_id === id && x.measure === m && x.shown === "TRUE");
    const gG = g0("pardons_granted"), gR = g0("pardons_refused");
    const main = PM.map(p => ({ p, g: gG(p.id), r: gR(p.id) })).filter(x => x.g);
    const alt = D.pardons.filter(x => x.shown !== "TRUE");
    const altNote = x => {
      const m = D.pardons.find(z => z.shown === "TRUE" && z.spell_id === x.spell_id && z.measure === x.measure);
      if (m && m.period_start === x.period_start) return +m.value === +x.value ? "Agrees with the figure shown" : `Differs from the figure shown (${num(+m.value)})`;
      return (x.note || "").split(/(?<=\.)\s/)[0];
    };
    $("#par-table").innerHTML = table([
      { h: "President", f: r => esc(r.p.name) },
      { h: "Period", d: 1, f: r => `${fmtD(r.g.period_start)} – ${per(r.g.period_end)}` },
      { h: "Pardoned", n: 1, f: r => num(+r.g.value) }, { h: "Refused", n: 1, f: r => r.r ? num(+r.r.value) : "–" },
      { h: "Pardoned a year", n: 1, f: r => num(+r.g.value / perYears(r.p), 1) },
      { h: "Source", f: r => srcLink(r.g.source) }], main) +
      `<p class="help" style="padding:6px 8px">* Duda: the source does not say whether the figure runs to the end of his presidency or only to June 2020, the date of the Chancellery's statistics it cites; his rate per year assumes the whole presidency. Nawrocki: to August 2026.</p>` +
      (alt.length ? `<h2 class="sec" style="margin:8px 8px 4px">Other published figures</h2>` + table([
        { h: "President", f: r => esc(nameOf(r.spell_id)) },
        { h: "Period", d: 1, f: r => `${per(r.period_start)} – ${per(r.period_end)}` },
        { h: "Figure", f: r => esc(r.measure.replace(/^pardons_/, "").replace(/_/g, " ")) },
        { h: "Number", n: 1, f: r => num(+r.value) },
        { h: "Source", f: r => (r.source_type === "official" ? '<span class="badge">official</span> ' : "") + srcLink(r.source) },
        { h: "Note", f: r => esc(altNote(r)) }], alt) : "");
  };
  $("#par-png").addEventListener("click", () => pngOf($("#par-chart"), "Pardons granted and refused" + (S.pr === "year" ? ", per year in office" : ""), "Polish presidents since 1989", "Source: the Chancellery's totals as reported on Polish Wikipedia (Ułaskawienie).", "presidents-pardons.png"));

  // ---- credit ----
  $("#credit").innerHTML = `<p>Sources: the Sejm's open API (bills, vetoes, roll calls and Monitor Polski through its legal database), the Constitutional Tribunal's portal, the National Electoral Commission (through my ${page("election-results", "", "Election results")}), my ${page("party-genealogy", "", "Parties in the Sejm")} tracker and ${page("governments", "", "Governments")} page; party ties, the heads of the Chancellery and the National Security Bureau, pardon totals and vetoes before 1997 from Polish Wikipedia (CC BY-SA 4.0), which cites the Chancellery and the press.</p><p>${ChartExport.citeHtml(PAGE)}</p>`;

  show(S.v);
})();
