// Constitutional Tribunal: composition, elections, rulings, activity, and the disputes since 2015. No libraries.
// Data: the JSON files beside this one, fetched together before the page is drawn.
(async function () {
  const J = f => fetch(f).then(r => { if (!r.ok) throw new Error(f + " " + r.status); return r.json(); });
  let loaded;
  try {
    const [composition, chronology, index, activity, pending] = await Promise.all([J("composition.json"), J("chronology.json"), J("rulings.json"), J("activity.json"), J("rulings-pending.json")]);
    const yrs = await Promise.all(index.years.map(y => J("rulings-" + y.year + ".json")));
    const cases = { pending }; yrs.forEach(y => { cases[y.year] = y; });
    loaded = { composition, chronology, ruling: { index, activity, cases } };
  } catch (e) {
    document.body.insertAdjacentHTML("afterbegin", '<p style="padding:16px;color:#8A5A00">The data could not be loaded. Please try again later.</p>');
    throw e;
  }
  window.TK = loaded; window.TKR = loaded.ruling;
  const C = window.TK.composition, H = window.TK.chronology;
  const $ = (s, r = document) => r.querySelector(s);
  const el = (tag, attrs = {}, html = "") => { const e = document.createElement(tag); for (const k in attrs) e.setAttribute(k, attrs[k]); e.innerHTML = html; return e; };
  const NS = "http://www.w3.org/2000/svg";
  const svg = (tag, attrs = {}) => { const e = document.createElementNS(NS, tag); for (const k in attrs) e.setAttribute(k, attrs[k]); return e; };
  const esc = s => String(s ?? "").replace(/[&<>"]/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const D = s => new Date(s + "T00:00:00Z").getTime();
  const fmt = s => { if (!s) return ""; const d = new Date(D(s)); return d.getUTCDate() + " " + ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"][d.getUTCMonth()] + " " + d.getUTCFullYear(); };
  $("#asof").textContent = fmt(C.meta.as_of);

  // ---- views ----
  document.querySelectorAll("#views button").forEach(b => b.addEventListener("click", () => {
    document.querySelectorAll("#views button").forEach(x => x.setAttribute("aria-pressed", x === b));
    for (const v of ["comp", "elect", "rul", "act", "disp"]) $("#v-" + v).hidden = (v !== b.dataset.v);
  }));

  { const h = location.hash.replace("#", ""); const b = h && document.querySelector(`#views button[data-v="${h}"]`); if (b) b.click(); }   // #elect or #disp opens that view
  // ---- composition ----
  const COL = Object.assign({}, C.meta.club_colours);
  const OTHER = { "Presidium of the Sejm": "#6B7280", unknown: "#C9C9C4", none: "#9CA3AF" };
  const ERA = { early: ["#8B7355", "1985–97 (before the Constitution of 1997)"], retired: ["#4B6A88", "1997–2026, term ended"], current: ["#8B0000", "In office now"] };
  const judges = C.judges.map(j => Object.assign({}, j, { t0: D(j.start), t1: D(j.end) })).sort((a, b) => a.t0 - b.t0);
  const yearsFrom = [1986, 1997, 2005, 2010, 2015];
  $("#from").innerHTML = yearsFrom.map(y => `<option value="${y}"${y === 1986 ? " selected" : ""}>${y}</option>`).join("");
  const colourOf = (j, mode) => {
    if (mode === "era") return ERA[j.list][0];
    if (mode === "standing") return j.standing && j.standing.category ? "#B45309" : "#4B6A88";
    const nc = j.election && j.election.nominator_club;
    if (!j.election) return OTHER.unknown;
    if (!nc) return OTHER.unknown;
    return COL[nc] || OTHER[nc] || OTHER.unknown;
  };
  function legend(mode) {
    let items;
    if (mode === "era") items = Object.values(ERA);
    else if (mode === "standing") items = [["#B45309", "Standing disputed or oath taken in disputed circumstances"], ["#4B6A88", "No dispute recorded"]];
    else { items = Object.entries(COL).map(([k, v]) => [v, k + " MPs"]); items.push([OTHER["Presidium of the Sejm"], "Presidium of the Sejm"], [OTHER.unknown, "Nominator not found (before 2007, or not read)"]); }
    $("#legend").innerHTML = items.map(([c, t]) => `<span><i style="background:${c}"></i>${esc(t)}</span>`).join("");
  }
  function draw() {
    const mode = $("#colourby").value, y0 = +$("#from").value;
    legend(mode);
    const box = $("#chart"); box.innerHTML = "";
    const W = 1000, padL = 34, padR = 8, topH = 96, rowH = 9, gap = 2;
    const t0 = D(y0 + "-01-01"), t1 = Date.now();
    const list = judges.filter(j => j.t1 >= t0);
    const H_ = topH + 14 + list.length * (rowH + gap) + 24;
    const x = t => padL + (Math.max(t, t0) - t0) / (t1 - t0) * (W - padL - padR);
    const s = svg("svg", { viewBox: `0 0 ${W} ${H_}`, role: "img", "aria-label": "Judges of the Constitutional Tribunal by term of office" });
    // year axis
    for (let y = Math.ceil(y0 / 5) * 5; y <= 2025; y += 5) {
      const xx = x(D(y + "-01-01"));
      s.appendChild(svg("line", { x1: xx, x2: xx, y1: topH, y2: H_ - 20, stroke: "#ECECEC" }));
      const t = svg("text", { x: xx, y: H_ - 6, "text-anchor": "middle", "font-size": 11, fill: "#767676" }); t.textContent = y; s.appendChild(t);
    }
    // seats band: in-office count vs seats
    const runs = C.seat_runs.filter(r => D(r.to) >= t0);
    const yTop = 24, yBot = topH - 10, nMin = 8, nMax = 15;   // axis from 8 so that vacancies show
    const yv = n => yBot - (Math.max(n, nMin) - nMin) / (nMax - nMin) * (yBot - yTop);
    [10, 12, 15].forEach(n => { const yy = yv(n); s.appendChild(svg("line", { x1: padL, x2: W - padR, y1: yy, y2: yy, stroke: "#ECECEC" })); const t = svg("text", { x: padL - 6, y: yy + 4, "text-anchor": "end", "font-size": 11, fill: "#767676" }); t.textContent = n; s.appendChild(t); });
    let path = "";
    runs.forEach((r, i) => { const xa = x(D(r.from)), xb = x(D(r.to) + 86400000); path += (i ? "L" : "M") + xa + " " + yv(r.in_office) + " L" + xb + " " + yv(r.in_office) + " "; });
    s.appendChild(svg("path", { d: path, fill: "none", stroke: "#141414", "stroke-width": 1.6 }));
    let sp = ""; const rs = C.meta.seats_rule;
    rs.forEach((r, i) => { const xa = x(D(r.from)), xb = x(i + 1 < rs.length ? D(rs[i + 1].from) : t1); sp += (sp ? " L" : "M") + xa + " " + yv(r.seats) + " L" + xb + " " + yv(r.seats); });
    s.appendChild(svg("path", { d: sp, fill: "none", stroke: "#8B0000", "stroke-dasharray": "4 3", "stroke-width": 1 }));
    const lab = svg("text", { x: padL + 4, y: 12, "font-size": 11.5, fill: "#8B0000" }); lab.textContent = "dashed: seats (12, then 15 from Dec 1997)"; s.appendChild(lab);
    const lab2 = svg("text", { x: padL + 300, y: 12, "font-size": 11.5, fill: "#141414" }); lab2.textContent = "solid: judges in office (axis starts at 8)"; s.appendChild(lab2);
    // bars
    list.forEach((j, i) => {
      const yy = topH + 14 + i * (rowH + gap);
      const r = svg("rect", { x: x(j.t0), y: yy, width: Math.max(2, x(j.t1) - x(j.t0)), height: rowH, rx: 2, fill: colourOf(j, mode), tabindex: 0, style: "cursor:pointer" });
      if (j.standing && j.standing.category && mode !== "standing") r.setAttribute("stroke", "#141414");
      r.addEventListener("mousemove", ev => tip(ev, j)); r.addEventListener("mouseleave", () => { const t = $(".tip"); if (t) t.remove(); });
      r.addEventListener("click", () => detail(j)); r.addEventListener("keydown", ev => { if (ev.key === "Enter") detail(j); });
      s.appendChild(r);
    });
    box.appendChild(s);
  }
  function tip(ev, j) {
    let t = $(".tip"); if (!t) { t = el("div", { class: "tip" }); $("#chart").appendChild(t); }
    const e = j.election;
    t.innerHTML = `<b>${esc(j.name)}</b><br>${fmt(j.start)} – ${fmt(j.end)}${j.role ? "<br>" + esc(j.role) : ""}` +
      (e ? `<br>Elected ${fmt(e.date)}${e.yes != null && !e.group_vote ? ": " + e.yes + " for, " + e.no + " against" : ""}` : "");
    const b = $("#chart").getBoundingClientRect();
    t.style.left = Math.min(ev.clientX - b.left + 14, b.width - 310) + "px"; t.style.top = (ev.clientY - b.top + 14) + "px";
  }
  function votesLine(e) {
    if (!e) return "Election not found among the Sejm's resolutions (before 1993)";
    let v = "";
    if (e.vote_kind === "ON_LIST") v = "Secret ballot (the API gives turnout, not the direction of votes)";
    else if (e.yes != null) v = `${e.yes} for, ${e.no} against, ${e.abstain} abstaining` + (e.group_vote ? " (one ballot for several judges)" : "");
    else v = "No recorded vote in the Sejm's open data";
    return v;
  }
  const KIND_LAB = { party_member: "Party member", party_activity: "Party activity", party_ticket: "On a party's list", elected_office: "Elected office", executive_office: "Government office", political_nomination: "Put forward by a party for another post", political_committee: "Political committee", declared_non_party: "Declared non-party" };
  function partyHistory(j) {
    const h0 = j.party_history, h = h0 == null ? [] : Array.isArray(h0) ? h0 : [h0];
    if (!h.length) return `<span class="muted">Nothing recorded in the sources searched (this does not mean no ties).</span>`;
    return h.map(r => `<div>${esc(KIND_LAB[r.kind] || r.kind)}: ${esc(r.label)}${r.from || r.to ? " (" + esc([r.from, r.to].filter(Boolean).join("–")) + ")" : ""} <span class="muted">· ${r.secondary ? "secondary source" : "official"}: ${r.url ? `<a href="${esc(r.url)}" target="_blank" rel="noopener">${esc(r.source)}</a>` : esc(r.source)}</span></div>`).join("");
  }
  function detail(j) {
    const e = j.election, s = j.standing, d = $("#detail"); d.hidden = false;
    d.innerHTML = `<h3>${esc(j.name)}</h3><dl class="kv">
      <dt>Term as listed</dt><dd>${fmt(j.start)} – ${fmt(j.end)}${j.in_office ? " (in office)" : ""}${j.end_reason ? " · ended: " + esc(j.end_reason) : ""}</dd>
      ${j.role ? `<dt>Office</dt><dd>${esc(j.role)}</dd>` : ""}
      <dt>Elected by the Sejm</dt><dd>${e ? fmt(e.date) + " · " + esc(e.resolution) : "no resolution found"}</dd>
      <dt>Vote</dt><dd>${esc(votesLine(e))}</dd>
      ${e && e.votes_by_club ? `<dt>By club (for-against-abstain)</dt><dd>${esc(e.votes_by_club)}</dd>` : ""}
      <dt>Nominated by</dt><dd>${e && e.nominator ? esc(e.nominator) : "not found"}</dd>
      ${s ? `<dt>Oath</dt><dd>${esc(s.category)}${s.oath_date ? " · " + fmt(s.oath_date) : ""} · before the President: ${esc(s.oath_before_president)}</dd>` : ""}
      <dt>Own party ties and offices</dt><dd>${partyHistory(j)}</dd>
      ${j.tribunal_page ? `<dt>Official page</dt><dd><a href="${esc(j.tribunal_page)}" target="_blank" rel="noopener">trybunal.gov.pl</a></dd>` : ""}</dl>`;
    d.scrollIntoView({ block: "nearest" });
  }
  $("#colourby").addEventListener("change", draw); $("#from").addEventListener("change", draw);
  const nl = C.elected_not_on_lists;
  $("#notinoffice").innerHTML = `The Sejm also elected ${nl.length} people who are not on the Tribunal's lists: ${nl.map(n => esc(n.name)).join(", ")}. See the Elections view and the disputes since 2015.`;
  draw();

  // ---- elections table ----
  function drawElect() {
    const only = $("#onlyvote").checked;
    const rows = [];
    for (const j of C.judges) if (j.election) rows.push({ name: j.name, e: j.election, listed: true });
    for (const n of nl) rows.push({ name: n.name, e: { date: n.election_date, resolution: n.resolution, vote_kind: n.vote_kind, yes: n.yes, no: n.no, abstain: n.abstain, votes_by_club: n.votes_by_club, nominator: n.nominator, group_vote: false }, listed: false });
    rows.sort((a, b) => a.e.date < b.e.date ? 1 : a.e.date > b.e.date ? -1 : 0);
    const shown = rows.filter(r => !only || r.e.yes != null || r.e.vote_kind === "ON_LIST");
    $("#electtable").innerHTML = `<thead><tr><th>Date</th><th>Judge</th><th class="num">For</th><th class="num">Against</th><th class="num">Abst.</th><th>By club (for-against-abstain)</th><th>Nominated by</th></tr></thead><tbody>` +
      shown.map(r => `<tr><td>${fmt(r.e.date)}</td><td>${esc(r.name)}${r.listed ? "" : ' <span class="badge warn">not on the Tribunal\'s lists</span>'}</td>` +
        (r.e.vote_kind === "ON_LIST" ? `<td colspan="3" class="muted">secret ballot</td>` : `<td class="num">${r.e.yes ?? ""}</td><td class="num">${r.e.no ?? ""}</td><td class="num">${r.e.abstain ?? ""}</td>`) +
        `<td>${esc(r.e.votes_by_club || "")}</td><td>${esc(r.e.nominator || "")}</td></tr>`).join("") + "</tbody>";
  }
  $("#onlyvote").addEventListener("change", drawElect); drawElect();

  // ---- disputes chronology ----
  let phase = "all";
  const phases = ["all"].concat(H.meta.phases);
  $("#phases").innerHTML = phases.map(p => `<button class="chip" data-p="${esc(p)}" aria-pressed="${p === "all"}">${esc(p === "all" ? "All" : p.replace(/^\d+ /, ""))}</button>`).join("");
  $("#phases").addEventListener("click", ev => { const b = ev.target.closest("button"); if (!b) return; phase = b.dataset.p; document.querySelectorAll("#phases button").forEach(x => x.setAttribute("aria-pressed", x === b)); drawEvents(); });
  function drawEvents() {
    const ev = H.events.filter(e => phase === "all" || e.phase === phase).sort((a, b) => a.date < b.date ? -1 : 1);
    $("#events").innerHTML = ev.map(e => `<div class="ev"><div class="d">${fmt(e.date)} · ${esc(e.phase.replace(/^\d+ /, ""))} · ${esc(e.actors)}</div>
      <div class="t">${esc(e.event)}${e.status !== "verified" ? `<span class="badge warn">${esc(e.status)}</span>` : ""}${e.source_type === "news" ? '<span class="badge warn">news report</span>' : ""}</div>
      <div>${esc(e.description)}</div>
      <div class="muted" style="font-size:12.5px">Source: ${e.source_url ? `<a href="${esc(e.source_url)}" target="_blank" rel="noopener">${esc(e.document)}</a>` : esc(e.document)}</div></div>`).join("");
  }
  drawEvents();

  // ---- rulings and activity (window.TKR, from rulings-data.js) ----
  const RR = window.TKR;
  if (RR) {
    const idx = RR.index, A = RR.activity;
    const OUTCOME_LAB = { unconstitutional: "Unconstitutional", mixed: "Mixed (some provisions)", constitutional: "Constitutional", discontinued: "Proceedings discontinued", other: "Other" };
    const OUT_COL = { unconstitutional: "#8B0000", mixed: "#C9732B", constitutional: "#4B6A88", discontinued: "#B9B9B2", other: "#E4E4DF" };
    const SER_COL = { K: "#8B0000", P: "#4B6A88", SK: "#2E7D6B", U: "#C9732B", Kp: "#7A5C99", Kpt: "#141414", W: "#999", S: "#BBB", Pp: "#B8860B" };
    const KIND_LAB = { judgment: "Judgment", order: "Order", signal: "Signal", decision: "Decision", other: "Other", pending: "Pending" };
    const judgeName = id => idx.judges[id] || id;
    const complete = idx.meta.years_complete;
    const allYears = complete.length && complete[0] === 1997 && complete[complete.length - 1] - 1997 + 1 === complete.length;
    $("#rulpartial").textContent = allYears
      ? `Every case filed from ${complete[0]} to ${complete[complete.length - 1]} is collected from the Tribunal's portal, with the metadata and outcome of each ruling (not the texts). Cases filed before 1997 are not collected yet, so the earliest years of rulings are slightly low (the Tribunal counts 33 judgments in 1998; this page has 32).`
      : `Cases are collected from the Tribunal's portal year by year. Filed in ${complete[0]}–${complete[complete.length - 1]} so far. A ruling can concern a case filed earlier, so counts of rulings by year are partial until every year from 1997 is collected.`;
    $("#actpartial").textContent = $("#rulpartial").textContent;

    // all cases in one list
    const all = [];
    const arr = v => v == null ? [] : Array.isArray(v) ? v : [v];   // a one-element list is exported as a bare value
    for (const k of Object.keys(RR.cases)) for (const c of RR.cases[k].cases) { c.b = arr(c.b); c.r = arr(c.r); c.x = arr(c.x); all.push(c); }
    all.sort((a, b) => (b.d || "9999") < (a.d || "9999") ? -1 : 1);
    const yrs = [...new Set(all.map(c => c.d && +c.d.slice(0, 4)).filter(Boolean))].sort();
    const fill = (id, opts, sel) => { $(id).innerHTML = opts.map(([v, t]) => `<option value="${esc(v)}"${v === sel ? " selected" : ""}>${esc(t)}</option>`).join(""); };
    fill("#r-y0", yrs.map(y => [y, y]), yrs[0]); fill("#r-y1", yrs.map(y => [y, y]), yrs[yrs.length - 1]);
    fill("#r-series", [["", "all"], ...Object.keys(idx.meta.series).map(k => [k, k + " · " + idx.meta.series[k]])], "");
    fill("#r-out", [["", "all"], ...idx.meta.outcomes.map(o => [o, OUTCOME_LAB[o]])], "");
    const apps = [...new Set(all.map(c => c.a))].sort();
    fill("#r-app", [["", "all"], ...apps.map(a => [a, a])], "");
    const js = Object.keys(idx.judges).sort((a, b) => idx.judges[a].localeCompare(idx.judges[b], "pl"));
    fill("#r-judge", [["", "anyone"], ...js.map(j => [j, idx.judges[j]])], "");
    let shown = 200;
    function drawRul() {
      const y0 = +$("#r-y0").value, y1 = +$("#r-y1").value, sr = $("#r-series").value, oc = $("#r-out").value, ap = $("#r-app").value, jd = $("#r-judge").value;
      const lead = $("#r-lead").checked, dis = $("#r-diss").checked;
      const rows = all.filter(c => (!c.d ? false : (+c.d.slice(0, 4) >= y0 && +c.d.slice(0, 4) <= y1)) && (!sr || c.s === sr) && (!oc || c.o === oc) && (!ap || c.a === ap) &&
        (!jd || (c.b || []).includes(jd)) && (!lead || !c.lead) && (!dis || (c.x || []).length));
      const cnt = {}; rows.forEach(r => { cnt[r.o || "—"] = (cnt[r.o || "—"] || 0) + 1; });
      $("#r-sum").innerHTML = `<span><b>${rows.length}</b> ${lead ? "rulings" : "cases"}</span>` + Object.keys(OUTCOME_LAB).filter(o => cnt[o]).map(o => `<span><i style="background:${OUT_COL[o]}"></i>${OUTCOME_LAB[o]} ${cnt[o]}</span>`).join("");
      const vis = rows.slice(0, shown);
      $("#r-table").innerHTML = `<thead><tr><th>Decided</th><th>Case</th><th>Kind</th><th>Applicant</th><th>Outcome</th><th>Presiding</th><th class="num">Bench</th><th>Dissent</th><th class="num">Days</th></tr></thead><tbody>` +
        vis.map(r => `<tr><td>${fmt(r.d)}</td><td><a href="https://ipo.trybunal.gov.pl/ipo/Sprawa?sygnatura=${encodeURIComponent(r.sig)}" target="_blank" rel="noopener">${esc(r.sig)}</a>${r.lead ? ` <span class="badge">with ${esc(r.lead)}</span>` : ""}<div class="muted" style="font-size:12px">${esc(r.subj || "")}</div></td>` +
          `<td>${KIND_LAB[r.k] || ""}</td><td>${esc(r.a || "")}</td><td>${r.o ? `<i style="display:inline-block;width:9px;height:9px;border-radius:2px;background:${OUT_COL[r.o]};margin-right:5px"></i>${OUTCOME_LAB[r.o]}` : ""}</td>` +
          `<td>${esc(judgeName(r.pres) || "")}</td><td class="num">${(r.b || []).length || ""}</td><td>${(r.x || []).map(judgeName).map(esc).join(", ")}</td><td class="num">${r.lag ?? ""}</td></tr>`).join("") + "</tbody>";
      $("#r-more").innerHTML = rows.length > shown ? `Showing ${shown} of ${rows.length}. <a href="#" id="r-showall">Show more</a>` : "";
      const m = $("#r-showall"); if (m) m.addEventListener("click", e => { e.preventDefault(); shown += 400; drawRul(); });
    }
    ["#r-y0", "#r-y1", "#r-series", "#r-out", "#r-app", "#r-judge", "#r-lead", "#r-diss"].forEach(id => $(id).addEventListener("change", () => { shown = 200; drawRul(); }));
    drawRul();

    // ---- charts ----
    const W = 1000, Hh = 210, m = { l: 44, r: 10, t: 10, b: 24 };
    function frame(el, years, ymax, fmtY) {
      const s = svg("svg", { viewBox: `0 0 ${W} ${Hh}`, role: "img" });
      const x = i => m.l + (i + 0.5) * (W - m.l - m.r) / years.length;
      const bw = (W - m.l - m.r) / years.length * 0.7;
      const y = v => Hh - m.b - v / ymax * (Hh - m.t - m.b);
      const ticks = 4; for (let t = 0; t <= ticks; t++) { const v = ymax * t / ticks, yy = y(v);
        s.appendChild(svg("line", { x1: m.l, x2: W - m.r, y1: yy, y2: yy, stroke: "#ECECEC" }));
        const tx = svg("text", { x: m.l - 6, y: yy + 4, "text-anchor": "end", "font-size": 11, fill: "#767676" }); tx.textContent = (fmtY || (v => Math.round(v)))(v); s.appendChild(tx); }
      years.forEach((yy, i) => { if (years.length < 18 || i % 2 === 0) { const t = svg("text", { x: x(i), y: Hh - 7, "text-anchor": "middle", "font-size": 11, fill: "#767676" }); t.textContent = yy; s.appendChild(t); } });
      el.innerHTML = ""; el.appendChild(s); return { s, x, y, bw };
    }
    const hatch = (s, id, colour) => { let d = s.querySelector("defs"); if (!d) { d = svg("defs"); s.appendChild(d); }
      const p = svg("pattern", { id, width: 6, height: 6, patternUnits: "userSpaceOnUse", patternTransform: "rotate(45)" });
      p.appendChild(svg("rect", { width: 6, height: 6, fill: "#fff" })); p.appendChild(svg("rect", { width: 3, height: 6, fill: colour })); d.appendChild(p); return `url(#${id})`; };
    function stacked(el, rows, keys, cols, partialOf, labels) {
      const years = rows.map(r => r.year), tot = rows.map(r => keys.reduce((a, k) => a + (r.v[k] || 0), 0)), ymax = Math.max(1, ...tot) * 1.08;
      const f = frame(el, years, ymax);
      rows.forEach((r, i) => { let acc = 0; keys.forEach(k => { const v = r.v[k] || 0; if (!v) return;
        const y1 = f.y(acc + v), y0 = f.y(acc), fillc = partialOf(r) ? hatch(f.s, `h-${el.id}-${k}`, cols[k]) : cols[k];
        const rc = svg("rect", { x: f.x(i) - f.bw / 2, y: y1, width: f.bw, height: Math.max(0.5, y0 - y1), fill: fillc, stroke: partialOf(r) ? cols[k] : "none", "stroke-width": 0.5 });
        const t = svg("title"); t.textContent = `${r.year} · ${labels ? labels[k] : k}: ${v}${partialOf(r) ? " (partial)" : ""}`; rc.appendChild(t); f.s.appendChild(rc); acc += v; }); });
      const lg = el.appendChild(el.ownerDocument.createElement("div")); lg.className = "legend"; lg.style.margin = "2px 8px 6px";
      lg.innerHTML = keys.map(k => `<span><i style="background:${cols[k]}"></i>${esc(labels ? labels[k] : k)}</span>`).join("");
    }
    function lines(el, rows, defs, ymax, fmtY, partialOf) {
      const years = rows.map(r => r.year), f = frame(el, years, ymax, fmtY);
      defs.forEach(d => { let path = "", pen = false; rows.forEach((r, i) => { const v = d.get(r); if (v == null || isNaN(v)) { pen = false; return; }
          path += (pen ? "L" : "M") + f.x(i) + " " + f.y(v) + " "; pen = true; });
        f.s.appendChild(svg("path", { d: path, fill: "none", stroke: d.col, "stroke-width": 1.8, "stroke-dasharray": d.dash || "" }));
        rows.forEach((r, i) => { const v = d.get(r); if (v == null || isNaN(v)) return; const c = svg("circle", { cx: f.x(i), cy: f.y(v), r: 3, fill: partialOf && partialOf(r) ? "#fff" : d.col, stroke: d.col });
          const t = svg("title"); t.textContent = `${r.year} · ${d.name}: ${(fmtY || (z => z))(v)}`; c.appendChild(t); f.s.appendChild(c); }); });
      const lg = el.appendChild(el.ownerDocument.createElement("div")); lg.className = "legend"; lg.style.margin = "2px 8px 6px";
      lg.innerHTML = defs.map(d => `<span><i style="background:${d.col}"></i>${esc(d.name)}</span>`).join("");
    }
    const Y = A.years;
    stacked($("#c-filed"), Y.map(y => ({ year: y.year, v: y.filed || {}, p: y.partial_filed })), Object.keys(idx.meta.series), SER_COL, r => r.p, idx.meta.series);
    stacked($("#c-rulings"), Y.map(y => ({ year: y.year, v: y.rulings || {}, p: y.partial_rulings })), idx.meta.outcomes, OUT_COL, r => r.p, OUTCOME_LAB);
    lines($("#c-cohort"), Y.filter(y => y.cohort && !y.partial_filed && y.cohort.followed_2y).map(y => ({ year: y.year, c: y.cohort })),
      [{ name: "decided within 1 year", col: "#4B6A88", get: r => r.c.within_1y }, { name: "decided within 2 years", col: "#8B0000", get: r => r.c.within_2y }, { name: "still pending now", col: "#767676", dash: "4 3", get: r => r.c.pending }], 1, v => Math.round(v * 100) + "%");
    lines($("#c-diss"), Y.filter(y => y.dissent).map(y => ({ year: y.year, d: y.dissent, p: y.partial_rulings })), [{ name: "share of judgments with a dissent (open circles: partial years)", col: "#8B0000", get: r => r.d.share_dissent }], 1, v => Math.round(v * 100) + "%", r => r.p);
    const P = A.long.publication.map(p => ({ year: p.y, p })), pmax = Math.max(...P.map(r => r.p.max_delay)) * 1.05;
    lines($("#c-pub"), P, [{ name: "median days", col: "#4B6A88", get: r => r.p.median_delay }, { name: "longest", col: "#8B0000", dash: "4 3", get: r => r.p.max_delay }], pmax, v => Math.round(v));
    lines($("#c-vac"), A.long.vacancy.map(v => ({ year: v.year, v })), [{ name: "mean seats vacant", col: "#141414", get: r => r.v.mean_vacant }], 6, v => v.toFixed(0));
  }
})();
