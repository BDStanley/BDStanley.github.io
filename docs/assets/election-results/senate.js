// The Senate: the view of an election of type "senate" (senate-<year>.json) and of the senators' careers
// (people.json). Loaded after app.js and uses its globals (E, G, CUR, EL, S, the formatting helpers, the
// cartogram). A Senate election has candidates, not lists, each standing in one district, so it has its
// own view instead of the list-based sections: seats by party, a map of the districts (or, where PKW
// has gmina-level files, of the gminas) coloured by who won, a table of the districts, each district's
// candidates, and the careers of the senators across the Sejm, the Senate and the European Parliament.
// Data: senate-<year>.json {facts, districts[{okreg, name, shape, seats, electorate, valid, gap, units,
// candidates[{id, person, name, label, votes, pct, elected, u, ...}]}]} and people.json {parties, persons,
// moves}, both written by "Election results/analysis/92_senate_export.R".
"use strict";

const SENATE_TERM = { 1991: 2, 1993: 3, 1997: 4, 2001: 5, 2005: 6, 2007: 7, 2011: 8, 2015: 9, 2019: 10, 2023: 11 };
const BODY_NAME = { sejm: "Sejm", senate: "Senate", ep: "European Parliament" };
const BODY_COL = { sejm: "#8B0000", senate: "#a15c00", ep: "#00796b" };
const ROMAN = ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI"];
const SS = { measure: "winner", level: "district", shape: "map", sel: null, q: "", sort: "no", asc: true, person: null };
let SJACT = null, EPACT = null, PEOPLE = null, PEOPLE_BY = new Map(), SEJMP = null, GMN = null;
const personById = id => PEOPLE_BY.get(id) || (SEJMP && SEJMP.get(id)) || null;

const senateOn = () => !!EL && EL.type === "senate" && !!CUR && !!CUR.districts;
const labCol = l => E.labels[l] || E.labels.Other || "#9E9E9E";
const partyOf = id => (PEOPLE && PEOPLE.parties[id]) || null;
const partyCol = id => { const p = partyOf(id); return p ? labCol(p.label) : "#9E9E9E"; };
const partyName = id => { if (id === "unresolved") return "party unknown"; const p = partyOf(id); return p ? (p.abbrev || p.name) : "–"; };
const yr = s => s ? s.slice(0, 4) : "";
const partyLink = id => { const t = esc(partyName(id)); if (!id || id === "unresolved" || !(E.genealogy_ids || []).includes(id)) return t;
  const p = partyOf(id); return `<a href="${PAGES}party-genealogy.html#pt=${encodeURIComponent(id)}" target="_top" title="${esc(p ? p.name : "")} in the party genealogy">${t}</a>`; };
const dist = okreg => CUR.districts.find(d => d.okreg === okreg);
// PKW's 2015 file names no district: the commission's seat from the geometry set (2023's names) stands in
const dname = d => { if (d.name) return d.name; const set = G.sets[CUR.facts.rules.set], i = set ? set.okreg.indexOf(d.okreg) : -1; return i >= 0 && set.name[i] ? set.name[i] : ""; };
const topCand = d => d.candidates[0];
const winners = d => d.candidates.filter(c => c.elected);
const basisOf = d => { const v = sum(d.candidates.map(c => c.votes)); return d.seats > 1 ? (d.valid || v / d.seats) : v; };
const marginOf = d => isNum(d.gap) ? d.gap / basisOf(d) : null;
const dot = col => `<span class="dot" style="background:${col}"></span>`;

async function loadPeople() {
  if (PEOPLE) return PEOPLE;
  try {
    const r = await fetch("people.json"); if (!r.ok) throw new Error(r.status);
    PEOPLE = await r.json();
    PEOPLE_BY = new Map(PEOPLE.persons.map(p => [p.id, p]));
  } catch (err) { console.warn("people.json unavailable:", err); PEOPLE = { parties: {}, persons: [], moves: [] }; }
  try {
    const r = await fetch("ep-activity.json"); if (!r.ok) throw new Error(r.status);
    EPACT = await r.json();
  } catch (err) { console.warn("ep-activity.json unavailable:", err); EPACT = { meta: {}, persons: {} }; }
  try {
    const r = await fetch("sejm-activity.json"); if (!r.ok) throw new Error(r.status);
    SJACT = await r.json();
  } catch (err) { console.warn("sejm-activity.json unavailable:", err); SJACT = { meta: {}, persons: {} }; }
  return PEOPLE;
}

async function loadSejmPeople() {
  if (SEJMP) return SEJMP;
  try {
    const r = await fetch("people-sejm.json"); if (!r.ok) throw new Error(r.status);
    SEJMP = new Map((await r.json()).persons.map(p => [p.id, p]));
  } catch (err) { console.warn("people-sejm.json unavailable:", err); SEJMP = new Map(); }
  return SEJMP;
}

// ---- switching the page between the list-based sections and the Senate's ------------------------
const STD_SECTIONS = ["results", "mapsec", "whosec"];
const JUMP_STD = () => $("jump").dataset.std;
function setSenateMode(on) {
  const j = $("jump"); if (!j.dataset.std) j.dataset.std = j.innerHTML;
  STD_SECTIONS.forEach(id => { $(id).hidden = on; });
  ["senatesec", "senatedist", "senatemove"].forEach(id => { $(id).hidden = !on; });
  if (on) $("senatedist").after($("membersec")); else $("results").after($("membersec"));
  j.innerHTML = on
    ? '<a href="#glance">At a glance</a><a href="#senatesec">Map</a><a href="#senatedist">Districts</a><a href="#membersec">Members</a><a href="#senatemove">Movement</a><a href="#allsec">All elections</a><a href="#data">Data</a>'
    : j.dataset.std;
  const cards = document.querySelectorAll("#data .card"); if (cards[0]) cards[0].hidden = on ? false : (CUR && CUR.facts.facts_only);
  ["csvresults", "csvgmina", "csvokreg"].forEach(id => { if ($(id)) $(id).hidden = on; });
  if ($("csvnational")) $("csvnational").textContent = on ? "Candidates (CSV)" : "National results";
  document.querySelectorAll('#glance [data-png], #mapsec [data-png]').forEach(b => { b.hidden = on; });
}

function renderSenateAll() {
  setSenateMode(true);
  $("profilecard").hidden = true;
  renderStrip(); senateGlance(); senateParties(); senateMapControls(); senateGmina(); senateMap(); senateCard(); senateTable(); senateMovement(); renderMembers();
  renderAllElections(); renderData();
  senateData();
  saveHash();
}

// ---- at a glance: the seats, and the facts -------------------------------------------------------
function senateGlance() {
  const f = CUR.facts, seats = f.seats_by_label.filter(s => s.seats > 0);
  $("glancetype").textContent = "Senate" + (EL.early ? " · early" : "");
  $("h-glance").textContent = `${f.name_en}, ${f.year}`;
  $("glancesub").textContent = f.name_pl;
  const camp = l => E.camp_order.indexOf(E.label_camps[l] || E.palette_camps[l] || "Other");
  const items = seats.slice().sort((a, b) => camp(a.label) - camp(b.label) || b.votes - a.votes), total = sum(items.map(i => i.seats));
  const rows = 5, pts = hemiLayout(total, rows), rad = Math.min(0.42 / (rows - 1), 0.046 * Math.sqrt(460 / Math.max(total, 1)));
  const o = []; let k = 0;
  items.forEach(it => { for (let j = 0; j < it.seats; j++, k++) { const p = pts[k]; o.push(`<circle cx="${p.x.toFixed(3)}" cy="${(-p.y).toFixed(3)}" r="${rad.toFixed(3)}" fill="${labCol(it.label)}"/>`); } });
  const top = items.slice().sort((a, b) => b.seats - a.seats)[0], maj = Math.floor(total / 2) + 1;
  o.push(`<line x1="0" x2="0" y1="-0.86" y2="-2.12" stroke="#141414" stroke-width="0.012" stroke-dasharray="0.04 0.04"/><text x="0" y="-2.16" text-anchor="middle" font-size="0.085" fill="#4B4B4B">${maj}</text>`);
  o.push(`<text x="0" y="-0.36" text-anchor="middle" font-size="0.2" fill="#141414">${total} seats</text>`);
  if (top) o.push(`<text x="0" y="-0.14" text-anchor="middle" font-size="0.085" fill="#767676">largest: ${esc(top.label)}, ${top.seats}${top.seats >= maj ? " (a majority)" : " (no majority)"}</text>`);
  const legend = items.map(it => `<span>${dot(labCol(it.label))}${esc(it.label)} <b>${it.seats}</b></span>`).join("");
  $("glancetitle").textContent = "Seats in the Senate";
  $("glancechart").innerHTML = `<svg id="glancesvg" viewBox="-2.2 -2.3 4.4 2.38" role="img" aria-label="Seats won by each party" font-family="${FONT}">${o.join("")}</svg><div class="legend" style="margin:4px 8px 4px">${legend}</div>`;
  $("glancenote").textContent = "Seats from left to right by camp (as in the site's party colours), then by votes. A senator takes the colour of the party whose committee nominated them; a committee of electors is grey.";
  const r = f.rules, h = [];
  h.push(`<span class="label">Facts</span><div style="font-size:13px">${esc(dday(f.date))}${f.early ? '<span class="badge">Early</span>' : ""}</div>`);
  const rows2 = [["Seats", r.seats], ["Districts", `${r.constituencies}${r.set_label ? ` (${esc(r.set_label)})` : ""}`],
    ["System", r.system === "block" ? "Block voting: each voter could vote for as many candidates as the district has seats" : "One senator per district, first past the post"],
    ["How the winners are found", r.formula]];
  h.push(`<dl class="facts">${rows2.map(([k2, v]) => `<dt>${k2}</dt><dd>${esc(v)}</dd>`).join("")}</dl>`);
  h.push(`<p class="note">${f.unit_level === "gmina" ? "PKW's files give each candidate's votes by gmina (polling districts added up), so the map can show every gmina." : f.unit_level === "powiat" ? "PKW gives each candidate's votes by powiat only: each powiat is one unit, covering its gminas." : "PKW's files give candidates' votes by district only, so the map shows districts."}</p>`);
  if (f.note) h.push(`<p class="note">${esc(f.note)}</p>`);
  if (f.shares_basis) h.push(`<p class="note">A candidate's share is of the district's ${esc(f.shares_basis)}.</p>`);
  h.push(`<span class="label" style="margin-top:6px">Sources</span><ul class="src">${(f.sources || []).map(s => `<li>${s.url ? `<a href="${esc(s.url)}" target="_blank" rel="noopener">${esc(s.label)}</a>` : esc(s.label)}</li>`).join("")}</ul>`);
  $("facts").innerHTML = h.join("");
}

// ---- seats by party -------------------------------------------------------------------------------
function senateParties() {
  const f = CUR.facts, rows = f.seats_by_label.slice().sort((a, b) => b.seats - a.seats || b.votes - a.votes);
  const ind = rows.find(r => r.label === "Independent");
  $("senatelede").textContent = `${f.rules.constituencies} districts, ${f.rules.seats} seats. Each candidate stands in one district, so there is no national vote for a party: the votes below are the sum of its candidates' votes in all districts${f.rules.system === "block" ? ", and a voter could vote for several candidates, so shares are of the votes cast, not of the voters" : ""}.${ind && ind.seats ? ` "Independent" is a committee that names no party, such as a committee of electors.` : ""}`;
  $("spartytable").innerHTML = `<thead><tr><th class="l">Party</th><th>Seats</th><th>Candidates</th><th>Votes</th><th>Share of candidate votes</th></tr></thead><tbody>` +
    rows.map(r => `<tr><td class="l">${dot(labCol(r.label))} ${esc(r.label)}</td><td><b>${r.seats}</b></td><td>${r.candidates}</td><td>${n0(r.votes)}</td><td>${pct(r.share)}</td></tr>`).join("") + "</tbody>";
}

// ---- the map ---------------------------------------------------------------------------------------
function unitLevelOn() { return !!CUR.facts.unit_level && CUR.districts.some(d => d.units); }
function senateMapControls() {
  if (SS.level === "gmina" && !unitLevelOn()) SS.level = "district";
  $("smeasure").innerHTML = [["winner", "Winner"], ["margin", "Margin"]].map(([k, t]) => `<button type="button" data-k="${k}" aria-pressed="${SS.measure === k}">${t}</button>`).join("");
  const lv = CUR.facts.unit_level === "powiat" ? "Powiat" : "Gmina";
  $("slevel").innerHTML = [["district", "District"], ["gmina", lv]].map(([k, t]) => `<button type="button" data-k="${k}" aria-pressed="${SS.level === k}" ${k === "gmina" && !unitLevelOn() ? 'disabled title="PKW has no files below the district for this election"' : ""}>${t}</button>`).join("");
  $("sshape").innerHTML = [["map", "Map"], ["cart", "Cartogram"]].map(([k, t]) => `<button type="button" data-k="${k}" aria-pressed="${SS.shape === k}">${t}</button>`).join("");
  $("slevelctrl").hidden = SS.shape === "cart";
}
// each gmina's unit: the leader and the margin, for the map of gminas
function senateGmina() {
  GMN = null;
  if (!unitLevelOn()) return;
  const n = G.gminas.teryt.length, best = new Array(n).fill(null);
  CUR.districts.forEach(d => {
    if (!d.units) return;
    d.units.g.forEach((gs, k) => {
      const valid = d.units.valid[k] || 0;
      let a = -1, av = -1, bv = -1;
      d.candidates.forEach((c, j) => { const v = c.u[k]; if (v > av) { bv = av; av = v; a = j; } else if (v > bv) bv = v; });
      if (a < 0 || av <= 0) return;
      gs.forEach(gi => { if (!best[gi] || valid > best[gi].valid) best[gi] = { d: d.okreg, k, c: a, valid, share: valid ? av / valid : null, margin: valid ? (av - Math.max(bv, 0)) / valid : null }; });
    });
  });
  GMN = best;
}
const tint = (col, m) => mix(col, clamp(0.28 + 0.72 * (isNum(m) ? m / 0.4 : 1), 0.28, 1));
function distFill(d) {
  const c = topCand(d), col = labCol(c.label);
  return SS.measure === "margin" ? tint(col, marginOf(d)) : col;
}
function senateMap() {
  const set = G.sets[CUR.facts.rules.set], svg = $("smap"), [W, H] = G.meta.view, ds = CUR.districts, byShape = new Map();
  ds.forEach(d => { const s = d.shape || d.okreg; (byShape.get(s) || byShape.set(s, []).get(s)).push(d); });
  const defs = [];
  let html = "";
  const sel = SS.sel;
  const hl = okreg => okreg === sel ? ' stroke="#141414" stroke-width="3"' : "";
  if (SS.shape === "cart") {
    const cv = set.circle_view, cc = cartCircles(CUR.facts.rules.set);
    svg.classList.add("cons"); svg.setAttribute("viewBox", [cv[0] - 30, cv[1] - 30, cv[2] + 60, cv[3] + 60].join(" "));
    html = `<path d="${cartOutlineOf(CUR.facts.rules.set).d}" fill="none" stroke="#141414" stroke-width="2.5" stroke-linejoin="round" pointer-events="none"/>`;
    cc.forEach((c, i) => {
      const d = dist(set.okreg[i]); if (!d) return;
      const w = winners(d), r = c.r - 1.2;
      if (d.seats <= 1 || w.length <= 1) html += `<circle class="u" cx="${c.x}" cy="${c.y}" r="${r}" fill="${distFill(d)}" stroke="#fff" stroke-width="1.6" data-d="${d.okreg}"/>`;
      else {   // a pie, one slice for each seat won
        let a0 = -Math.PI / 2;
        w.forEach(x => {
          const a1 = a0 + 2 * Math.PI / w.length, p = (a, rr) => `${(c.x + rr * Math.cos(a)).toFixed(1)} ${(c.y + rr * Math.sin(a)).toFixed(1)}`;
          html += `<path class="u" d="M${c.x} ${c.y} L${p(a0, r)} A${r} ${r} 0 ${a1 - a0 > Math.PI ? 1 : 0} 1 ${p(a1, r)} Z" fill="${labCol(x.label)}" stroke="#fff" stroke-width="1.2" data-d="${d.okreg}"/>`;
          a0 = a1;
        });
      }
      if (d.okreg === sel) html += `<circle cx="${c.x}" cy="${c.y}" r="${r}" fill="none" stroke="#141414" stroke-width="3" pointer-events="none"/>`;
    });
  } else {
    svg.classList.add("cons"); svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
    if (SS.level === "gmina" && GMN) {
      const fb = gi => { const s = set.members[gi], g = (byShape.get(s) || [])[0]; return g ? distFill(g) : "#EDEDED"; };
      html = G.gminas.d.map((p, gi) => {
        const b = GMN[gi]; let fill;
        if (b) { const d = dist(b.d), col = labCol(d.candidates[b.c].label); fill = SS.measure === "margin" ? tint(col, b.margin) : col; } else fill = fb(gi);
        return `<path class="u" d="${p}" fill="${fill}" data-g="${gi}"/>`;
      }).join("");
      html += set.d.map((p, i) => p ? `<path d="${p}" fill="none" stroke="#141414" stroke-width="${set.okreg.length > 60 ? 0.9 : 1.6}" stroke-linejoin="round" pointer-events="none"/>` : "").join("");
    } else {
      html = set.d.map((p, i) => {
        if (!p) return "";
        const group = byShape.get(set.okreg[i]) || [], fills = group.map(distFill);
        let fill = fills[0];
        if (group.length > 1) {
          const id = "sg" + set.okreg[i], n = fills.length;
          defs.push(`<linearGradient id="${id}" x1="0" x2="1" y1="0" y2="0">${fills.map((c, k) => `<stop offset="${k / n}" stop-color="${c}"/><stop offset="${(k + 1) / n}" stop-color="${c}"/>`).join("")}</linearGradient>`);
          fill = `url(#${id})`;
        }
        return `<path class="u" d="${p}" fill="${fill}" stroke="#fff" stroke-width="${set.okreg.length > 60 ? 0.8 : 1.4}" data-d="${set.okreg[i]}" tabindex="0" role="button" aria-label="${esc(distTitle(group[0] || {}))}"/>`;
      }).join("");
    }
    if (sel != null) { const d = dist(sel), s = d && (d.shape || d.okreg), k = set.okreg.indexOf(s); if (k >= 0 && set.d[k]) html += `<path d="${set.d[k]}" fill="none" stroke="#141414" stroke-width="3" pointer-events="none"/>`; }
  }
  svg.setAttribute("font-family", FONT);
  svg.innerHTML = (defs.length ? `<defs>${defs.join("")}</defs>` : "") + html;
  // legend: the parties that won, and the margin scale
  const used = [...new Set(ds.flatMap(d => winners(d).map(c => c.label)))].sort((a, b) => sum(CUR.facts.seats_by_label.filter(s => s.label === b).map(s => s.seats)) - sum(CUR.facts.seats_by_label.filter(s => s.label === a).map(s => s.seats)));
  $("slegend").innerHTML = `<div class="legend">${used.map(l => `<span>${dot(labCol(l))}${esc(l)}</span>`).join("")}${SS.measure === "margin" ? '<span class="muted">Paler = a closer result (the winner\'s lead over the runner-up, up to 40% of the votes)</span>' : ""}</div>`;
  const notes = [];
  if (SS.shape === "cart") notes.push("One circle for each district, sized by its electorate" + (set.okreg.length > 1 ? "; a district with several seats is divided into one slice for each seat won." : "."));
  else if (SS.level === "gmina" && GMN) notes.push(`Each gmina${CUR.facts.unit_level === "powiat" ? " (here a powiat, covering its gminas)" : ""} is coloured by the candidate with the most votes in it; the lines are the district boundaries. A gmina shared by several districts (Warsaw, Wrocław, Łódź, Kraków) takes the district with the most voters.`);
  else notes.push(`${set.label}. ${ds.some(d => (d.shape || d.okreg) !== d.okreg) ? "Districts inside one city (Warsaw, Wrocław, Łódź, Kraków) share one shape here, split into bands by district; the cartogram shows each one." : ""}`);
  if (CUR.facts.rules.system === "block") notes.push("The colour is that of the candidate with the most votes; the winners of the other seats are in the district's card.");
  $("snote").textContent = notes.join(" ");
}
const distTitle = d => d.okreg ? `District ${d.okreg}${dname(d) ? ", " + dname(d) : ""}` : "";

// ---- a district's card ------------------------------------------------------------------------------
function candRow(c, d) {
  const per = c.person && personById(c.person);
  const nm = per ? `<a href="#" class="plink" data-p="${esc(c.person)}">${esc(c.name)}</a>` : esc(c.name);
  const sub = [c.party && c.party !== "none" ? c.party : null, c.occupation].filter(Boolean).join(" · ");
  return `<tr class="${c.elected ? "won" : ""}"><td class="l name">${dot(labCol(c.label))} ${nm}${c.elected ? ' <span class="badge">Elected</span>' : ""}<span class="full">${esc(c.label === "Independent" ? c.committee : c.label)}${sub ? " · " + esc(sub) : ""}</span></td><td>${n0(c.votes)}</td><td>${pct(c.pct)}</td></tr>`;
}
function senateCard() {
  const el = $("sdcard");
  if (SS.sel == null) { el.innerHTML = `<span class="label">District</span><p class="note">Click a district on the map, or a row in the table below, for its candidates and their votes.</p>`; if (SS.person) careerCard(SS.person); else $("scareer").hidden = true; return; }
  const d = dist(SS.sel); if (!d) { SS.sel = null; return senateCard(); }
  const t = sum(d.candidates.map(c => c.votes)), base = basisOf(d);
  const facts = [["Seats", d.seats], ["Electorate", d.electorate ? n0(d.electorate) : null], [d.seats > 1 ? "Valid ballots" : "Valid votes", d.valid ? n0(d.valid) : (d.seats > 1 ? null : n0(t))],
    [d.seats > 1 ? "Last winner's lead over the first loser" : "Winner's lead", isNum(d.gap) ? `${n0(d.gap)} votes (${pct(d.gap / base)})` : null]].filter(f => f[1] != null);
  el.innerHTML = `<span class="label">District ${d.okreg}</span><h3 style="margin:2px 0 4px;font-size:18px;font-weight:500">${esc(dname(d))}</h3>
    <dl class="facts">${facts.map(([k, v]) => `<dt>${k}</dt><dd>${esc(v)}</dd>`).join("")}</dl>
    <div class="scroll"><table class="t proftable"><thead><tr><th class="l">Candidate</th><th>Votes</th><th>${d.seats > 1 ? "of ballots" : "Share"}</th></tr></thead><tbody>${d.candidates.map(c => candRow(c, d)).join("")}</tbody></table></div>
    <button type="button" class="linkbtn" id="sdclose">Close</button>`;
  if (SS.person) careerCard(SS.person); else $("scareer").hidden = true;
}

// ---- a senator's career -----------------------------------------------------------------------------
const rom = n => ROMAN[n] || n;
function mandLabel(m) {
  const [body, term, s, e] = m;
  const span = `${yr(s)}${e ? "–" + yr(e) : "–"}`;
  return `${BODY_NAME[body]}${body === "ep" ? (m[5] === "delegation" ? " (observer)" : "") : " " + (body === "sejm" && term === 0 ? "1989–91" : rom(term))}, ${span}`;
}
function careerCard(id, el = $("scareer")) {
  const p = personById(id); if (!p) { el.hidden = true; return; }
  if (el.id === "scareer") SS.person = id;
  el.hidden = false;
  const ms = p.mandates, now = new Date().getFullYear() + 1;
  const y0 = Math.min(...ms.map(m => +yr(m[2]))), y1 = Math.max(...ms.map(m => m[3] ? +yr(m[3]) + 1 : now)), W = 300, L = 70, span = Math.max(1, y1 - y0);
  const X = y => L + (y - y0) / span * (W - L - 6), lanes = ["sejm", "senate", "ep"].filter(b => ms.some(m => m[0] === b));
  const bars = lanes.map((b, li) => `<text x="0" y="${li * 22 + 14}" font-size="10" fill="#4B4B4B">${BODY_NAME[b].replace("European Parliament", "EP")}</text>` +
    ms.filter(m => m[0] === b).map(m => { const a = X(+yr(m[2]) + (+m[2].slice(5, 7) - 1) / 12), z = X(m[3] ? +yr(m[3]) + (+m[3].slice(5, 7)) / 12 : now - 0.5);
      return `<rect x="${a.toFixed(1)}" y="${li * 22 + 4}" width="${Math.max(2, z - a).toFixed(1)}" height="14" rx="2" fill="${m[4] ? partyCol(m[4]) : "#BDBDBD"}" stroke="${BODY_COL[b]}" stroke-width="1.4"${m[8] ? ' stroke-dasharray="3 2"' : ""}><title>${esc(mandLabel(m))}${m[4] ? ", " + esc(partyName(m[4])) : ""}</title></rect>`; }).join("")).join("");
  const axis = [y0, y0 + Math.round(span / 2), y1].map(y => `<text x="${X(y).toFixed(1)}" y="${lanes.length * 22 + 12}" font-size="9" fill="#767676" text-anchor="middle">${y}</text>`).join("");
  const rows = ms.map(m => `<tr><td class="l">${esc(mandLabel(m))}${m[8] ? ' <span class="muted" title="A date on this row rests on Polish Wikipedia">*</span>' : ""}</td><td class="l">${m[4] ? dot(partyCol(m[4])) + " " + partyLink(m[4]) : '<span class="muted">no party</span>'}</td><td class="l muted" style="white-space:normal">${esc(({ election: "elected", by_election: "by-election", replacement: "replacement", delegation: "delegated" })[m[5]] || m[5] || "")}${m[6] && m[6] !== "term_end" ? ", " + esc(m[6].replace(/_/g, " ")) : ""}</td></tr>`).join("");
  const mv = (PEOPLE ? PEOPLE.moves : []).filter(x => x.person === id).map(x => {
    const fp = x.from_party, tp = x.to_party, ch = fp && tp && fp !== "unresolved" && tp !== "unresolved" && fp !== tp;
    return `<li>${esc(BODY_NAME[x.from])} → ${esc(BODY_NAME[x.to])}: ${x.kind === "concurrent" ? "at the same time" : x.kind === "direct" ? "straight on" : `after ${x.gap != null ? Math.round(x.gap / 365.25 * 10) / 10 + " years" : "a gap"}`}${ch ? `, ${dot(partyCol(fp))}${esc(partyName(fp))} → ${dot(partyCol(tp))}${esc(partyName(tp))}` : ""}</li>`;
  }).join("");
  const ea = EPACT && EPACT.persons[id], pc = x => x == null ? NA : Math.round(x * 100) + "%";
  const epv = ea && ea.votes && ea.votes.length ? `<span class="label" style="margin-top:6px">In the European Parliament: roll-call votes</span><div class="scroll"><table class="t proftable"><thead><tr><th class="l">Term</th><th>Votes</th><th>Voted</th><th>With group</th><th>With party</th></tr></thead><tbody>${ea.votes.map(v => `<tr><td class="l">${rom(v[0])}</td><td>${v[1].toLocaleString("en-GB")}</td><td>${pc(v[3])}</td><td>${pc(v[4])}</td><td>${pc(v[5])}</td></tr>`).join("")}</tbody></table></div><p class="note">Terms VI–VIII from VoteWatch Europe (Hix, Frantescu and Hagemann 2022), IX–X from HowTheyVote.eu; both secondary, compiled from the Parliament's roll-call results. Voted: share of roll-call votes held in the mandate on which the MEP voted for, against or abstained. With group, with party: share of votes cast that matched the plurality position of the EP group, or of the Polish MEPs of the same party.</p>` : "";
  const sa = SJACT && SJACT.persons[id];
  const sjv = sa && sa.length ? `<span class="label" style="margin-top:6px">In the Sejm: roll-call votes</span><div class="scroll"><table class="t proftable"><thead><tr><th class="l">Term</th><th>Votes</th><th>Voted</th><th>Against club</th><th>Speeches</th><th>Questions</th></tr></thead><tbody>${sa.map(v => `<tr><td class="l"><a href="${PAGES}legislative-activity.html#view=mps&t=${v[0]}&mp=${v[1]}" target="_top" title="Full profile on the Legislative activity page">${rom(v[0])}</a></td><td>${v[2].toLocaleString("en-GB")}</td><td>${pc(v[4])}</td><td>${v[6] == null ? NA : (v[6] * 100).toFixed(1) + "%"}</td><td>${v[7] == null ? NA : v[7].toLocaleString("en-GB")}</td><td>${v[8] == null ? NA : v[8].toLocaleString("en-GB")}</td></tr>`).join("")}</tbody></table></div><p class="note">From the Sejm's own API, via the Legislative activity page (the term links to the deputy's full profile). Voted: share of roll calls held in the mandate on which the deputy voted for, against or abstained. Against club: share of votes cast against the club's line. Speeches from term VI and questions from term VII.</p>` : "";
  const ROLE = { CHAIR: "chair", CHAIR_VICE: "vice-chair", MEMBER: "member", MEMBER_SUBSTITUTE: "substitute", OBSERVER: "observer" };
  const bdy = ea && ea.bodies && ea.bodies.length ? `<details style="margin-top:6px"><summary class="label" style="cursor:pointer">European Parliament committees and delegations (${ea.bodies.length})</summary><div class="scroll"><table class="t proftable"><tbody>${ea.bodies.map(b => `<tr><td class="l" style="white-space:normal"><span title="${esc(b[2])}">${esc(b[1])}</span> <span class="muted">${b[0] === "delegation" ? "delegation" : b[0] === "working_group" ? "working group" : "committee"}</span></td><td class="l">${esc(ROLE[b[3]] || b[3].toLowerCase().replace(/_/g, " "))}</td><td class="l muted">${yr(b[4])}${b[5] ? "–" + yr(b[5]) : "–"}</td></tr>`).join("")}</tbody></table></div><p class="note">From the European Parliament's open data. Hover an abbreviation for the full name.</p></details>` : "";
  el.innerHTML = `<span class="label">Career</span><h3 style="margin:2px 0 2px;font-size:18px;font-weight:500">${esc(p.name)}</h3>
    <div class="note">${p.born ? "Born " + (p.born.length > 4 ? dlong(p.born) : p.born) + (p.place ? ", " + esc(p.place) : "") : ""}</div>
    <svg viewBox="0 0 ${W} ${lanes.length * 22 + 18}" role="img" aria-label="Mandates in the Sejm, the Senate and the European Parliament over time" style="width:100%;max-width:${W}px;overflow:visible">${bars}${axis}</svg>
    <div class="scroll"><table class="t proftable"><tbody>${rows}</tbody></table></div>
    ${sjv}
    ${epv}
    ${bdy}
    ${mv ? `<span class="label" style="margin-top:6px">Moves between bodies</span><ul class="note" style="margin:2px 0 0;padding-left:16px">${mv}</ul>` : ""}
    <p class="note">Mandates from the Senate's own pages, the European Parliament's open data and Party tracker's Sejm records. A bar's colour is the party the mandate was won for; a dashed outline marks a date that rests on Polish Wikipedia. Bars start and end in the months the mandates did.</p>
    <button type="button" class="linkbtn careerclose" data-for="${el.id}">Close</button>`;
}

// ---- the districts' table -------------------------------------------------------------------------
function senateTable() {
  const q = SS.q.trim().toLowerCase();
  const rows = CUR.districts.map(d => ({ d, w: winners(d), top: topCand(d), m: marginOf(d) }))
    .filter(r => !q || dname(r.d).toLowerCase().includes(q) || String(r.d.okreg) === q || r.w.some(c => c.name.toLowerCase().includes(q)) || r.d.candidates.some(c => c.name.toLowerCase().includes(q)));
  const key = { no: r => r.d.okreg, name: r => dname(r.d), seats: r => r.d.seats, pct: r => r.top.pct || 0, margin: r => r.m == null ? -1 : r.m }[SS.sort];
  rows.sort((a, b) => { const x = key(a), y = key(b); return (x < y ? -1 : x > y ? 1 : 0) * (SS.asc ? 1 : -1); });
  const th = (k, t, cls = "") => `<th class="${cls}" data-s="${k}" style="cursor:pointer">${t}${SS.sort === k ? (SS.asc ? " ▲" : " ▼") : ""}</th>`;
  $("sdisttable").innerHTML = `<thead><tr>${th("no", "No.", "l")}${th("name", "District", "l")}${th("seats", "Seats")}<th class="l">Elected</th>${th("pct", "Top votes")}${th("margin", "Lead")}</tr></thead><tbody>` +
    rows.map(r => `<tr data-d="${r.d.okreg}" class="${r.d.okreg === SS.sel ? "sel" : ""}" style="cursor:pointer"><td class="l">${r.d.okreg}</td><td class="l">${esc(dname(r.d))}</td><td>${r.d.seats}</td>
      <td class="l name">${r.w.map(c => `${dot(labCol(c.label))} ${c.person ? `<a href="#" class="plink" data-p="${esc(c.person)}">${esc(c.name)}</a>` : esc(c.name)}`).join("<br>")}</td><td>${pct(r.top.pct)}</td><td>${r.m == null ? NA : pct(r.m)}</td></tr>`).join("") + "</tbody>";
}

// ---- movement between the bodies -------------------------------------------------------------------
function moveRow(x, dir) {
  const p = personById(x.person), fp = x.from_party, tp = x.to_party, ch = fp && tp && fp !== "unresolved" && tp !== "unresolved" && fp !== tp;
  const other = dir === "in" ? `${BODY_NAME[x.from]}${x.from === "ep" ? "" : " " + rom(x.from_term)}` : `${BODY_NAME[x.to]}${x.to === "ep" ? "" : " " + rom(x.to_term)}`;
  return `<tr><td class="l">${p ? `<a href="#" class="plink" data-p="${esc(x.person)}">${esc(p.name)}</a>` : esc(x.person)}</td><td class="l">${esc(other)}</td>
    <td class="l">${x.kind === "concurrent" ? "at once" : x.kind === "direct" ? "straight on" : (x.gap != null ? `after ${Math.round(x.gap / 365.25 * 10) / 10} y` : "after a gap")}</td>
    <td class="l">${fp ? dot(partyCol(fp)) + " " + esc(partyName(fp)) : NA}${ch ? ` → ${dot(partyCol(tp))} ${esc(partyName(tp))}` : tp && !fp ? ` → ${esc(partyName(tp))}` : ""}</td></tr>`;
}
function senateMovement() {
  const T = SENATE_TERM[EL.year], mv = PEOPLE ? PEOPLE.moves : [];
  const inn = mv.filter(x => x.to === "senate" && x.to_term === T).sort((a, b) => (a.to_start < b.to_start ? -1 : 1));
  const out = mv.filter(x => x.from === "senate" && x.from_term === T).sort((a, b) => ((a.from_end || "") < (b.from_end || "") ? -1 : 1));
  const tbl = (rows, dir, head) => rows.length ? `<div class="scroll"><table class="t proftable"><thead><tr><th class="l">Senator</th><th class="l">${head}</th><th class="l">How</th><th class="l">Party</th></tr></thead><tbody>${rows.map(x => moveRow(x, dir)).join("")}</tbody></table></div>` : '<p class="note">None recorded.</p>';
  const known = id => id && id !== "unresolved";
  const chg = rows => rows.filter(x => known(x.from_party) && known(x.to_party) && x.from_party !== x.to_party).length;
  $("smovebox").innerHTML = `<div class="grid even"><div class="card"><span class="label">Came to this Senate from</span><div style="font-size:13px">${inn.length} senator${inn.length === 1 ? "" : "s"} of term ${rom(T)} had sat in the Sejm or the European Parliament${inn.length ? `; ${chg(inn)} changed party` : ""}</div>${tbl(inn, "in", "Before")}</div>
    <div class="card"><span class="label">Went on to</span><div style="font-size:13px">${out.length} later sat in the Sejm or the European Parliament${out.length ? `; ${chg(out)} changed party` : ""}</div>${tbl(out, "out", "Next")}</div></div>`;
  // every move, by the bodies and how directly
  const by = {}; mv.forEach(x => { const k = `${x.from}>${x.to}`; (by[k] = by[k] || { direct: 0, after_gap: 0, concurrent: 0 })[x.kind]++; });
  const bs = ["sejm", "senate", "ep"], cell = (f, t) => { const c = by[`${f}>${t}`]; return c ? `<b>${c.direct + c.after_gap + c.concurrent}</b> <span class="muted">(${c.direct} straight on)</span>` : NA; };
  $("smoveall").innerHTML = `<span class="label">All moves since 1989, between the bodies</span><div class="scroll"><table class="t"><thead><tr><th class="l">From \\ to</th>${bs.map(b => `<th>${BODY_NAME[b]}</th>`).join("")}</tr></thead><tbody>${bs.map(f => `<tr><td class="l">${BODY_NAME[f]}</td>${bs.map(t => `<td>${f === t ? NA : cell(f, t)}</td>`).join("")}</tr>`).join("")}</tbody></table></div>
    <p class="note">${mv.length} moves by ${new Set(mv.map(x => x.person)).size} people: a person's next mandate in another body, counting a move as "straight on" when it began within 31 days of the last one ending. The EP observers of 2003–04, delegated by the Sejm and Senate, count as moves "at once". Built from the Senate's pages, the European Parliament's open data and Party tracker's Sejm records; dates that no official source gives come from Polish Wikipedia (marked * on a career).</p>`;
}

// ---- data -----------------------------------------------------------------------------------------
function senateData() {
  $("csvnote").textContent = "One row for each candidate in the election: district, name, committee, party label, votes, share, whether elected, and the person's id in Party tracker's people table where the candidate was elected.";
}
function csvSenate() {
  const head = ["election", "district", "district_name", "seats", "candidate_id", "person_id", "name", "committee", "party_label", "party_membership", "votes", "share", "elected"];
  const rows = [head];
  CUR.districts.forEach(d => d.candidates.forEach(c => rows.push([EL.id, d.okreg, d.name, d.seats, c.id, c.person, c.name, c.committee, c.label, c.party, c.votes, c.pct, c.elected ? 1 : 0])));
  download(`${EL.id}-candidates.csv`, csvBlob(rows));
}

// ---- events -----------------------------------------------------------------------------------------
function selectDistrict(okreg, scroll) {
  SS.sel = okreg; SS.person = null; senateMap(); senateCard(); senateTable(); saveHash();
  if (scroll) $("sdcard").scrollIntoView({ behavior: "smooth", block: "nearest" });
}
function bindSenate() {
  const seg = (id, key) => $(id).addEventListener("click", e => {
    const b = e.target.closest("button[data-k]"); if (!b || b.disabled || SS[key] === b.dataset.k) return;
    SS[key] = b.dataset.k; senateMapControls(); senateMap(); saveHash();
  });
  seg("smeasure", "measure"); seg("slevel", "level"); seg("sshape", "shape");
  const smap = $("smapbox"), tip = smap.querySelector(".tip");
  smap.addEventListener("mousemove", e => {
    const t = e.target.closest("[data-d],[data-g]"); if (!t || !senateOn()) { tip.hidden = true; return; }
    let h = "";
    if (t.dataset.g != null && GMN && GMN[+t.dataset.g]) {
      const g = +t.dataset.g, b = GMN[g], d = dist(b.d), cs = d.candidates.map((c, j) => ({ c, v: c.u[b.k] })).filter(x => x.v > 0).sort((x, y) => y.v - x.v).slice(0, 4);
      h = `<b>${esc(G.gminas.name[g])}</b><div class="sub">District ${d.okreg}${dname(d) ? ", " + esc(dname(d)) : ""}</div>` + cs.map(x => tipRow(labCol(x.c.label), x.c.name, b.valid ? pct(x.v / b.valid) : n0(x.v))).join("");
    } else if (t.dataset.d != null) {
      const d = dist(+t.dataset.d), grp = CUR.districts.filter(x => (x.shape || x.okreg) === (d.shape || d.okreg));
      h = grp.map(x => `<b>${esc(distTitle(x))}</b>` + (x.seats > 1 ? `<div class="sub">${x.seats} seats</div>` : "") + winners(x).map(c => tipRow(labCol(c.label), c.name, pct(c.pct))).join("")).join("<hr style='margin:4px 0;border:0;border-top:1px solid #ECECEC'>");
    }
    if (h) placeTip(tip, smap, e, h); else tip.hidden = true;
  });
  smap.addEventListener("mouseleave", () => { tip.hidden = true; });
  smap.addEventListener("click", e => {
    const t = e.target.closest("[data-d],[data-g]"); if (!t) return;
    if (t.dataset.g != null && GMN && GMN[+t.dataset.g]) selectDistrict(GMN[+t.dataset.g].d, false);
    else if (t.dataset.d != null) selectDistrict(+t.dataset.d, false);
  });
  smap.addEventListener("keydown", e => { if ((e.key === "Enter" || e.key === " ") && e.target.dataset && e.target.dataset.d) { e.preventDefault(); selectDistrict(+e.target.dataset.d, false); } });
  $("sdisttable").addEventListener("click", e => {
    const h = e.target.closest("th[data-s]"); if (h) { const k = h.dataset.s; if (SS.sort === k) SS.asc = !SS.asc; else { SS.sort = k; SS.asc = k === "no" || k === "name"; } senateTable(); return; }
    const r = e.target.closest("tr[data-d]"); if (r) { selectDistrict(+r.dataset.d, true); $("senatesec").scrollIntoView({ behavior: "smooth", block: "start" }); }
  });
  $("sdq").addEventListener("input", debounce(e => { SS.q = e.target.value; senateTable(); }, 120));
  document.addEventListener("click", e => {
    const a = e.target.closest("a.plink"); if (a && !senateOn()) return;   // the members' links are handled in members.js
    if (a) { e.preventDefault(); SS.person = a.dataset.p; if (SS.sel == null) { const p = personById(a.dataset.p), m = p && p.mandates.find(x => x[9] && x[9].startsWith(EL.year + "-")); if (m) SS.sel = +m[9].split("-")[1]; } senateCard(); careerCard(SS.person); senateMap(); $("sdcard").scrollIntoView({ behavior: "smooth", block: "nearest" }); return; }
    if (e.target.id === "sdclose") { SS.sel = null; SS.person = null; senateMap(); senateCard(); senateTable(); saveHash(); }
    if (e.target.classList && e.target.classList.contains("careerclose")) {
      const el = $(e.target.dataset.for); el.hidden = true;
      if (el.id === "scareer") SS.person = null; else if (typeof MM !== "undefined") MM.person = null;
      saveHash();
    }
  });
}
