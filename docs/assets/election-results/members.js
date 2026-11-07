// Members: for a Sejm or European Parliament election, everyone who sat in that term, from Party tracker's
// people track (people.json for those who also sat in the Senate or the EP, people-sejm.json for the other
// deputies), with the party each was elected for and a link to their career (the card of senate.js). Loaded
// after senate.js and app.js and uses their globals. A Sejm election's members are the deputies of the
// term of the same number in Party tracker (1989 is term 0); an EP election's are the MEPs of that EP term.
"use strict";

const MEMBER_TERM = { sejm: { 1989: 0, 1991: 1, 1993: 2, 1997: 3, 2001: 4, 2005: 5, 2007: 6, 2011: 7, 2015: 8, 2019: 9, 2023: 10 },
                      ep: { 2004: 6, 2009: 7, 2014: 8, 2019: 9, 2024: 10 }, senate: SENATE_TERM };
const MM = { q: "", party: "", sort: "party", asc: true, person: null, rows: [] };
const membersOn = () => !!EL && (EL.type === "presidential" || ((EL.type === "sejm" || EL.type === "ep" || EL.type === "senate") && MEMBER_TERM[EL.type][EL.year] != null));

function memberRows() {
  const body = EL.type, term = MEMBER_TERM[body][EL.year], rows = [];
  const add = p => {
    const m = p.mandates.find(x => x[0] === body && x[1] === term && x[5] !== "delegation");
    if (!m) return;
    const others = [...new Set(p.mandates.filter(x => x[0] !== body).map(x => x[0]))].map(b => BODY_NAME[b]);
    rows.push({ p, m, party: m[4], others, terms: p.mandates.filter(x => x[0] === body).length });
  };
  if (PEOPLE) PEOPLE.persons.forEach(add);
  if (SEJMP) SEJMP.forEach(add);
  return rows;
}

function renderMembers() {
  const sec = $("membersec"), j = $("jump");
  if (!membersOn()) { sec.hidden = true; return; }
  sec.hidden = false;
  if (j.dataset.std && !j.innerHTML.includes("#membersec")) j.innerHTML = j.dataset.std.replace('<a href="#mapsec">', '<a href="#membersec">Members</a><a href="#mapsec">');
  if (EL.type === "presidential") return renderCandidates();
  if (MM.lastType === "presidential") { MM.sort = "party"; MM.asc = true; MM.lastType = null; }
  const body = EL.type, name = { sejm: "the Sejm", senate: "the Senate", ep: "the European Parliament" }[EL.type];
  $("h-members").textContent = `Who sat in ${name}, ${EL.year}`;
  Promise.all([loadPeople(), EL.type === "sejm" ? loadSejmPeople() : Promise.resolve(null)]).then(() => {
    if (!membersOn()) return;
    MM.rows = memberRows();
    const parties = [...new Map(MM.rows.map(r => [r.party || "", r.party === "unresolved" ? "Party unknown" : r.party ? partyName(r.party) : "No party"])).entries()];
    const n = MM.rows.length, el = MM.rows.filter(r => r.m[5] !== "replacement").length;
    const also = k => MM.rows.filter(r => r.others.includes(k)).length;
    $("memberlede").textContent = `${n} ${{ sejm: "deputies", senate: "senators", ep: "MEPs" }[EL.type]} sat in this term: ${el} elected${n - el ? ` and ${n - el} who took a seat during the term` : ""}. ${EL.type === "sejm" ? `${also("Senate")} also sat in the Senate, ${also("European Parliament")} in the European Parliament` : EL.type === "senate" ? `${also("Sejm")} also sat in the Sejm, ${also("European Parliament")} in the European Parliament` : `${also("Sejm")} also sat in the Sejm, ${also("Senate")} in the Senate`} (before or after). The party shown is the one whose list the mandate was won on, not necessarily one the member belonged to: the data do not record party membership, and someone elected on a party's list may never have joined it. For deputies of terms 6 to 10, the National Electoral Commission's candidate lists record whether each stood as a member of a party, and the table says so where they do. For a deputy it is the lead party of the club they started the term in; for a senator, the party of the committee that nominated them; for an MEP, their national party when the mandate began. Click a name for a career.`;
    $("mparty").innerHTML = `<option value="">All parties</option>` + parties.sort((a, b) => a[1].localeCompare(b[1])).map(([id, t]) => `<option value="${esc(id)}">${esc(t)}</option>`).join("");
    $("mparty").value = MM.party; MM.party = $("mparty").value;
    renderMemberTable();
    if (MM.person) { const p = personById(MM.person); if (p) careerCard(MM.person, $("mcareer")); else $("mcareer").hidden = true; } else $("mcareer").hidden = true;
  });
}
const mFrom = r => r.m[2], mTo = r => r.m[3] || "9999";
function renderMemberTable() {
  if (EL.type === "presidential") return renderCandTable();
  const q = MM.q.trim().toLowerCase();
  let rows = MM.rows.filter(r => (!q || r.p.name.toLowerCase().includes(q)) && (!MM.party || (r.party || "") === MM.party));
  const key = { name: r => r.p.name.split(" ").slice(-1)[0] + r.p.name, party: r => (r.party ? partyName(r.party) : "~") + r.p.name, from: r => mFrom(r) + r.p.name }[MM.sort];
  rows.sort((a, b) => { const x = key(a), y = key(b); return (x < y ? -1 : x > y ? 1 : 0) * (MM.asc ? 1 : -1); });
  const th = (k, t) => `<th class="l" data-s="${k}" style="cursor:pointer">${t}${MM.sort === k ? (MM.asc ? " ▲" : " ▼") : ""}</th>`;
  const how = r => r.m[5] === "replacement" ? "took a seat during the term" : r.m[5] === "by_election" ? "by-election" : "elected";
  $("mtable").innerHTML = `<thead><tr>${th("name", "Name")}${th("party", "Elected on list of")}${th("from", "From – to")}<th class="l">Also</th></tr></thead><tbody>` +
    rows.map(r => `<tr class="${r.p.id === MM.person ? "sel" : ""}"><td class="l name"><a href="#" class="mlink" data-p="${esc(r.p.id)}">${esc(r.p.name)}</a>${r.m[5] === "replacement" ? ' <span class="badge" title="Took a seat during the term">Replacement</span>' : ""}</td>
      <td class="l">${r.party ? dot(partyCol(r.party)) + " " + partyLink(r.party) : '<span class="muted">no party</span>'}${r.m[10] ? ` <span class="muted" title="Stated by the candidate to the National Electoral Commission when registering">${r.m[10][0] === "member" ? "· member" : "· not a member"}</span>` : ""}</td>
      <td class="l">${dlong(r.m[2])}${r.m[3] ? " – " + dlong(r.m[3]) : " – "}${r.m[3] && r.m[6] && r.m[6] !== "term_end" ? `<span class="muted">, ${esc(r.m[6].replace(/_/g, " "))}</span>` : ""}${r.m[8] ? ' <span class="muted" title="A date on this row rests on Polish Wikipedia">*</span>' : ""}</td>
      <td class="l">${r.others.map(o => `<span class="badge" style="background:#F3F3F1;color:var(--soft)">${esc(o)}</span>`).join(" ")}${r.terms > 1 ? ` <span class="muted">${r.terms} terms</span>` : ""}</td></tr>`).join("") + "</tbody>";
  $("mcount").textContent = rows.length === MM.rows.length ? "" : `${rows.length} of ${MM.rows.length}`;
}
function bindMembers() {
  $("mq").addEventListener("input", debounce(e => { MM.q = e.target.value; renderMemberTable(); }, 120));
  $("mparty").addEventListener("change", e => { MM.party = e.target.value; renderMemberTable(); });
  $("mtable").addEventListener("click", e => {
    const h = e.target.closest("th[data-s]"); if (h) { const k = h.dataset.s; if (MM.sort === k) MM.asc = !MM.asc; else { MM.sort = k; MM.asc = true; } renderMemberTable(); return; }
    const a = e.target.closest("a.mlink"); if (!a) return;
    e.preventDefault(); MM.person = a.dataset.p; if (EL.type === "presidential") candCard(MM.person); else careerCard(MM.person, $("mcareer")); renderMemberTable(); saveHash();
    if (window.innerWidth < 900) $("mcareer").scrollIntoView({ behavior: "smooth", block: "nearest" });
  });
  // a career opened from the movement of a Sejm or EP page's card
  document.addEventListener("click", e => {
    const a = e.target.closest("a.plink"); if (!a || senateOn() || !membersOn()) return;
    e.preventDefault(); MM.person = a.dataset.p; if (EL.type === "presidential") candCard(MM.person); else careerCard(MM.person, $("mcareer")); renderMemberTable(); saveHash();
    if (!e.target.closest("#membersec")) $("membersec").scrollIntoView({ behavior: "smooth", block: "start" });
  });
}

// ---- presidential elections: the candidates of the election, with a career where they also sat in the Sejm, the Senate or the EP
function candidateRows() {
  const by = new Map();
  CUR.results.forEach(pt => { const rd = /-r2$/.test(pt.part) ? 2 : 1;
    pt.items.forEach(it => {
      const r = by.get(it.id) || { key: it.person || it.id, item: it.id, name: it.name, label: it.label, colour: it.colour, backing: it.backing, note: it.note, incumbent: it.incumbent, person: it.person || "", res: {} };
      r.res[rd] = { pct: it.pct, rank: it.rank, votes: it.votes }; by.set(it.id, r); }); });
  return [...by.values()].map(r => { const p = r.person ? personById(r.person) : null;
    return Object.assign(r, { others: p ? [...new Set(p.mandates.map(x => x[0]))].map(b => BODY_NAME[b]) : [] }); });
}
function renderCandidates() {
  $("h-members").textContent = `Who stood in the presidential election, ${EL.year}`;
  Promise.all([loadPeople(), loadSejmPeople()]).then(() => {
    if (!membersOn() || EL.type !== "presidential") return;
    if (MM.lastType !== "presidential") { MM.sort = "r1"; MM.asc = false; }
    MM.lastType = "presidential";
    MM.rows = candidateRows();
    const n = MM.rows.length, two = MM.rows.filter(r => r.res[2]).length, ct = MM.rows.filter(r => r.others.length).length;
    $("memberlede").textContent = `${n} candidates stood${two ? `, ${two} in the second round` : ""}. ${ct} also sat in the Sejm, the Senate or the European Parliament, and their names open a career. The colour is the candidate's palette label (independents and small committees are grey); a candidate's backing is the parties named as backing them. Click a name for the candidacy.`;
    const labels = [...new Set(MM.rows.map(r => r.label || ""))].sort();
    $("mparty").innerHTML = `<option value="">All colours</option>` + labels.map(l => `<option value="${esc(l)}">${esc(l || "No label")}</option>`).join("");
    $("mparty").value = MM.party; MM.party = $("mparty").value;
    renderMemberTable();
    candCard(MM.person);
  });
}
const pctf = x => x == null ? "" : (x * 100).toFixed(1).replace(/\.0$/, "") + "%";
function candCard(key) {
  const el = $("mcareer"), r = MM.rows.find(x => x.key === key);
  if (!key || !r) { el.hidden = true; return; }
  const blk = `<span class="label">Candidate</span><h3 style="margin:2px 0 2px;font-size:18px;font-weight:500">${esc(r.name)}</h3>
    <div class="note">${dot(r.colour || "#9E9E9E")} ${esc(r.label || "")}${r.incumbent ? " · incumbent" : ""}</div>
    <div class="note">${[1, 2].filter(k => r.res[k]).map(k => `${k === 1 ? "First" : "Second"} round: ${pctf(r.res[k].pct)}, ${r.res[k].votes != null ? r.res[k].votes.toLocaleString("en-GB") + " votes, " : ""}${r.res[k].rank === 1 ? "first" : "place " + r.res[k].rank}`).join("<br>")}</div>
    ${r.backing ? `<div class="note">Backed by ${esc(r.backing)}.</div>` : ""}${r.note ? `<div class="note">${esc(r.note)}</div>` : ""}`;
  if (r.person && personById(r.person)) { careerCard(r.person, el); el.insertAdjacentHTML("afterbegin", blk + "<hr>"); }
  else { el.hidden = false; el.innerHTML = blk + `<p class="note">No Sejm, Senate or European Parliament mandate is recorded for this candidate in Party tracker's people track (the link to a person is made by hand, from a source).</p><button type="button" class="linkbtn careerclose" data-for="mcareer">Close</button>`; }
}
function renderCandTable() {
  const q = MM.q.trim().toLowerCase();
  const rows = MM.rows.filter(r => (!q || r.name.toLowerCase().includes(q)) && (!MM.party || (r.label || "") === MM.party));
  const key = { name: r => r.name.split(" ").slice(-1)[0] + r.name, party: r => (r.label || "~") + r.name, r1: r => (r.res[1] ? r.res[1].pct : -1) }[MM.sort];
  rows.sort((a, b) => { const x = key(a), y = key(b); return (x < y ? -1 : x > y ? 1 : 0) * (MM.asc ? 1 : -1); });
  const th = (k, t) => `<th class="l" data-s="${k}" style="cursor:pointer">${t}${MM.sort === k ? (MM.asc ? " ▲" : " ▼") : ""}</th>`;
  $("mtable").innerHTML = `<thead><tr>${th("name", "Name")}${th("party", "Colour")}${th("r1", "First round")}<th class="l">Second round</th><th class="l">Also</th></tr></thead><tbody>` +
    rows.map(r => `<tr class="${r.key === MM.person ? "sel" : ""}"><td class="l name"><a href="#" class="mlink" data-p="${esc(r.key)}">${esc(r.name)}</a>${r.incumbent ? ' <span class="badge" title="Incumbent">Incumbent</span>' : ""}</td>
      <td class="l">${dot(r.colour || "#9E9E9E")} ${esc(r.label || "")}</td><td class="l">${pctf(r.res[1] && r.res[1].pct)}</td><td class="l">${r.res[2] ? pctf(r.res[2].pct) : ""}</td>
      <td class="l">${r.others.map(o => `<span class="badge" style="background:#F3F3F1;color:var(--soft)">${esc(o)}</span>`).join(" ")}</td></tr>`).join("") + "</tbody>";
  $("mcount").textContent = rows.length === MM.rows.length ? "" : `${rows.length} of ${MM.rows.length}`;
}

// a presidential candidate's name in the outcome, linked to the candidacy (and career) in the Members section
function candLink(name) {
  let key = null;
  if (EL && EL.type === "presidential" && CUR) CUR.results.forEach(pt => pt.items.forEach(it => { if (!key && it.name === name) key = it.person || it.id; }));
  return key ? `<a href="#" class="plink" data-p="${esc(key)}">${esc(name)}</a>` : esc(name);
}
