// Members: for a Sejm or European Parliament election, everyone who sat in that term, from Party tracker's
// people track (people.json for those who also sat in the Senate or the EP, people-sejm.json for the other
// deputies), with the party each was elected for and a link to their career (the card of senate.js). Loaded
// after senate.js and app.js and uses their globals. A Sejm election's members are the deputies of the
// term of the same number in Party tracker (1989 is term 0); an EP election's are the MEPs of that EP term.
"use strict";

const MEMBER_TERM = { sejm: { 1989: 0, 1991: 1, 1993: 2, 1997: 3, 2001: 4, 2005: 5, 2007: 6, 2011: 7, 2015: 8, 2019: 9, 2023: 10 },
                      ep: { 2004: 6, 2009: 7, 2014: 8, 2019: 9, 2024: 10 } };
const MM = { q: "", party: "", sort: "party", asc: true, person: null, rows: [] };
const membersOn = () => !!EL && (EL.type === "sejm" || EL.type === "ep") && MEMBER_TERM[EL.type][EL.year] != null;

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
  const body = EL.type, name = EL.type === "sejm" ? "the Sejm" : "the European Parliament";
  $("h-members").textContent = `Who sat in ${name}, ${EL.year}`;
  Promise.all([loadPeople(), EL.type === "sejm" ? loadSejmPeople() : Promise.resolve(null)]).then(() => {
    if (!membersOn()) return;
    MM.rows = memberRows();
    const parties = [...new Map(MM.rows.map(r => [r.party || "", r.party === "unresolved" ? "Party unknown" : r.party ? partyName(r.party) : "No party"])).entries()];
    const n = MM.rows.length, el = MM.rows.filter(r => r.m[5] !== "replacement").length;
    const also = k => MM.rows.filter(r => r.others.includes(k)).length;
    $("memberlede").textContent = `${n} ${EL.type === "sejm" ? "deputies" : "MEPs"} sat in this term: ${el} elected${n - el ? ` and ${n - el} who took a seat during the term` : ""}. ${EL.type === "sejm" ? `${also("Senate")} also sat in the Senate, ${also("European Parliament")} in the European Parliament` : `${also("Sejm")} also sat in the Sejm, ${also("Senate")} in the Senate`} (before or after). The party is the one the mandate was won for: for a deputy, the lead party of the club they started the term in; for an MEP, their national party when the mandate began. Click a name for a career.`;
    $("mparty").innerHTML = `<option value="">All parties</option>` + parties.sort((a, b) => a[1].localeCompare(b[1])).map(([id, t]) => `<option value="${esc(id)}">${esc(t)}</option>`).join("");
    $("mparty").value = MM.party;
    renderMemberTable();
    if (MM.person) { const p = personById(MM.person); if (p) careerCard(MM.person, $("mcareer")); else $("mcareer").hidden = true; } else $("mcareer").hidden = true;
  });
}
const mFrom = r => r.m[2], mTo = r => r.m[3] || "9999";
function renderMemberTable() {
  const q = MM.q.trim().toLowerCase();
  let rows = MM.rows.filter(r => (!q || r.p.name.toLowerCase().includes(q)) && (!MM.party || (r.party || "") === MM.party));
  const key = { name: r => r.p.name.split(" ").slice(-1)[0] + r.p.name, party: r => (r.party ? partyName(r.party) : "~") + r.p.name, from: r => mFrom(r) + r.p.name }[MM.sort];
  rows.sort((a, b) => { const x = key(a), y = key(b); return (x < y ? -1 : x > y ? 1 : 0) * (MM.asc ? 1 : -1); });
  const th = (k, t) => `<th class="l" data-s="${k}" style="cursor:pointer">${t}${MM.sort === k ? (MM.asc ? " ▲" : " ▼") : ""}</th>`;
  const how = r => r.m[5] === "replacement" ? "took a seat during the term" : r.m[5] === "by_election" ? "by-election" : "elected";
  $("mtable").innerHTML = `<thead><tr>${th("name", "Name")}${th("party", "Party")}${th("from", "From – to")}<th class="l">Also</th></tr></thead><tbody>` +
    rows.map(r => `<tr class="${r.p.id === MM.person ? "sel" : ""}"><td class="l name"><a href="#" class="mlink" data-p="${esc(r.p.id)}">${esc(r.p.name)}</a>${r.m[5] === "replacement" ? ' <span class="badge" title="Took a seat during the term">Replacement</span>' : ""}</td>
      <td class="l">${r.party ? dot(partyCol(r.party)) + " " + esc(partyName(r.party)) : '<span class="muted">no party</span>'}</td>
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
    e.preventDefault(); MM.person = a.dataset.p; careerCard(MM.person, $("mcareer")); renderMemberTable(); saveHash();
    if (window.innerWidth < 900) $("mcareer").scrollIntoView({ behavior: "smooth", block: "nearest" });
  });
  // a career opened from the movement of a Sejm or EP page's card
  document.addEventListener("click", e => {
    const a = e.target.closest("a.plink"); if (!a || senateOn() || !membersOn()) return;
    e.preventDefault(); MM.person = a.dataset.p; careerCard(MM.person, $("mcareer")); renderMemberTable(); saveHash();
  });
}
