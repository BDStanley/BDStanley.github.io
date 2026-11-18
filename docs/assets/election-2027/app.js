/* Election 2027 · Where the polls stand — the live panel of the Election 2027 page.
 *
 * Reads the Polling tracker's polls.json (../polling-tracker/) and the blog's posts.json (beside this file) and
 * shows the tracker's latest week in four panels, each linking into the tracker:
 *   vote shares with 80% ranges, the change since the data week of the latest published post, and the 2023 lists
 *   now; a compact hemicycle of the median seats (dealt by party, in the tracker's hemicycle order) with ranges;
 *   majorities: the week's published combinations that the tracker's rules allow (no pair in rules.forbidden) with a
 *   majority in at least 5% of simulations and no surplus party, and the governing parties; the thresholds in doubt;
 *   a small cartogram of the 41 constituencies (the tracker's circles) coloured by the list projected to win most
 *   seats, from a single D'Hondt allocation at the national medians with the 2023 regional profiles.
 * Every number is the tracker's, for an election held that week. The page sizes its iframe to its content.
 */
"use strict";
(() => {
  const $ = (id) => document.getElementById(id);
  const esc = (s) => String(s == null ? "" : s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
  const MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
  const dateOf = (d) => new Date(d * 864e5);
  const dlong = (d) => { const t = dateOf(d); return t.getUTCDate() + " " + MONTHS[t.getUTCMonth()] + " " + t.getUTCFullYear(); };
  const iso = (d) => dateOf(d).toISOString().slice(0, 10);
  const fromIso = (s) => Math.round(Date.parse(s + "T00:00:00Z") / 864e5);
  const prob = (p) => (p == null || isNaN(p) ? "–" : p > 0.99 ? ">99%" : p < 0.01 ? "<1%" : Math.round(p * 100) + "%");
  const signed = (v) => { const r = +v.toFixed(1); return (r > 0 ? "+" : r < 0 ? "−" : "±") + Math.abs(r).toFixed(1); };
  const sum = (a) => a.reduce((x, y) => x + y, 0);

  // Into the tracker, from assets/election-2027/ (the links open in the whole window, not the frame)
  const PAGES = "../../pages/polish-political-analyses/";
  const TRACKER = { polls: "polling-tracker.html", seats: "polling-tracker/seats.html", geography: "polling-tracker/geography.html", scenarios: "polling-tracker/scenarios.html" };
  const track = (addr, hash) => PAGES + TRACKER[addr] + (hash ? "#" + hash : "");
  const more = (addr, hash, text) => `<a class="more" target="_top" href="${esc(track(addr, hash))}">${esc(text)} →</a>`;

  function fit() {
    try { const f = window.frameElement; if (f) f.style.height = Math.ceil(document.documentElement.getBoundingClientRect().height) + 4 + "px"; } catch (e) { /* not same-origin */ }
  }

  function render(D, posts) {
    const R = D.rules, P = R.parties, SP = R.seatParties, W = D.weeks, w = W[W.length - 1];
    const col = (p) => (R.colours && R.colours[p]) || "#808080";
    const at = (arr, p, list) => { const v = arr[list.indexOf(p)]; return v == null ? null : v; };
    const share = (p) => at(w.vote, p, P), lo = (p) => at(w.voteLo, p, P), hi = (p) => at(w.voteHi, p, P);
    const seats = (p) => at(w.seats, p, SP) || 0, sLo = (p) => at(w.seatsLo, p, SP) || 0, sHi = (p) => at(w.seatsHi, p, SP) || 0;
    const hashW = "w=" + iso(w.date);
    $("asof").textContent = `Live: updated each week from the Polling tracker · week of ${dlong(w.date)}`;

    // The data week of the latest published post, if there is one and it is earlier
    const last = posts && posts.posts && posts.posts.length ? posts.posts[0] : null;
    const pw = last ? W.find((x) => x.date === fromIso(last.data_week)) : null;
    const prev = pw && pw.date !== w.date ? pw : null;

    // 1. Vote shares
    const order = P.filter((p) => share(p) != null).sort((a, b) => share(b) - share(a));
    const vrows = order.map((p) => `<tr><td><span class="dot" style="background:${col(p)}"></span>${esc(p)}</td><td class="red">${share(p).toFixed(1)}</td>` +
      `<td class="muted">${lo(p).toFixed(1)}–${hi(p).toFixed(1)}</td>` +
      (prev ? `<td>${at(prev.vote, p, P) == null ? "new" : signed(share(p) - at(prev.vote, p, P))}</td>` : "") + "</tr>").join("");
    const since23 = (R.election2023 || []).filter((l) => l.list !== "MN").map((l) => {
      const now = sum(l.parties.map((p) => share(p) || 0));
      const name = l.parties.length > 1 ? l.parties.join(" and ") : l.list;
      return `${esc(name)} ${now.toFixed(1)} (${signed(now - l.vote)})`;
    }).join(" · ");
    const p1 = `<div class="panel"><span class="label">Vote shares</span>
      <table class="t"><tr><th>Party</th><th>%</th><th>80% range</th>${prev ? `<th title="Change since the week of ${dlong(prev.date)}, the figures of the latest update">Since update</th>` : ""}</tr>${vrows}</table>
      ${prev ? `<p class="help">Since update: the change since the week of ${dlong(prev.date)}, the figures of the latest update.</p>` : ""}
      <p class="since"><b>Against the 2023 lists</b>, adding up the parties that stood on each: ${since23}.</p>
      ${more("polls", hashW, "Trends since 2023")}</div>`;

    // 2. The Sejm if the election were this week: compact hemicycle of the median seats
    const hord = R.hemicycleOrder.filter((p) => seats(p) > 0);
    let deal = hord.map((p) => [p, seats(p)]);
    const tot = sum(deal.map((d) => d[1]));
    if (tot !== 460 && deal.length) { const big = deal.reduce((a, b) => (b[1] > a[1] ? b : a)); big[1] += 460 - tot; }   // medians need not add up to 460
    const seq = []; deal.forEach(([p, n]) => { for (let i = 0; i < n; i++) seq.push(col(p)); });
    const rows = 10, radii = Array.from({ length: rows }, (_, i) => 1 + i / (rows - 1)), rs = sum(radii);
    const counts = radii.map((r) => Math.floor(460 * r / rs)); let rem = 460 - sum(counts);
    for (let k = rows - 1; rem > 0; k = (k + rows - 1) % rows, rem--) counts[k]++;
    const pts = [];
    counts.forEach((nc, ri) => { for (let j = 0; j < nc; j++) { const a = Math.PI - j * Math.PI / (nc - 1); pts.push({ a, r: radii[ri] }); } });
    pts.sort((p, q) => q.a - p.a || p.r - q.r);
    const S = 66, cx = 150, cy = 140;
    const dots = pts.map((p, i) => `<circle cx="${(cx + S * 2 * p.r * Math.cos(p.a) / 2).toFixed(1)}" cy="${(cy - S * p.r * Math.sin(p.a)).toFixed(1)}" r="2.7" fill="${seq[i] || "#D5D5D5"}"/>`).join("");
    const hemi = `<svg class="hemi" viewBox="0 0 300 150" role="img" aria-label="The 460 seats of the Sejm by party, median seats if an election were held this week">${dots}` +
      `<text x="150" y="128" text-anchor="middle" font-size="12" fill="#767676">231 for a majority</text></svg>`;
    const srows = SP.filter((p) => sHi(p) > 0).sort((a, b) => seats(b) - seats(a))
      .map((p) => `<tr><td><span class="dot" style="background:${col(p)}"></span>${esc(p)}</td><td><b>${seats(p)}</b></td><td class="muted">${sLo(p)}–${sHi(p)}</td></tr>`).join("");
    const p2 = `<div class="panel"><span class="label">If the election were this week</span>${hemi}
      <table class="t"><tr><th>Party</th><th>Seats</th><th>80% range</th></tr>${srows}</table>
      <p class="help">Median seats and 80% range of the tracker's simulations.</p>
      ${more("seats", hashW, "Seats and every seat")}</div>`;

    // 3. Majorities and thresholds in doubt
    const forb = R.forbidden || [];
    const allowed = (m) => !forb.some((f) => f.every((x) => m.includes(x)));
    const ord = (m) => m.slice().sort((a, b) => SP.indexOf(a) - SP.indexOf(b));
    const co = (w.coalitions || []).map((c) => ({ m: ord(c.m), s: c.s, lo: c.lo, hi: c.hi, p: c.p }));
    const shown = co.filter((c) => allowed(c.m) && c.p >= 0.05);
    const most = shown.filter((c) => c.p >= 0.5);
    let list = shown.filter((c) => !most.some((o) => o.m.length < c.m.length && o.m.every((x) => c.m.includes(x))));
    const gov = ["KO", "Lewica"].concat(["Polska 2050", "PSL", "Razem"].filter((p) => seats(p) > 0));
    const g = co.find((c) => c.m.length === gov.length && gov.every((x) => c.m.includes(x)));
    const govRow = g ? Object.assign({}, g, { gov: true }) : { m: ord(gov), s: sum(gov.map(seats)), p: null, gov: true };
    list = list.filter((c) => !(c.m.length === govRow.m.length && govRow.m.every((x) => c.m.includes(x)))).concat([govRow]);
    const majHtml = list.map((c) => {
      const label = c.m.join(" + ") + (c.gov ? ` <span class="muted">(governing${c.m.includes("Razem") ? ", with Razem" : ""})</span>` : "");
      return `<div class="maj"><div class="row"><span>${label}</span><span><b>${c.s}</b> · ${c.p == null ? "–" : prob(c.p)}</span></div>` +
        `<div class="track"><div class="fill${c.gov ? " gov" : ""}" style="width:${Math.min(100, c.s / 460 * 100).toFixed(1)}%"></div><div class="tick" style="left:${(231 / 460 * 100).toFixed(1)}%"></div></div></div>`;
    }).join("");
    const doubt = P.filter((p) => share(p) != null && SiteUI.inDoubt(share(p) / 100, R.thresholdParty, at(w.pThr, p, P)));
    const thr = doubt.length ? `<table class="t"><tr><th>Party</th><th>%</th><th>Chance of 5%</th></tr>` +
      doubt.sort((a, b) => share(b) - share(a)).map((p) => `<tr><td>${esc(p)}</td><td>${share(p).toFixed(1)}</td><td>${prob(at(w.pThr, p, P))}</td></tr>`).join("") + "</table>"
      : `<p class="help">None this week.</p>`;
    const p3 = `<div class="panel"><span class="label">Majorities</span>${majHtml}
      <p class="help">Seats (median) and chance of 231 or more. The line marks 231. Combinations the tracker's rules allow with a majority in at least 5% of simulations, and the governing parties.</p>
      <span class="label" style="margin-top:6px">Thresholds in doubt</span>${thr}
      <p class="help">A share within 3 points of 5%, or a chance of clearing it between 2% and 98%.</p>
      ${more("seats", hashW, "Possible governments")}</div>`;

    // 4. The cartogram: the list projected to win most seats in each constituency (single D'Hondt at the medians)
    const C = D.constituencies, box = D.cartogramBox || [1000, 1000];
    const thrOK = (p) => (share(p) || 0) / 100 >= R.thresholdParty;
    const lead = {};
    const circles = C.map((c) => {
      const v = SP.map((p) => {
        if (p === "MN") return c.okreg === R.mnOkreg ? c.validvotes * R.mnOpoleShare : 0;
        return thrOK(p) ? c.validvotes * (share(p) / 100) * (c[R.coef[p]] || 0) : 0;
      });
      const won = SP.map(() => 0);
      for (let k = 0; k < c.magnitude; k++) {
        let best = -1, bq = 0;
        v.forEach((x, i) => { const q = x / (won[i] + 1); if (q > bq) { bq = q; best = i; } });
        if (best < 0) break; won[best]++;
      }
      let top = 0; won.forEach((n, i) => { if (n > won[top] || (n === won[top] && v[i] > v[top])) top = i; });
      const p = SP[top]; lead[p] = (lead[p] || 0) + 1;
      const tip = `${c.okreg}. ${c.name}: ` + SP.map((q, i) => [q, won[i]]).filter((x) => x[1] > 0).sort((a, b) => b[1] - a[1]).map((x) => x[0] + " " + x[1]).join(", ");
      return `<circle cx="${c.x}" cy="${c.y}" r="${c.r}" fill="${col(p)}" fill-opacity="0.45" stroke="${col(p)}" stroke-width="3"><title>${esc(tip)}</title></circle>`;
    }).join("");
    const leadTxt = Object.entries(lead).sort((a, b) => b[1] - a[1]).map(([p, n]) => `${esc(p)} in ${n}`).join(", ");
    const p4 = `<div class="panel"><span class="label">Where the seats fall</span>
      <svg class="carto" viewBox="-20 -20 ${box[0] + 40} ${box[1] + 40}" role="img" aria-label="Cartogram of the 41 constituencies, sized by electorate, coloured by the list projected to win most seats">${circles}</svg>
      <p class="help">The 41 constituencies, sized by electorate. Projected from this week's medians with the 2023 regional profiles (one allocation, no uncertainty): ${leadTxt} would win most seats. Hover for each constituency's seats; the tracker has each seat's chances.</p>
      ${more("geography", hashW, "Constituencies")}</div>`;

    $("grid").innerHTML = p1 + p2 + p3 + p4;
    fit();
  }

  Promise.all([
    fetch("../polling-tracker/polls.json").then((r) => { if (!r.ok) throw new Error(r.status); return r.json(); }),
    fetch("posts.json").then((r) => (r.ok ? r.json() : null)).catch(() => null)
  ]).then(([D, posts]) => render(D, posts)).catch((e) => {
    $("asof").textContent = "";
    $("grid").innerHTML = `<p class="help err">The Polling tracker's data could not be loaded (${esc(e.message)}). The figures are on the <a target="_top" href="${esc(track("polls"))}">Polling tracker</a>.</p>`;
    fit();
  });
  if (typeof ResizeObserver !== "undefined") new ResizeObserver(fit).observe(document.body);
  addEventListener("resize", fit);
})();
