/* engine.js: the swing explorer's arithmetic, shared by the page and the Node tests (test_engine.js).
   The raking is the same algorithm as analysis/00_setup.R: a turnout constraint shifts logit(v) by one constant in
   the cells it covers; a vote constraint shifts the log-odds of one party against the rest among voters, the others
   keeping their relative sizes; constraints are cycled until all are met. Seats follow the Polling tracker's rules. */
(function (root, factory) {
  if (typeof module === "object" && module.exports) module.exports = factory();
  else root.E27Engine = factory();
})(typeof self !== "undefined" ? self : this, function () {
  "use strict";

  const logit = (p) => Math.log(p / (1 - p));
  const expit = (x) => 1 / (1 + Math.exp(-x));
  const clamp = (x, lo, hi) => Math.min(hi, Math.max(lo, x));

  /* ---- Cells ---- */
  function baseCells(D, splitShares) {
    // 2023 cells expanded to the 2027 parties: a child takes `share` of its 2023 list in every cell
    const n = D.cells.w.length, parties = D.parties.map((p) => p.id);
    const split = {};
    D.splits.forEach((s) => { split[s.child] = s; split[s.stays] = s; });
    const q = new Array(n);
    for (let i = 0; i < n; i++) {
      const row = new Float64Array(parties.length);
      parties.forEach((p, k) => {
        const parent = D.parties[k].parent;
        let x = D.cells.q[parent][i];
        const s = split[p];
        if (s) {
          const sh = splitShares && splitShares[s.child] != null ? splitShares[s.child] : s.share;
          x *= p === s.child ? sh : 1 - sh;
        }
        row[k] = x;
      });
      q[i] = row;
    }
    return { n, parties, w: Float64Array.from(D.cells.w), v: Float64Array.from(D.cells.v), q,
             a: D.cells.a, s: D.cells.s, e: D.cells.e, r7: D.cells.r7, res: D.cells.r7.map((r) => D.dims.res7.toResidence[r]) };
  }

  function cloneCells(c) {
    return Object.assign({}, c, { v: Float64Array.from(c.v), q: c.q.map((r) => Float64Array.from(r)) });
  }

  // Which cells a group covers. group = {age, sex, education, residence}: index or null (everyone)
  function members(c, g) {
    const out = [];
    for (let i = 0; i < c.n; i++) {
      if (g.age != null && c.a[i] !== g.age) continue;
      if (g.sex != null && c.s[i] !== g.sex) continue;
      if (g.education != null && c.e[i] !== g.education) continue;
      if (g.residence != null && c.res[i] !== g.residence) continue;
      if (g.r7in != null && g.r7in.indexOf(c.r7[i]) < 0) continue;
      out.push(i);
    }
    return out;
  }

  function newton(f, df, x0) {
    let x = x0 || 0;
    for (let it = 0; it < 60; it++) {
      const fx = f(x);
      if (Math.abs(fx) < 1e-12) break;
      const d = df(x);
      if (!(d > 1e-14)) break;
      x -= clamp(fx / d, -5, 5);
    }
    return x;
  }

  function applyTurnout(c, idx, target) {
    let W = 0; const L = idx.map((i) => { W += c.w[i]; return logit(clamp(c.v[i], 1e-12, 1 - 1e-12)); });
    const f = (a) => idx.reduce((s, i, j) => s + c.w[i] * expit(L[j] + a), 0) / W - target;
    const df = (a) => idx.reduce((s, i, j) => { const p = expit(L[j] + a); return s + c.w[i] * p * (1 - p); }, 0) / W;
    const a = newton(f, df, 0);
    idx.forEach((i, j) => { c.v[i] = expit(L[j] + a); });
  }

  function applyVote(c, idx, k, target) {
    const om = idx.map((i) => c.w[i] * c.v[i]);
    const Om = om.reduce((s, x) => s + x, 0);
    const qk = idx.map((i) => c.q[i][k]);
    const nk = (b, x) => (x * Math.exp(b)) / (1 - x + x * Math.exp(b));
    const f = (b) => idx.reduce((s, i, j) => s + om[j] * nk(b, qk[j]), 0) / Om - target;
    const df = (b) => idx.reduce((s, i, j) => { const p = nk(b, qk[j]); return s + om[j] * p * (1 - p); }, 0) / Om;
    const b = newton(f, df, 0);
    idx.forEach((i, j) => {
      const p = nk(b, qk[j]);
      const scale = qk[j] < 1 ? (1 - p) / (1 - qk[j]) : 0;
      const row = c.q[i];
      for (let m = 0; m < row.length; m++) row[m] *= scale;
      row[k] = p;
    });
  }

  function value(c, con) {
    let num = 0, den = 0;
    for (const i of con.idx) {
      if (con.type === "turnout") { num += c.w[i] * c.v[i]; den += c.w[i]; }
      else { const om = c.w[i] * c.v[i]; num += om * c.q[i][con.k]; den += om; }
    }
    return num / den;
  }

  function rake(c, cons, maxCycles, tol) {
    maxCycles = maxCycles || 400; tol = tol || 1e-7;
    let err = 0, cyc = 0;
    if (!cons.length) return { cycles: 0, err: 0 };
    for (cyc = 1; cyc <= maxCycles; cyc++) {
      for (const con of cons) {
        if (con.type === "turnout") applyTurnout(c, con.idx, con.target);
        else applyVote(c, con.idx, con.k, con.target);
      }
      err = Math.max(...cons.map((con) => Math.abs(value(c, con) - con.target)));
      if (err < tol) break;
    }
    return { cycles: cyc, err };
  }

  /* ---- Summaries ---- */
  function summarise(c, idx) {
    idx = idx || [...Array(c.n).keys()];
    let W = 0, Om = 0; const sh = new Float64Array(c.parties.length);
    for (const i of idx) {
      W += c.w[i]; const om = c.w[i] * c.v[i]; Om += om;
      for (let k = 0; k < sh.length; k++) sh[k] += om * c.q[i][k];
    }
    const shares = {}; c.parties.forEach((p, k) => { shares[p] = sh[k] / Om; });
    return { turnout: Om / W, shares, size: W };
  }

  /* ---- Scenario ---- */
  // spec: {base: "2023"|"polls", splitShares, levers: [{group, measure: "turnout"|party, change (points), from: null|party}],
  //        hold: bool (keep the national result of the starting point)}
  // The raked starting point depends only on the base and the split shares; levers are applied to a copy
  const BASE_CACHE = new Map();
  function scenario(D, spec) {
    const key = (spec.base || "2023") + "|" + JSON.stringify(spec.splitShares || {});
    const hit = BASE_CACHE.get(key);
    const c = hit ? cloneCells(hit.c) : baseCells(D, spec.splitShares);
    const all = [...Array(c.n).keys()];
    let baseInfo = hit ? hit.info : { cycles: 0, err: 0 };
    if (!hit && (spec.base === "polls" || spec.base === "groups")) {
      // "groups": the group targets already average to the tracker in each grouping, so they replace the national ones
      const cons = spec.base === "groups" && D.groupTrends ? []
        : c.parties.map((p, k) => ({ type: "vote", idx: all, k, target: D.tracker.vote[p] / 100 })).slice(0, -1);
      // "groups": each group's share of each party as the pollsters' group tables have moved it since 2023
      // (30_group_trends.R), on top of the national figures; Other follows as the remainder
      if (spec.base === "groups" && D.groupTrends) {
        Object.keys(D.groupTrends.dims).forEach((dim) => D.groupTrends.dims[dim].targets.forEach((t) => {
          const idx = members(c, t.group);
          c.parties.forEach((p, k) => { if (p !== "Other" && t.shares[p] != null) cons.push({ type: "vote", idx, k, target: Math.max(t.shares[p], 0.0005) }); });
          // turnout moved as stated intentions have moved between groups, national turnout kept at 2023's
          if (t.turnout != null) cons.push({ type: "turnout", idx, target: t.turnout });
        }));
      }
      baseInfo = rake(c, cons);
    }
    if (!hit) { if (BASE_CACHE.size > 20) BASE_CACHE.clear(); BASE_CACHE.set(key, { c: cloneCells(c), info: baseInfo }); }
    const base = cloneCells(c);
    const cons = [];
    (spec.levers || []).forEach((lv, j) => {
      let idx = members(c, lv.group || {});
      if (lv.invert) { const inG = new Set(idx); idx = all.filter((i) => !inG.has(i)); }   // everyone outside the group
      if (!idx.length || (!lv.change && lv.to == null)) return;
      const s0 = summarise(base, idx);
      if (lv.measure === "turnout") {
        // `to` sets turnout outright (what-if analyses: 0.999 = everyone votes, 0.001 = no one does)
        const t = lv.to != null ? clamp(lv.to, 0.001, 0.999) : clamp(s0.turnout + lv.change / 100, 0.01, 0.99);
        cons.push({ type: "turnout", idx, target: t, lever: j });
      } else {
        const k = c.parties.indexOf(lv.measure);
        cons.push({ type: "vote", idx, k, target: clamp(s0.shares[lv.measure] + lv.change / 100, 0.0005, 0.99), lever: j });
        if (lv.from && lv.from !== lv.measure) {
          c.parties.forEach((p, m) => {
            if (p === lv.measure || p === "Other") return;
            const t = p === lv.from ? s0.shares[p] - lv.change / 100 : s0.shares[p];
            cons.push({ type: "vote", idx, k: m, target: clamp(t, 0.0005, 0.99), lever: j });
          });
        }
      }
    });
    if (spec.hold) {
      const s0 = summarise(base);
      cons.push({ type: "turnout", idx: all, target: s0.turnout, hold: true });
      c.parties.slice(0, -1).forEach((p, k) => cons.push({ type: "vote", idx: all, k, target: s0.shares[p], hold: true }));
    }
    const info = rake(c, cons);
    return { cells: c, base, info, baseInfo, national: summarise(c), nationalBase: summarise(base) };
  }

  /* ---- Seats: the Polling tracker's rules ---- */
  function mulberry32(a) {
    return function () {
      a |= 0; a = (a + 0x6d2b79f5) | 0;
      let t = Math.imul(a ^ (a >>> 15), 1 | a);
      t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
  }
  function normal(rng) {
    let u = 0, v = 0;
    while (u === 0) u = rng();
    while (v === 0) v = rng();
    return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
  }
  function dhondt(votes, seats) {
    const out = new Array(votes.length).fill(0);
    for (let s = 0; s < seats; s++) {
      let best = -1, bq = -1;
      for (let k = 0; k < votes.length; k++) {
        const q = votes[k] / (out[k] + 1);
        if (q > bq) { bq = q; best = k; }
      }
      out[best]++;
    }
    return out;
  }

  // lists: [{id, members: [party], share (fraction of valid votes), joint: bool}]; coefFor(party) -> column name
  function allocate(D, lists, opts) {
    opts = opts || {};
    const R = D.tracker.rules, C = D.tracker.constituencies;
    const thr = (l) => (l.threshold != null ? l.threshold : l.joint ? R.thresholdJoint : R.thresholdParty);
    const ok = lists.filter((l) => l.share >= thr(l));
    const coefCol = (p) => opts.coef && opts.coef[p] ? opts.coef[p] : R.coef[p];
    const sdOf = (p) => (R.swingSd[p] != null ? R.swingSd[p] : R.swingSdPooled);
    const seats = {}; lists.forEach((l) => (seats[l.id] = 0)); seats.MN = 0;
    for (const k of C) {
      const votes = ok.map((l) => l.members.reduce((s, m) => {
        let f = k[coefCol(m)];
        if (opts.rng) { const sd = sdOf(m); f *= Math.exp(-sd * sd / 2 + sd * normal(opts.rng)); }
        return s + k.validvotes * (l.memberShares ? l.memberShares[m] : l.share / l.members.length) * f;
      }, 0));
      const ids = ok.map((l) => l.id);
      if (k.okreg === R.mnOkreg) { votes.push(k.validvotes * R.mnOpoleShare); ids.push("MN"); }
      const s = dhondt(votes, k.magnitude);
      ids.forEach((id, j) => (seats[id] += s[j]));
    }
    return seats;
  }

  // Lists for 2027 from national party shares and the joint lists switched on (Other never wins seats)
  function lists2027(shares, joint) {
    const out = [], used = new Set();
    (joint || []).forEach((j) => {
      const memberShares = {}; let tot = 0;
      j.members.forEach((m) => { memberShares[m] = shares[m]; tot += shares[m]; used.add(m); });
      out.push({ id: j.name, members: j.members, share: tot, memberShares, joint: true });
    });
    Object.keys(shares).forEach((p) => {
      if (p === "Other" || used.has(p)) return;
      out.push({ id: p, members: [p], share: shares[p], memberShares: { [p]: shares[p] }, joint: false });
    });
    return out;
  }

  function seatRanges(D, lists, nSims, seed) {
    const rng = mulberry32(seed || 2027);
    const sims = [];
    for (let s = 0; s < (nSims || 200); s++) sims.push(allocate(D, lists, { rng }));
    const ids = lists.map((l) => l.id).concat(["MN"]);
    const out = {};
    ids.forEach((id) => {
      const xs = sims.map((x) => x[id]).sort((a, b) => a - b);
      out[id] = { lo: xs[Math.floor(0.1 * (xs.length - 1))], hi: xs[Math.ceil(0.9 * (xs.length - 1))] };
    });
    return { ranges: out, sims };
  }

  /* ---- The Polling tracker's seat simulation ----
     A port of simulate() and allocate() in the tracker's app.js (Website repo, assets/polling-tracker/), with the same
     seed, draw order and number types, so that with nothing changed every seat's chance is the tracker's figure.
     Each of the week's poll draws (vote shares) is allocated once, each party's regional profile perturbed by a
     mean-one lognormal factor for every draw x constituency x party (CONST_SWING_SD). A scenario scales each party's
     draws so that their median is the scenario's share, as the tracker's scenario view does. Change one, change both. */
  function quant(sorted, p) { // R's quantile type 7
    const n = sorted.length; if (!n) return NaN;
    const h = (n - 1) * p, lo = Math.floor(h);
    return lo + 1 < n ? sorted[lo] + (h - lo) * (sorted[lo + 1] - sorted[lo]) : sorted[lo];
  }
  const SWING_SEED = 780045;
  let SIMP = null;
  function simPrep(D) {
    if (SIMP && SIMP.D === D) return SIMP;
    const T = D.tracker, R = T.rules, P = R.parties, SP = R.seatParties, C = T.constituencies;
    const NP = P.length, NC = C.length, PIDX = {};
    SP.forEach((p, i) => (PIDX[p] = i));
    const COEFI = SP.map((p) => p === "MN"
      ? C.map((c) => (c.okreg === R.mnOkreg ? 100 * R.mnOpoleShare / R.mnNational2023 : 0))
      : C.map((c) => c[R.coef[p]]));
    const dr = T.draws, K = dr.categories.length, n = dr.n;
    const arr = new Float32Array(dr.values.length);
    for (let i = 0; i < dr.values.length; i++) arr[i] = dr.values[i] / 10000;
    const map = dr.categories.map((c) => (c in PIDX ? PIDX[c] : -1));
    const med = new Float64Array(SP.length);
    map.forEach((i, k) => { if (i < 0) return; const v = []; for (let d = 0; d < n; d++) v.push(arr[d * K + k]); v.sort((a, b) => a - b); med[i] = quant(v, 0.5); });
    // The swing factors, as the tracker's makeSwing()
    const r = mulberry32(SWING_SEED), swing = new Float32Array(n * NC * NP);
    let spare = null;
    const nrm = () => { if (spare != null) { const s = spare; spare = null; return s; }
      let u = 0, v = 0; while (u === 0) u = r(); v = r();
      const m = Math.sqrt(-2 * Math.log(u)); spare = m * Math.sin(2 * Math.PI * v); return m * Math.cos(2 * Math.PI * v); };
    const sd = P.map((p) => (R.swingSd[p] != null ? R.swingSd[p] : R.swingSdPooled));
    for (let d = 0; d < n; d++) for (let c = 0; c < NC; c++) for (let i = 0; i < NP; i++) {
      swing[(d * NC + c) * NP + i] = Math.exp(-sd[i] * sd[i] / 2 + sd[i] * nrm());
    }
    SIMP = { D, R, P, SP, C, NP, NC, PIDX, COEFI, VV: C.map((c) => c.validvotes), MAG: C.map((c) => c.magnitude), n, K, arr, map, med, swing };
    return SIMP;
  }
  function simDhondt(votes, n, out) { // the tracker's dhondt(): lists with no votes never win
    out.fill(0);
    for (let s = 0; s < n; s++) {
      let best = -1, bq = 0;
      for (let k = 0; k < votes.length; k++) {
        if (votes[k] <= 0) continue;
        const q = votes[k] / (out[k] + 1);
        if (q > bq) { bq = q; best = k; }
      }
      if (best < 0) break;
      out[best]++;
    }
  }
  // scen: null for the tracker's own draws, or { shares: {party: fraction of the vote}, joint: [{name, members}] }.
  // Returns per draw: national seats (nat, n x E), constituency seats (cons, n x NC x E), list shares (tots, n x E)
  // and the parties' shares (sh, n x SP).
  function simulate(D, scen) {
    const Z = simPrep(D), { R, SP, NP, NC, PIDX, COEFI, VV, MAG, n, K, arr, map, med, swing } = Z;
    const joint = (scen && scen.joint) || [];
    const ents = joint.map((j) => ({ id: j.name, members: j.members.slice().sort((a, b) => PIDX[a] - PIDX[b]), joint: true }));
    SP.forEach((p) => { if (!joint.some((j) => j.members.includes(p))) ents.push({ id: p, members: [p], joint: false }); });
    ents.forEach((e) => { e.idx = e.members.map((p) => PIDX[p]); e.mnOnly = !e.joint && e.members[0] === "MN"; });
    const E = ents.length, iMN = PIDX.MN;
    const nat = new Int16Array(n * E), cons = new Int16Array(n * NC * E), tots = new Float32Array(n * E), shAll = new Float32Array(n * SP.length);
    const factor = new Float64Array(SP.length), fixed = new Float64Array(SP.length);
    if (scen) SP.forEach((p, i) => {
      if (p === "MN") return;
      const u = scen.shares[p] || 0, m = med[i];
      if (m > 0) factor[i] = u / m; else { factor[i] = 0; fixed[i] = u; }
    });
    const sh = new Float64Array(SP.length), tot = new Float64Array(E), active = new Uint8Array(E), votes = new Float64Array(E), seats = new Int16Array(E);
    for (let d = 0; d < n; d++) {
      sh.fill(0);
      map.forEach((i, k) => { if (i >= 0) sh[i] = arr[d * K + k]; });
      if (scen) for (let i = 0; i < SP.length; i++) if (i !== iMN) sh[i] = factor[i] > 0 || fixed[i] === 0 ? sh[i] * factor[i] : fixed[i];
      sh[iMN] = R.mnNational2023 / 100;
      for (let i = 0; i < SP.length; i++) shAll[d * SP.length + i] = sh[i];
      for (let k = 0; k < E; k++) {
        let t = 0; for (const i of ents[k].idx) t += sh[i];
        tot[k] = t; tots[d * E + k] = t;
        active[k] = t > 0 && (ents[k].mnOnly || t >= (ents[k].joint ? R.thresholdJoint : R.thresholdParty)) ? 1 : 0;
      }
      for (let c = 0; c < NC; c++) {
        for (let k = 0; k < E; k++) {
          if (!active[k]) { votes[k] = 0; continue; }
          let v = 0;
          for (const i of ents[k].idx) {
            const s = sh[i]; if (s <= 0) continue;
            let cf = COEFI[i][c];
            if (i < NP) cf *= swing[(d * NC + c) * NP + i];
            v += cf * s;
          }
          votes[k] = VV[c] * v;
        }
        simDhondt(votes, MAG[c], seats);
        for (let k = 0; k < E; k++) { nat[d * E + k] += seats[k]; cons[d * NC * E + c * E + k] = seats[k]; }
      }
    }
    return { n, NC, ents, nat, cons, tots, sh: shAll, SP };
  }
  // Each list's national seats: median and 80% range (rounded as the tracker does), and Pr(clearing its threshold)
  function simSeats(D, sim) {
    const R = D.tracker.rules, { n, ents, nat, tots } = sim, E = ents.length, out = {};
    ents.forEach((e, k) => {
      const v = []; let clear = 0;
      const thr = e.joint ? R.thresholdJoint : R.thresholdParty;
      for (let d = 0; d < n; d++) { v.push(nat[d * E + k]); if (tots[d * E + k] >= thr - 1e-9) clear++; }
      v.sort((a, b) => a - b);
      out[e.id] = { seats: Math.round(quant(v, 0.5)), lo: Math.round(quant(v, 0.1)), hi: Math.round(quant(v, 0.9)), pThr: e.mnOnly ? null : clear / n };
    });
    return out;
  }
  // Share of draws in which the parties' combined vote reaches thr (a joint list's chance of clearing 8%)
  function simClear(sim, parties, thr) {
    const ix = parties.map((p) => sim.SP.indexOf(p)), m = sim.SP.length;
    let c = 0;
    for (let d = 0; d < sim.n; d++) { let t = 0; ix.forEach((i) => (t += sim.sh[d * m + i])); if (t >= thr - 1e-9) c++; }
    return c / sim.n;
  }
  // Share of draws in which the lists together hold at least `cut` seats
  function simProb(sim, ids, cut) {
    const ks = ids.map((id) => sim.ents.findIndex((e) => e.id === id)).filter((k) => k >= 0), E = sim.ents.length;
    let c = 0;
    for (let d = 0; d < sim.n; d++) { let s = 0; ks.forEach((k) => (s += sim.nat[d * E + k])); if (s >= cut) c++; }
    return c / sim.n;
  }
  /* Seat by seat in every constituency. A list's k-th seat in a constituency is won in the share of draws in which the
     list wins at least k seats there (the tracker's definition). The constituency's projected seats are its m most
     probable list-seats (m = its magnitude; ties go to the larger list, then the lower k), safest first; the rest are
     the contenders. The chances of all list-seats in a constituency sum to m, so what the projected seats lack
     (1 - p summed) is exactly what the contenders hold.
     Who takes a projected seat when its holder loses it: in each draw, the projected seats lost (a list winning fewer
     than its projected seats loses its highest ones) are matched to the seats won beyond the projection, the least
     secure lost seat to the likeliest contender seat, and so on. Each seat's `take` then gives every list's chance of
     holding it, the holder's included, summing to 1. */
  // Certainty of a projected seat, judged on its projected holder's chance (the holder is the list most likely to win
  // it): certain 97.5% or more; probable 66% to 97.5%; possible 50% to 66% (in play, but more likely than not);
  // toss-up under 50%. The same cut-offs as the Polling tracker.
  const SEAT_CERTAIN = 0.975, SEAT_PROBABLE = 0.66, SEAT_POSSIBLE = 0.5;
  const seatCat = (p) => (p >= SEAT_CERTAIN ? "certain" : p >= SEAT_PROBABLE ? "probable" : p >= SEAT_POSSIBLE ? "possible" : "toss-up");
  function seatByConstituency(D, sim) {
    const { n, NC, ents, cons } = sim, E = ents.length, C = D.tracker.constituencies;
    return C.map((c, ci) => {
      const mag = c.magnitude, lists = [], all = [];
      ents.forEach((e, k) => {
        const cnt = new Int32Array(mag + 1), v = [];
        for (let d = 0; d < n; d++) { const s = cons[d * NC * E + ci * E + k]; cnt[Math.min(s, mag)]++; v.push(s); }
        v.sort((a, b) => a - b);
        const p = []; let ge = 0;
        for (let j = mag; j >= 1; j--) { ge += cnt[j]; p[j - 1] = ge / n; }
        while (p.length && p[p.length - 1] === 0) p.pop();
        if (!p.length) return;
        lists.push({ id: e.id, k, p, lo: Math.round(quant(v, 0.1)), hi: Math.round(quant(v, 0.9)), median: Math.round(quant(v, 0.5)) });
        p.forEach((pp, j) => all.push({ id: e.id, k, j: j + 1, p: pp }));
      });
      const tot = {}; lists.forEach((l) => (tot[l.id] = l.p.reduce((a, b) => a + b, 0)));
      all.sort((a, b) => b.p - a.p || tot[b.id] - tot[a.id] || a.k - b.k || a.j - b.j);
      const projected = all.slice(0, mag), contenders = all.slice(mag);
      projected.forEach((s, i) => { s.rank = i + 1; s.cat = seatCat(s.p); s.tally = {}; });
      lists.forEach((l) => (l.projected = projected.filter((s) => s.id === l.id).length));
      const proj = {}; lists.forEach((l) => (proj[l.k] = l.projected));
      const crank = {}; contenders.forEach((x, i) => (crank[x.k + ":" + x.j] = i));
      for (let d = 0; d < n; d++) {
        const lost = [], gained = [];
        for (let i = mag - 1; i >= 0; i--) { const s = projected[i]; if (cons[d * NC * E + ci * E + s.k] < s.j) lost.push(s); }
        if (!lost.length) continue;
        lists.forEach((l) => { const s = cons[d * NC * E + ci * E + l.k]; for (let j = proj[l.k] + 1; j <= s; j++) gained.push({ id: l.id, r: crank[l.k + ":" + j] }); });
        gained.sort((a, b) => a.r - b.r);
        lost.forEach((s, i) => { const g = gained[i]; if (g) s.tally[g.id] = (s.tally[g.id] || 0) + 1; });
      }
      projected.forEach((s) => {
        s.take = [{ id: s.id, p: s.p }].concat(Object.keys(s.tally).map((id) => ({ id, p: s.tally[id] / n }))).sort((a, b) => b.p - a.p);
        s.next = s.take.find((x) => x.id !== s.id) || null;
        delete s.tally;
      });
      return { okreg: c.okreg, name: c.name, magnitude: mag, lists, projected, contenders };
    });
  }

  /* ---- What if a group (all) turned out, or only that group did ---- */
  // mode "all": everyone in the group votes, others as in the starting point; "only": no one outside the group votes.
  // New voters are assumed to vote like the cell's existing voters (same age, sex, education and residence).
  function whatIf(D, spec) {
    const g = spec.group || {}, any = Object.keys(g).some((k) => g[k] != null);
    const levers = [];
    if (spec.mode === "all") levers.push({ group: g, measure: "turnout", to: 0.999 });
    else if (any) levers.push({ group: g, invert: true, measure: "turnout", to: 0.001 });
    const res = scenario(D, { base: spec.base, splitShares: spec.splitShares, levers, hold: false });
    return { res, seats: allocate(D, lists2027(res.national.shares, [])) };
  }

  return { whatIf, baseCells, cloneCells, members, rake, summarise, scenario, allocate, lists2027, seatRanges, dhondt,
    simulate, simSeats, simClear, simProb, seatByConstituency, seatCat, quant, SEAT_CERTAIN, SEAT_PROBABLE, SEAT_POSSIBLE };
});
