// Shared by the Polling tracker and the election maps: the outline round a cartogram's circles, one dot
// for each seat won inside a circle, and the number badges and key that only go into a PNG.
// Circles are {x, y, r}; a view is [x0, y0, width, height] in the same units.
const Cartogram = (() => {
  // The union of the circles is grown by CL and shrunk by CL again (closing the gaps between them), and
  // the edge of the result, PAD clear of every circle, is traced with marching squares.
  function outline(circles, view, close = 45) {
    const [vx, vy, vw, vh] = view, st = 5, m = close + 60, nx = Math.ceil((vw + 2 * m) / st) + 1, ny = Math.ceil((vh + 2 * m) / st) + 1, PAD = 12, CL = close, SQ = Math.SQRT2;
    const ox = vx - m, oy = vy - m, d = new Float32Array(nx * ny);
    for (let j = 0; j < ny; j++) for (let i = 0; i < nx; i++) {
      const x = ox + i * st, y = oy + j * st;
      let f = 1e9; for (const c of circles) f = Math.min(f, Math.hypot(x - c.x, y - c.y) - c.r);
      d[j * nx + i] = f < PAD + CL ? 1e9 : 0;
    }
    for (let j = 0; j < ny; j++) for (let i = 0; i < nx; i++) {
      const k = j * nx + i; if (!d[k]) continue; let v = d[k];
      if (i > 0) v = Math.min(v, d[k - 1] + 1);
      if (j > 0) { v = Math.min(v, d[k - nx] + 1); if (i > 0) v = Math.min(v, d[k - nx - 1] + SQ); if (i < nx - 1) v = Math.min(v, d[k - nx + 1] + SQ); }
      d[k] = v;
    }
    for (let j = ny - 1; j >= 0; j--) for (let i = nx - 1; i >= 0; i--) {
      const k = j * nx + i; if (!d[k]) continue; let v = d[k];
      if (i < nx - 1) v = Math.min(v, d[k + 1] + 1);
      if (j < ny - 1) { v = Math.min(v, d[k + nx] + 1); if (i < nx - 1) v = Math.min(v, d[k + nx + 1] + SQ); if (i > 0) v = Math.min(v, d[k + nx - 1] + SQ); }
      d[k] = v;
    }
    const G = (i, j) => (d[j * nx + i] - 0.5) * st - CL;
    const pt = new Map(), adj = new Map();
    const link = (p, q) => { (adj.get(p) || adj.set(p, []).get(p)).push(q); (adj.get(q) || adj.set(q, []).get(q)).push(p); };
    for (let j = 0; j < ny - 1; j++) for (let i = 0; i < nx - 1; i++) {
      const a = G(i, j), b = G(i + 1, j), c = G(i + 1, j + 1), e = G(i, j + 1);
      const code = (a > 0 ? 1 : 0) | (b > 0 ? 2 : 0) | (c > 0 ? 4 : 0) | (e > 0 ? 8 : 0);
      if (code === 0 || code === 15) continue;
      const edge = t => {
        let k, x, y;
        if (t === 0) { k = `h${i},${j}`; x = ox + (i + a / (a - b)) * st; y = oy + j * st; }
        else if (t === 2) { k = `h${i},${j + 1}`; x = ox + (i + e / (e - c)) * st; y = oy + (j + 1) * st; }
        else if (t === 3) { k = `v${i},${j}`; x = ox + i * st; y = oy + (j + a / (a - e)) * st; }
        else { k = `v${i + 1},${j}`; x = ox + (i + 1) * st; y = oy + (j + b / (b - c)) * st; }
        pt.set(k, [x, y]); return k;
      };
      const segs = [[], [3, 0], [0, 1], [3, 1], [1, 2], [3, 0, 1, 2], [0, 2], [3, 2], [2, 3], [0, 2], [0, 1, 2, 3], [1, 2], [3, 1], [0, 1], [3, 0]][code];
      for (let q = 0; q < segs.length; q += 2) link(edge(segs[q]), edge(segs[q + 1]));
    }
    const seen = new Set(); let best = [];
    adj.forEach((_, k0) => {
      if (seen.has(k0)) return;
      const loop = []; let prev = null, k = k0;
      while (k && !seen.has(k)) { seen.add(k); loop.push(pt.get(k)); const n = adj.get(k).find(x => x !== prev && !seen.has(x)); prev = k; k = n; }
      if (loop.length > best.length) best = loop;
    });
    for (let it = 0; it < 2; it++) {
      const nxt = [];
      best.forEach((p, i) => { const q = best[(i + 1) % best.length]; nxt.push([0.75 * p[0] + 0.25 * q[0], 0.75 * p[1] + 0.25 * q[1]], [0.25 * p[0] + 0.75 * q[0], 0.25 * p[1] + 0.75 * q[1]]); });
      best = nxt;
    }
    return { pts: best, d: "M" + best.map(p => p[0].toFixed(1) + "," + p[1].toFixed(1)).join("L") + "Z" };
  }
  // n dots packed inside a circle in a sunflower spiral, in the order given
  function dots(c, n) {
    const R = c.r - 6, r = Math.min(0.22 * c.r, 0.36 * R * Math.sqrt(Math.PI / Math.max(n, 1)) * 1.15), out = [];
    for (let i = 0; i < n; i++) { const rho = n === 1 ? 0 : (R - r) * Math.sqrt((i + 0.5) / n), th = i * 2.399963; out.push([c.x + rho * Math.cos(th), c.y + rho * Math.sin(th)]); }
    return { r, pts: out };
  }
  // Number badges on each circle's rim, half in and half out, at the angle that keeps clearest of the
  // outline and of the other circles. rim(c) is the radius the circle is drawn at.
  function badges(circles, labels, line, rim = c => c.r - 1.5) {
    const BR = 11, o = [];
    circles.forEach((c, u) => {
      let best = null;
      for (let k = 0; k < 24; k++) {
        const th = -Math.PI / 4 + k * Math.PI / 12, bx = c.x + rim(c) * Math.cos(th), by = c.y + rim(c) * Math.sin(th);
        let clear = Infinity;
        for (const p of line) clear = Math.min(clear, Math.hypot(p[0] - bx, p[1] - by) - BR - 6);
        circles.forEach(q => { if (q !== c) clear = Math.min(clear, Math.hypot(q.x - bx, q.y - by) - rim(q) - BR - 3); });
        const score = clear - k * 0.05;   // near-ties go to the upper right
        if (!best || score > best.score) best = { score, bx, by };
      }
      o.push(`<circle cx="${best.bx.toFixed(1)}" cy="${best.by.toFixed(1)}" r="${BR}" fill="#E4E4E4" stroke="#9A9A9A" stroke-width="1"/><text x="${best.bx.toFixed(1)}" y="${(best.by + 5).toFixed(1)}" text-anchor="middle" font-size="14" font-weight="600" fill="#141414">${labels[u]}</text>`);
    });
    return o.join("");
  }
  // A key down the right-hand side: [number, name] rows, spaced to fit the height
  function key(rows, x, y0, h) {
    const rh = Math.min(22, (h - 20) / rows.length), fs = Math.min(16, rh * 0.74);
    return rows.map(([n, name], i) => `<text x="${x}" y="${(y0 + 14 + i * rh).toFixed(1)}" font-size="${fs.toFixed(1)}" fill="#141414"><tspan font-weight="600">${n}</tspan>  ${name}</text>`).join("");
  }
  return { outline, dots, badges, key };
})();
