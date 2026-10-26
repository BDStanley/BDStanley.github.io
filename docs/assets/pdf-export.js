// Vector PDF export for the Polish political analyses apps, the counterpart of chart-export.js.
// The apps draw every export on a canvas, so ChartPdf.context() returns an object with the
// canvas calls they use (fillText, arc, lineTo, drawImage and so on), which draws the same
// export into a jsPDF page instead: text stays text, in Jost, and shapes stay shapes. An
// SVG chart passed to drawImage() as { _svg: element } is drawn as vector too, through
// svg2pdf.js. jsPDF, svg2pdf.js and the Jost files are self-hosted (vendor/, fonts/ beside
// this file) and loaded on the first PDF, so a PNG export never pays for them.
//
// Layout units are the apps' canvas pixels. A page is either sized to its content
// (1 px = 0.75 pt) or A4 portrait, where the content is scaled to fit inside the margins.
(function () {
  "use strict";
  const BASE = ((document.currentScript && document.currentScript.src) || "").replace(/[^/]*$/, "");
  // [family, jsPDF style, file, weight]: jsPDF knows only normal, bold and italic, so each
  // other weight of Jost is a family of its own
  const FONTS = [["Jost", "normal", "Jost-Regular.ttf", 400], ["Jost", "italic", "Jost-Italic.ttf", 400], ["Jost", "bold", "Jost-Bold.ttf", 700],
    ["Jost Light", "normal", "Jost-Light.ttf", 300], ["Jost Medium", "normal", "Jost-Medium.ttf", 500], ["Jost SemiBold", "normal", "Jost-SemiBold.ttf", 600]];
  const A4 = [595.28, 841.89], MARGIN = 28;
  let ready;

  const script = src => new Promise((res, rej) => {
    const s = document.createElement("script"); s.src = src; s.onload = res;
    s.onerror = () => rej(new Error("Could not load " + src)); document.head.appendChild(s);
  });
  const b64 = buf => { const a = new Uint8Array(buf); let s = ""; for (let i = 0; i < a.length; i += 0x8000) s += String.fromCharCode.apply(null, a.subarray(i, i + 0x8000)); return btoa(s); };
  function load() {
    ready = ready || (async () => {
      if (!window.jspdf) await script(BASE + "vendor/jspdf.umd.min.js");
      if (!(window.jspdf.jsPDF.API && window.jspdf.jsPDF.API.svg)) await script(BASE + "vendor/svg2pdf.umd.min.js");
      return Promise.all(FONTS.map(f => fetch(BASE + "fonts/" + f[2]).then(r => { if (!r.ok) throw new Error("Could not load " + f[2]); return r.arrayBuffer(); }).then(b64)));
    })().catch(e => { ready = null; throw e; });
    return ready;
  }

  // the jsPDF family and style for a CSS weight
  function face(weight, italic) {
    if (weight <= 300) return ["Jost Light", "normal"];
    if (weight >= 700) return ["Jost", "bold"];
    if (weight >= 600) return ["Jost SemiBold", "normal"];
    if (weight >= 500) return ["Jost Medium", "normal"];
    return ["Jost", italic ? "italic" : "normal"];
  }

  // Resolve every text node's weight against the attached SVG, then name the matching
  // Jost family on it, because svg2pdf looks fonts up by family and bold only.
  function fixFonts(svg) {
    svg.querySelectorAll("style").forEach(n => n.remove());
    const nodes = [...svg.querySelectorAll("text, tspan")].map(n => { const cs = getComputedStyle(n); return [n, parseInt(cs.fontWeight, 10) || 400, cs.fontStyle === "italic"]; });
    nodes.forEach(([n, w, it]) => { const [fam, st] = face(w, it); n.style.fontFamily = fam; n.style.fontWeight = st === "bold" ? "700" : "400"; n.style.fontStyle = st === "italic" ? "italic" : "normal"; });
  }

  // opts: a4 (portrait A4, content scaled to fit), title, subject
  async function context(W, H, opts) {
    opts = opts || {};
    const files = await load(), { jsPDF } = window.jspdf;
    let pw, ph, k, ox = 0, oy = 0;
    if (opts.a4) { [pw, ph] = A4; k = Math.min((pw - 2 * MARGIN) / W, (ph - 2 * MARGIN) / H); ox = (pw - W * k) / 2; oy = MARGIN; }
    else { k = 0.75; pw = W * k; ph = H * k; }
    const doc = new jsPDF({ unit: "pt", format: [pw, ph], orientation: pw > ph ? "l" : "p", compress: true });
    FONTS.forEach((f, i) => { doc.addFileToVFS(f[2], files[i]); doc.addFont(f[2], f[0], f[1], undefined, "Identity-H"); });
    doc.setProperties({ title: opts.title || "", subject: opts.subject || "", author: "Ben Stanley", creator: "benstanley.eu" });

    const mc = document.createElement("canvas").getContext("2d"), pc = document.createElement("canvas").getContext("2d");
    const colour = c => {
      pc.fillStyle = "#000"; pc.fillStyle = c; const v = pc.fillStyle;
      if (v[0] === "#") return [parseInt(v.slice(1, 3), 16), parseInt(v.slice(3, 5), 16), parseInt(v.slice(5, 7), 16), 1];
      const m = v.match(/[\d.]+/g).map(Number); return [m[0], m[1], m[2], m.length > 3 ? m[3] : 1];
    };
    let tx = 0, ty = 0, path = [], cur = [0, 0], opacity = 1;
    const stack = [], queue = [];
    const X = x => ox + (tx + x) * k, Y = y => oy + (ty + y) * k;
    const alpha = a => { if (a !== opacity) { doc.setGState(new doc.GState({ opacity: a, "stroke-opacity": a })); opacity = a; } };
    const g = {
      fillStyle: "#000", strokeStyle: "#000", lineWidth: 1, globalAlpha: 1, font: "10px sans-serif", textAlign: "left",
      save() { stack.push({ tx, ty, fillStyle: g.fillStyle, strokeStyle: g.strokeStyle, lineWidth: g.lineWidth, globalAlpha: g.globalAlpha, font: g.font, textAlign: g.textAlign }); },
      restore() { const s = stack.pop(); if (!s) return; tx = s.tx; ty = s.ty; ["fillStyle", "strokeStyle", "lineWidth", "globalAlpha", "font", "textAlign"].forEach(p => { g[p] = s[p]; }); },
      translate(x, y) { tx += x; ty += y; },
      beginPath() { path = []; },
      moveTo(x, y) { cur = [x, y]; },
      lineTo(x, y) { path.push({ x1: cur[0], y1: cur[1], x2: x, y2: y }); cur = [x, y]; },
      arc(x, y, r) { path.push({ c: true, x, y, r }); },
      fill() {
        const [r, gr, b, a] = colour(g.fillStyle); doc.setFillColor(r, gr, b); alpha(g.globalAlpha * a);
        path.forEach(p => { if (p.c) doc.circle(X(p.x), Y(p.y), p.r * k, "F"); });
      },
      stroke() {
        const [r, gr, b, a] = colour(g.strokeStyle); doc.setDrawColor(r, gr, b); doc.setLineWidth(g.lineWidth * k); alpha(g.globalAlpha * a);
        path.forEach(p => { if (p.c) doc.circle(X(p.x), Y(p.y), p.r * k, "S"); else doc.line(X(p.x1), Y(p.y1), X(p.x2), Y(p.y2)); });
      },
      fillRect(x, y, w, h) { const [r, gr, b, a] = colour(g.fillStyle); doc.setFillColor(r, gr, b); alpha(g.globalAlpha * a); doc.rect(X(x), Y(y), w * k, h * k, "F"); },
      fillText(t, x, y) {
        const m = /^(italic\s+)?(?:(\d{3})\s+)?([\d.]+)px/.exec(String(g.font).trim()) || [];
        const [fam, st] = face(+(m[2] || 400), !!m[1]), [r, gr, b, a] = colour(g.fillStyle);
        doc.setFont(fam, st); doc.setFontSize(+(m[3] || 10) * k); doc.setTextColor(r, gr, b); alpha(g.globalAlpha * a);
        const al = g.textAlign === "right" || g.textAlign === "end" ? "right" : g.textAlign === "center" ? "center" : "left";
        doc.text(String(t), X(x), Y(y), { align: al });
      },
      doc,
      measureText(t) { mc.font = g.font; return mc.measureText(t); },
      drawImage(img, x, y, w, h) { if (img && img._svg) queue.push({ svg: img._svg, x: X(x), y: Y(y), w: w * k, h: h * k }); },
      // draws the queued SVG charts, then returns the PDF as a Blob
      async finish() {
        const holder = document.createElement("div"); holder.style.cssText = "position:fixed;left:-100000px;top:0";
        document.body.appendChild(holder);
        try {
          for (const q of queue) {
            holder.appendChild(q.svg); fixFonts(q.svg);
            alpha(1); await doc.svg(q.svg, { x: q.x, y: q.y, width: q.w, height: q.h });
            holder.removeChild(q.svg);
          }
        } finally { holder.remove(); }
        return doc.output("blob");
      },
    };
    return g;
  }

  window.ChartPdf = { context };
})();
