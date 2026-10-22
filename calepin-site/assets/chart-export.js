// Shared by the Polish political analyses apps (assets/*/index.html) for their PNG
// downloads. Each app exports a chart by serialising its SVG into an <img> and
// drawing that on a canvas; but the image sees neither the page's stylesheet nor
// its web fonts, so the chart's labels fell back to a serif face. clone() returns
// a self-contained copy of the SVG: every text element carries its computed font
// and fill, and Jost is embedded as data URIs in a <style>. fonts() loads the
// faces the canvas draws the title and credits in, and TITLE etc. are those
// fonts, so that every export is set like the site's headings.
(function () {
  "use strict";
  const NS = "http://www.w3.org/2000/svg";
  const JOST = "https://fonts.googleapis.com/css2?family=Jost:ital,wght@0,300..700;1,300..700&display=swap";
  const PROPS = ["font-family", "font-size", "font-weight", "font-style", "letter-spacing", "fill", "fill-opacity", "visibility", "display"];
  let jostCss;

  // The Google Fonts CSS for Jost with its font files inlined, or "" offline.
  function jost() {
    jostCss = jostCss || fetch(JOST).then(r => r.text()).then(async css => {
      const urls = [...new Set(css.match(/https:\/\/[^)'"\s]+/g) || [])];
      const data = await Promise.all(urls.map(u => fetch(u).then(r => r.blob()).then(b => new Promise((res, rej) => {
        const fr = new FileReader(); fr.onload = () => res(fr.result); fr.onerror = rej; fr.readAsDataURL(b);
      }))));
      urls.forEach((u, i) => { css = css.split(u).join(data[i]); });
      return css;
    }).catch(() => "");
    return jostCss;
  }

  // Copy before removing anything from the clone, so its text nodes still pair
  // up one to one with the original's.
  async function clone(svg) {
    const c = svg.cloneNode(true), src = svg.querySelectorAll("text, tspan"), dst = c.querySelectorAll("text, tspan");
    src.forEach((n, i) => {
      const cs = getComputedStyle(n);
      dst[i].setAttribute("style", PROPS.map(p => `${p}:${cs.getPropertyValue(p)}`).join(";"));
    });
    const css = await jost();
    if (css) { const st = document.createElementNS(NS, "style"); st.textContent = css; c.insertBefore(st, c.firstChild); }
    return c;
  }

  function fonts() {
    return Promise.all(["300 22px Jost", "400 13px Jost", "500 13px Jost"].map(f => document.fonts.load(f))).catch(() => {});
  }

  window.ChartExport = {
    clone, fonts,
    TITLE: "300 22px Jost, sans-serif",   // dark red, like the site's h2
    SUBTITLE: "13px Jost, sans-serif",    // #4B4B4B
    CREDIT: "12px Jost, sans-serif",      // #767676
  };
})();
