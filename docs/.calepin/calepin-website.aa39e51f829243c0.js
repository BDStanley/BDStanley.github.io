(() => {
  "use strict";

  const selects = document.querySelectorAll("select[data-calepin-language-picker]");
  if (!selects.length) return;

  function escapeHtml(value) {
    return String(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function closeAll(except) {
    document.querySelectorAll(".calepin-language-picker.is-open").forEach((picker) => {
      if (picker === except) return;
      picker.classList.remove("is-open");
      picker.open = false;
      const summary = picker.querySelector("summary");
      if (summary) summary.setAttribute("aria-expanded", "false");
    });
  }

  function enhance(select) {
    if (select.dataset.calepinLanguageBound === "true") return;
    select.dataset.calepinLanguageBound = "true";

    const options = Array.from(select.options).filter((option) => option.value);
    if (options.length <= 1) return;

    const selected = options.find((option) => option.selected) || options[0];
    const picker = document.createElement("details");
    picker.className = "calepin-language-picker dropdown";

    const summary = document.createElement("summary");
    summary.className = "calepin-language-picker-button";
    summary.setAttribute("role", "button");
    summary.setAttribute("aria-haspopup", "menu");
    summary.setAttribute("aria-expanded", "false");
    summary.setAttribute("aria-label", select.getAttribute("aria-label") || "Language");
    summary.innerHTML = `<span>${escapeHtml(selected.textContent)}</span>`;

    const menu = document.createElement("ul");
    menu.className = "calepin-language-picker-menu";
    menu.setAttribute("role", "menu");

    options.forEach((option) => {
      const li = document.createElement("li");
      const item = document.createElement("a");
      item.href = option.value;
      item.setAttribute("role", "menuitem");
      item.innerHTML = `<span>${escapeHtml(option.textContent)}</span>`;
      if (option.selected) item.setAttribute("aria-current", "true");
      li.appendChild(item);
      menu.appendChild(li);
    });

    summary.addEventListener("click", () => {
      const open = !picker.open;
      closeAll(open ? picker : null);
      picker.classList.toggle("is-open", open);
      summary.setAttribute("aria-expanded", open ? "true" : "false");
    });

    picker.addEventListener("toggle", () => {
      picker.classList.toggle("is-open", picker.open);
      summary.setAttribute("aria-expanded", picker.open ? "true" : "false");
    });

    picker.append(summary, menu);
    select.hidden = true;
    select.insertAdjacentElement("afterend", picker);
  }

  selects.forEach(enhance);

  document.addEventListener("click", (event) => {
    if (!event.target.closest(".calepin-language-picker")) closeAll();
  });
  document.addEventListener("keydown", (event) => {
    if (event.key === "Escape") closeAll();
  });
})();


(() => {
  "use strict";

  const selector = window.CalepinCopyCode?.selector ||
    "div.sourceCode, pre.sourceCode, .cell-output, .cell-output-stdout, .cell-output-stderr";
  const buttonClass = window.CalepinCopyCode?.buttonClass || "calepin-copy-code";
  const copiedClass = window.CalepinCopyCode?.copiedClass || "copied";
  const icon = window.CalepinCopyCode?.icon ||
    `<svg aria-hidden="true" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="9" y="9" width="13" height="13" rx="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path></svg>`;
  const copiedIcon = window.CalepinCopyCode?.copiedIcon ||
    `<svg aria-hidden="true" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M20 6 9 17 4 12"></path></svg>`;

  function copyText(text) {
    if (navigator.clipboard?.writeText) {
      return navigator.clipboard.writeText(text);
    }
    const textarea = document.createElement("textarea");
    textarea.value = text;
    textarea.setAttribute("readonly", "");
    textarea.style.position = "fixed";
    textarea.style.opacity = "0";
    document.body.appendChild(textarea);
    textarea.select();
    try {
      document.execCommand("copy");
    } finally {
      textarea.remove();
    }
    return Promise.resolve();
  }

  function codeText(node) {
    if (!node) return "";
    if (node.nodeType === Node.TEXT_NODE) return node.nodeValue || "";
    if (node.nodeType !== Node.ELEMENT_NODE) return "";
    if (node.tagName === "BR") return "\n";
    return Array.from(node.childNodes).map(codeText).join("");
  }

  document.querySelectorAll(selector).forEach((block) => {
    if (block.dataset.calepinCopyBound === "true" || block.querySelector(`:scope > .${buttonClass}`)) {
      return;
    }
    block.dataset.calepinCopyBound = "true";
    const button = document.createElement("button");
    button.type = "button";
    button.className = buttonClass;
    button.setAttribute("aria-label", "Copy code");
    button.setAttribute("title", "Copy code");
    button.innerHTML = icon;
    let restoreTimeout = null;
    button.addEventListener("click", async () => {
      if (restoreTimeout !== null) {
        window.clearTimeout(restoreTimeout);
        restoreTimeout = null;
      }
      const code = block.querySelector("pre code, code, pre");
      const text = codeText(code);
      try {
        await copyText(text);
        button.classList.add(copiedClass);
        button.innerHTML = copiedIcon;
        restoreTimeout = window.setTimeout(() => {
          button.classList.remove(copiedClass);
          button.innerHTML = icon;
          restoreTimeout = null;
        }, 900);
      } catch {
        button.classList.remove(copiedClass);
        button.innerHTML = icon;
      }
    });
    block.prepend(button);
  });
})();


(() => {
  "use strict";

  const pagefindModal = () => document.querySelector("pagefind-modal");

  function modalSearchInput(modal) {
    if (!modal) return null;
    return modal.querySelector("pagefind-input input, input[type='search'], input");
  }

  function setModalSearchQuery(modal, query) {
    if (!query) return false;
    const input = modalSearchInput(modal);
    if (!input) return false;
    input.value = query;
    input.dispatchEvent(new Event("input", { bubbles: true }));
    return true;
  }

  function focusModalSearch(modal) {
    const input = modalSearchInput(modal);
    if (!input) return false;
    input.focus();
    return true;
  }

  async function openPagefindSearch(query) {
    const modal = pagefindModal();
    if (!modal) return false;

    if (typeof modal.open !== "function" && window.customElements?.whenDefined) {
      try {
        await window.customElements.whenDefined("pagefind-modal");
      } catch {
        /* ignore custom element registration errors */
      }
    }

    if (typeof modal.open !== "function") return false;
    modal.open();

    const sync = () => {
      const hasQuery = setModalSearchQuery(modal, query);
      const hasFocus = focusModalSearch(modal);
      return hasQuery || hasFocus;
    };

    if (!sync()) {
      requestAnimationFrame(() => {
        if (!sync()) window.setTimeout(sync, 50);
      });
    }

    return true;
  }

  function isTextEntryElement(element) {
    if (!element) return false;
    return (
      element.isContentEditable ||
      element.tagName === "INPUT" ||
      element.tagName === "SELECT" ||
      element.tagName === "TEXTAREA"
    );
  }

  const SEARCH_SUGGESTION_LIMIT = 20;
  const SEARCH_SUBRESULT_LIMIT = 3;
  const SEARCH_SUGGESTION_MIN_LENGTH = 2;
  const SEARCH_SUGGESTION_DELAY = 160;
  let pagefindModulePromise = null;

  function pagefindBundlePath() {
    const script = document.querySelector(
      'script[src$="pagefind-component-ui.js"], script[src$="pagefind-ui.js"]',
    );
    if (script?.src) return new URL("./", script.src).toString();
    return new URL("pagefind/", window.location.href).toString();
  }

  function loadPagefindModule() {
    if (!pagefindModulePromise) {
      pagefindModulePromise = import(`${pagefindBundlePath()}pagefind.js`);
    }
    return pagefindModulePromise;
  }

  function plainTextFromHtml(html) {
    const element = document.createElement("div");
    element.innerHTML = html || "";
    return element.textContent?.trim() || "";
  }

  function setSearchResultsOpen(input, results, isOpen) {
    results.hidden = !isOpen;
    input.setAttribute("aria-expanded", String(isOpen));
  }

  function clearSearchResults(input, results) {
    results.innerHTML = "";
    setSearchResultsOpen(input, results, false);
  }

  function renderSearchMessage(input, results, message) {
    results.innerHTML = "";
    const item = document.createElement("p");
    item.className = "calepin-website-search-empty";
    item.textContent = message;
    results.appendChild(item);
    setSearchResultsOpen(input, results, true);
  }

  function renderSearchResults(input, results, items) {
    results.innerHTML = "";
    if (!items.length) {
      renderSearchMessage(input, results, "No results");
      return;
    }

    const basePathPrefix = localCanonicalBasePathPrefix();
    const addSearchLink = (item, className, titleClassName, excerptClassName) => {
      const href = hrefWithoutLocalBasePath(item.meta?.url || item.url || "", basePathPrefix);
      if (!href) return null;

      const link = document.createElement("a");
      link.className = className;
      link.href = href;
      link.setAttribute("role", "option");

      const title = document.createElement("span");
      title.className = titleClassName;
      title.textContent = item.meta?.title || item.title || href;
      link.appendChild(title);

      const excerptText = plainTextFromHtml(item.excerpt || item.content || "");
      if (excerptText) {
        const excerpt = document.createElement("span");
        excerpt.className = excerptClassName;
        excerpt.textContent = excerptText;
        link.appendChild(excerpt);
      }

      return link;
    };

    for (const item of items) {
      const link = addSearchLink(
        item,
        "calepin-website-search-result",
        "calepin-website-search-result-title",
        "calepin-website-search-result-excerpt",
      );
      if (!link) continue;
      results.appendChild(link);

      const subResults = Array.isArray(item.sub_results)
        ? item.sub_results.slice(0, SEARCH_SUBRESULT_LIMIT)
        : [];
      if (!subResults.length) continue;

      const nested = document.createElement("div");
      nested.className = "calepin-website-search-subresults";
      for (const subResult of subResults) {
        const subLink = addSearchLink(
          subResult,
          "calepin-website-search-subresult",
          "calepin-website-search-subresult-title",
          "calepin-website-search-subresult-excerpt",
        );
        if (subLink) nested.appendChild(subLink);
      }
      if (nested.children.length) results.appendChild(nested);
    }

    setSearchResultsOpen(input, results, Boolean(results.children.length));
  }

  function initMobileSearchTriggers() {
    document.querySelectorAll("[data-calepin-mobile-search-trigger]").forEach((button) => {
      if (button.dataset.calepinSearchBound === "true") return;
      button.dataset.calepinSearchBound = "true";
      button.addEventListener("click", (event) => {
        event.preventDefault();
        openPagefindSearch("");
      });
    });
  }

  function initNavbarSearch() {
    const input = document.querySelector("[data-calepin-search-input]");
    if (!input) return;
    const form = input.closest("[data-calepin-search-form]");
    const results = document.querySelector("[data-calepin-search-results]");

    let opening = false;
    let searchTimer = 0;
    let searchToken = 0;
    const openFromInput = async () => {
      if (opening) return;
      opening = true;
      try {
        const opened = await openPagefindSearch(input.value.trim());
        if (opened) input.value = "";
        if (opened && results) clearSearchResults(input, results);
      } finally {
        opening = false;
      }
    };

    const scheduleSuggestions = () => {
      if (!results) return;
      window.clearTimeout(searchTimer);
      const query = input.value.trim();
      const token = ++searchToken;
      if (query.length < SEARCH_SUGGESTION_MIN_LENGTH) {
        clearSearchResults(input, results);
        return;
      }

      searchTimer = window.setTimeout(async () => {
        try {
          const pagefind = await loadPagefindModule();
          const search = await pagefind.search(query);
          if (token !== searchToken || input.value.trim() !== query) return;
          const items = await Promise.all(
            search.results.slice(0, SEARCH_SUGGESTION_LIMIT).map((result) => result.data()),
          );
          if (token !== searchToken || input.value.trim() !== query) return;
          renderSearchResults(input, results, items);
        } catch {
          if (token === searchToken) renderSearchMessage(input, results, "Search unavailable");
        }
      }, SEARCH_SUGGESTION_DELAY);
    };

    form?.addEventListener("submit", (event) => {
      event.preventDefault();
      openFromInput();
    });

    input.addEventListener("input", scheduleSuggestions);
    input.addEventListener("focus", scheduleSuggestions);
    input.addEventListener("keydown", (event) => {
      if (event.key === "Enter") {
        event.preventDefault();
        openFromInput();
      } else if (event.key === "Escape") {
        if (results) clearSearchResults(input, results);
        input.blur();
      }
    });

    document.addEventListener("click", (event) => {
      if (!results || form?.contains(event.target)) return;
      clearSearchResults(input, results);
    });

    document.addEventListener("keydown", (event) => {
      const modifier = /Mac|iPhone|iPad|iPod/.test(window.navigator.platform)
        ? event.metaKey
        : event.ctrlKey;
      if (
        !modifier ||
        event.altKey ||
        event.shiftKey ||
        event.key.toLowerCase() !== "k" ||
        isTextEntryElement(document.activeElement)
      ) {
        return;
      }

      event.preventDefault();
      input.focus();
    });
  }

  function isLoopbackHost(hostname) {
    return (
      hostname === "localhost" ||
      hostname === "127.0.0.1" ||
      hostname === "0.0.0.0" ||
      hostname === "::1" ||
      hostname === "[::1]"
    );
  }

  function localCanonicalBasePathPrefix() {
    if (!isLoopbackHost(window.location.hostname)) return "";
    const canonical = document.querySelector('link[rel="canonical"]')?.href;
    if (!canonical) return "";

    try {
      const canonicalPath = new URL(canonical).pathname;
      const currentPath = window.location.pathname || "/";
      if (canonicalPath === currentPath) return "";
      if (canonicalPath.endsWith(currentPath)) {
        return canonicalPath.slice(0, -currentPath.length).replace(/\/$/, "");
      }
      if (canonicalPath.endsWith("/index.html")) {
        const indexPath = currentPath.endsWith("/") ? `${currentPath}index.html` : currentPath;
        if (canonicalPath.endsWith(indexPath)) {
          return canonicalPath.slice(0, -indexPath.length).replace(/\/$/, "");
        }
      }
    } catch {
      return "";
    }
    return "";
  }

  function hrefWithoutLocalBasePath(href, basePathPrefix) {
    if (!href || !basePathPrefix) return href;
    try {
      const url = new URL(href, window.location.href);
      if (url.origin !== window.location.origin) return href;
      if (url.pathname !== basePathPrefix && !url.pathname.startsWith(`${basePathPrefix}/`)) {
        return href;
      }
      url.pathname = url.pathname.slice(basePathPrefix.length) || "/";
      return url.toString();
    } catch {
      return href;
    }
  }

  function initPagefindLocalLinks() {
    const basePathPrefix = localCanonicalBasePathPrefix();
    if (!basePathPrefix) return;

    const selector =
      "pagefind-modal a[href], pagefind-searchbox a[href], .calepin-website-search-results a[href]";
    const normalizeLinks = () => {
      document.querySelectorAll(selector).forEach((link) => {
        const next = hrefWithoutLocalBasePath(link.getAttribute("href"), basePathPrefix);
        if (next && next !== link.getAttribute("href")) link.setAttribute("href", next);
      });
    };

    document.addEventListener("click", (event) => {
      const link = event.target.closest(selector);
      if (!link) return;
      const next = hrefWithoutLocalBasePath(link.getAttribute("href"), basePathPrefix);
      if (!next || next === link.getAttribute("href")) return;
      event.preventDefault();
      window.location.href = next;
    });

    normalizeLinks();
    new MutationObserver(normalizeLinks).observe(document.body, {
      childList: true,
      subtree: true,
    });
  }

  const hasSearch = !!(
    document.querySelector("pagefind-modal") ||
    document.querySelector("pagefind-searchbox") ||
    document.querySelector("[data-calepin-search-input]") ||
    document.querySelector("[data-calepin-search-results]") ||
    document.querySelector("[data-calepin-mobile-search-trigger]") ||
    document.querySelector(".calepin-website-search-results")
  );
  if (!hasSearch) return;

  initMobileSearchTriggers();
  initNavbarSearch();
  initPagefindLocalLinks();
})();


(() => {
  "use strict";

  function initScrollAwareTopbar() {
    const topbar = document.querySelector(".academic-topbar");
    if (!topbar) return;

    const menu = document.getElementById("academic-menu");
    const scrollThreshold = Math.max(32, topbar.offsetHeight * 1.5);
    let lastScrollY = window.scrollY;
    let ticking = false;

    function menuIsOpen() {
      return menu?.classList.contains("is-open") ?? false;
    }

    function setHidden(hidden) {
      topbar.classList.toggle("is-hidden", hidden);
    }

    function update() {
      const currentScrollY = Math.max(0, window.scrollY);
      const delta = currentScrollY - lastScrollY;
      const belowThreshold = currentScrollY > scrollThreshold;
      const scrollingDown = delta > 6;
      const scrollingUp = delta < -6;

      if (!belowThreshold || scrollingUp || menuIsOpen() || topbar.matches(":focus-within")) {
        setHidden(false);
      } else if (scrollingDown) {
        setHidden(true);
      }

      lastScrollY = currentScrollY;
      ticking = false;
    }

    function requestUpdate() {
      if (ticking) return;
      ticking = true;
      window.requestAnimationFrame(update);
    }

    window.addEventListener("scroll", requestUpdate, { passive: true });
    topbar.addEventListener("focusin", () => setHidden(false));
    topbar.addEventListener("focusout", requestUpdate);
  }

  function initMobileMenu() {
    const button = document.querySelector(".academic-nav-toggle");
    const menu = document.getElementById("academic-menu");
    if (!button || !menu) return;

    const topbar = button.closest(".academic-topbar");

    function setOpen(open) {
      menu.classList.toggle("is-open", open);
      topbar?.classList.remove("is-hidden");
      button.setAttribute("aria-expanded", open ? "true" : "false");
    }

    button.addEventListener("click", () => {
      setOpen(!menu.classList.contains("is-open"));
    });

    menu.addEventListener("click", (event) => {
      if (event.target.closest("a")) setOpen(false);
    });

    document.addEventListener("keydown", (event) => {
      if (event.key === "Escape") setOpen(false);
    });
  }

  initScrollAwareTopbar();
  initMobileMenu();
})();
