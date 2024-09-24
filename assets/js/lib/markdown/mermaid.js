import { md5Base64 } from "../../lib/utils";
import CacheLRU from "../../lib/cache_lru";

let idCount = 0;
let getId = () => `mermaid-graph-${idCount++}`;

const fontAwesomeVersion = "5.15.4";

const cache = new CacheLRU(25);

/**
 * Renders SVG graph from mermaid definition.
 */
export function renderMermaid(definition, options) {
  const hash = md5Base64(definition);
  const svg = cache.get(hash);

  if (svg) {
    return Promise.resolve(svg);
  }

  return importMermaid().then((mermaid) => {
    injectFontAwesomeIfNeeded(definition);

    // There is no way to specify options for individual render, so
    // we call initialize every time to set the global options.
    // See https://github.com/mermaid-js/mermaid/issues/5427
    mermaid.initialize({ startOnLoad: false, ...options });

    return mermaid
      .render(getId(), definition)
      .then(({ svg }) => {
        cache.set(hash, svg);
        return svg;
      })
      .catch((error) => {
        return `<div class="error-box whitespace-pre-wrap"><span class="font-semibold">Mermaid</span>\n${error.message}</div>`;
      });
  });
}

function importMermaid() {
  return import("mermaid").then(({ default: mermaid }) => mermaid);
}

function injectFontAwesomeIfNeeded(definition) {
  const fontAwesomeUrl = `https://cdnjs.cloudflare.com/ajax/libs/font-awesome/${fontAwesomeVersion}/css/all.min.css`;

  // Graphs may include Font Awesome icons via fa: prefix, so we
  // load the icon set if needed
  if (
    definition.includes("fa:") &&
    !document.querySelector(`link[href="${fontAwesomeUrl}"]`)
  ) {
    const link = document.createElement("link");
    link.rel = "stylesheet";
    link.type = "text/css";
    link.href = fontAwesomeUrl;
    document.head.appendChild(link);
  }
}
