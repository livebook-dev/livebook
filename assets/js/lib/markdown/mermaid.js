import { md5Base64 } from "../../lib/utils";
import CacheLRU from "../../lib/cache_lru";

let idCount = 0;
let getId = () => `mermaid-graph-${idCount++}`;

let mermaidInitialized = false;

const fontAwesomeVersion = "5.15.4";

const cache = new CacheLRU(25);

/**
 * Renders SVG graph from mermaid definition.
 */
export function renderMermaid(definition) {
  const hash = md5Base64(definition);
  const svg = cache.get(hash);

  if (svg) {
    return Promise.resolve(svg);
  }

  return importMermaid().then((mermaid) => {
    injectFontAwesomeIfNeeded(definition);

    try {
      const svg = mermaid.render(getId(), definition);
      cache.set(hash, svg);
      return svg;
    } catch (e) {
      return `<div class="error-box whitespace-pre-wrap"><span class="font-semibold">Mermaid</span>\n${e.message}</div>`;
    }
  });
}

function importMermaid() {
  return import(
    /* webpackChunkName: "mermaid" */
    "mermaid"
  ).then(({ default: mermaid }) => {
    if (!mermaidInitialized) {
      mermaid.initialize({ startOnLoad: false });
      mermaidInitialized = true;
    }
    return mermaid;
  });
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
