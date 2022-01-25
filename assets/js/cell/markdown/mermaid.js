let idCount = 0;
let getId = () => `mermaid-graph-${idCount++}`;

let mermaidInitialized = false;

const fontAwesomeVersion = "5.15.4";

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

const maybeInjectFontAwesome = (value) => {
  const fontAwesomeUrl = `https://cdnjs.cloudflare.com/ajax/libs/font-awesome/${fontAwesomeVersion}/css/all.min.css`;
  if (
    value.includes("fa:") &&
    !document.querySelector(`link[href="${fontAwesomeUrl}"]`)
  ) {
    const link = document.createElement("link");
    link.rel = "stylesheet";
    link.type = "text/css";
    link.href = fontAwesomeUrl;
    document.head.appendChild(link);
  }
};

export function renderMermaid(value) {
  return importMermaid().then((mermaid) => {
    // Inject font-awesome when fa: prefix is used
    maybeInjectFontAwesome(value);

    try {
      return mermaid.render(getId(), value);
    } catch (e) {
      return `<div class="error-box whitespace-pre-wrap"><span class="font-semibold">Mermaid</span>\n${e.message}</div>`;
    }
  });
}
