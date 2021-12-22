let idCount = 0;
let getId = () => `mermaid-graph-${idCount++}`;

let mermaidInitialized = false;

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

export function renderMermaid(value) {
  return importMermaid().then((mermaid) => {
    try {
      return mermaid.render(getId(), value);
    } catch (e) {
      return `<pre><code>${e.message}</code></pre>`;
    }
  });
}
