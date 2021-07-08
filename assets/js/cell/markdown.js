import marked from "marked";
import morphdom from "morphdom";
import DOMPurify from "dompurify";
import katex from "katex";
import { highlight } from "./live_editor/monaco";

// Custom renderer overrides
const renderer = new marked.Renderer();
renderer.link = function (href, title, text) {
  // Browser normalizes URLs with .. so we use a __parent__ modifier
  // instead and handle it on the server
  href = href
    .split("/")
    .map((part) => (part === ".." ? "__parent__" : part))
    .join("/");

  return marked.Renderer.prototype.link.call(this, href, title, text);
};

marked.setOptions({
  renderer,
  // Reuse Monaco highlighter for Markdown code blocks
  highlight: (code, lang, callback) => {
    highlight(code, lang)
      .then((html) => callback(null, html))
      .catch((error) => callback(error, null));
  },
});

// Modify external links, so that they open in a new tab.
// See https://github.com/cure53/DOMPurify/tree/main/demos#hook-to-open-all-links-in-a-new-window-link
DOMPurify.addHook("afterSanitizeAttributes", (node) => {
  if (node.tagName.toLowerCase() === "a") {
    if (node.host !== window.location.host) {
      node.setAttribute("target", "_blank");
      node.setAttribute("rel", "noreferrer noopener");
    } else {
      node.setAttribute("data-phx-link", "redirect");
      node.setAttribute("data-phx-link-state", "push");
    }
  }
});

/**
 * Renders markdown content in the given container.
 */
class Markdown {
  constructor(container, content, { baseUrl = null, emptyText = "" } = {}) {
    this.container = container;
    this.content = content;
    this.baseUrl = baseUrl;
    this.emptyText = emptyText;

    this.__render();
  }

  setContent(content) {
    this.content = content;
    this.__render();
  }

  __render() {
    this.__getHtml().then((html) => {
      // Wrap the HTML in another element, so that we
      // can use morphdom's childrenOnly option.
      const wrappedHtml = `<div>${html}</div>`;

      morphdom(this.container, wrappedHtml, { childrenOnly: true });
    });
  }

  __getHtml() {
    return new Promise((resolve, reject) => {
      // Marked requires a trailing slash in the base URL
      const opts = { baseUrl: this.baseUrl + "/" };

      // Render math formulas using KaTeX.
      // The resulting <span> tags will pass through
      // marked.js and sanitization unchanged.
      //
      // We render math before anything else, because passing
      // TeX through markdown renderer may have undesired
      // effects like rendering \\ as \.
      const contentWithRenderedMath = this.__renderMathInString(this.content);

      marked(contentWithRenderedMath, opts, (error, html) => {
        const sanitizedHtml = DOMPurify.sanitize(html);

        if (sanitizedHtml) {
          resolve(sanitizedHtml);
        } else {
          resolve(`
            <div class="text-gray-300">
              ${this.emptyText}
            </div>
          `);
        }
      });
    });
  }

  // Replaces TeX formulas in string with rendered HTML using KaTeX.
  __renderMathInString(string) {
    return string.replace(
      /(\${1,2})([\s\S]*?)\1/g,
      (match, delimiter, math) => {
        const displayMode = delimiter === "$$";

        return katex.renderToString(math.trim(), {
          displayMode,
          throwOnError: false,
          errorColor: "inherit",
        });
      }
    );
  }
}

export default Markdown;
