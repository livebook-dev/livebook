import marked from "marked";
import morphdom from "morphdom";
import DOMPurify from "dompurify";
import katex from "katex";
import monaco from "./live_editor/monaco";

// Reuse Monaco highlighter for Markdown code blocks
marked.setOptions({
  highlight: (code, lang, callback) => {
    monaco.editor
      .colorize(code, lang)
      .then((result) => {
        // `colorize` always adds additional newline, so we remove it
        result = result.replace(/<br\/>$/, "");
        callback(null, result);
      })
      .catch((error) => {
        callback(error, null);
      });
  },
});

// Modify external links, so that they open in a new tab.
// See https://github.com/cure53/DOMPurify/tree/main/demos#hook-to-open-all-links-in-a-new-window-link
DOMPurify.addHook("afterSanitizeAttributes", (node) => {
  if (
    node.tagName.toLowerCase() === "a" &&
    node.host !== window.location.host
  ) {
    node.setAttribute("target", "_blank");
    node.setAttribute("rel", "noreferrer noopener");
  }
});

/**
 * Renders markdown content in the given container.
 */
class Markdown {
  constructor(container, content, baseUrl = null) {
    this.container = container;
    this.content = content;
    this.baseUrl = baseUrl;

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
              Empty markdown cell
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
