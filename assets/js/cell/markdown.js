import marked from "marked";
import morphdom from "morphdom";

/**
 * Renders markdown content in the given container.
 */
class Markdown {
  constructor(container, content) {
    this.container = container;
    this.content = content;

    this.__render();
  }

  setContent(content) {
    this.content = content;
    this.__render();
  }

  __render() {
    const html = this.__getHtml();
    // Wrap the HTML in another element, so that we
    // can use morphdom's childrenOnly option.
    const wrappedHtml = `<div>${html}</div>`;

    morphdom(this.container, wrappedHtml, { childrenOnly: true });
  }

  __getHtml() {
    const html = marked(this.content);

    if (html) {
      return html;
    } else {
      return `
        <div class="text-gray-300">
          Empty markdown cell
        </div>
      `;
    }
  }
}

export default Markdown;
