import { getAttributeOrThrow } from "../lib/attribute";
import Markdown from "../lib/markdown";
import { findChildOrThrow } from "../lib/utils";

/**
 * A hook used to render Markdown content on the client.
 *
 * ## Configuration
 *
 *   * `data-base-path` - the path to resolve relative URLs against
 *
 *   * `data-allowed-uri-schemes` - a comma separated list of additional
 *     URI schemes that should be kept during sanitization
 *
 * The element should have two children:
 *
 *   * `[data-template]` - a hidden container containing the markdown
 *     content. The DOM structure is ignored, only text content matters
 *
 *   * `[data-content]` - the target element to render results into
 *
 */
const MarkdownRenderer = {
  mounted() {
    this.props = this.getProps();

    this.templateEl = findChildOrThrow(this.el, "[data-template]");
    this.contentEl = findChildOrThrow(this.el, "[data-content]");

    this.markdown = new Markdown(this.contentEl, this.templateEl.textContent, {
      baseUrl: this.props.basePath,
      allowedUriSchemes: this.props.allowedUriSchemes.split(","),
    });
  },

  updated() {
    this.props = this.getProps();

    this.markdown.setContent(this.templateEl.textContent);
  },

  getProps() {
    return {
      basePath: getAttributeOrThrow(this.el, "data-base-path"),
      allowedUriSchemes: getAttributeOrThrow(
        this.el,
        "data-allowed-uri-schemes"
      ),
    };
  },
};

export default MarkdownRenderer;
