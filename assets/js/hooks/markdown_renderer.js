import { getAttributeOrThrow } from "../lib/attribute";
import Markdown from "../lib/markdown";

/**
 * A hook used to render Markdown content on the client.
 *
 * ## Configuration
 *
 *   * `data-id` - id of the renderer, under which the content event
 *     is pushed
 */
const MarkdownRenderer = {
  mounted() {
    this.props = this.getProps();

    const markdown = new Markdown(this.el, "");

    this.handleEvent(
      `markdown_renderer:${this.props.id}:content`,
      ({ content }) => {
        markdown.setContent(content);
      }
    );
  },

  getProps() {
    return {
      id: getAttributeOrThrow(this.el, "data-id"),
    };
  },
};

export default MarkdownRenderer;
