import { getAttributeOrThrow } from "../lib/attribute";
import Markdown from "../cell/markdown";

/**
 * A hook used to render markdown content on the client.
 *
 * Configuration:
 *
 *   * `data-id` - id of the renderer, under which the content event is pushed
 */
const MarkdownRenderer = {
  mounted() {
    this.props = getProps(this);

    const markdown = new Markdown(this.el, "");

    this.handleEvent(
      `markdown-renderer:${this.props.id}:content`,
      ({ content }) => {
        markdown.setContent(content);
      }
    );
  },

  updated() {
    this.props = getProps(this);
  },
};

function getProps(hook) {
  return {
    id: getAttributeOrThrow(hook.el, "data-id"),
  };
}

export default MarkdownRenderer;
