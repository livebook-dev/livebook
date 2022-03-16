import HyperList from "hyperlist";
import {
  getAttributeOrThrow,
  parseBoolean,
  parseInteger,
} from "../lib/attribute";
import { findChildOrThrow, getLineHeight, isScrolledToEnd } from "../lib/utils";

/**
 * A hook used to render text lines as a virtual list, so that only
 * the visible lines are actually in the DOM.
 *
 * ## Configuration
 *
 *   * `data-max-height` - the maximum height of the element, exceeding
 *     this height enables scrolling
 *
 *   * `data-follow` - whether to automatically scroll to the bottom as
 *     new lines appear
 *
 * The element should have two children:
 *
 *   * `[data-template]` - a hidden container containing all the line
 *     elements, each with a data-line attribute
 *
 *   * `[data-content]` - the target element to render the virtualized
 *     lines into, it should contain the styling relevant text styles
 */
const VirtualizedLines = {
  mounted() {
    this.props = this.getProps();

    this.lineHeight = getLineHeight(this.el);
    this.templateEl = findChildOrThrow(this.el, "[data-template]");
    this.contentEl = findChildOrThrow(this.el, "[data-content]");

    const config = this.hyperListConfig();
    this.virtualizedList = new HyperList(this.contentEl, config);
  },

  updated() {
    this.props = this.getProps();

    const scrollToEnd = this.props.follow && isScrolledToEnd(this.contentEl);

    const config = this.hyperListConfig();
    this.virtualizedList.refresh(this.contentEl, config);

    if (scrollToEnd) {
      this.contentEl.scrollTop = this.contentEl.scrollHeight;
    }
  },

  getProps() {
    return {
      maxHeight: getAttributeOrThrow(this.el, "data-max-height", parseInteger),
      follow: getAttributeOrThrow(this.el, "data-follow", parseBoolean),
    };
  },

  hyperListConfig() {
    const lineEls = this.templateEl.querySelectorAll("[data-line]");
    const numberOfLines = lineEls.length;

    const height = Math.min(
      this.props.maxHeight,
      this.lineHeight * numberOfLines
    );

    return {
      height,
      total: numberOfLines,
      itemHeight: this.lineHeight,
      generate: (index) => {
        const node = lineEls[index].cloneNode(true);
        node.removeAttribute("id");
        return node;
      },
      afterRender: () => {
        // The content element has a fixed height and when the horizontal
        // scrollbar appears, it's treated as part of the element's content.
        // To accommodate for the scrollbar we dynamically add more height
        // to the element.
        if (this.contentEl.scrollWidth > this.contentEl.clientWidth) {
          this.contentEl.style.height = `${height + 12}px`;
        } else {
          this.contentEl.style.height = `${height}px`;
        }
      },
    };
  },
};

export default VirtualizedLines;
