import HyperList from "hyperlist";
import {
  getAttributeOrDefault,
  getAttributeOrThrow,
  parseBoolean,
  parseInteger,
} from "../lib/attribute";
import {
  findChildOrThrow,
  getLineHeight,
  isScrolledToEnd,
  scrollToEnd,
} from "../lib/utils";

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
 *     new lines appear. Defaults to false
 *
 *   * `data-max-lines` - the maximum number of lines to keep in the DOM.
 *     By default all lines are kept
 *
 *   * `data-ignore-trailing-empty-line` - whether to ignore the last
 *     line if it is empty. Defaults to false
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

    this.capLines();

    const config = this.hyperListConfig();
    this.virtualizedList = new HyperList(this.contentEl, config);

    if (this.props.follow) {
      scrollToEnd(this.contentEl);
    }
  },

  updated() {
    this.props = this.getProps();

    this.capLines();

    const shouldScrollToEnd =
      this.props.follow && isScrolledToEnd(this.contentEl);

    const config = this.hyperListConfig();
    this.virtualizedList.refresh(this.contentEl, config);

    if (shouldScrollToEnd) {
      scrollToEnd(this.contentEl);
    }
  },

  getProps() {
    return {
      maxHeight: getAttributeOrThrow(this.el, "data-max-height", parseInteger),
      follow: getAttributeOrDefault(
        this.el,
        "data-follow",
        false,
        parseBoolean
      ),
      maxLines: getAttributeOrDefault(
        this.el,
        "data-max-lines",
        null,
        parseInteger
      ),
      ignoreTrailingEmptyLine: getAttributeOrDefault(
        this.el,
        "data-ignore-trailing-empty-line",
        false,
        parseBoolean
      ),
    };
  },

  hyperListConfig() {
    const lineEls = this.getLineElements();
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

  getLineElements() {
    const lineEls = Array.from(this.templateEl.querySelectorAll("[data-line]"));

    if (lineEls.length === 0) {
      return [];
    }

    const lastLineEl = lineEls[lineEls.length - 1];

    if (this.props.ignoreTrailingEmptyLine && lastLineEl.innerText === "") {
      return lineEls.slice(0, -1);
    } else {
      return lineEls;
    }
  },

  capLines() {
    if (this.props.maxLines) {
      const lineEls = Array.from(
        this.templateEl.querySelectorAll("[data-line]")
      );
      const ignoredLineEls = lineEls.slice(0, -this.props.maxLines);

      const [first, ...rest] = ignoredLineEls;
      rest.forEach((lineEl) => lineEl.remove());

      if (first) {
        first.innerHTML = "...";
      }
    }
  },
};

export default VirtualizedLines;
