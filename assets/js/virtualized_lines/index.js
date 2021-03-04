import HyperList from "hyperlist";
import { getAttributeOrThrow, parseInteger, parseJSON } from "../lib/attribute";

/**
 * A hook used to render text lines as a virtual list,
 * so that only the visible lines are actually in the DOM.
 *
 * Configuration:
 *
 *   * `data-lines` - a JSON array of lines to render (a line may contain HTML elements)
 *   * `data-max-height` - the maximum height of the element, exceeding this height enables scrolling
 */
const VirtualizedLines = {
  mounted() {
    this.props = getProps(this);

    const computedStyle = window.getComputedStyle(this.el);
    this.lineHeight = parseInt(computedStyle.lineHeight, 10);

    const config = hyperListConfig(
      this.props.maxHeight,
      this.props.lines,
      this.lineHeight
    );
    this.virtualizedList = new HyperList(this.el, config);
  },

  updated() {
    this.props = getProps(this);

    const config = hyperListConfig(
      this.props.maxHeight,
      this.props.lines,
      this.lineHeight
    );
    this.virtualizedList.refresh(this.el, config);
  },
};

function hyperListConfig(maxHeight, lines, lineHeight) {
  return {
    height: Math.min(maxHeight, lineHeight * lines.length),
    total: lines.length,
    itemHeight: lineHeight,
    generate: (index) => {
      const node = document.createElement("div");
      node.innerHTML = lines[index];
      return node;
    },
  };
}

function getProps(hook) {
  return {
    lines: getAttributeOrThrow(hook.el, "data-lines", parseJSON),
    maxHeight: getAttributeOrThrow(hook.el, "data-max-height", parseInteger),
  };
}

export default VirtualizedLines;
