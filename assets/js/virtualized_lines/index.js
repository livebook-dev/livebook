import HyperList from "hyperlist";
import { getAttributeOrThrow, parseInteger } from "../lib/attribute";
import { getLineHeight } from "../lib/utils";

/**
 * A hook used to render text lines as a virtual list,
 * so that only the visible lines are actually in the DOM.
 *
 * Configuration:
 *
 *   * `data-max-height` - the maximum height of the element, exceeding this height enables scrolling
 *
 * The element should have two children:
 *
 *   * one annotated with `data-template` attribute, it should be hidden
 *     and contain all the line elements as its children
 *
 *   * one annotated with `data-content` where the visible elements are rendered,
 *     it should contain any styling relevant for the container
 */
const VirtualizedLines = {
  mounted() {
    this.props = getProps(this);
    this.state = {
      lineHeight: null,
      templateElement: null,
      contentElement: null,
      virtualizedList: null,
    };

    this.state.lineHeight = getLineHeight(this.el);

    this.state.templateElement = this.el.querySelector("[data-template]");

    if (!this.state.templateElement) {
      throw new Error(
        "VirtualizedLines must have a child with data-template attribute"
      );
    }

    this.state.contentElement = this.el.querySelector("[data-content]");

    if (!this.state.templateElement) {
      throw new Error("VirtualizedLines must have a child with data-content");
    }

    const config = hyperListConfig(
      this.state.templateElement,
      this.props.maxHeight,
      this.state.lineHeight
    );
    this.virtualizedList = new HyperList(this.state.contentElement, config);
  },

  updated() {
    this.props = getProps(this);

    const config = hyperListConfig(
      this.state.templateElement,
      this.props.maxHeight,
      this.state.lineHeight
    );
    this.virtualizedList.refresh(this.state.contentElement, config);
  },
};

function hyperListConfig(templateElement, maxHeight, lineHeight) {
  const numberOfLines = templateElement.childElementCount;

  return {
    height: Math.min(maxHeight, lineHeight * numberOfLines),
    total: numberOfLines,
    itemHeight: lineHeight,
    generate: (index) => {
      // Clone n-th child of the template container.
      return templateElement.children.item(index).cloneNode(true);
    },
  };
}

function getProps(hook) {
  return {
    maxHeight: getAttributeOrThrow(hook.el, "data-max-height", parseInteger),
  };
}

export default VirtualizedLines;
