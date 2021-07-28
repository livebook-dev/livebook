import HyperList from "hyperlist";
import {
  getAttributeOrThrow,
  parseBoolean,
  parseInteger,
} from "../lib/attribute";
import { findChildOrThrow, getLineHeight } from "../lib/utils";

/**
 * A hook used to render text lines as a virtual list,
 * so that only the visible lines are actually in the DOM.
 *
 * Configuration:
 *
 *   * `data-max-height` - the maximum height of the element, exceeding this height enables scrolling
 *
 *   * `data-follow` - whether to automatically scroll to the bottom as new lines appear
 *
 * The element should have two children:
 *
 *   * one annotated with `data-template` attribute, it should be hidden
 *     and contain all the line elements as its children
 *
 *   * one annotated with `data-content` where the visible elements are rendered,
 *     it should contain any styling relevant for the container
 *
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

    this.state.templateElement = findChildOrThrow(this.el, "[data-template]");
    this.state.contentElement = findChildOrThrow(this.el, "[data-content]");

    const config = hyperListConfig(
      this.state.contentElement,
      this.state.templateElement,
      this.props.maxHeight,
      this.state.lineHeight
    );
    this.state.virtualizedList = new HyperList(
      this.state.contentElement,
      config
    );
  },

  updated() {
    this.props = getProps(this);

    const config = hyperListConfig(
      this.state.contentElement,
      this.state.templateElement,
      this.props.maxHeight,
      this.state.lineHeight
    );

    const scrollTop = Math.round(this.state.contentElement.scrollTop);
    const maxScrollTop = Math.round(
      this.state.contentElement.scrollHeight -
        this.state.contentElement.clientHeight
    );
    const isAtTheEnd = scrollTop === maxScrollTop;

    this.state.virtualizedList.refresh(this.state.contentElement, config);

    if (this.props.follow && isAtTheEnd) {
      this.state.contentElement.scrollTop =
        this.state.contentElement.scrollHeight;
    }
  },
};

function hyperListConfig(
  contentElement,
  templateElement,
  maxHeight,
  lineHeight
) {
  const numberOfLines = templateElement.childElementCount;
  const height = Math.min(maxHeight, lineHeight * numberOfLines);

  return {
    height,
    total: numberOfLines,
    itemHeight: lineHeight,
    generate: (index) => {
      // Clone n-th child of the template container.
      return templateElement.children.item(index).cloneNode(true);
    },
    afterRender: () => {
      // The content element has a fixed height and when the horizontal
      // scrollbar appears, it's treated as part of the element's content.
      // To accommodate for the scrollbar we dynamically add more height
      // to the element.
      if (contentElement.scrollWidth > contentElement.clientWidth) {
        contentElement.style.height = `${height + 12}px`;
      } else {
        contentElement.style.height = `${height}px`;
      }
    },
  };
}

function getProps(hook) {
  return {
    maxHeight: getAttributeOrThrow(hook.el, "data-max-height", parseInteger),
    follow: getAttributeOrThrow(hook.el, "data-follow", parseBoolean),
  };
}

export default VirtualizedLines;
