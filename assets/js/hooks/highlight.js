import { getAttributeOrThrow } from "../lib/attribute";
import { highlight } from "./cell_editor/live_editor/monaco";
import { findChildOrThrow } from "../lib/utils";

/**
 * A hook used to highlight source code in the root element.
 *
 * ## Configuration
 *
 *   * `data-language` - language of the source code
 *
 * The element should have two children:
 *
 *   * `[data-source]` - an element containing the source code to be
 *     highlighted
 *
 *   * `[data-target]` - the element to render highlighted code into
 */
const Highlight = {
  mounted() {
    this.props = this.getProps();

    this.sourceEl = findChildOrThrow(this.el, "[data-source]");
    this.targetEl = findChildOrThrow(this.el, "[data-target]");

    this.updateDOM();
  },

  updated() {
    this.props = this.getProps();
    this.updateDOM();
  },

  getProps() {
    return {
      language: getAttributeOrThrow(this.el, "data-language"),
    };
  },

  updateDOM() {
    const code = this.sourceEl.innerText;

    highlight(code, this.props.language).then((html) => {
      this.targetEl.innerHTML = html;
      this.el.setAttribute("data-highlighted", "");
    });
  },
};

export default Highlight;
