import { parseHookProps } from "../lib/attribute";
import { highlight } from "./cell_editor/live_editor/highlight";
import { findChildOrThrow } from "../lib/utils";

/**
 * A hook used to highlight source code in the root element.
 *
 * ## Props
 *
 *   * `language` - language of the source code
 *
 * ## Children
 *
 *   * `[data-source]` - an element containing the source code to be
 *     highlighted
 *
 *   * `[data-target]` - the element to render highlighted code into
 *
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
    return parseHookProps(this.el, ["language"]);
  },

  updateDOM() {
    const code = this.sourceEl.innerText;

    const html = highlight(code, this.props.language);
    this.targetEl.innerHTML = html;
    this.sourceEl.style.display = "none";
  },
};

export default Highlight;
