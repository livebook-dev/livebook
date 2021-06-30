import { getAttributeOrThrow } from "../lib/attribute";
import { highlight } from "../cell/live_editor/monaco";

/**
 * A hook used to highlight source code in the root element.
 *
 * Configuration:
 *
 *   * `data-language` - language of the source code
 */
const Highlight = {
  mounted() {
    this.props = getProps(this);

    const code = this.el.innerText;

    highlight(code, this.props.language).then((html) => {
      this.el.innerHTML = html;
    });
  },

  updated() {
    this.props = getProps(this);
  },
};

function getProps(hook) {
  return {
    language: getAttributeOrThrow(hook.el, "data-language"),
  };
}

export default Highlight;
