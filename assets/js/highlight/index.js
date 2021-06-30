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

    highlightIn(this.el, this.props.language);
  },

  updated() {
    this.props = getProps(this);

    highlightIn(this.el, this.props.language);
  },
};

function getProps(hook) {
  return {
    language: getAttributeOrThrow(hook.el, "data-language"),
  };
}

function highlightIn(element, language) {
  const code = element.innerText;

  highlight(code, language).then((html) => {
    element.innerHTML = html;
  });
}

export default Highlight;
