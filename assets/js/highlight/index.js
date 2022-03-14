import { getAttributeOrThrow } from "../lib/attribute";
import { highlight } from "../cell_editor/live_editor/monaco";
import { findChildOrThrow } from "../lib/utils";

/**
 * A hook used to highlight source code in the root element.
 *
 * Configuration:
 *
 *   * `data-language` - language of the source code
 *
 * The element should have two children:
 *
 *   * one annotated with `data-source` attribute, it should contain
 *     the source code to be highlighted
 *
 *   * one annotated with `data-target` where the highlighted code
 *     is rendered
 */
const Highlight = {
  mounted() {
    this.props = getProps(this);
    this.state = {
      sourceElement: null,
      originalElement: null,
    };

    this.state.sourceElement = findChildOrThrow(this.el, "[data-source]");
    this.state.targetElement = findChildOrThrow(this.el, "[data-target]");

    highlightInto(
      this.state.targetElement,
      this.state.sourceElement,
      this.props.language
    ).then(() => {
      this.el.setAttribute("data-highlighted", "true");
    });
  },

  updated() {
    this.props = getProps(this);

    highlightInto(
      this.state.targetElement,
      this.state.sourceElement,
      this.props.language
    ).then(() => {
      this.el.setAttribute("data-highlighted", "true");
    });
  },
};

function getProps(hook) {
  return {
    language: getAttributeOrThrow(hook.el, "data-language"),
  };
}

function highlightInto(targetElement, sourceElement, language) {
  const code = sourceElement.innerText;

  return highlight(code, language).then((html) => {
    targetElement.innerHTML = html;
  });
}

export default Highlight;
