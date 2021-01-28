import {
  getAttributeOrThrow,
  parseBoolean,
  parseInteger,
} from "../lib/attribute";
import LiveEditor from "./live_editor";
import Markdown from "./markdown";

/**
 * A hook managing a single cell.
 *
 * Mounts and manages the collaborative editor,
 * takes care of markdown rendering and focusing the editor when applicable.
 *
 * Configuration:
 *
 *   * `data-cell-id` - id of the cell being edited
 *   * `data-type` - editor type (i.e. language), either "markdown" or "elixir" is expected
 *   * `data-focused` - whether the cell is currently focused
 *   * `data-expanded` - whether the cell is currently expanded (relevant for markdown cells)
 *
 * Additionally the root element should have a `div[data-init]` child (rendered once)
 * holding the initial data rendered only once:
 *
 *   * `data-source` - the initial cell source
 *   * `data-revision` - the initial cell revision corresponding to the source
 */
const Cell = {
  mounted() {
    this.props = getProps(this);

    const editorContainer = this.el.querySelector("[data-editor-container]");
    // Remove the content placeholder.
    editorContainer.firstElementChild.remove();
    // Create an empty container for the editor to be mounted in.
    const editorElement = document.createElement("div");
    editorContainer.appendChild(editorElement);
    // Setup the editor instance.
    this.liveEditor = new LiveEditor(
      this,
      editorElement,
      this.props.cellId,
      this.props.type,
      this.props.init.source,
      this.props.init.revision
    );

    // Setup markdown rendering.
    if (this.props.type === "markdown") {
      const markdownContainer = this.el.querySelector(
        "[data-markdown-container]"
      );
      const markdown = new Markdown(markdownContainer, this.props.init.source);

      this.liveEditor.onChange((newSource) => {
        markdown.setContent(newSource);
      });
    }
  },

  updated() {
    const prevProps = this.props;
    this.props = getProps(this);

    if (!isActive(prevProps) && isActive(this.props)) {
      this.liveEditor.focus();
    }

    if (isActive(prevProps) && !isActive(this.props)) {
      this.liveEditor.blur();
    }
  },
};

function getProps(hook) {
  const initElement = hook.el.querySelector("[data-init]");

  return {
    cellId: getAttributeOrThrow(hook.el, "data-cell-id"),
    type: getAttributeOrThrow(hook.el, "data-type"),
    isFocused: getAttributeOrThrow(hook.el, "data-focused", parseBoolean),
    isExpanded: getAttributeOrThrow(hook.el, "data-expanded", parseBoolean),
    init: {
      source: getAttributeOrThrow(initElement, "data-source"),
      revision: getAttributeOrThrow(initElement, "data-revision", parseInteger),
    },
  };
}

/**
 * Checks if the cell editor is active and should have focus.
 */
function isActive(props) {
  if (props.type === "markdown") {
    return props.isFocused && props.isExpanded;
  } else {
    return props.isFocused;
  }
}

export default Cell;
