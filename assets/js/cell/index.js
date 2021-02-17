import { getAttributeOrThrow, parseBoolean } from "../lib/attribute";
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
 *   * `data-insert-mode` - whether insert mode is currently enabled
 */
const Cell = {
  mounted() {
    this.props = getProps(this);

    this.pushEvent("cell_init", { cell_id: this.props.cellId }, (payload) => {
      const { source, revision } = payload;

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
        source,
        revision
      );

      // Setup markdown rendering.
      if (this.props.type === "markdown") {
        const markdownContainer = this.el.querySelector(
          "[data-markdown-container]"
        );
        const markdown = new Markdown(markdownContainer, source);

        this.liveEditor.onChange((newSource) => {
          markdown.setContent(newSource);
        });
      }

      // New cells are initially focused, so check for such case.

      if (isActive(this.props)) {
        this.liveEditor.focus();
      }

      if (this.props.isFocused) {
        this.el.scrollIntoView({ behavior: "smooth", block: "center" });
      }
    });
  },

  updated() {
    const prevProps = this.props;
    this.props = getProps(this);

    // Note: this.liveEditor is crated once we receive initial data
    // so here we have to make sure it's defined.

    if (!isActive(prevProps) && isActive(this.props)) {
      this.liveEditor && this.liveEditor.focus();
    }

    if (isActive(prevProps) && !isActive(this.props)) {
      this.liveEditor && this.liveEditor.blur();
    }

    if (!prevProps.isFocused && this.props.isFocused) {
      // Note: it's important to trigger scrolling after focus, so it doesn't get interrupted.
      this.el.scrollIntoView({ behavior: "smooth", block: "center" });
    }
  },
};

function getProps(hook) {
  return {
    cellId: getAttributeOrThrow(hook.el, "data-cell-id"),
    type: getAttributeOrThrow(hook.el, "data-type"),
    isFocused: getAttributeOrThrow(hook.el, "data-focused", parseBoolean),
    insertMode: getAttributeOrThrow(hook.el, "data-insert-mode", parseBoolean),
  };
}

/**
 * Checks if the cell editor is active and should have focus.
 */
function isActive(props) {
  return props.isFocused && props.insertMode;
}

export default Cell;
