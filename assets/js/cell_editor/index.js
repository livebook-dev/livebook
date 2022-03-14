import LiveEditor from "./live_editor";
import { getAttributeOrThrow } from "../lib/attribute";

const CellEditor = {
  mounted() {
    this.props = getProps(this);

    this.handleEvent(
      `cell_editor_init:${this.props.cellId}:${this.props.tag}`,
      ({ source_view, language, intellisense, read_only }) => {
        const editorContainer = this.el.querySelector(
          `[data-element="editor-container"]`
        );

        // Remove the content placeholder
        editorContainer.firstElementChild.remove();
        const editorEl = document.createElement("div");
        editorContainer.appendChild(editorEl);

        this.liveEditor = new LiveEditor(
          this,
          editorEl,
          this.props.cellId,
          this.props.tag,
          source_view.source,
          source_view.revision,
          language,
          intellisense,
          read_only
        );

        this.el.dispatchEvent(
          new CustomEvent("lb:cell:editor_created", {
            detail: { tag: this.props.tag, liveEditor: this.liveEditor },
            bubbles: true,
          })
        );
      }
    );
  },

  destroyed() {
    if (this.liveEditor) {
      this.el.dispatchEvent(
        new CustomEvent("lb:cell:editor_removed", {
          detail: { tag: this.props.tag },
          bubbles: true,
        })
      );
      this.liveEditor.dispose();
    }
  },
};

function getProps(hook) {
  return {
    cellId: getAttributeOrThrow(hook.el, "data-cell-id"),
    tag: getAttributeOrThrow(hook.el, "data-tag"),
  };
}

export default CellEditor;
