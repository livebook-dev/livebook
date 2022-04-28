import LiveEditor from "./cell_editor/live_editor";
import { getAttributeOrThrow } from "../lib/attribute";

const CellEditor = {
  mounted() {
    this.props = this.getProps();

    this.handleEvent(
      `cell_editor_init:${this.props.cellId}:${this.props.tag}`,
      ({ source_view, language, intellisense, read_only }) => {
        const editorContainer = this.el.querySelector(
          `[data-el-editor-container]`
        );

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

        this.liveEditor.onMount(() => {
          // Remove the content placeholder
          const skeletonEl =
            editorContainer.querySelector(`[data-el-skeleton]`);
          skeletonEl && skeletonEl.remove();
        });

        this.el.dispatchEvent(
          new CustomEvent("lb:cell:editor_created", {
            detail: { tag: this.props.tag, liveEditor: this.liveEditor },
            bubbles: true,
          })
        );
      }
    );
  },

  disconnected() {
    // When disconnected, this client is no longer seen by the server
    // and misses all collaborative changes. On reconnection we want
    // to clean up and mount a fresh hook, which we force by ensuring
    // the DOM id doesn't match
    this.el.removeAttribute("id");
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

  getProps() {
    return {
      cellId: getAttributeOrThrow(this.el, "data-cell-id"),
      tag: getAttributeOrThrow(this.el, "data-tag"),
    };
  },
};

export default CellEditor;
