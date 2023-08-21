import LiveEditor from "./cell_editor/live_editor";
import {
  getAttributeOrDefault,
  getAttributeOrThrow,
  parseBoolean,
} from "../lib/attribute";
import { settingsStore } from "../lib/settings";
import { initVimMode } from "monaco-vim";

const CellEditor = {
  mounted() {
    this.props = this.getProps();
    const settings = settingsStore.get();

    this.handleEvent(
      `cell_editor_init:${this.props.cellId}:${this.props.tag}`,
      ({ source, revision, doctest_reports, code_markers }) => {
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
          source,
          revision,
          this.props.language,
          this.props.intellisense,
          this.props.readOnly,
          code_markers,
          doctest_reports
        );

        this.liveEditor.onMount(() => {
          // Remove the content placeholder
          const skeletonEl =
            editorContainer.querySelector(`[data-el-skeleton]`);
          skeletonEl && skeletonEl.remove();

          if (settings.editor_vim_mode) {
            this.vimMode = initVimMode(this.liveEditor.editor);
            this.vimMode.on("vim-mode-change", ({"mode": mode}) => {
              editorEl.setAttribute("data-vim-mode", mode);
            });
          }
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
      language: getAttributeOrDefault(this.el, "data-language", null),
      intellisense: getAttributeOrThrow(
        this.el,
        "data-intellisense",
        parseBoolean
      ),
      readOnly: getAttributeOrThrow(this.el, "data-read-only", parseBoolean),
    };
  },
};

export default CellEditor;
