import monaco from "./monaco";
import EditorClient from "./editor_client";
import MonacoEditorAdapter from "./monaco_editor_adapter";
import HookServerAdapter from "./hook_server_adapter";

/**
 * A hook managing an editable cell.
 *
 * Mounts a Monaco Editor and provides real-time collaboration mechanism
 * by sending all changes as `Delta` objects to the server
 * and handling such objects sent by other clients.
 *
 * Configuration:
 *
 *   * `data-cell-id` - id of the cell being edited
 *   * `data-type` - editor type (i.e. language), either "markdown" or "elixir" is expected
 *
 * Additionally the root element should have a direct `div` child
 * with `data-source` and `data-revision` providing the initial values.
 */
const Editor = {
  mounted() {
    this.cellId = this.el.dataset.cellId;
    this.type = this.el.dataset.type;

    this.editorContainer = this.el.querySelector("div");

    if (!this.editorContainer) {
      throw new Error("Editor Hook root element should have a div child");
    }

    this.__mountEditor();

    const source = this.editorContainer.dataset.source;
    const revision = +this.editorContainer.dataset.revision;

    this.editor.getModel().setValue(source);

    new EditorClient(
      new HookServerAdapter(this, this.cellId),
      new MonacoEditorAdapter(this.editor),
      revision
    );
  },

  updated() {
    if (!this.__isHidden()) {
      // The editor might've been hidden and didn't get a change
      // to fit the space, so let's trigger that.
      this.__adjustEditorLayout();
    }
  },

  __isHidden() {
    return this.el.dataset.hidden === "true";
  },

  __mountEditor() {
    this.editor = monaco.editor.create(this.editorContainer, {
      language: this.type,
      value: "",
      scrollbar: {
        vertical: "hidden",
        handleMouseWheel: false,
      },
      minimap: {
        enabled: false,
      },
      overviewRulerLanes: 0,
      scrollBeyondLastLine: false,
      quickSuggestions: false,
      renderIndentGuides: false,
      occurrencesHighlight: false,
      renderLineHighlight: "none",
      theme: "custom",
    });

    this.editor.getModel().updateOptions({
      tabSize: 2,
    });

    this.editor.updateOptions({
      autoIndent: true,
      tabSize: 2,
      formatOnType: true,
    });

    this.editor.onDidContentSizeChange(() => this.__adjustEditorLayout());
    this.__adjustEditorLayout();

    window.addEventListener("resize", (event) => {
      this.editor.layout();
    });
  },

  __adjustEditorLayout() {
    // Dynamically adjust editor height to the content, see https://github.com/microsoft/monaco-editor/issues/794
    const contentHeight = this.editor.getContentHeight();
    this.editorContainer.style.height = `${contentHeight}px`;
    this.editor.layout();
  }
};

export default Editor;
