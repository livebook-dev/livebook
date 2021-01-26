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

    const editorContainer = this.el.querySelector("div");

    if (!editorContainer) {
      throw new Error("Editor Hook root element should have a div child");
    }

    const source = editorContainer.dataset.source;
    const revision = +editorContainer.dataset.revision;

    this.editor = this.__mountEditor(editorContainer);

    this.editor.getModel().setValue(source);

    new EditorClient(
      new HookServerAdapter(this, this.cellId),
      new MonacoEditorAdapter(this.editor),
      revision
    );
  },

  __mountEditor(editorContainer) {
    const editor = monaco.editor.create(editorContainer, {
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

    editor.getModel().updateOptions({
      tabSize: 2,
    });

    editor.updateOptions({
      autoIndent: true,
      tabSize: 2,
      formatOnType: true,
    });

    // Dynamically adjust editor height to the content, see https://github.com/microsoft/monaco-editor/issues/794
    function adjustEditorLayout() {
      const contentHeight = editor.getContentHeight();
      editorContainer.style.height = `${contentHeight}px`;
      editor.layout();
    }

    editor.onDidContentSizeChange(adjustEditorLayout);
    adjustEditorLayout();

    window.addEventListener("resize", (event) => {
      editor.layout();
    });

    return editor;
  },
};

export default Editor;
