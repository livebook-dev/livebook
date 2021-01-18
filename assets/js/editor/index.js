import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import EditorClient from "./editor_client";
import MonacoAdapter from "./monaco_adapter";
import HookAdapter from "./hook_adapter";

const Editor = {
  mounted() {
    this.cellId = this.el.dataset.id;
    this.type = this.el.dataset.type;

    const editorContainer = document.createElement("div");
    this.el.appendChild(editorContainer);

    this.editor = this.__mountEditor(editorContainer);

    const source = this.el.dataset.source;
    const revision = +this.el.dataset.revision;

    this.editor.getModel().setValue(source);

    new EditorClient(
      new HookAdapter(this, this.cellId),
      new MonacoAdapter(this.editor),
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
    });

    editor.getModel().updateOptions({
      tabSize: 2,
    });
    editor.updateOptions({
      autoIndent: true,
      tabSize: 2,
      formatOnType: true,
    });

    // Dynamically adjust editor width to the content, see https://github.com/microsoft/monaco-editor/issues/794
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

    // // Handle Ctrl + Enter
    // editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter, () => {
    //   this.pushEvent("evaluate_cell", { cell_id: this.cellId });
    // });

    return editor;
  },
};

export default Editor;
