import monaco from "./live_editor/monaco";
import EditorClient from "./live_editor/editor_client";
import MonacoEditorAdapter from "./live_editor/monaco_editor_adapter";
import HookServerAdapter from "./live_editor/hook_server_adapter";

/**
 * Mounts cell source editor with real-time collaboration mechanism.
 */
class LiveEditor {
  constructor(hook, container, cellId, type, source, revision) {
    this.container = container;
    this.cellId = cellId;
    this.type = type;
    this.source = source;
    this._onChange = null;
    this._onBlur = null;

    this.__mountEditor();

    const serverAdapter = new HookServerAdapter(hook, cellId);
    const editorAdapter = new MonacoEditorAdapter(this.editor);
    this.editorClient = new EditorClient(
      serverAdapter,
      editorAdapter,
      revision
    );

    this.editorClient.onDelta((delta) => {
      this.source = delta.applyToString(this.source);
      this._onChange && this._onChange(this.source);
    });

    this.editor.onDidBlurEditorWidget(() => {
      this._onBlur && this._onBlur();
    });
  }

  /**
   * Registers a callback called with a new cell content whenever it changes.
   */
  onChange(callback) {
    this._onChange = callback;
  }

  /**
   * Registers a callback called whenever the editor loses focus.
   */
  onBlur(callback) {
    this._onBlur = callback;
  }

  focus() {
    this.editor.focus();
  }

  blur() {
    if (this.editor.hasTextFocus()) {
      document.activeElement.blur();
    }
  }

  __mountEditor() {
    this.editor = monaco.editor.create(this.container, {
      language: this.type,
      value: this.source,
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

    // Automatically adjust the editor size to fit the container.
    const resizeObserver = new ResizeObserver((entries) => {
      entries.forEach((entry) => {
        // Ignore hidden container.
        if (this.container.offsetHeight > 0) {
          this.editor.layout();
        }
      });
    });

    resizeObserver.observe(this.container);

    // Whenever editor content size changes (new line is added/removed)
    // update the container height. Thanks to the above observer
    // the editor is resized to fill the container.
    // Related: https://github.com/microsoft/monaco-editor/issues/794#issuecomment-688959283
    this.editor.onDidContentSizeChange(() => {
      const contentHeight = this.editor.getContentHeight();
      this.container.style.height = `${contentHeight}px`;
    });
  }
}

export default LiveEditor;
