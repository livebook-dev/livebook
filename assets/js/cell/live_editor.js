import monaco from "./live_editor/monaco";
import EditorClient from "./live_editor/editor_client";
import MonacoEditorAdapter from "./live_editor/monaco_editor_adapter";
import HookServerAdapter from "./live_editor/hook_server_adapter";

/**
 * Mounts cell source editor with real-time collaboration mechanism.
 */
class LiveEditor {
  constructor(hook, container, cellId, type, source, revision) {
    this.hook = hook;
    this.container = container;
    this.cellId = cellId;
    this.type = type;
    this.source = source;
    this._onChange = null;
    this._onBlur = null;

    this.__mountEditor();

    if (type === "elixir") {
      this.__setupCompletion();
    }

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

  insert(text) {
    const range = this.editor.getSelection();
    this.editor
      .getModel()
      .pushEditOperations([], [{ forceMoveMarkers: true, range, text }]);
  }

  /**
   * Performs necessary cleanup actions.
   */
  destroy() {
    // Explicitly destroy the editor instance and its text model.
    this.editor.dispose();

    const model = this.editor.getModel();

    if (model) {
      model.dispose();
    }
  }

  __mountEditor() {
    this.editor = monaco.editor.create(this.container, {
      language: this.type,
      value: this.source,
      scrollbar: {
        vertical: "hidden",
        alwaysConsumeMouseWheel: false,
      },
      minimap: {
        enabled: false,
      },
      overviewRulerLanes: 0,
      scrollBeyondLastLine: false,
      renderIndentGuides: false,
      occurrencesHighlight: false,
      renderLineHighlight: "none",
      theme: "custom",
      fontFamily: "JetBrains Mono",
      tabIndex: -1,
      quickSuggestions: false,
      tabCompletion: "on",
      suggestSelection: "first",
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

  __setupCompletion() {
    /**
     * Completion happens asynchronously, the flow goes as follows:
     *
     *   * the user opens the completion list, which triggers the global
     *     completion provider registered in `live_editor/monaco.js`
     *
     *   * the global provider delegates to the cell-specific `__getCompletionItems`
     *     defined below. That's a little bit hacky, but this way we make
     *     completion cell-specific
     *
     *   * then `__getCompletionItems` sends a completion request to the LV process
     *     and gets a unique reference, under which it keeps completion callback
     *
     *   * finally the hook receives the "completion_response" event with completion items,
     *     it looks up completion callback for the received reference and calls it
     *     with the received items list
     */

    const completionHandlerByRef = {};

    this.editor.getModel().__getCompletionItems = (model, position) => {
      const line = model.getLineContent(position.lineNumber);
      const lineUntilCursor = line.slice(0, position.column - 1);

      return new Promise((resolve, reject) => {
        this.hook.pushEvent(
          "completion_request",
          {
            hint: lineUntilCursor,
            cell_id: this.cellId,
          },
          ({ completion_ref: completionRef }) => {
            if (completionRef) {
              completionHandlerByRef[completionRef] = (items) => {
                const suggestions = completionItemsToSuggestions(items);
                resolve({ suggestions });
              };
            } else {
              resolve({ suggestions: [] });
            }
          }
        );
      });
    };

    this.hook.handleEvent(
      "completion_response",
      ({ completion_ref: completionRef, items }) => {
        const handler = completionHandlerByRef[completionRef];

        if (handler) {
          handler(items);
          delete completionHandlerByRef[completionRef];
        }
      }
    );
  }
}

function completionItemsToSuggestions(items) {
  return items.map(parseItem).map((suggestion, index) => ({
    ...suggestion,
    sortText: numberToSortableString(index, items.length),
  }));
}

// See `Livebook.Runtime` for completion item definition
function parseItem(item) {
  return {
    label: item.label,
    kind: parseItemKind(item.kind),
    detail: item.detail,
    documentation: item.documentation && {
      value: item.documentation,
      isTrusted: true,
    },
    insertText: item.insert_text,
  };
}

function parseItemKind(kind) {
  switch (kind) {
    case "function":
      return monaco.languages.CompletionItemKind.Function;
    case "module":
      return monaco.languages.CompletionItemKind.Module;
    case "type":
      return monaco.languages.CompletionItemKind.Class;
    case "variable":
      return monaco.languages.CompletionItemKind.Variable;
    case "field":
      return monaco.languages.CompletionItemKind.Field;
    default:
      return null;
  }
}

function numberToSortableString(number, maxNumber) {
  return String(number).padStart(maxNumber, "0");
}

export default LiveEditor;
