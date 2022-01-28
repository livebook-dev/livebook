import monaco from "./live_editor/monaco";
import EditorClient from "./live_editor/editor_client";
import MonacoEditorAdapter from "./live_editor/monaco_editor_adapter";
import HookServerAdapter from "./live_editor/hook_server_adapter";
import RemoteUser from "./live_editor/remote_user";
import { replacedSuffixLength } from "../lib/text_utils";
import { settingsStore } from "../lib/settings";

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
    this._onCursorSelectionChange = null;
    this._remoteUserByClientPid = {};

    this.__mountEditor();

    if (type === "elixir") {
      this.__setupIntellisense();
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

    this.editor.onDidFocusEditorWidget(() => {
      this.editor.updateOptions({ matchBrackets: "always" });
    });

    this.editor.onDidBlurEditorWidget(() => {
      this.editor.updateOptions({ matchBrackets: "never" });
      this._onBlur && this._onBlur();
    });

    this.editor.onDidChangeCursorSelection((event) => {
      this._onCursorSelectionChange &&
        this._onCursorSelectionChange(event.selection);
    });
  }

  /**
   * Returns current editor content.
   */
  getSource() {
    return this.source;
  }

  /**
   * Registers a callback called with a new cell content whenever it changes.
   */
  onChange(callback) {
    this._onChange = callback;
  }

  /**
   * Registers a callback called with a new cursor selection whenever it changes.
   */
  onCursorSelectionChange(callback) {
    this._onCursorSelectionChange = callback;
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
  dispose() {
    // Explicitly destroy the editor instance and its text model.
    this.editor.dispose();

    const model = this.editor.getModel();

    if (model) {
      model.dispose();
    }
  }

  /**
   * Either adds or moves remote user cursor to the new position.
   */
  updateUserSelection(client, selection) {
    if (this._remoteUserByClientPid[client.pid]) {
      this._remoteUserByClientPid[client.pid].update(selection);
    } else {
      this._remoteUserByClientPid[client.pid] = new RemoteUser(
        this.editor,
        selection,
        client.hex_color,
        client.name
      );
    }
  }

  /**
   * Removes remote user cursor.
   */
  removeUserSelection(client) {
    if (this._remoteUserByClientPid[client.pid]) {
      this._remoteUserByClientPid[client.pid].dispose();
      delete this._remoteUserByClientPid[client.pid];
    }
  }

  /**
   * Adds an underline marker for the given syntax error.
   *
   * To clear an existing marker `null` error is also supported.
   */
  setCodeErrorMarker(error) {
    const owner = "elixir.error.syntax";

    if (error) {
      const line = this.editor.getModel().getLineContent(error.line);
      const [, leadingWhitespace, trailingWhitespace] =
        line.match(/^(\s*).*?(\s*)$/);

      monaco.editor.setModelMarkers(this.editor.getModel(), owner, [
        {
          startLineNumber: error.line,
          startColumn: leadingWhitespace.length + 1,
          endLineNumber: error.line,
          endColumn: line.length + 1 - trailingWhitespace.length,
          message: error.description,
          severity: monaco.MarkerSeverity.Error,
        },
      ]);
    } else {
      monaco.editor.setModelMarkers(this.editor.getModel(), owner, []);
    }
  }

  __mountEditor() {
    const settings = settingsStore.get();

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
      guides: {
        indentation: false,
      },
      occurrencesHighlight: false,
      renderLineHighlight: "none",
      theme: settings.editor_theme,
      fontFamily: "JetBrains Mono, Droid Sans Mono, monospace",
      fontSize: settings.editor_font_size,
      tabIndex: -1,
      tabSize: 2,
      autoIndent: true,
      formatOnType: true,
      formatOnPaste: true,
      quickSuggestions:
        this.type === "elixir" && settings.editor_auto_completion,
      tabCompletion: "on",
      suggestSelection: "first",
      // For Elixir word suggestions are confusing at times.
      // For example given `defmodule<CURSOR> Foo do`, if the
      // user opens completion list and then jumps to the end
      // of the line we would get "defmodule" as a word completion.
      wordBasedSuggestions: this.type !== "elixir",
      parameterHints: this.type === "elixir" && settings.editor_auto_signature,
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

    /* Overrides */

    // Move the command palette widget to overflowing widgets container,
    // so that it's visible on small editors.
    // See: https://github.com/microsoft/monaco-editor/issues/70
    const commandPaletteNode = this.editor.getContribution(
      "editor.controller.quickInput"
    ).widget.domNode;
    commandPaletteNode.remove();
    this.editor._modelData.view._contentWidgets.overflowingContentWidgetsDomNode.domNode.appendChild(
      commandPaletteNode
    );
  }

  /**
   * Defines cell-specific providers for various editor features.
   */
  __setupIntellisense() {
    const settings = settingsStore.get();

    this.handlerByRef = {};

    /**
     * Intellisense requests such as completion or formatting are
     * handled asynchronously by the runtime.
     *
     * As an example, let's go through the steps for completion:
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
     *   * finally the hook receives the "intellisense_response" event with completion
     *     response, it looks up completion callback for the received reference and calls
     *     it with the response, which finally returns the completion items to the editor
     */

    this.editor.getModel().__getCompletionItems = (model, position) => {
      const line = model.getLineContent(position.lineNumber);
      const lineUntilCursor = line.slice(0, position.column - 1);

      return this.__asyncIntellisenseRequest("completion", {
        hint: lineUntilCursor,
        editor_auto_completion: settings.editor_auto_completion,
      })
        .then((response) => {
          const suggestions = completionItemsToSuggestions(
            response.items,
            settings
          ).map((suggestion) => {
            const replaceLength = replacedSuffixLength(
              lineUntilCursor,
              suggestion.insertText
            );

            const range = new monaco.Range(
              position.lineNumber,
              position.column - replaceLength,
              position.lineNumber,
              position.column
            );

            return { ...suggestion, range };
          });

          return { suggestions };
        })
        .catch(() => null);
    };

    this.editor.getModel().__getHover = (model, position) => {
      const line = model.getLineContent(position.lineNumber);
      const column = position.column;

      return this.__asyncIntellisenseRequest("details", { line, column })
        .then((response) => {
          const contents = response.contents.map((content) => ({
            value: content,
            isTrusted: true,
          }));

          const range = new monaco.Range(
            position.lineNumber,
            response.range.from,
            position.lineNumber,
            response.range.to
          );

          return { contents, range };
        })
        .catch(() => null);
    };

    const signatureCache = {
      codeUntilLastStop: null,
      response: null,
    };

    this.editor.getModel().__getSignatureHelp = (model, position) => {
      const lines = model.getLinesContent();
      const lineIdx = position.lineNumber - 1;
      const prevLines = lines.slice(0, lineIdx);
      const lineUntilCursor = lines[lineIdx].slice(0, position.column - 1);
      const codeUntilCursor = [...prevLines, lineUntilCursor].join("\n");

      const codeUntilLastStop = codeUntilCursor
        // Remove trailing characters that don't affect the signature
        .replace(/[^(),\s]*?$/, "")
        // Remove whitespace before delimiter
        .replace(/([(),])\s*$/, "$1");

      // Cache subsequent requests for the same prefix, so that we don't
      // make unnecessary requests
      if (codeUntilLastStop === signatureCache.codeUntilLastStop) {
        return {
          value: signatureResponseToSignatureHelp(signatureCache.response),
          dispose: () => {},
        };
      }

      return this.__asyncIntellisenseRequest("signature", {
        hint: codeUntilCursor,
      })
        .then((response) => {
          signatureCache.response = response;
          signatureCache.codeUntilLastStop = codeUntilLastStop;

          return {
            value: signatureResponseToSignatureHelp(response),
            dispose: () => {},
          };
        })
        .catch(() => null);
    };

    this.editor.getModel().__getDocumentFormattingEdits = (model) => {
      const content = model.getValue();

      return this.__asyncIntellisenseRequest("format", { code: content })
        .then((response) => {
          this.setCodeErrorMarker(response.code_error);

          if (response.code) {
            /**
             * We use a single edit replacing the whole editor content,
             * but the editor itself optimises this into a list of edits
             * that produce minimal diff using the Myers string difference.
             *
             * References:
             *   * https://github.com/microsoft/vscode/blob/628b4d46357f2420f1dbfcea499f8ff59ee2c251/src/vs/editor/contrib/format/format.ts#L324
             *   * https://github.com/microsoft/vscode/blob/628b4d46357f2420f1dbfcea499f8ff59ee2c251/src/vs/editor/common/services/editorSimpleWorker.ts#L489
             *   * https://github.com/microsoft/vscode/blob/628b4d46357f2420f1dbfcea499f8ff59ee2c251/src/vs/base/common/diff/diff.ts#L227-L231
             *
             * Eventually the editor will received the optimised list of edits,
             * which we then convert to Delta and send to the server.
             * Consequently, the Delta carries only the minimal formatting diff.
             *
             * Also, if edits are applied to the editor, either by typing
             * or receiving remote changes, the formatting is cancelled.
             * In other words the formatting changes are actually applied
             * only if the editor stays intact.
             *
             * References:
             *   * https://github.com/microsoft/vscode/blob/628b4d46357f2420f1dbfcea499f8ff59ee2c251/src/vs/editor/contrib/format/format.ts#L313
             *   * https://github.com/microsoft/vscode/blob/628b4d46357f2420f1dbfcea499f8ff59ee2c251/src/vs/editor/browser/core/editorState.ts#L137
             *   * https://github.com/microsoft/vscode/blob/628b4d46357f2420f1dbfcea499f8ff59ee2c251/src/vs/editor/contrib/format/format.ts#L326
             */

            const replaceEdit = {
              range: model.getFullModelRange(),
              text: response.code,
            };

            return [replaceEdit];
          } else {
            return [];
          }
        })
        .catch(() => null);
    };

    this.hook.handleEvent("intellisense_response", ({ ref, response }) => {
      const handler = this.handlerByRef[ref];

      if (handler) {
        handler(response);
        delete this.handlerByRef[ref];
      }
    });
  }

  /**
   * Pushes an intellisense request.
   *
   * The returned promise is either resolved with a valid
   * response or rejected with null.
   */
  __asyncIntellisenseRequest(type, props) {
    return new Promise((resolve, reject) => {
      this.hook.pushEvent(
        "intellisense_request",
        { cell_id: this.cellId, type, ...props },
        ({ ref }) => {
          if (ref) {
            this.handlerByRef[ref] = (response) => {
              if (response) {
                resolve(response);
              } else {
                reject(null);
              }
            };
          } else {
            reject(null);
          }
        }
      );
    });
  }
}

function completionItemsToSuggestions(items, settings) {
  return items
    .map((item) => parseItem(item, settings))
    .map((suggestion, index) => ({
      ...suggestion,
      sortText: numberToSortableString(index, items.length),
    }));
}

// See `Livebook.Runtime` for completion item definition
function parseItem(item, settings) {
  return {
    label: item.label,
    kind: parseItemKind(item.kind),
    detail: item.detail,
    documentation: item.documentation && {
      value: item.documentation,
      isTrusted: true,
    },
    insertText: item.insert_text,
    insertTextRules:
      monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
    command: settings.editor_auto_signature
      ? {
          title: "Trigger Parameter Hint",
          id: "editor.action.triggerParameterHints",
        }
      : null,
  };
}

function parseItemKind(kind) {
  switch (kind) {
    case "function":
      return monaco.languages.CompletionItemKind.Function;
    case "module":
      return monaco.languages.CompletionItemKind.Module;
    case "struct":
      return monaco.languages.CompletionItemKind.Struct;
    case "interface":
      return monaco.languages.CompletionItemKind.Interface;
    case "type":
      return monaco.languages.CompletionItemKind.Class;
    case "variable":
      return monaco.languages.CompletionItemKind.Variable;
    case "field":
      return monaco.languages.CompletionItemKind.Field;
    case "keyword":
      return monaco.languages.CompletionItemKind.Keyword;
    default:
      return null;
  }
}

function numberToSortableString(number, maxNumber) {
  return String(number).padStart(maxNumber, "0");
}

function signatureResponseToSignatureHelp(response) {
  return {
    activeSignature: 0,
    activeParameter: response.active_argument,
    signatures: response.signature_items.map((signature_item) => ({
      label: signature_item.signature,
      parameters: signature_item.arguments.map((argument) => ({
        label: argument,
      })),
      documentation: null,
    })),
  };
}

export default LiveEditor;
