import monaco from "./monaco";
import Delta, { isDelete, isInsert, isRetain } from "../lib/delta";

/**
 * Encapsulates logic related to getting/applying changes to the editor.
 *
 * Uses the given Monaco editor instance.
 */
export default class MonacoEditorAdapter {
  constructor(editor) {
    this.editor = editor;
    this._onDelta = null;

    this.editor.onDidChangeModelContent((event) => {
      if (this.ignoreChange) {
        return;
      }
      const delta = this.__deltaFromEditorChange(event);
      this._onDelta && this._onDelta(delta);
    });
  }

  /**
   * Registers a callback called whenever the user makes a change
   * to the editor content. The change is represented by a delta object.
   */
  onDelta(callback) {
    this._onDelta = callback;
  }

  /**
   * Applies the given delta to the editor content.
   */
  applyDelta(delta) {
    const operations = this.__deltaToEditorOperations(delta);
    this.ignoreChange = true;
    // Apply the operations without adding them to the undo stack
    this.editor.getModel().applyEdits(operations);
    this.ignoreChange = false;

    // Clear the undo/redo stack as the operations may no longer be valid
    // after applying the concurrent change.
    // Note: there's not public method for getting EditStack for the text model,
    // so we use the private attribute.
    // (https://github.com/microsoft/vscode/blob/11ac71b27220a2354b6bb28966ed3ead183cc495/src/vs/editor/common/model/textModel.ts#L287)
    const editStack = this.editor.getModel()._commandManager;
    editStack.clear();
  }

  __deltaFromEditorChange(event) {
    const deltas = event.changes.map((change) => {
      const { rangeOffset, rangeLength, text } = change;

      const delta = new Delta();

      if (rangeOffset) {
        delta.retain(rangeOffset);
      }

      if (rangeLength) {
        delta.delete(rangeLength);
      }

      if (text) {
        delta.insert(text);
      }

      return delta;
    });

    return deltas.reduce((delta1, delta2) => delta1.compose(delta2));
  }

  __deltaToEditorOperations(delta) {
    const model = this.editor.getModel();

    const operations = [];
    let index = 0;

    delta.ops.forEach((op) => {
      if (isRetain(op)) {
        index += op.retain;
      }

      if (isInsert(op)) {
        const start = model.getPositionAt(index);

        operations.push({
          forceMoveMarkers: true,
          range: new monaco.Range(
            start.lineNumber,
            start.column,
            start.lineNumber,
            start.column
          ),
          text: op.insert,
        });
      }

      if (isDelete(op)) {
        const start = model.getPositionAt(index);
        const end = model.getPositionAt(index + op.delete);

        operations.push({
          forceMoveMarkers: false,
          range: new monaco.Range(
            start.lineNumber,
            start.column,
            end.lineNumber,
            end.column
          ),
          text: null,
        });

        index += op.delete;
      }
    });

    return operations;
  }
}
