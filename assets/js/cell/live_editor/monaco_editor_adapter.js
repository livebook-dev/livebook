import monaco from "./monaco";
import Delta, { isDelete, isInsert, isRetain } from "../../lib/delta";

/**
 * Encapsulates logic related to getting/applying changes to the editor.
 *
 * Uses the given Monaco editor instance.
 */
export default class MonacoEditorAdapter {
  constructor(editor) {
    this.editor = editor;
    this._onDelta = null;
    this.isLastChangeRemote = false;

    this.editor.onDidChangeModelContent((event) => {
      if (this.ignoreChange) {
        return;
      }

      this.isLastChangeRemote = false;

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
    const isStandaloneChange = delta.ops.some((op) => {
      if (isDelete(op)) {
        return true;
      }

      if (isInsert(op)) {
        return op.insert.match(/\s+/);
      }

      return false;
    });

    // Explicitly close the last stack element when the remote
    // change inserts whitespace or deletes text. Otherwise
    // merge subsequent remote changes whenever possible.
    if (isStandaloneChange || !this.isLastChangeRemote) {
      this.editor.getModel().pushStackElement();
    } else {
      this.editor.getModel().popStackElement();
    }

    const operations = this.__deltaToEditorOperations(delta);
    this.ignoreChange = true;
    // Apply the operations and add them to the undo stack
    this.editor.getModel().pushEditOperations(null, operations, null);
    // Close the stack element upfront in case the next
    // change is local. If another remote change comes,
    // we open the element back using `popStackElement`.
    this.editor.getModel().pushStackElement();
    this.ignoreChange = false;

    this.isLastChangeRemote = true;
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
