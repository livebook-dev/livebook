import Delta from "../lib/delta";
import * as monaco from "monaco-editor/esm/vs/editor/editor.api";

export default class MonacoAdapter {
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

  onDelta(callback) {
    this._onDelta = callback;
  }

  applyDelta(delta) {
    const operations = this.__deltaToEditorOperations(delta);
    this.ignoreChange = true;
    this.editor.getModel().pushEditOperations([], operations);
    this.ignoreChange = false;
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
      if (typeof op.retain === "number") {
        index += op.retain;
      }

      if (typeof op.insert === "string") {
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

      if (typeof op.delete === "number") {
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
