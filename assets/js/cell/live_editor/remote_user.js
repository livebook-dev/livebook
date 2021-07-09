import monaco from "./monaco";
import { randomId } from "../../lib/utils";

/**
 * Remote user visual indicators within the editor.
 *
 * Consists of a cursor widget and a selection highlight.
 * Both elements have the user's hex color of choice.
 */
export default class RemoteUser {
  constructor(editor, selection, hexColor, label) {
    this._cursorWidget = new CursorWidget(
      editor,
      selection.getPosition(),
      hexColor,
      label
    );

    this._selectionDecoration = new SelectionDecoration(
      editor,
      selection,
      hexColor
    );
  }

  /**
   * Updates indicators to match the given selection.
   */
  update(selection) {
    this._cursorWidget.update(selection.getPosition());
    this._selectionDecoration.update(selection);
  }

  /**
   * Performs necessary cleanup actions.
   */
  dispose() {
    this._cursorWidget.dispose();
    this._selectionDecoration.dispose();
  }
}

class CursorWidget {
  constructor(editor, position, hexColor, label) {
    this._id = randomId();
    this._editor = editor;
    this._position = position;
    this._isPositionValid = this.__checkPositionValidity(position);

    this.__buildDomNode(hexColor, label);

    this._editor.addContentWidget(this);

    this._onDidChangeModelContentDisposable =
      this._editor.onDidChangeModelContent((event) => {
        // We may receive new cursor position before content update,
        // and the position may be invalid (e.g. column 10, even though the line has currently length 9).
        // If that's the case then we want to update the cursor once the content is updated.
        if (!this._isPositionValid) {
          this.update(this._position);
        }
      });
  }

  getId() {
    return this._id;
  }

  getPosition() {
    return {
      position: this._position,
      preference: [monaco.editor.ContentWidgetPositionPreference.EXACT],
    };
  }

  update(position) {
    this._position = position;
    this._isPositionValid = this.__checkPositionValidity(position);
    this.__updateDomNode();
    this._editor.layoutContentWidget(this);
  }

  getDomNode() {
    return this._domNode;
  }

  dispose() {
    this._editor.removeContentWidget(this);
    this._onDidChangeModelContentDisposable.dispose();
  }

  __checkPositionValidity(position) {
    const validPosition = this._editor.getModel().validatePosition(position);
    return position.equals(validPosition);
  }

  __buildDomNode(hexColor, label) {
    const lineHeight = this._editor.getOption(
      monaco.editor.EditorOption.lineHeight
    );

    const node = document.createElement("div");
    node.classList.add("monaco-cursor-widget-container");

    const cursorNode = document.createElement("div");
    cursorNode.classList.add("monaco-cursor-widget-cursor");
    cursorNode.style.background = hexColor;
    cursorNode.style.height = `${lineHeight}px`;

    const labelNode = document.createElement("div");
    labelNode.classList.add("monaco-cursor-widget-label");
    labelNode.style.height = `${lineHeight}px`;
    labelNode.innerText = label;
    labelNode.style.background = hexColor;

    node.appendChild(cursorNode);
    node.appendChild(labelNode);

    this._domNode = node;
    this.__updateDomNode();
  }

  __updateDomNode() {
    const isFirstLine = this._position.lineNumber === 1;
    this._domNode.classList.toggle("inline", isFirstLine);
  }
}

class SelectionDecoration {
  constructor(editor, selection, hexColor) {
    this._editor = editor;
    this._decorations = [];

    // Dynamically create CSS class for the given hex color

    this._className = `user-selection-${hexColor.replace("#", "")}`;

    this._styleElement = document.createElement("style");
    this._styleElement.innerHTML = `
      .${this._className} {
        background-color: ${hexColor}30;
      }
    `;
    document.body.appendChild(this._styleElement);

    this.update(selection);
  }

  update(selection) {
    const newDecorations = [
      {
        range: selection,
        options: { className: this._className },
      },
    ];

    this._decorations = this._editor.deltaDecorations(
      this._decorations,
      newDecorations
    );
  }

  dispose() {
    this._editor.deltaDecorations(this._decorations, []);
    this._styleElement.remove();
  }
}
