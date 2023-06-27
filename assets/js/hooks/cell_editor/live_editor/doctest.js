import monaco from "./monaco";

/**
 * Doctest visual indicators within the editor.
 *
 * Consists of a status widget and optional error details.
 */
export default class Doctest {
  constructor(editor, doctestReport) {
    this._editor = editor;

    this._statusDecoration = new StatusDecoration(
      editor,
      doctestReport.line,
      doctestReport.status
    );

    if (doctestReport.status === "failed") {
      this._detailsWidget = new DetailsWidget(editor, doctestReport);
    }
  }

  /**
   * Updates doctest indicator.
   */
  update(doctestReport) {
    this._statusDecoration.update(doctestReport.status);

    if (doctestReport.status === "failed") {
      this._detailsWidget && this._detailsWidget.dispose();
      this._detailsWidget = new DetailsWidget(this._editor, doctestReport);
    }
  }

  /**
   * Performs necessary cleanup actions.
   */
  dispose() {
    this._statusDecoration.dispose();
    this._detailsWidget && this._detailsWidget.dispose();
  }
}

class StatusDecoration {
  constructor(editor, lineNumber, status) {
    this._editor = editor;
    this._lineNumber = lineNumber;
    this._decorations = [];

    this.update(status);
  }

  update(status) {
    const newDecorations = [
      {
        range: new monaco.Range(this._lineNumber, 1, this._lineNumber, 1),
        options: {
          isWholeLine: true,
          linesDecorationsClassName: `doctest-status-decoration-${status}`,
        },
      },
    ];

    this._decorations = this._editor.deltaDecorations(
      this._decorations,
      newDecorations
    );
  }

  dispose() {
    this._editor.deltaDecorations(this._decorations, []);
  }
}

class DetailsWidget {
  constructor(editor, doctestReport) {
    this._editor = editor;

    const { line, end_line, details, column } = doctestReport;
    const detailsHtml = details.join("\n");
    const numberOfLines = details.length;

    const fontSize = this._editor.getOption(
      monaco.editor.EditorOption.fontSize
    );

    const lineHeight = this._editor.getOption(
      monaco.editor.EditorOption.lineHeight
    );

    const detailsNode = document.createElement("div");
    detailsNode.innerHTML = detailsHtml;
    detailsNode.classList.add(
      "doctest-details-widget",
      "editor-theme-aware-ansi"
    );
    detailsNode.style.fontSize = `${fontSize}px`;
    detailsNode.style.lineHeight = `${lineHeight}px`;

    this._overlayWidget = {
      getId: () => `livebook.doctest.overlay.${line}`,
      getDomNode: () => detailsNode,
      getPosition: () => null,
    };

    this._editor.addOverlayWidget(this._overlayWidget);

    this._editor.changeViewZones((changeAccessor) => {
      this._viewZone = changeAccessor.addZone({
        afterLineNumber: end_line,
        // Placeholder for all lines and additional padding
        heightInPx: numberOfLines * lineHeight + 12,
        domNode: document.createElement("div"),
        onDomNodeTop: (top) => {
          detailsNode.style.top = `${top}px`;

          const marginWidth = this._editor
            .getDomNode()
            .querySelector(".margin-view-overlays").offsetWidth;

          detailsNode.style.paddingLeft = `calc(${marginWidth}px + ${column}ch)`;
        },
        onComputedHeight: (height) => {
          detailsNode.style.height = `${height}px`;
        },
      });
    });
  }

  dispose() {
    this._editor.removeOverlayWidget(this._overlayWidget);
    this._editor.changeViewZones((changeAccessor) => {
      changeAccessor.removeZone(this._viewZone);
    });
  }
}
