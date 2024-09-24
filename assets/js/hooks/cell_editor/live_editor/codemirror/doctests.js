import { EditorView, Decoration, WidgetType } from "@codemirror/view";
import { StateField, StateEffect } from "@codemirror/state";

const baseTheme = EditorView.baseTheme({
  ".cm-doctestDetails": {
    paddingTop: "4px",
    paddingBottom: "4px",
    paddingLeft: "6px",
  },

  ".cm-doctestDetailsContent": {
    whiteSpace: "pre-wrap",
    border: "1px solid #424857",
    paddingTop: "6px",
    paddingBottom: "6px",
    marginLeft: "-8px",
    paddingLeft: "8px",
    borderRadius: "4px",
    marginRight: "16px",
  },

  "&light .cm-doctestDetailsContent": {
    borderColor: "#b6b7b9",
  },

  ".cm-doctestStatus": {
    position: "relative",
  },

  ".cm-doctestStatus::before": {
    borderRadius: "2px",

    width: "10px",
    height: "10px",
    display: "block",
    content: "''",

    position: "absolute",
    top: "50%",
    left: "0",
    transform: "translate(calc(-100% - 6px), -50%)",
  },

  ".cm-doctestStatus-running::before": {
    backgroundColor: "#91a4b7",
  },

  ".cm-doctestStatus-success::before": {
    backgroundColor: "#4ade80",
  },

  ".cm-doctestStatus-failed::before": {
    backgroundColor: "#e97579",
  },
});

const updateDoctestsEffect = StateEffect.define();
const clearDoctestsEffect = StateEffect.define();

const doctestsField = StateField.define({
  create(state) {
    return Decoration.none;
  },

  update(decorations, tr) {
    decorations = decorations.map(tr.changes);

    for (const effect of tr.effects) {
      if (effect.is(updateDoctestsEffect)) {
        const reports = effect.value;

        decorations = decorations.update({
          filter: (from, to, decoration) => {
            return !reports.some(
              (report) => decoration.spec.report.line === report.line,
            );
          },
          add: reports.flatMap((report) =>
            decorationsForDoctest(report, tr.state.doc),
          ),
          sort: true,
        });
      }

      if (effect.is(clearDoctestsEffect)) {
        decorations = Decoration.none;
      }
    }

    return decorations;
  },

  provide(field) {
    return EditorView.decorations.from(field);
  },
});

function decorationsForDoctest(report, doc) {
  const pos = doc.line(report.line).from + report.column;

  const decorations = [
    Decoration.mark({
      class: `cm-doctestStatus cm-doctestStatus-${report.status}`,
      report,
    }).range(pos, pos + 1),
  ];

  if (report.status === "failed") {
    const detailsLine = doc.line(report.end_line + 1);

    decorations.push(
      Decoration.widget({
        widget: new DoctestDetailsWidget(report),
        block: true,
        report,
      }).range(detailsLine.from),
    );
  }

  return decorations;
}

class DoctestDetailsWidget extends WidgetType {
  constructor(report) {
    super();

    this.report = report;
  }

  toDOM(view) {
    const node = document.createElement("div");
    node.classList.add("cm-doctestDetails");

    const detailsNode = document.createElement("div");
    detailsNode.classList.add("cm-doctestDetailsContent");
    detailsNode.classList.add("editor-theme-aware-ansi");
    node.style.marginLeft = `${this.report.column}ch`;
    detailsNode.innerHTML = this.report.details;
    node.appendChild(detailsNode);

    return node;
  }

  eq(other) {
    return this.report === other.report;
  }
}

/**
 * Updates doctest decorations based on the given doctest reports.
 *
 * Note that doctests not present in the reports are kept as is.
 */
export function updateDoctests(view, reports) {
  const effects = [updateDoctestsEffect.of(reports)];
  view.dispatch({ effects: maybeEnableDoctests(view.state, effects) });
}

/**
 * Clears all doctest decorations.
 */
export function clearDoctests(view) {
  const effects = [clearDoctestsEffect.of(null)];
  view.dispatch({ effects: maybeEnableDoctests(view.state, effects) });
}

const doctestsExtensions = [doctestsField, baseTheme];

function maybeEnableDoctests(state, effects) {
  return state.field(doctestsField, false)
    ? effects
    : effects.concat(StateEffect.appendConfig.of(doctestsExtensions));
}
