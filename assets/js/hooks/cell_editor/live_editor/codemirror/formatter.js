import { ViewPlugin, keymap } from "@codemirror/view";
import {
  StateField,
  StateEffect,
  Facet,
  combineConfig,
} from "@codemirror/state";

const formatterConfig = Facet.define({
  combine(configs) {
    return combineConfig(configs, {});
  },
});

const startFormatEffect = StateEffect.define();

const formatterField = StateField.define({
  create() {
    return { doc: null };
  },

  update({ doc }, tr) {
    // Whenever the document changes, we reset the state and ignore
    // any pending formats
    if (tr.docChanged) {
      doc = null;
    }

    for (const effect of tr.effects) {
      if (effect.is(startFormatEffect)) {
        doc = tr.state.doc;
      }
    }

    return { doc };
  },
});

const formatterPlugin = ViewPlugin.fromClass(
  class {
    constructor(view) {
      this.view = view;
      this.query = null;
    }

    update(update) {
      const formatterState = update.state.field(formatterField);

      if (formatterState.doc !== update.startState.field(formatterField).doc) {
        this.maybeAbortQuery();

        if (formatterState.doc) {
          this.requestFormatterChanges(update.state);
        }
      }
    }

    destroy() {
      this.maybeAbortQuery();
    }

    requestFormatterChanges(state) {
      const { doc } = state.field(formatterField);
      const { source } = state.facet(formatterConfig);

      const query = { aborted: false };

      source(doc).then((changes) => {
        if (!query.aborted && changes) {
          this.view.dispatch({ changes });
        }
      });

      this.query = query;
    }

    maybeAbortQuery() {
      if (this.query) {
        this.query.aborted = true;
        this.query = null;
      }
    }
  },
);

function startFormat(view) {
  if (view.state.readOnly) return false;
  view.dispatch({ effects: [startFormatEffect.of(null)] });
  return true;
}

const formatterKeymap = [
  {
    key: "Ctrl-Shift-i",
    mac: "Alt-Shift-f",
    win: "Alt-Shift-f",
    run: startFormat,
  },
];

/**
 * Returns an extension that enables code formatting.
 *
 * Expects a formatter source, which given the text document will return
 * a change spec (or null if there are no changes).
 */
export function formatter(source) {
  return [
    formatterField,
    formatterPlugin,
    formatterConfig.of({ source }),
    keymap.of(formatterKeymap),
  ];
}
