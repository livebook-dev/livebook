import { EditorView, ViewPlugin, showTooltip, keymap } from "@codemirror/view";
import { StateField, StateEffect } from "@codemirror/state";

const baseTheme = EditorView.baseTheme({
  ".cm-readOnlyHint": {
    padding: "4px",
  },
});

const showHintEffect = StateEffect.define();
const hideHintEffect = StateEffect.define();

const hintField = StateField.define({
  create() {
    return { tooltip: null };
  },

  update({ tooltip }, tr) {
    if (!tr.state.selection.eq(tr.startState.selection)) {
      tooltip = null;
    }

    for (const effect of tr.effects) {
      if (effect.is(showHintEffect)) {
        tooltip = {
          pos: tr.state.selection.main.head,
          above: true,
          create: createTooltip,
        };
      }

      if (effect.is(hideHintEffect)) {
        tooltip = null;
      }
    }

    return { tooltip };
  },

  provide(field) {
    return showTooltip.from(field, (value) => value.tooltip);
  },
});

function createTooltip(view) {
  const dom = document.createElement("div");
  dom.classList.add("cm-readOnlyHint");
  dom.textContent = "This editor is read-only";
  return { dom };
}

const hintPlugin = ViewPlugin.fromClass(
  class {
    constructor(view) {
      this.view = view;
    }
  },
  {
    eventHandlers: {
      input(event) {
        if (!this.view.state.readOnly) return;

        if (!this.view.state.field(hintField).tooltip) {
          this.view.dispatch({ effects: showHintEffect.of(null) });
        }
      },

      blur(event) {
        if (!this.view.state.readOnly) return;

        if (this.view.state.field(hintField).tooltip) {
          setTimeout(() => {
            // Dispatch state update in the next event cycle (https://github.com/codemirror/dev/issues/1316)
            this.view.dispatch({ effects: [hideHintEffect.of(null)] });
          }, 0);
        }
      },
    },
  },
);

function closeHint(view) {
  const hintState = view.state.field(hintField, false);
  if (!hintState || !hintState.tooltip) return false;
  view.dispatch({ effects: [hideHintEffect.of(null)] });
  return true;
}

const hintKeymap = [{ key: "Escape", run: closeHint }];

/**
 * Returns an extension that shows a tooltip whenever the user tries
 * to type and the read-only mode is enabled.
 */
export function readOnlyHint() {
  return [hintField, hintPlugin, keymap.of(hintKeymap), baseTheme];
}
