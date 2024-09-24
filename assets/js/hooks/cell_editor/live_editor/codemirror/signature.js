import { EditorView, ViewPlugin, showTooltip, keymap } from "@codemirror/view";
import {
  StateField,
  StateEffect,
  Facet,
  combineConfig,
  Prec,
} from "@codemirror/state";

const baseTheme = EditorView.baseTheme({
  ".cm-signatureHint": {
    display: "flex",
  },

  ".cm-signatureHintStepper": {
    padding: "4px 8px",
    borderRight: "1px solid black",
  },

  ".cm-signatureHintContent": {
    padding: "4px",
  },

  ".cm-signatureHintActiveArgument": {
    color: "gray",
  },
});

const signatureConfig = Facet.define({
  combine(configs) {
    return combineConfig(configs, {});
  },
});

const characterSetsConfig = Facet.define({}).from(
  signatureConfig,
  ({ triggerCharacters, retriggerCharacters }) => ({
    triggerCharacters: new Set(triggerCharacters),
    // Note: all trigger characters are also retrigger characters
    retriggerCharacters: new Set(triggerCharacters.concat(retriggerCharacters)),
  }),
);

const setSignatureResultEffect = StateEffect.define();
const startSignatureEffect = StateEffect.define();
const closeSignatureEffect = StateEffect.define();
const setSelectedEffect = StateEffect.define();

const signatureField = StateField.define({
  create() {
    return {
      context: null,
      hint: null,
    };
  },

  update({ context, hint }, tr) {
    hint = hint && hint.setPosition(tr.state.selection.main.head);

    const isOpen = !!hint;

    if (shouldRequestSignature(tr, isOpen)) {
      context = getSignatureContext(tr.state);
    }

    for (const effect of tr.effects) {
      if (effect.is(setSignatureResultEffect)) {
        if (effect.value.signatureResult) {
          hint = SignatureHint.build(tr.state, effect.value.signatureResult);
        } else {
          context = null;
          hint = null;
        }
      }

      if (effect.is(startSignatureEffect)) {
        context = getSignatureContext(tr.state);
      }

      if (effect.is(closeSignatureEffect)) {
        context = null;
        hint = null;
      }

      if (effect.is(setSelectedEffect)) {
        hint = hint && hint.setSelected(effect.value);
      }
    }

    return { context, hint };
  },

  provide(field) {
    return showTooltip.from(field, (value) => value.hint && value.hint.tooltip);
  },
});

function shouldRequestSignature(tr, isOpen) {
  const { activateOnTyping } = tr.state.facet(signatureConfig);
  const { triggerCharacters, retriggerCharacters } =
    tr.state.facet(characterSetsConfig);

  const startCursorPos = tr.startState.selection.main.head;
  const cursorPos = tr.state.selection.main.head;
  const isUserInput = tr.isUserEvent("input");

  if (tr.docChanged) {
    let request = false;

    tr.changes.iterChangedRanges((fromA, toA, fromB, toB) => {
      if (request) return;

      if (fromA < toA && fromA <= startCursorPos && startCursorPos <= toA) {
        const deleted = tr.startState.doc.sliceString(fromA, toA);

        if (isOpen) {
          // When open and main cursor deleted any retrigger character
          request = request || includesAnyChar(deleted, retriggerCharacters);
        }
      }

      if (fromB < toB && fromB <= cursorPos && cursorPos <= toB) {
        const inserted = tr.state.doc.sliceString(fromB, cursorPos);

        if (isOpen) {
          // When open and main cursor inserted any retrigger character
          request = request || includesAnyChar(inserted, retriggerCharacters);
        } else if (activateOnTyping && isUserInput) {
          // When on-typing is enabled and the main cursor inserted any
          // trigger character
          request = request || includesAnyChar(inserted, triggerCharacters);
        }
      }
    });

    return request;
  }

  if (!tr.docChanged && startCursorPos !== cursorPos) {
    const movedOver = tr.state.doc.sliceString(
      Math.min(startCursorPos, cursorPos),
      Math.max(startCursorPos, cursorPos),
    );

    if (isOpen) {
      // When open and the main cursor moved over any retrigger character
      return includesAnyChar(movedOver, retriggerCharacters);
    }
  }

  return false;
}

function includesAnyChar(string, charsSet) {
  for (const char of string) {
    if (charsSet.has(char)) {
      return true;
    }
  }

  return false;
}

function getSignatureContext(state) {
  const pos = state.selection.main.head;
  return { state, pos };
}

class SignatureHint {
  constructor(signatureResult, selectedIdx, tooltip) {
    this.signatureResult = signatureResult;
    this.selectedIdx = selectedIdx;
    this.tooltip = tooltip;
  }

  static build(state, signatureResult) {
    return new SignatureHint(signatureResult, 0, {
      pos: state.selection.main.head,
      above: true,
      create: (view) => new SignatureTooltip(view),
    });
  }

  setSelected(selectedIdx) {
    return new SignatureHint(this.signatureResult, selectedIdx, this.tooltip);
  }

  setPosition(pos) {
    if (pos === this.tooltip.pos) return this;

    return new SignatureHint(this.signatureResult, this.selectedIdx, {
      ...this.tooltip,
      pos: pos,
    });
  }
}

class SignatureTooltip {
  constructor(view) {
    this.view = view;

    const { signatureResult } = view.state.field(signatureField).hint;

    this.dom = document.createElement("div");
    this.dom.classList.add("cm-signatureHint");

    if (signatureResult.items.length > 1) {
      this.stepper = document.createElement("div");
      this.stepper.classList.add("cm-signatureHintStepper");
      this.dom.appendChild(this.stepper);
    }

    const content = document.createElement("div");
    content.classList.add("cm-signatureHintContent");

    this.contentLeft = document.createElement("span");
    this.contentActive = document.createElement("span");
    this.contentActive.classList.add("cm-signatureHintActiveArgument");
    this.contentRight = document.createElement("span");
    content.appendChild(this.contentLeft);
    content.appendChild(this.contentActive);
    content.appendChild(this.contentRight);

    this.dom.appendChild(content);
  }

  mount() {
    this.updateSelected();
  }

  update(update) {
    const startSignatureState = update.startState.field(signatureField);
    const signatureState = update.state.field(signatureField);

    if (startSignatureState !== signatureState) {
      this.updateSelected();
    }
  }

  updateSelected() {
    const { signatureResult, selectedIdx } =
      this.view.state.field(signatureField).hint;

    const { activeArgumentIdx, items } = signatureResult;

    const item = items[selectedIdx];
    const activeArgument = item.arguments[activeArgumentIdx];

    if (this.stepper) {
      this.stepper.textContent = `${selectedIdx + 1}/${items.length}`;
    }

    const idx = item.signature.indexOf(activeArgument);

    this.contentLeft.textContent = item.signature.slice(0, idx);
    this.contentActive.textContent = activeArgument;
    this.contentRight.textContent = item.signature.slice(
      idx + activeArgument.length,
    );
  }
}

const signaturePlugin = ViewPlugin.fromClass(
  class {
    constructor(view) {
      this.view = view;
      this.query = null;
    }

    update(update) {
      const signatureState = update.state.field(signatureField);

      if (
        signatureState.context !==
        update.startState.field(signatureField).context
      ) {
        this.maybeAbortQuery();

        if (signatureState.context) {
          this.requestSignature(update.state);
        }
      }
    }

    destroy() {
      this.maybeAbortQuery();
    }

    requestSignature(state) {
      const { context } = state.field(signatureField);
      const { source } = state.facet(signatureConfig);

      const query = { aborted: false };

      source(context).then((signatureResult) => {
        if (!query.aborted) {
          this.view.dispatch({
            effects: setSignatureResultEffect.of({ signatureResult }),
          });
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
  {
    eventHandlers: {
      blur(event) {
        let signatureState = this.view.state.field(signatureField, false);

        if (
          signatureState &&
          signatureState.hint &&
          this.view.state.facet(signatureConfig).closeOnBlur
        ) {
          // Dispatch state update in the next event cycle (https://github.com/codemirror/dev/issues/1316)
          setTimeout(() => {
            this.view.dispatch({ effects: [closeSignatureEffect.of(null)] });
          }, 0);
        }
      },
    },
  },
);

function startSignature(view) {
  view.dispatch({ effects: [startSignatureEffect.of(null)] });
  return true;
}

/**
 * Closes the currently active completion.
 */
export function closeSignature(view) {
  const signatureState = view.state.field(signatureField, false);
  if (!signatureState || !signatureState.hint) return false;
  view.dispatch({ effects: [closeSignatureEffect.of(null)] });
  return true;
}

const moveSignatureSelection = (forward) => {
  return (view) => {
    const signatureState = view.state.field(signatureField, false);
    if (!signatureState || !signatureState.hint) return false;

    const { signatureResult, selectedIdx } = signatureState.hint;
    if (signatureResult.items.length === 1) return false;
    const length = signatureResult.items.length;

    let newSelectedIdx = selectedIdx + (forward ? 1 : -1);
    if (newSelectedIdx < 0) newSelectedIdx += length;
    if (newSelectedIdx >= length) newSelectedIdx -= length;

    view.dispatch({ effects: [setSelectedEffect.of(newSelectedIdx)] });

    return true;
  };
};

const signatureKeymap = [
  { key: "Mod-Shift-Space", run: startSignature },
  { key: "Escape", run: closeSignature },
  { key: "ArrowDown", run: moveSignatureSelection(true) },
  { key: "ArrowUp", run: moveSignatureSelection(false) },
];

/**
 * Returns an extension that enables signature hints.
 */
export function signature(
  source,
  {
    activateOnTyping = true,
    closeOnBlur = true,
    triggerCharacters = ["(", ","],
    retriggerCharacters = [")"],
  } = {},
) {
  return [
    signatureField,
    signaturePlugin,
    signatureConfig.of({
      source,
      activateOnTyping,
      closeOnBlur,
      triggerCharacters,
      retriggerCharacters,
    }),
    characterSetsConfig,
    Prec.high(keymap.of(signatureKeymap)),
    baseTheme,
  ];
}
