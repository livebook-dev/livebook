import { EditorView, keymap } from "@codemirror/view";
import { Compartment } from "@codemirror/state";

/**
 * Returns an extension that toggles the given extension with the given
 * keyboard shortcut.
 */
export function toggleWith(key, extension) {
  const compartment = new Compartment();

  function toggle(view) {
    const isEnabled = compartment.get(view.state) === extension;

    view.dispatch({
      effects: compartment.reconfigure(isEnabled ? [] : extension),
    });

    return true;
  }

  return [compartment.of([]), keymap.of({ key, run: toggle })];
}
