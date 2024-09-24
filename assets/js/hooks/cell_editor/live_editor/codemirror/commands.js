import { closeCompletion } from "@codemirror/autocomplete";
import { insertBlankLine } from "@codemirror/commands";
import { closeSignature } from "./signature";

/**
 * This command, when multi-cursor is active, collapses the selection
 * to the main cursor only.
 */
export function exitMulticursor(view) {
  const selection = view.state.selection;

  if (selection.ranges.length > 1) {
    view.dispatch({ selection: selection.asSingle() });
    return true;
  }

  return false;
}

/**
 * Calls `insertBlankLine` and closes active intellisense hints.
 */
export function insertBlankLineAndCloseHints(view) {
  if (insertBlankLine(view)) {
    closeCompletion(view);
    closeSignature(view);
    return true;
  }

  return false;
}
