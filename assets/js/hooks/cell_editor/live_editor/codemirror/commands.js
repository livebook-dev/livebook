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
