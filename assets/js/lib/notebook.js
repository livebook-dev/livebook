/**
 * Checks if the given cell type is eligible for evaluation.
 */
export function isEvaluable(cellType) {
  return ["code", "smart"].includes(cellType);
}

/**
 * Checks if the given cell type has editable editor.
 */
export function isDirectlyEditable(cellType) {
  return ["markdown", "code"].includes(cellType);
}
