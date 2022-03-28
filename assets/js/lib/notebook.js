/**
 * Checks if the given cell type is eligible for evaluation.
 */
export function isEvaluable(cellType) {
  return ["code", "smart", "setup"].includes(cellType);
}

/**
 * Checks if the given cell type has primary editable editor.
 */
export function isDirectlyEditable(cellType) {
  return ["markdown", "code", "setup"].includes(cellType);
}
