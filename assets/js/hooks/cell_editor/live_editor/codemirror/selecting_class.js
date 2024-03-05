import { EditorView } from "@codemirror/view";

const attributesFacet = EditorView.editorAttributes.compute(
  ["selection"],
  (state) => {
    const allRangesEmpty = state.selection.ranges.every((range) => range.empty);
    return allRangesEmpty ? {} : { class: "cm-selecting" };
  },
);

/**
 * Returns an extension that adds `cm-selecting` class to the root
 * element, whenever the editor has a non-empty selection range.
 */
export function selectingClass() {
  return [attributesFacet];
}
