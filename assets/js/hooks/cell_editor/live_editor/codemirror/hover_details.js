import { Decoration, EditorView, hoverTooltip } from "@codemirror/view";
import { RangeSet } from "@codemirror/state";

/**
 * Returns an extension that enables hover details tooltip.
 */
export function hoverDetails(hoverTooltipSource) {
  const tooltipExtension = hoverTooltip(hoverTooltipSource);

  const decorationsExtension = EditorView.decorations.from(
    tooltipExtension.active,
    (tooltips) => {
      const backgroundDecoration = Decoration.mark({
        class: "cm-hoverDocsSelection",
      });

      const decorationRanges = tooltips.map((tooltip) =>
        backgroundDecoration.range(tooltip.pos, tooltip.end),
      );

      return RangeSet.of(decorationRanges, true);
    },
  );

  return [tooltipExtension, decorationsExtension];
}
