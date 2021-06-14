import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import ElixirOnTypeFormattingEditProvider from "./elixir/on_type_formatting_edit_provider";
import theme from "./theme";

monaco.languages.registerOnTypeFormattingEditProvider(
  "elixir",
  ElixirOnTypeFormattingEditProvider
);

// Define custom theme
monaco.editor.defineTheme("custom", theme);

// See https://github.com/microsoft/monaco-editor/issues/648#issuecomment-564978560
// Without this selecting text with whitespace shrinks the whitespace.
document.fonts.addEventListener("loadingdone", (event) => {
  const jetBrainsMonoLoaded = event.fontfaces.some(
    // font-family may be either "JetBrains Mono" or "\"JetBrains Mono\""
    (fontFace) => fontFace.family.includes("JetBrains Mono")
  );

  if (jetBrainsMonoLoaded) {
    // We use JetBrains Mono in all instances of the editor,
    // so we wait until it loads and then tell Monaco to remeasure
    // fonts and updates its cache.
    monaco.editor.remeasureFonts();
  }
});

// Define custom completion provider.
// In our case the completion behaviour is cell-dependent,
// so we delegate the implementation to the appropriate cell.
// See cell/live_editor.js for more details.
monaco.languages.registerCompletionItemProvider("elixir", {
  provideCompletionItems: (model, position) => {
    if (model.__getCompletionItems) {
      return model.__getCompletionItems(model, position);
    } else {
      return [];
    }
  },
});

export default monaco;
