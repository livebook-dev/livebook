import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import { CommandsRegistry } from "monaco-editor/esm/vs/platform/commands/common/commands";
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

/**
 * Define custom providers for various editor features.
 *
 * In our case, each cell has its own editor and behaviour
 * of requests like completion and hover are cell dependent.
 * For this reason we delegate the implementation to the
 * specific cell by using its text model object.
 *
 * See cell/live_editor.js for more details.
 */

monaco.languages.registerCompletionItemProvider("elixir", {
  provideCompletionItems: (model, position, context, token) => {
    if (model.__getCompletionItems) {
      return model.__getCompletionItems(model, position);
    } else {
      return null;
    }
  },
});

monaco.languages.registerHoverProvider("elixir", {
  provideHover: (model, position, token) => {
    if (model.__getHover) {
      return model.__getHover(model, position);
    } else {
      return null;
    }
  },
});

monaco.languages.registerDocumentFormattingEditProvider("elixir", {
  provideDocumentFormattingEdits: (model, options, token) => {
    if (model.__getDocumentFormattingEdits) {
      return model.__getDocumentFormattingEdits(model);
    } else {
      return null;
    }
  },
});

export default monaco;

/**
 * Highlights the given code using the same rules as in the editor.
 *
 * Returns a promise resolving to HTML that renders as the highlighted code.
 */
export function highlight(code, language) {
  return monaco.editor.colorize(code, language).then((result) => {
    // `colorize` always adds additional newline, so we remove it
    return result.replace(/<br\/>$/, "");
  });
}

/**
 * Updates keybinding for the given editor command.
 *
 * This uses an internal API, since there is no clean support
 * for customizing keybindings.
 * See https://github.com/microsoft/monaco-editor/issues/102#issuecomment-822981429
 */
export function addKeybinding(editor, id, newKeybinding) {
  const { handler, when } = CommandsRegistry.getCommand(id) ?? {};

  if (handler) {
    editor._standaloneKeybindingService.addDynamicKeybinding(
      id,
      newKeybinding,
      handler,
      when
    );
  }
}
