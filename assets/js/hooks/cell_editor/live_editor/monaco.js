import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import { CommandsRegistry } from "monaco-editor/esm/vs/platform/commands/common/commands";
import ElixirOnTypeFormattingEditProvider from "./elixir/on_type_formatting_edit_provider";
import { theme, highContrast } from "./theme";

import { PieceTreeTextBufferFactory } from 'monaco-editor/esm/vs/editor/common/model/pieceTreeTextBuffer/pieceTreeTextBufferBuilder';

// Force LF for line ending.
//
// Monaco infers EOL based on the text content if any, otherwise uses
// a system dependent value (CRLF for Windows). Then, the content is
// always normalized to use that EOL. We need to ensure consistent
// behaviour for collaborative editing to work. We already enforce
// LF when importing/exporting Live Markdown, so the easiest approach
// is to enforce it in the editor as well.
//
// There is no direct configuration to accomplish this, so we use an
// override of [1] instead. There is also a long-running discussion
// around EOL in [2].
//
// An alternative approach would be to disable line normalization and
// possibly set the default EOL to LF (used when there is no content
// to infer EOL from). Currently neither of those is configurable and
// requires more complex overrides.
//
// [1]: https://github.com/microsoft/vscode/blob/34f184263de048a6283af1d9eb9faab84da4547d/src/vs/editor/common/model/pieceTreeTextBuffer/pieceTreeTextBufferBuilder.ts#L27-L40
// [2]: https://github.com/microsoft/vscode/issues/127
if (PieceTreeTextBufferFactory.prototype._getEOL) {
  PieceTreeTextBufferFactory.prototype._getEOL = function (defaultEOL) {
    return "\n";
  }
} else {
  throw new Error("failed to override line endings to LF");
}

monaco.languages.registerOnTypeFormattingEditProvider(
  "elixir",
  ElixirOnTypeFormattingEditProvider
);

// Define custom theme
monaco.editor.defineTheme("default", theme);
monaco.editor.defineTheme("highContrast", highContrast);

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
    if (model.__getCompletionItems__) {
      return model.__getCompletionItems__(model, position);
    } else {
      return null;
    }
  },
});

monaco.languages.registerHoverProvider("elixir", {
  provideHover: (model, position, token) => {
    if (model.__getHover__) {
      return model.__getHover__(model, position);
    } else {
      return null;
    }
  },
});

monaco.languages.registerSignatureHelpProvider("elixir", {
  signatureHelpTriggerCharacters: ["(", ","],
  provideSignatureHelp: (model, position, token, context) => {
    if (model.__getSignatureHelp__) {
      return model.__getSignatureHelp__(model, position);
    } else {
      return null;
    }
  },
});

monaco.languages.registerDocumentFormattingEditProvider("elixir", {
  provideDocumentFormattingEdits: (model, options, token) => {
    if (model.__getDocumentFormattingEdits__) {
      return model.__getDocumentFormattingEdits__(model);
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
