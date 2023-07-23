// For the full list of features see [1] and [2]. The Monaco Webpack
// plugin always ignores certain imports [3], so we ignore these as
// well. On top of that, we ignore some other features which we
// clearly don't use.
//
// [1]: https://github.com/microsoft/vscode/blob/main/src/vs/editor/editor.main.ts
// [2]: https://github.com/microsoft/vscode/blob/main/src/vs/editor/editor.all.ts
// [3]: https://github.com/microsoft/monaco-editor/blob/v0.38.0/build/releaseMetadata.ts#L212-L221

// === Monaco features ===

import "monaco-editor/esm/vs/editor/browser/coreCommands";
// import 'monaco-editor/esm/vs/editor/browser/widget/codeEditorWidget';
// import 'monaco-editor/esm/vs/editor/browser/widget/diffEditorWidget';
// import 'monaco-editor/esm/vs/editor/browser/widget/diffNavigator';
import "monaco-editor/esm/vs/editor/contrib/anchorSelect/browser/anchorSelect";
import "monaco-editor/esm/vs/editor/contrib/bracketMatching/browser/bracketMatching";
import "monaco-editor/esm/vs/editor/contrib/caretOperations/browser/caretOperations";
import "monaco-editor/esm/vs/editor/contrib/caretOperations/browser/transpose";
import "monaco-editor/esm/vs/editor/contrib/clipboard/browser/clipboard";
import "monaco-editor/esm/vs/editor/contrib/codeAction/browser/codeActionContributions";
// import 'monaco-editor/esm/vs/editor/contrib/codelens/browser/codelensController';
// import 'monaco-editor/esm/vs/editor/contrib/colorPicker/browser/colorContributions';
// import 'monaco-editor/esm/vs/editor/contrib/colorPicker/browser/standaloneColorPickerActions';
import "monaco-editor/esm/vs/editor/contrib/comment/browser/comment";
import "monaco-editor/esm/vs/editor/contrib/contextmenu/browser/contextmenu";
import "monaco-editor/esm/vs/editor/contrib/cursorUndo/browser/cursorUndo";
import "monaco-editor/esm/vs/editor/contrib/dnd/browser/dnd";
// import 'monaco-editor/esm/vs/editor/contrib/dropOrPasteInto/browser/copyPasteContribution';
// import 'monaco-editor/esm/vs/editor/contrib/dropOrPasteInto/browser/dropIntoEditorContribution';
import "monaco-editor/esm/vs/editor/contrib/find/browser/findController";
import "monaco-editor/esm/vs/editor/contrib/folding/browser/folding";
import "monaco-editor/esm/vs/editor/contrib/fontZoom/browser/fontZoom";
import "monaco-editor/esm/vs/editor/contrib/format/browser/formatActions";
// import 'monaco-editor/esm/vs/editor/contrib/documentSymbols/browser/documentSymbols';
import "monaco-editor/esm/vs/editor/contrib/inlineCompletions/browser/inlineCompletions.contribution";
import "monaco-editor/esm/vs/editor/contrib/inlineProgress/browser/inlineProgress";
import "monaco-editor/esm/vs/editor/contrib/gotoSymbol/browser/goToCommands";
import "monaco-editor/esm/vs/editor/contrib/gotoSymbol/browser/link/goToDefinitionAtPosition";
import "monaco-editor/esm/vs/editor/contrib/gotoError/browser/gotoError";
import "monaco-editor/esm/vs/editor/contrib/hover/browser/hover";
import "monaco-editor/esm/vs/editor/contrib/indentation/browser/indentation";
import "monaco-editor/esm/vs/editor/contrib/inlayHints/browser/inlayHintsContribution";
import "monaco-editor/esm/vs/editor/contrib/inPlaceReplace/browser/inPlaceReplace";
import "monaco-editor/esm/vs/editor/contrib/lineSelection/browser/lineSelection";
import "monaco-editor/esm/vs/editor/contrib/linesOperations/browser/linesOperations";
import "monaco-editor/esm/vs/editor/contrib/linkedEditing/browser/linkedEditing";
import "monaco-editor/esm/vs/editor/contrib/links/browser/links";
import "monaco-editor/esm/vs/editor/contrib/longLinesHelper/browser/longLinesHelper";
import "monaco-editor/esm/vs/editor/contrib/multicursor/browser/multicursor";
import "monaco-editor/esm/vs/editor/contrib/parameterHints/browser/parameterHints";
import "monaco-editor/esm/vs/editor/contrib/rename/browser/rename";
import "monaco-editor/esm/vs/editor/contrib/semanticTokens/browser/documentSemanticTokens";
import "monaco-editor/esm/vs/editor/contrib/semanticTokens/browser/viewportSemanticTokens";
import "monaco-editor/esm/vs/editor/contrib/smartSelect/browser/smartSelect";
import "monaco-editor/esm/vs/editor/contrib/snippet/browser/snippetController2";
import "monaco-editor/esm/vs/editor/contrib/stickyScroll/browser/stickyScrollContribution";
import "monaco-editor/esm/vs/editor/contrib/suggest/browser/suggestController";
import "monaco-editor/esm/vs/editor/contrib/suggest/browser/suggestInlineCompletions";
// import 'monaco-editor/esm/vs/editor/contrib/tokenization/browser/tokenization';
// import 'monaco-editor/esm/vs/editor/contrib/toggleTabFocusMode/browser/toggleTabFocusMode';
import "monaco-editor/esm/vs/editor/contrib/unicodeHighlighter/browser/unicodeHighlighter";
import "monaco-editor/esm/vs/editor/contrib/unusualLineTerminators/browser/unusualLineTerminators";
import "monaco-editor/esm/vs/editor/contrib/wordHighlighter/browser/wordHighlighter";
import "monaco-editor/esm/vs/editor/contrib/wordOperations/browser/wordOperations";
import "monaco-editor/esm/vs/editor/contrib/wordPartOperations/browser/wordPartOperations";
import "monaco-editor/esm/vs/editor/contrib/readOnlyMessage/browser/contribution";

// import 'monaco-editor/esm/vs/editor/common/standaloneStrings';
// import 'vs/base/browser/ui/codicons/codiconStyles';

import "monaco-editor/esm/vs/editor/standalone/browser/accessibilityHelp/accessibilityHelp";
// import "monaco-editor/esm/vs/editor/standalone/browser/iPadShowKeyboard/iPadShowKeyboard";
import "monaco-editor/esm/vs/editor/standalone/browser/inspectTokens/inspectTokens";
import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneHelpQuickAccess";
import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneGotoLineQuickAccess";
import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneGotoSymbolQuickAccess";
import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneCommandsQuickAccess";
import "monaco-editor/esm/vs/editor/standalone/browser/referenceSearch/standaloneReferenceSearch";
// import 'monaco-editor/esm/vs/editor/standalone/browser/toggleHighContrast/toggleHighContrast';

import * as monaco from "monaco-editor/esm/vs/editor/editor.api";

// === Languages ===

// Note: JSON doesn't have a monarch grammar in basic-languages, so
// we use the advanced handler (which also has its own web worker)
import "monaco-editor/esm/vs/language/json/monaco.contribution";

import "monaco-editor/esm/vs/basic-languages/elixir/elixir.contribution";
import "monaco-editor/esm/vs/basic-languages/markdown/markdown.contribution";
import "monaco-editor/esm/vs/basic-languages/javascript/javascript.contribution";
import "monaco-editor/esm/vs/basic-languages/sql/sql.contribution";
import "monaco-editor/esm/vs/basic-languages/css/css.contribution";
import "monaco-editor/esm/vs/basic-languages/html/html.contribution";
import "monaco-editor/esm/vs/basic-languages/xml/xml.contribution";
import "monaco-editor/esm/vs/basic-languages/dockerfile/dockerfile.contribution";

// === Configuration ===

import { CommandsRegistry } from "monaco-editor/esm/vs/platform/commands/common/commands";
import ElixirOnTypeFormattingEditProvider from "./elixir/on_type_formatting_edit_provider";
import ErlangMonarchLanguage from "./erlang/monarch_language";
import ErlangLanguageConfiguration from "./erlang/language_configuration";
import { theme, lightTheme } from "./theme";

import { PieceTreeTextBufferBuilder } from "monaco-editor/esm/vs/editor/common/model/pieceTreeTextBuffer/pieceTreeTextBufferBuilder";

import { settingsStore } from "../../../lib/settings";

window.MonacoEnvironment = {
  // Certain browser extensions are Monaco-aware, so we expose it on
  // the window object
  globalAPI: true,
  getWorkerUrl(_workerId, label) {
    if (label === "json") {
      return "/assets/language/json/json.worker.js";
    }

    return "/assets/editor/editor.worker.js";
  },
};

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
if (PieceTreeTextBufferBuilder.prototype.finish) {
  const original = PieceTreeTextBufferBuilder.prototype.finish;

  // We don't have access to the factory class directly, so we override
  // the builder, such that we modify the factory object once created
  PieceTreeTextBufferBuilder.prototype.finish = function (...args) {
    const factory = original.apply(this, args);

    if (factory._getEOL) {
      factory._getEOL = function (defaultEOL) {
        return "\n";
      };
    } else {
      throw new Error("failed to override line endings to LF");
    }

    return factory;
  };
} else {
  throw new Error("failed to override line endings to LF");
}

monaco.languages.registerOnTypeFormattingEditProvider(
  "elixir",
  ElixirOnTypeFormattingEditProvider
);

monaco.languages.register({ id: "erlang" });
monaco.languages.setMonarchTokensProvider("erlang", ErlangMonarchLanguage);
monaco.languages.setLanguageConfiguration(
  "erlang",
  ErlangLanguageConfiguration
);

// Define custom theme
monaco.editor.defineTheme("default", theme);
monaco.editor.defineTheme("light", lightTheme);

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

let completionItemProvider = null;

settingsStore.getAndSubscribe((settings) => {
  // We replace the completion provider to always reflect the settings
  if (completionItemProvider) {
    completionItemProvider.dispose();
  }

  completionItemProvider = monaco.languages.registerCompletionItemProvider(
    "elixir",
    {
      // Trigger characters always open the popup, so we add dot only
      // when completion while typing is enabled
      triggerCharacters: settings.editor_auto_completion ? ["."] : [],
      provideCompletionItems: (model, position, context, token) => {
        if (model.__getCompletionItems__) {
          return model.__getCompletionItems__(model, position);
        } else {
          return null;
        }
      },
    }
  );
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
  // Currently monaco.editor.colorize doesn't support passing theme
  // directly and uses the theme from last editor initialization, so
  // we need to make sure there was at least one editor initialization
  // with the configured theme.
  //
  // Tracked in https://github.com/microsoft/monaco-editor/issues/3302
  if (!highlight.initialized) {
    const settings = settingsStore.get();
    monaco.editor.create(document.createElement("div"), {
      theme: settings.editor_theme,
    });
    highlight.initialized = true;
  }

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
