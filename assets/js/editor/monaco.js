import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import ElixirLanguageConfiguration from "./elixir/language_configuration";
import ElixirMonarchLanguage from "./elixir/monarch_language";
import ElixirOnTypeFormattingEditProvider from "./elixir/on_type_formatting_edit_provider";

// Register the Elixir language and add relevant configuration
monaco.languages.register({ id: "elixir" });

monaco.languages.setLanguageConfiguration(
  "elixir",
  ElixirLanguageConfiguration
);

monaco.languages.registerOnTypeFormattingEditProvider(
  "elixir",
  ElixirOnTypeFormattingEditProvider
);

monaco.languages.setMonarchTokensProvider("elixir", ElixirMonarchLanguage);

// Define custom theme

monaco.editor.defineTheme("custom", {
  base: "vs",
  inherit: false,
  rules: [
    { token: "", foreground: "#444444" },
    { token: "variable", foreground: "#ca4956" },
    { token: "constant", foreground: "#3c91cf" },
    { token: "constant.character.escape", foreground: "#3c91cf" },
    { token: "comment", foreground: "#9e9e9e" },
    { token: "number", foreground: "#bf8b56" },
    { token: "regexp", foreground: "#ca4956" },
    { token: "type", foreground: "#ca4956" },
    { token: "string", foreground: "#50a14f" },
    { token: "keyword", foreground: "#9c00b0" },
    { token: "operator", foreground: "#cc5c52" },
    { token: "delimiter.bracket.embed", foreground: "#204a87" },
    { token: "sigil", foreground: "#bf8b56" },
    { token: "function", foreground: "#3c91cf" },
    { token: "function.call", foreground: "#444444" },

    // Markdown specific
    { token: "emphasis", fontStyle: "italic" },
    { token: "strong", fontStyle: "bold" },
    { token: "keyword.md", foreground: "#ca4956" },
    { token: "keyword.table", foreground: "#ca4956" },
    { token: "string.link.md", foreground: "#3c91cf" },
    { token: "variable.md", foreground: "#204a87" },
  ],
  colors: {
    "editor.background": "#fafafa",
    "editorLineNumber.foreground": "#cfd8dc",
    "editorCursor.foreground": "#666666",
    "editor.selectionBackground": "#eeeeee",
  },
});

export default monaco;
