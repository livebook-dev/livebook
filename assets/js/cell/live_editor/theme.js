// This is a port of the One Dark theme to the Monaco editor.

const theme = {
  base: "vs-dark",
  inherit: false,
  rules: [
    { token: "", foreground: "#abb2bf" },
    { token: "variable", foreground: "#e06c75" },
    { token: "constant", foreground: "#61afef" },
    { token: "constant.character.escape", foreground: "#61afef" },
    { token: "comment", foreground: "#5c6370" },
    { token: "number", foreground: "#61afef" },
    { token: "regexp", foreground: "#e06c75" },
    { token: "type", foreground: "#e06c75" },
    { token: "string", foreground: "#98c379" },
    { token: "keyword", foreground: "#c678dd" },
    { token: "operator", foreground: "#d19a66" },
    { token: "delimiter.bracket.embed", foreground: "#be5046" },
    { token: "sigil", foreground: "#56b6c2" },
    { token: "function", foreground: "#61afef" },
    { token: "function.call", foreground: "#abb2bf" },

    // Markdown specific
    { token: "emphasis", fontStyle: "italic" },
    { token: "strong", fontStyle: "bold" },
    { token: "keyword.md", foreground: "#e06c75" },
    { token: "keyword.table", foreground: "#e06c75" },
    { token: "string.link.md", foreground: "#61afef" },
    { token: "variable.md", foreground: "#56b6c2" },
  ],

  colors: {
    "editor.background": "#282c34",
    "editor.foreground": "#abb2bf",
    "editorLineNumber.foreground": "#636d83",
    "editorCursor.foreground": "#636d83",
    "editor.selectionBackground": "#3e4451",
    "editor.findMatchHighlightBackground": "#528bff3D",
    "editorSuggestWidget.background": "#21252b",
    "editorSuggestWidget.border": "#181a1f",
    "editorSuggestWidget.selectedBackground": "#2c313a",
    "input.background": "#1b1d23",
    "input.border": "#181a1f",
  },
};

export default theme;
