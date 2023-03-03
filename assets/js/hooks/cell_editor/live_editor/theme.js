// This is a port of the One Dark theme to the Monaco editor.
// We color graded the comment so it has AA accessibility and
// then similarly scaled the default font.
const colors = {
  default: "#c4cad6",
  lightRed: "#e06c75",
  blue: "#61afef",
  gray: "#8c92a3",
  green: "#98c379",
  purple: "#c678dd",
  red: "#be5046",
  teal: "#56b6c2",
  peach: "#d19a66",
};

const hcColors = {
  default: "#304254",
  lightRed: "#A42419",
  blue: "#0E49C8",
  gray: "#707177",
  green: "#2E602E",
  purple: "#912291",
  red: "#a90f38",
  teal: "#01597E",
  peach: "#745001",
};

const background = { default: "#282c34", highContrast: "#fafafa" };

const theme = {
  base: "vs-dark",
  inherit: false,
  rules: [
    { token: "", foreground: colors.default },
    { token: "variable", foreground: colors.lightRed },
    { token: "constant", foreground: colors.blue },
    { token: "constant.character.escape", foreground: colors.blue },
    { token: "comment", foreground: colors.gray },
    { token: "number", foreground: colors.blue },
    { token: "regexp", foreground: colors.lightRed },
    { token: "type", foreground: colors.lightRed },
    { token: "string", foreground: colors.green },
    { token: "keyword", foreground: colors.purple },
    { token: "operator", foreground: colors.peach },
    { token: "delimiter.bracket.embed", foreground: colors.red },
    { token: "sigil", foreground: colors.teal },
    { token: "function", foreground: colors.blue },
    { token: "function.call", foreground: colors.default },

    // Markdown specific
    { token: "emphasis", fontStyle: "italic" },
    { token: "strong", fontStyle: "bold" },
    { token: "keyword.md", foreground: colors.lightRed },
    { token: "keyword.table", foreground: colors.lightRed },
    { token: "string.link.md", foreground: colors.blue },
    { token: "variable.md", foreground: colors.teal },
    { token: "string.md", foreground: colors.default },
    { token: "variable.source.md", foreground: colors.default },

    // XML specific
    { token: "tag", foreground: colors.lightRed },
    { token: "metatag", foreground: colors.lightRed },
    { token: "attribute.name", foreground: colors.peach },
    { token: "attribute.value", foreground: colors.green },

    // JSON specific
    { token: "string.key", foreground: colors.lightRed },
    { token: "keyword.json", foreground: colors.blue },

    // SQL specific
    { token: "operator.sql", foreground: colors.purple },
  ],

  colors: {
    "editor.background": background.default,
    "editor.foreground": colors.default,
    "editorLineNumber.foreground": "#636d83",
    "editorCursor.foreground": "#636d83",
    "editor.selectionBackground": "#3e4451",
    "editor.findMatchHighlightBackground": "#528bff3D",
    "editorSuggestWidget.background": "#21252b",
    "editorSuggestWidget.border": "#181a1f",
    "editorSuggestWidget.selectedBackground": "#2c313a",
    "input.background": "#1b1d23",
    "input.border": "#181a1f",
    "editorBracketMatch.border": "#282c34",
    "editorBracketMatch.background": "#3e4451",
  },
};

const highContrast = {
  base: "vs-dark",
  inherit: false,
  rules: [
    { token: "", foreground: hcColors.default },
    { token: "variable", foreground: hcColors.lightRed },
    { token: "constant", foreground: hcColors.blue },
    { token: "constant.character.escape", foreground: hcColors.blue },
    { token: "comment", foreground: hcColors.gray },
    { token: "number", foreground: hcColors.blue },
    { token: "regexp", foreground: hcColors.lightRed },
    { token: "type", foreground: hcColors.lightRed },
    { token: "string", foreground: hcColors.green },
    { token: "keyword", foreground: hcColors.purple },
    { token: "operator", foreground: hcColors.peach },
    { token: "delimiter.bracket.embed", foreground: hcColors.red },
    { token: "sigil", foreground: hcColors.teal },
    { token: "function", foreground: hcColors.blue },
    { token: "function.call", foreground: hcColors.default },

    // Markdown specific
    { token: "emphasis", fontStyle: "italic" },
    { token: "strong", fontStyle: "bold" },
    { token: "keyword.md", foreground: hcColors.lightRed },
    { token: "keyword.table", foreground: hcColors.lightRed },
    { token: "string.link.md", foreground: hcColors.blue },
    { token: "variable.md", foreground: hcColors.teal },
    { token: "string.md", foreground: hcColors.default },
    { token: "variable.source.md", foreground: hcColors.default },

    // XML specific
    { token: "tag", foreground: hcColors.lightRed },
    { token: "metatag", foreground: hcColors.lightRed },
    { token: "attribute.name", foreground: hcColors.peach },
    { token: "attribute.value", foreground: hcColors.green },

    // JSON specific
    { token: "string.key", foreground: hcColors.lightRed },
    { token: "keyword.json", foreground: hcColors.blue },

    // SQL specific
    { token: "operator.sql", foreground: hcColors.purple },
  ],

  colors: {
    "editor.background": "#fafafa",
    "editor.foreground": hcColors.default,
    "editorLineNumber.foreground": "#9D9D9F",
    "editorCursor.foreground": "#526FFF",
    "editor.selectionBackground": "#E5E5E6",
    "editor.findMatchHighlightBackground": "#526FFF33",
    "editorSuggestWidget.background": "#EAEAEB",
    "editorSuggestWidget.border": "#DBDBDC",
    "editorSuggestWidget.selectedBackground": "#FFFFFF",
    "input.background": "#FFFFFF",
    "input.border": "#DBDBDC",

    "editorBracketMatch.border": "#fafafa",
    "editorBracketMatch.background": "#e5e5e6",
  },
};

export { theme, highContrast };
