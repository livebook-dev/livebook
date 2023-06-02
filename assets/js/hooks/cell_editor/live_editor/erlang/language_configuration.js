// Adapted from https://github.com/erlang-ls/vscode/blob/0.0.39/language-configuration.json
const ErlangLanguageConfiguration = {
  comments: {
    lineComment: "%",
  },
  brackets: [
    ["(", ")"],
    ["[", "]"],
    ["{", "}"],
  ],
  autoClosingPairs: [
    { open: "(", close: ")" },
    { open: "[", close: "]" },
    { open: "{", close: "}" },
    { open: "'", close: "'", notIn: ["string", "comment"] },
    { open: '"', close: '"', notIn: ["string"] },
    { open: '<<"', close: '">>', notIn: ["string"] },
  ],
  surroundingPairs: [
    { open: "(", close: ")" },
    { open: "[", close: "]" },
    { open: "{", close: "}" },
    { open: "'", close: "'" },
    { open: '"', close: '"' },
  ],
  indentationRules: {
    // Indent if a line ends brackets, "->" or most keywords. Also if prefixed
    // with "||". This should work with most formatting models.
    // The ((?!%).)* is to ensure this doesn't match inside comments.
    increaseIndentPattern:
      /^((?!%).)*([{([]|->|after|begin|case|catch|fun|if|of|try|when|(\|\|.*))\s*$/,
    // Dedent after brackets, end or lone "->". The latter happens in a spec
    // with indented types, typically after "when". Only do this if it's _only_
    // preceded by whitespace.
    decreaseIndentPattern: /^\s*([)}]]|end|->\s*$)/,
    // Indent if after an incomplete map association operator, list
    // comprehension and type specifier. But only once, then return to the
    // previous indent.
    indentNextLinePattern: /^((?!%).)*(::|=>|:=|<-)\s*$/,
  },
};

export default ErlangLanguageConfiguration;
