/**
 * Defines Elixir traits to enable various editor features,
 * like automatic bracket insertion and indentation.
 */
const ElixirLanguageConfiguration = {
  comments: {
    lineComment: "#",
  },
  brackets: [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"],
  ],
  surroundingPairs: [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"],
    ["'", "'"],
    ['"', '"'],
  ],
  autoClosingPairs: [
    { open: "'", close: "'", notIn: ["string", "comment"] },
    { open: '"', close: '"', notIn: ["comment"] },
    { open: '"""', close: '"""' },
    { open: "`", close: "`", notIn: ["string", "comment"] },
    { open: "(", close: ")" },
    { open: "{", close: "}" },
    { open: "[", close: "]" },
    { open: "<<", close: ">>" },
  ],
  indentationRules: {
    increaseIndentPattern: /^\s*(after|else|catch|rescue|fn|[^#]*(do|<\-|\->|\{|\[|\=))\s*$/,
    decreaseIndentPattern: /^\s*((\}|\])\s*$|(after|else|catch|rescue|end)\b)/,
  },
};

export default ElixirLanguageConfiguration;
