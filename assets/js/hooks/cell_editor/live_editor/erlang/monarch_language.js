const ErlangMonarchLanguage = {
  // Set defaultToken to invalid to see what you do not tokenize yet
  // defaultToken: 'invalid',

  keywords: [
    "case",
    "if",
    "begin",
    "end",
    "when",
    "of",
    "fun",
    "maybe",
    "else",
    "try",
    "catch",
    "receive",
    "after",
  ],

  attributes: [
    "-module",
    "-record",
    "-export",
    "-spec",
    "-include",
    "-include_lib",
    "-export",
    "-undef",
    "-ifdef",
    "-ifndef",
    "-else",
    "-endif",
    "-if",
    "-elif",
    "-define",
  ],

  operators: [
    "=",
    "==",
    "=:=",
    "/=",
    "=/=",
    ">",
    "<",
    "=<",
    ">=",
    "+",
    "++",
    "-",
    "--",
    "*",
    "/",
    "!",
    "and",
    "or",
    "not",
    "xor",
    "andalso",
    "orelse",
    "bnot",
    "div",
    "rem",
    "band",
    "bor",
    "bxor",
    "bsl",
    "bsr",
    ":=",
    "=>",
    "->",
    "?=",
    "<-",
    "||",
  ],

  builtins: ["error", "exit"],

  brackets: [
    ["(", ")", "delimiter.parenthesis"],
    ["{", "}", "delimiter.curly"],
    ["[", "]", "delimiter.square"],
  ],

  symbols: /[=><~&|+\-*\/%@#]+/,

  escapes:
    /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

  tokenizer: {
    root: [
      [
        /(-[a-z_]+)/,
        {
          cases: {
            "@attributes": "keyword",
            "@default": "identifier",
          },
        },
      ],

      [/(\?[a-zA-Z_0-9]+)/, "constant"],

      [/[A-Z_][a-z0-9_]*/, "identifier"],
      [
        /[a-z_][\w\-']*/,
        {
          cases: {
            "@builtins": "predefined.identifier",
            "@keywords": "keyword",
            "@default": "identifier",
          },
        },
      ],

      // whitespace
      { include: "@whitespace" },

      // delimiters and operators
      [/[()\[\]\{\}]/, "@brackets"],
      [
        /@symbols/,
        {
          cases: {
            "@operators": "predefined.operator",
            "@default": "operator",
          },
        },
      ],

      // numbers
      [/\d*\.\d+([eE][\-+]?\d+)?/, "number.float"],
      [/16#[0-9a-fA-F]+/, "number.hex"],
      [/\d+/, "number"],

      // strings
      [/"([^"\\]|\\.)*$/, "string.invalid"], // non-teminated string
      [/"/, { token: "string.quote", bracket: "@open", next: "@string" }],
    ],

    string: [
      [/[^\\"]+/, "string"],
      [/@escapes/, "string.escape"],
      [/\\./, "string.escape.invalid"],
      [/"/, { token: "string.quote", bracket: "@close", next: "@pop" }],
    ],

    whitespace: [
      [/[ \t\r\n]+/, "white"],
      [/%.*$/, "comment"],
    ],
  },
};

export default ErlangMonarchLanguage;
