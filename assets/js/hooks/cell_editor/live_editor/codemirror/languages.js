import {
  LanguageDescription,
  StreamLanguage,
  LanguageSupport,
} from "@codemirror/language";
import { markdown, markdownLanguage } from "@codemirror/lang-markdown";
import { sql } from "@codemirror/lang-sql";
import { json } from "@codemirror/lang-json";
import { xml } from "@codemirror/lang-xml";
import { css } from "@codemirror/lang-css";
import { html } from "@codemirror/lang-html";
import { python } from "@codemirror/lang-python";
import { yaml } from "@codemirror/lang-yaml";
import { javascript } from "@codemirror/lang-javascript";
import { erlang } from "@codemirror/legacy-modes/mode/erlang";
import { dockerFile } from "@codemirror/legacy-modes/mode/dockerfile";
import { shell } from "@codemirror/legacy-modes/mode/shell";
import { toml } from "@codemirror/legacy-modes/mode/toml";
import { elixir } from "codemirror-lang-elixir";

export const elixirDesc = LanguageDescription.of({
  name: "Elixir",
  support: elixir(),
});

const erlangDesc = LanguageDescription.of({
  name: "Erlang",
  support: new LanguageSupport(StreamLanguage.define(erlang)),
});

const sqlDesc = LanguageDescription.of({
  name: "SQL",
  support: sql(),
});

const pythonDesc = LanguageDescription.of({
  name: "Python",
  support: python(),
});

const yamlDesc = LanguageDescription.of({
  name: "YAML",
  support: yaml(),
});

const jsonDesc = LanguageDescription.of({
  name: "JSON",
  support: json(),
});

const xmlDesc = LanguageDescription.of({
  name: "XML",
  support: xml(),
});

const cssDesc = LanguageDescription.of({
  name: "CSS",
  support: css(),
});

const htmlDesc = LanguageDescription.of({
  name: "HTML",
  support: html(),
});

const javascriptDesc = LanguageDescription.of({
  name: "JavaScript",
  support: javascript(),
});

const dockerfileDesc = LanguageDescription.of({
  name: "Dockerfile",
  support: new LanguageSupport(StreamLanguage.define(dockerFile)),
});

const shellDesc = LanguageDescription.of({
  name: "Shell",
  support: new LanguageSupport(StreamLanguage.define(shell)),
});

const tomlDesc = LanguageDescription.of({
  name: "TOML",
  alias: ["pyproject.toml"],
  support: new LanguageSupport(StreamLanguage.define(toml)),
});

const markdownDesc = LanguageDescription.of({
  name: "Markdown",
  support: markdown({
    base: markdownLanguage,
    codeLanguages: [
      elixirDesc,
      erlangDesc,
      sqlDesc,
      pythonDesc,
      yamlDesc,
      jsonDesc,
      xmlDesc,
      cssDesc,
      htmlDesc,
      javascriptDesc,
      dockerfileDesc,
      shellDesc,
      tomlDesc,
    ],
  }),
});

export const languages = [
  elixirDesc,
  erlangDesc,
  sqlDesc,
  pythonDesc,
  yamlDesc,
  jsonDesc,
  xmlDesc,
  cssDesc,
  htmlDesc,
  javascriptDesc,
  dockerfileDesc,
  shellDesc,
  tomlDesc,
  markdownDesc,
];
