import { LanguageDescription } from "@codemirror/language";
import { highlightCode } from "@lezer/highlight";
import { StyleModule } from "style-mod";
import { languages } from "./codemirror/languages";
import { highlightStyle, lightHighlightStyle } from "./codemirror/theme";
import { escapeHtml } from "../../../lib/utils";
import { settingsStore } from "../../../lib/settings";

export function highlight(code, language) {
  const languageDesc = LanguageDescription.matchLanguageName(
    languages,
    language,
  );

  if (!languageDesc) {
    return escapeHtml(code);
  }

  const highlightStyle = getHighlightStyle();

  // Ensure the highlighter CSS is added to the page
  StyleModule.mount(window.document, highlightStyle.module);

  const tree = languageDesc.support.language.parser.parse(code);

  let html = "";

  highlightCode(
    code,
    tree,
    highlightStyle,
    (code, classes) => {
      html += `<span class="${classes}">${escapeHtml(code)}</span>`;
    },
    () => {
      html += "<br />";
    },
  );

  return html;
}

function getHighlightStyle() {
  const settings = settingsStore.get();

  return settings.editor_theme === "light"
    ? lightHighlightStyle
    : highlightStyle;
}
