import { EditorView } from "@codemirror/view";
import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";
import { tags as t } from "@lezer/highlight";

// Most of the colors for the editor widgets are based on the One Dark
// theme in the Zed editor. The highlighting colors are based on the
// Atom One Dark theme from VS Code, since they are more contrasting.
// The comment color is brightened for AA accessibility.

function buildEditorTheme(colors, { dark }) {
  const fonts = {
    sans: "Inter",
    mono: "JetBrains Mono, monospace",
  };

  return EditorView.theme(
    {
      "&": {
        color: colors.text,
        backgroundColor: colors.background,
        borderRadius: "8px",
        fontSize: "14px",
        fontFamily: fonts.mono,

        // A workaround for a strange contenteditable behaviour in
        // Chrome and Safari, where clicking on the right side of the
        // editor results in focus. See https://discuss.codemirror.net/t/editor-focus-happens-when-clicking-outside-the-editor/7544/3
        display: "inline-flex !important",
        width: "100%",
      },

      "&.cm-focused": {
        outline: "none",
      },

      ".cm-scroller": {
        fontFamily: "inherit",
        // We add padding directly to the scroll container, rather
        // than the editor parent, so that there is additional space
        // for the scrollbar when it appears. Without this padding,
        // the scrollbar would overlap the editor content
        padding: "0.75rem 0",
      },

      ".cm-content": {
        caretColor: colors.cursor,
        padding: "0",
      },

      // Scroll

      "*": {
        "&": {
          scrollbarWidth: "thin",
          scrollbarColor: `${colors.backgroundLightest} transparent`,
        },

        // Fallback for Safari, which does not implement scrollbar-*
        // CSS properties yet
        "&::-webkit-scrollbar": {
          width: "8px",
          height: "8px",
        },

        "&::-webkit-scrollbar-thumb": {
          background: colors.backgroundLightest,
        },

        "&::-webkit-scrollbar-track": {
          background: "transparent",
        },
      },

      // Cursor and selection

      ".cm-activeLine": {
        backgroundColor: "transparent",
      },

      "&.cm-focused:not(.cm-selecting) .cm-activeLine": {
        backgroundColor: colors.activeLine,
      },

      ".cm-cursor, .cm-dropCursor": {
        borderLeft: "1px solid",
        borderRight: "1px solid",
        marginLeft: "-1px",
        marginRight: "-1px",
        borderColor: colors.cursor,
      },

      ".cm-selectionBackground": {
        backgroundColor: colors.inactiveSelectionBackground,
      },

      "&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground":
        {
          backgroundColor: colors.selectionBackground,
        },

      ".cm-selectionMatch": {
        backgroundColor: colors.selectionMatchBackground,
      },

      // Vim cursor

      "&:not(.cm-focused) .cm-fat-cursor": {
        outline: "none !important",
      },

      // Base components

      ".cm-gutters": {
        backgroundColor: colors.background,
        color: colors.gutterText,
        border: "none",
      },

      ".cm-gutters .cm-activeLineGutter": {
        backgroundColor: "transparent",
      },

      "&.cm-focused:not(.cm-selecting) .cm-gutters .cm-activeLineGutter": {
        backgroundColor: colors.activeLine,
      },

      ".cm-tooltip": {
        backgroundColor: colors.backgroundLighter,
        boxShadow: "0 2px 6px 0 rgba(0, 0, 0, 0.2)",
        color: colors.text,
        borderRadius: "8px",
        border: `1px solid ${colors.border}`,

        "& .cm-tooltip-arrow::before": {
          borderTopColor: colors.backgroundLighter,
          borderBottomColor: colors.backgroundLighter,
        },
      },

      ".cm-panels": {
        backgroundColor: "transparent",
        color: colors.text,

        "&.cm-panels-top": {
          borderBottom: `2px solid ${colors.separator}`,
        },

        "&.cm-panels-bottom": {
          borderTop: `2px solid ${colors.separator}`,
        },
      },

      // Line numbers

      ".cm-gutter.cm-lineNumbers": {
        "& .cm-gutterElement": {
          color: colors.lineNumber,
          whiteSpace: "pre",

          "&.cm-activeLineGutter": {
            color: colors.lineNumberActive,
          },
        },
      },

      // Folding

      ".cm-gutter.cm-foldGutter": {
        "& .cm-gutterElement": {
          cursor: "pointer",
        },

        "&:not(:hover) .cm-gutterElement > .cm-gutterFoldMarker-open": {
          visibility: "hidden",
        },
      },

      ".cm-foldPlaceholder": {
        backgroundColor: "transparent",
        border: "none",
        color: "unset",
        borderRadius: "2px",
        "&:hover": {
          backgroundColor: colors.selectionBackground,
        },
      },

      // Search

      ".cm-searchMatch": {
        backgroundColor: colors.searchMatchBackground,

        "&.cm-searchMatch-selected": {
          backgroundColor: colors.searchMatchActiveBackground,
        },
      },

      // Brackets

      "&.cm-focused .cm-matchingBracket, &.cm-focused .cm-nonmatchingBracket": {
        backgroundColor: colors.selectionBackground,
      },

      // Completion

      ".cm-tooltip.cm-tooltip-autocomplete": {
        padding: "4px",
        backgroundColor: colors.backgroundLighter,
        color: colors.text,

        "& > ul": {
          fontFamily: "inherit",

          "&  > li": {
            padding: "4px",
            borderRadius: "4px",
            display: "flex",
            alignItems: "center",

            "&[aria-selected]": {
              background: "none",
              backgroundColor: colors.backgroundLightest,
              color: "unset",
            },

            "& .cm-completionIcon": {
              lineHeight: "1",

              "&:after": { fontVariantLigatures: "normal" },
              "&.cm-completionIcon-function:after": { content: "'ƒ'" },
              "&.cm-completionIcon-module:after": { content: "'m'" },
              "&.cm-completionIcon-struct:after": { content: "'⭘'" },
              "&.cm-completionIcon-interface:after": { content: "'*'" },
              "&.cm-completionIcon-type:after": { content: "'t'" },
              "&.cm-completionIcon-variable:after": { content: "'𝑥'" },
              "&.cm-completionIcon-field:after": { content: "'•'" },
              "&.cm-completionIcon-keyword:after": { content: "'⚡'" },
            },
          },
        },

        "& .cm-completionMatchedText": {
          textDecoration: "none",
          color: colors.matchingText,
        },
      },

      ".cm-tooltip.cm-completionInfo": {
        borderRadius: "8px",
        backgroundColor: colors.backgroundLighter,
        color: colors.text,
        top: "0 !important",
        padding: "0",

        "&.cm-completionInfo-right": {
          marginLeft: "4px",
        },

        "&.cm-completionInfo-left": {
          marginRight: "4px",
        },

        "& .cm-completionInfoDocs": {
          padding: "6px",
        },
      },

      // Hover docs

      ".cm-tooltip .cm-hoverDocs": {
        maxWidth: "800px",
        maxHeight: "300px",
        overflowY: "auto",
        display: "flex",
        flexDirection: "column",

        "& .cm-hoverDocsDefinitionLink": {
          padding: "4px 8px",
          cursor: "pointer",
          fontSize: "0.875em",
          fontFamily: fonts.sans,
          opacity: 0.8,
          borderBottom: `1px solid ${colors.separator}`,

          "& i": {
            marginRight: "2px",
          },
        },

        "& .cm-hoverDocsContents": {
          padding: "8px",
          display: "flex",
          flexDirection: "column",
          gap: "64px",
        },
      },

      ".cm-hoverDocsSelection": {
        backgroundColor: colors.selectionMatchBackground,
      },

      // Signature

      ".cm-tooltip.cm-signatureHint": {
        "& .cm-signatureHintStepper": {
          borderColor: colors.separator,
        },

        "& .cm-signatureHintActiveArgument": {
          color: colors.matchingText,
        },
      },

      // Diagnostics

      ".cm-tooltip-lint": {
        display: "flex",
        flexDirection: "column",
        gap: "8px",
        maxWidth: "600px",

        "& .cm-diagnostic": {
          display: "flex",
          border: "none",
          margin: "0",

          "&::before": {
            content: "'➜'",
            fontVariantLigatures: "common-ligatures",
            fontSize: "1.5em",
            lineHeight: "1",
            whiteSpace: "nowrap",
            marginRight: "6px",
          },

          "&.cm-diagnostic-error::before": {
            color: "#be5046",
          },

          "&.cm-diagnostic-warning::before": {
            color: "#d19a66",
          },
        },
      },

      // Search
      //
      // It is possible to build a fully custom panel and hook into the
      // search actions, but search is rarely useful, since the notebook
      // is broken into cells, so we just do some basic styling

      ".cm-panel.cm-search": {
        display: "flex",
        alignItems: "center",
        flexWrap: "wrap",
        padding: "8px",
        background: colors.background,

        "& br": {
          content: '" "',
          display: "block",
          width: "100%",
        },

        "& .cm-textfield": {
          borderRadius: "4px",
          border: `1px solid ${colors.border}`,

          "&:focus": {
            outline: "none",
          },
        },

        "& .cm-button": {
          borderRadius: "4px",
          background: colors.backgroundLightest,
          border: "none",

          "&:focus-visible": {
            outline: `1px solid ${colors.text}`,
          },
        },

        "& label:first-of-type": {
          marginLeft: "8px",
        },

        "& label": {
          display: "inline-flex",
          alignItems: "center",
          gap: "4px",
        },

        "& label input": {
          appearance: "none",
          border: `1px solid ${colors.backgroundLightest}`,
          borderRadius: "4px",
          width: "18px",
          height: "18px",

          "&:checked": {
            backgroundImage: `url("data:image/svg+xml,%3csvg viewBox='0 0 16 16' fill='white' xmlns='http://www.w3.org/2000/svg'%3e%3cpath d='M12.207 4.793a1 1 0 010 1.414l-5 5a1 1 0 01-1.414 0l-2-2a1 1 0 011.414-1.414L6.5 9.086l4.293-4.293a1 1 0 011.414 0z'/%3e%3c/svg%3e")`,
            backgroundColor: colors.backgroundLightest,
            borderColor: "transparent",
          },

          "&:focus-visible": {
            outline: `1px solid ${colors.text}`,
          },
        },

        '& button[name="close"]': {
          display: "none",
        },
      },

      // Markdown

      ".cm-markdown": {
        "&": {
          fontFamily: fonts.sans,
        },

        "& p": {
          marginTop: "0.5rem",
          marginBottom: "0.5rem",
        },

        "& hr": {
          borderTop: `1px solid ${colors.separator}`,
          marginTop: "0.5rem",
          marginBottom: "0.5rem",
          marginRight: "-6px",
          marginLeft: "-6px",
        },

        "& a": {
          borderBottom: `1px solid ${colors.text}`,
        },

        "& h2": {
          marginTop: "0.5rem",
          marginBottom: "0.5rem",
          fontSize: "1.125em",
          fontWeight: "600",
        },

        "& ul": {
          marginTop: "1rem",
          marginBottom: "1rem",
          marginLeft: "2rem",
          listStylePosition: "outside",
          listStyleType: "disc",

          "& ul": {
            listStyleType: "circle",

            "& ul": {
              listStyleType: "square",
            },
          },
        },

        "& li": {
          marginTop: "0.25rem",
          marginBottom: "0.25rem",

          "& > ul, & > ol": {
            marginTop: "0",
            marginBottom: "0",
          },
        },

        "& ol": {
          marginTop: "1rem",
          marginBottom: "1rem",
          marginLeft: "2rem",
          listStylePosition: "outside",
          listStyleType: "decimal",
        },

        "& code": {
          background: colors.selectionBackground,
          borderRadius: "3px",
        },

        "& pre": {
          marginTop: "0.5rem",
          marginBottom: "0.5rem",

          "& code": {
            background: "transparent",
            whiteSpace: "pre-wrap",
          },
        },

        "& > :first-child": {
          marginTop: "0",
        },

        "& > :last-child": {
          marginBottom: "0",
        },
      },
    },
    { dark },
  );
}

function buildHighlightStyle({
  base,
  lightRed,
  blue,
  gray,
  green,
  purple,
  red,
  teal,
  peach,
  yellow,
}) {
  return HighlightStyle.define([
    { tag: t.keyword, color: purple },
    { tag: t.null, color: blue },
    { tag: t.bool, color: blue },
    { tag: t.number, color: blue },
    { tag: t.string, color: green },
    { tag: t.special(t.string), color: yellow },
    { tag: t.character, color: blue },
    { tag: t.escape, color: blue },
    { tag: t.atom, color: blue },
    { tag: t.variableName, color: base },
    { tag: t.special(t.variableName), color: lightRed },
    {
      tag: [t.function(t.variableName), t.function(t.propertyName)],
      color: blue,
    },
    { tag: t.namespace, color: teal },
    { tag: t.operator, color: peach },
    { tag: t.comment, color: gray },
    { tag: [t.docString, t.docComment], color: gray },
    {
      tag: [t.paren, t.squareBracket, t.brace, t.angleBracket, t.separator],
      color: base,
    },
    { tag: t.special(t.brace), color: red },

    // Markdown specific
    { tag: t.strong, fontWeight: "bold" },
    { tag: t.emphasis, fontStyle: "italic" },
    { tag: t.strikethrough, textDecoration: "line-through" },
    { tag: t.link, color: blue },
    { tag: t.heading, color: lightRed },
    { tag: t.monospace, color: green },

    // JSON specific
    { tag: t.propertyName, color: lightRed },

    // HTML specific
    { tag: t.tagName, color: purple },

    // CSS specific
    { tag: t.className, color: peach },
  ]);
}

const editorTheme = buildEditorTheme(
  {
    text: "#c8ccd4",
    background: "#282c34",
    backgroundLighter: "#2f343e",
    backgroundLightest: "#454a56",
    border: "#363c46",
    cursor: "#73ade8",
    activeLine: "#2d323b",
    selectionBackground: "#394c5f",
    inactiveSelectionBackground: "#29333d",
    selectionMatchBackground: "#343f4d",
    gutterText: "#c8ccd4",
    lineNumber: "#60646c",
    lineNumberActive: "#c8ccd4",
    matchingText: "#73ade8",
    searchMatchBackground: "#4c6582",
    searchMatchActiveBackground: "#54789e",
    separator: "#464b57",
  },
  { dark: true },
);

export const highlightStyle = buildHighlightStyle({
  base: "#c8ccd4",
  lightRed: "#e06c75",
  blue: "#61afef",
  gray: "#8c92a3",
  green: "#98c379",
  purple: "#c678dd",
  red: "#be5046",
  teal: "#56b6c2",
  peach: "#d19a66",
  yellow: "#e5c07b",
});

export const theme = [editorTheme, syntaxHighlighting(highlightStyle)];

const lightEditorTheme = buildEditorTheme(
  {
    text: "#383a41",
    background: "#fafafa",
    backgroundLighter: "#ebebec",
    backgroundLightest: "#cacaca",
    border: "#dfdfe0",
    cursor: "#5c79e2",
    activeLine: "#efeff0",
    selectionBackground: "#d4dbf4",
    inactiveSelectionBackground: "#ebeef9",
    selectionMatchBackground: "#d3d5e1",
    gutterText: "#383a41",
    lineNumber: "#b6b7b9",
    lineNumberActive: "#383a41",
    matchingText: "#73ade8",
    searchMatchBackground: "#bbc6f1",
    searchMatchActiveBackground: "#9daeec",
    separator: "#c9c9ca",
  },
  { dark: false },
);

export const lightHighlightStyle = buildHighlightStyle({
  base: "#304254",
  lightRed: "#e45649",
  blue: "#4078F2",
  gray: "#707177",
  green: "#50a14f",
  purple: "#a726a4",
  red: "#ca1243",
  teal: "#0084bc",
  peach: "#986801",
  yellow: "#c18401",
});

export const lightTheme = [
  lightEditorTheme,
  syntaxHighlighting(lightHighlightStyle),
];
