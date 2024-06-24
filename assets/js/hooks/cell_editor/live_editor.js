import {
  EditorView,
  hoverTooltip,
  keymap,
  highlightSpecialChars,
  drawSelection,
  highlightActiveLine,
  dropCursor,
  rectangularSelection,
  crosshairCursor,
  lineNumbers,
  highlightActiveLineGutter,
} from "@codemirror/view";
import { EditorState } from "@codemirror/state";
import {
  indentOnInput,
  bracketMatching,
  foldGutter,
  LanguageDescription,
  codeFolding,
  syntaxTree,
} from "@codemirror/language";
import { history } from "@codemirror/commands";
import { highlightSelectionMatches } from "@codemirror/search";
import {
  autocompletion,
  closeBrackets,
  snippetCompletion,
} from "@codemirror/autocomplete";
import { setDiagnostics } from "@codemirror/lint";
import { vscodeKeymap } from "@replit/codemirror-vscode-keymap";
import { vim } from "@replit/codemirror-vim";
import { emacs } from "@replit/codemirror-emacs";

import { collab, deltaToChanges } from "./live_editor/codemirror/collab";
import { collabMarkers } from "./live_editor/codemirror/collab_markers";
import { theme, lightTheme } from "./live_editor/codemirror/theme";
import {
  clearDoctests,
  updateDoctests,
} from "./live_editor/codemirror/doctests";
import { signature } from "./live_editor/codemirror/signature";
import { formatter } from "./live_editor/codemirror/formatter";
import { replacedSuffixLength } from "../../lib/text_utils";
import { settingsStore } from "../../lib/settings";
import Delta from "../../lib/delta";
import Markdown from "../../lib/markdown";
import { readOnlyHint } from "./live_editor/codemirror/read_only_hint";
import { wait } from "../../lib/utils";
import Emitter from "../../lib/emitter";
import CollabClient from "./live_editor/collab_client";
import { languages } from "./live_editor/codemirror/languages";
import {
  exitMulticursor,
  insertBlankLineAndCloseHints,
} from "./live_editor/codemirror/commands";
import { ancestorNode, closestNode } from "./live_editor/codemirror/tree_utils";
import { selectingClass } from "./live_editor/codemirror/selecting_class";

/**
 * Mounts cell source editor with real-time collaboration mechanism.
 *
 * The actual editor must be mounted explicitly by calling either of
 * the `mount` or `focus` methods, but it can be done at any point.
 * Even when not mounted, the editor consumes collaborative updates
 * and invokes change listeners.
 */
export default class LiveEditor {
  /** @private */
  _onMount = new Emitter();

  /**
   * Registers a callback called when the editor is mounted in DOM.
   */
  onMount = this._onMount.event;

  /** @private */
  _onChange = new Emitter();

  /**
   * Registers a callback called with a new cell content whenever it changes.
   */
  onChange = this._onChange.event;

  /** @private */
  _onBlur = new Emitter();

  /**
   * Registers a callback called whenever the editor loses focus.
   */
  onBlur = this._onBlur.event;

  /** @private */
  _onFocus = new Emitter();

  /**
   * Registers a callback called whenever the editor gains focus.
   */
  onFocus = this._onFocus.event;

  constructor(
    container,
    connection,
    source,
    revision,
    language,
    intellisense,
    readOnly,
  ) {
    this.container = container;
    this.source = source;
    this.language = language;
    this.intellisense = intellisense;
    this.readOnly = readOnly;
    this.initialWidgets = {};

    this.connection = connection;
    this.collabClient = new CollabClient(connection, revision);

    this.deltaSubscription = this.collabClient.onDelta((delta, info) => {
      this.source = delta.applyToString(this.source);
      this._onChange.dispatch(this.source);
    });
  }

  /**
   * Checks if an editor instance has been mounted in the DOM.
   */
  isMounted() {
    return !!this.view;
  }

  /**
   * Mounts and configures an editor instance in the DOM.
   */
  mount() {
    if (this.isMounted()) {
      throw new Error("The editor is already mounted");
    }

    this.mountEditor();

    this.setInitialWidgets();

    this._onMount.dispatch();
  }

  /**
   * Returns current editor content.
   */
  getSource() {
    return this.source;
  }

  /**
   * Returns an element closest to the current main cursor position.
   */
  getElementAtCursor() {
    if (!this.isMounted()) {
      return this.container;
    }

    const { node } = this.view.domAtPos(this.view.state.selection.main.head);
    if (node instanceof Element) return node;
    return node.parentElement;
  }

  /**
   * Focuses the editor.
   *
   * Note that this forces the editor to be mounted, if it is not already
   * mounted.
   */
  focus() {
    if (!this.isMounted()) {
      this.mount();
    }

    this.view.focus();
  }

  /**
   * Removes focus from the editor.
   */
  blur() {
    if (this.isMounted() && this.view.hasFocus) {
      this.view.contentDOM.blur();
    }
  }

  /**
   * Performs necessary cleanup actions.
   */
  destroy() {
    if (this.isMounted()) {
      this.view.destroy();
    }

    this.collabClient.destroy();
    this.deltaSubscription.destroy();
  }

  /**
   * Either adds or updates doctest indicators.
   */
  updateDoctests(doctestReports) {
    if (this.isMounted()) {
      updateDoctests(this.view, doctestReports);
    } else {
      this.initialWidgets.doctestReportsByLine =
        this.initialWidgets.doctestReportsByLine || {};

      for (const report of doctestReports) {
        this.initialWidgets.doctestReportsByLine[report.line] = report;
      }
    }
  }

  /**
   * Removes doctest indicators.
   */
  clearDoctests() {
    if (this.isMounted()) {
      clearDoctests(this.view);
    } else {
      delete this.initialWidgets.doctestReportsByLine;
    }
  }

  /**
   * Sets underline markers for warnings and errors.
   *
   * Passing an empty list clears all markers.
   */
  setCodeMarkers(codeMarkers) {
    if (this.isMounted()) {
      const doc = this.view.state.doc;

      const diagnostics = codeMarkers.map((marker) => {
        const line = doc.line(marker.line);

        const [, leadingWhitespace, trailingWhitespace] =
          line.text.match(/^(\s*).*?(\s*)$/);

        const from = line.from + leadingWhitespace.length;
        const to = line.to - trailingWhitespace.length;

        return {
          from,
          to,
          severity: marker.severity,
          message: marker.description,
        };
      });

      this.view.dispatch(setDiagnostics(this.view.state, diagnostics));
    } else {
      this.initialWidgets.codeMarkers = codeMarkers;
    }
  }

  /** @private */
  mountEditor() {
    const settings = settingsStore.get();

    const formatLineNumber = (number) => number.toString().padStart(3, " ");

    const foldGutterMarkerDOM = (open) => {
      const node = document.createElement("i");
      node.classList.add(
        open ? "ri-arrow-down-s-line" : "ri-arrow-right-s-line",
        open ? "cm-gutterFoldMarker-open" : null,
      );
      return node;
    };

    const fontSizeTheme = EditorView.theme({
      "&": { fontSize: `${settings.editor_font_size}px` },
    });

    const ligaturesTheme = EditorView.theme({
      "&": {
        fontVariantLigatures: `${settings.editor_ligatures ? "normal" : "none"}`,
      },
    });

    const lineWrappingEnabled =
      this.language === "markdown" && settings.editor_markdown_word_wrap;

    const language =
      this.language &&
      LanguageDescription.matchLanguageName(languages, this.language, false);

    const customKeymap = [
      { key: "Escape", run: exitMulticursor },
      { key: "Alt-Enter", run: insertBlankLineAndCloseHints },
    ];

    this.view = new EditorView({
      parent: this.container,
      doc: this.source,
      extensions: [
        lineNumbers({ formatNumber: formatLineNumber }),
        highlightActiveLine(),
        highlightActiveLineGutter(),
        highlightSpecialChars(),
        highlightSelectionMatches(),
        foldGutter({ markerDOM: foldGutterMarkerDOM }),
        codeFolding({ placeholderText: "⋯" }),
        drawSelection(),
        dropCursor(),
        rectangularSelection(),
        selectingClass(),
        crosshairCursor(),
        EditorState.allowMultipleSelections.of(true),
        bracketMatching(),
        closeBrackets(),
        indentOnInput(),
        history(),
        EditorState.readOnly.of(this.readOnly),
        readOnlyHint(),
        keymap.of(customKeymap),
        keymap.of(vscodeKeymap),
        EditorState.tabSize.of(2),
        EditorState.lineSeparator.of("\n"),
        lineWrappingEnabled ? EditorView.lineWrapping : [],
        // We bind tab to actions within the editor, which would trap
        // the user if they tabbed into the editor, so we remove it
        // from the tab navigation
        EditorView.contentAttributes.of({ tabIndex: -1 }),
        fontSizeTheme,
        settings.editor_theme === "light" ? lightTheme : theme,
        ligaturesTheme,
        collab(this.collabClient),
        collabMarkers(this.collabClient),
        autocompletion({
          activateOnTyping: settings.editor_auto_completion,
          defaultKeymap: false,
        }),
        this.intellisense
          ? [
              autocompletion({ override: [this.completionSource.bind(this)] }),
              hoverTooltip(this.docsHoverTooltipSource.bind(this)),
              signature(this.signatureSource.bind(this), {
                activateOnTyping: settings.editor_auto_signature,
              }),
              formatter(this.formatterSource.bind(this)),
            ]
          : [],
        settings.editor_mode === "vim" ? [vim()] : [],
        settings.editor_mode === "emacs" ? [emacs()] : [],
        language ? language.support : [],
        EditorView.domEventHandlers({
          keydown: this.handleEditorKeydown.bind(this),
          blur: this.handleEditorBlur.bind(this),
          focus: this.handleEditorFocus.bind(this),
        }),
      ],
    });
  }

  /** @private */
  handleEditorKeydown(event) {
    // We dispatch escape event, but only if it is not consumed by any
    // registered handler in the editor, such as closing autocompletion
    // or escaping Vim insert mode

    if (event.key === "Escape") {
      this.container.dispatchEvent(
        new CustomEvent("lb:editor_escape", { bubbles: true }),
      );
    }

    return false;
  }

  /** @private */
  handleEditorBlur(event) {
    if (!this.container.contains(event.relatedTarget)) {
      this._onBlur.dispatch();
    }

    return false;
  }

  /** @private */
  handleEditorFocus(event) {
    this._onFocus.dispatch();

    return false;
  }

  /** @private */
  completionSource(context) {
    const settings = settingsStore.get();

    // Trigger completion implicitly only for identifiers and members
    const triggerBeforeCursor = context.matchBefore(/[\w?!.]$/);

    if (!triggerBeforeCursor && !context.explicit) {
      return null;
    }

    const textUntilCursor = this.getCompletionHint(context);

    return this.connection
      .intellisenseRequest("completion", {
        hint: textUntilCursor,
        editor_auto_completion: settings.editor_auto_completion,
      })
      .then((response) => {
        if (response.items.length === 0) return null;

        const completions = response.items.map((item, index) => {
          const completion = this.completionItemToCompletions(item);

          return {
            ...completion,
            // Keep the ordering from the server
            boost: 1 - index / response.items.length,
          };
        });

        const replaceLength = replacedSuffixLength(
          textUntilCursor,
          response.items[0].insert_text,
        );

        return {
          from: context.pos - replaceLength,
          options: completions,
          validFor: /^\w*[!?]?$/,
        };
      })
      .catch(() => null);
  }

  /** @private */
  getCompletionHint(context) {
    // By default we only send the current line content until cursor
    // as completion hint. We use the local AST to send more context
    // for multiline expressions where we know it's relevant.

    const tree = syntaxTree(context.state);
    const node = tree.resolve(context.pos);

    if (node && this.language === "elixir") {
      const boundaryNode = closestNode(node, ["Map", "Bitstring"]);

      if (boundaryNode) {
        return context.state.doc.sliceString(boundaryNode.from, context.pos);
      }
    }

    return context.matchBefore(/^.*/).text;
  }

  /** @private */
  completionItemToCompletions(item) {
    const completion = {
      label: item.label,
      type: item.kind,
      info: (completion) => {
        if (item.documentation === null) return null;

        // The info popup is shown automatically, we delay it a bit
        // to not distract the user too much as they are typing
        return wait(350).then(() => {
          const node = document.createElement("div");
          node.classList.add("cm-completionInfoDocs");
          node.classList.add("cm-markdown");
          new Markdown(node, item.documentation, {
            defaultCodeLanguage: this.language,
            useDarkTheme: this.usesDarkTheme(),
          });
          return node;
        });
      },
    };

    // Place cursor at the end, if not explicitly specified
    const template = item.insert_text.includes("${}")
      ? item.insert_text
      : item.insert_text + "${}";

    return snippetCompletion(template, completion);
  }

  /** @private */
  docsHoverTooltipSource(view, pos, side) {
    const line = view.state.doc.lineAt(pos);
    const lineLength = line.to - line.from;

    const text = line.text;
    // If we are on the right side of the position, we add one to
    // convert it to column
    const column = pos - line.from + (side === 1 ? 1 : 0);
    if (column < 1 || column > lineLength) return null;

    return this.connection
      .intellisenseRequest("details", { line: text, column })
      .then((response) => {
        // Note: the response range is a right-exclusive column range

        return {
          pos: line.from + response.range.from - 1,
          end: line.from + response.range.to - 1,
          above: true,
          create: (view) => {
            const dom = document.createElement("div");
            dom.classList.add("cm-hoverDocs");

            for (const content of response.contents) {
              const item = document.createElement("div");
              item.classList.add("cm-hoverDocsContent");
              item.classList.add("cm-markdown");
              dom.appendChild(item);
              new Markdown(item, content, {
                defaultCodeLanguage: this.language,
                useDarkTheme: this.usesDarkTheme(),
              });
            }

            return { dom };
          },
        };
      })
      .catch(() => null);
  }

  /** @private */
  signatureSource({ state, pos }) {
    const textUntilCursor = this.getSignatureHint(state, pos);

    return this.connection
      .intellisenseRequest("signature", {
        hint: textUntilCursor,
      })
      .then((response) => {
        return {
          activeArgumentIdx: response.active_argument,
          items: response.items,
        };
      })
      .catch(() => null);
  }

  /** @private */
  getSignatureHint(state, pos) {
    // By default we send all text until cursor as signature hint.
    // We use the local AST to limit the hint to the relevanat call
    // expression.

    const tree = syntaxTree(state);
    const node = tree.resolve(pos);

    if (node && this.language === "elixir") {
      let callNode = closestNode(node, [
        "Call",
        "FunctionDefinitionCall",
        "KernelCall",
      ]);

      if (callNode) {
        const pipeNode = ancestorNode(callNode, ["Right", "PipeOperator"]);
        const boundaryNode = pipeNode || callNode;

        return state.doc.sliceString(boundaryNode.from, pos);
      }
    }

    return state.doc.sliceString(0, pos);
  }

  formatterSource(doc) {
    return this.connection
      .intellisenseRequest("format", { code: doc.toString() })
      .then((response) => {
        this.setCodeMarkers(response.code_markers);

        if (response.delta) {
          const delta = Delta.fromCompressed(response.delta);
          return deltaToChanges(delta);
        } else {
          return null;
        }
      })
      .catch(() => null);
  }

  /** @private */
  setInitialWidgets() {
    if (this.initialWidgets.doctestReportsByLine) {
      const doctestReports = Object.values(
        this.initialWidgets.doctestReportsByLine,
      );
      this.updateDoctests(doctestReports);
    }

    if (this.initialWidgets.codeMarkers) {
      this.setCodeMarkers(this.initialWidgets.codeMarkers);
    }

    this.initialWidgets = {};
  }

  /** @private */
  usesDarkTheme() {
    const settings = settingsStore.get();
    return settings.editor_theme !== "light";
  }
}
