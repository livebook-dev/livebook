import { getAttributeOrDefault, getAttributeOrThrow } from "../lib/attribute";
import Markdown from "../lib/markdown";
import { globalPubSub } from "../lib/pub_sub";
import { md5Base64, smoothlyScrollToElement } from "../lib/utils";
import scrollIntoView from "scroll-into-view-if-needed";
import { isEvaluable } from "../lib/notebook";

/**
 * A hook managing a single cell.
 *
 * Manages the collaborative editor, takes care of markdown rendering
 * and focusing the editor when applicable.
 *
 * ## Configuration
 *
 *   * `data-cell-id` - id of the cell being edited
 *
 *   * `data-type` - type of the cell
 *
 *   * `data-session-path` - root path to the current session
 *
 *   * `data-evaluation-digest` - digest of the last evaluated cell source
 */
const Cell = {
  mounted() {
    this.props = this.getProps();

    this.isFocused = false;
    this.insertMode = false;
    this.liveEditors = {};

    this.updateInsertModeAvailability();

    // Setup action handlers

    if (this.props.type === "code") {
      const amplifyButton = this.el.querySelector(
        `[data-element="amplify-outputs-button"]`
      );
      amplifyButton.addEventListener("click", (event) => {
        this.el.toggleAttribute("data-js-amplified");
      });
    }

    if (this.props.type === "smart") {
      const toggleSourceButton = this.el.querySelector(
        `[data-element="toggle-source-button"]`
      );
      toggleSourceButton.addEventListener("click", (event) => {
        this.el.toggleAttribute("data-js-source-visible");
        this.updateInsertModeAvailability();
        this.maybeFocusCurrentEditor();
      });
    }

    // Setup listeners

    this.el.addEventListener("lb:cell:editor_created", (event) => {
      const { tag, liveEditor } = event.detail;
      this.handleCellEditorCreated(tag, liveEditor);
    });

    this.el.addEventListener("lb:cell:editor_removed", (event) => {
      const { tag } = event.detail;
      this.handleCellEditorRemoved(tag);
    });

    // We manually track hover to correctly handle absolute iframe

    this.el.addEventListener("mouseenter", (event) => {
      this.el.setAttribute("data-js-hover", "");
    });

    this.el.addEventListener("mouseleave", (event) => {
      this.el.removeAttribute("data-js-hover");
    });

    this.unsubscribeFromNavigationEvents = globalPubSub.subscribe(
      "navigation",
      (event) => this.handleNavigationEvent(event)
    );

    this.unsubscribeFromCellsEvents = globalPubSub.subscribe("cells", (event) =>
      this.handleCellsEvent(event)
    );
  },

  disconnected() {
    // When disconnected, this client is no longer seen by the server
    // and misses all collaborative changes. On reconnection we want
    // to clean up and mount a fresh hook, which we force by ensuring
    // the DOM id doesn't match
    this.el.removeAttribute("id");
  },

  destroyed() {
    this.unsubscribeFromNavigationEvents();
    this.unsubscribeFromCellsEvents();
  },

  updated() {
    const prevProps = this.props;
    this.props = this.getProps();

    if (this.props.evaluationDigest !== prevProps.evaluationDigest) {
      this.updateChangeIndicator();
    }
  },

  getProps() {
    return {
      cellId: getAttributeOrThrow(this.el, "data-cell-id"),
      type: getAttributeOrThrow(this.el, "data-type"),
      sessionPath: getAttributeOrThrow(this.el, "data-session-path"),
      evaluationDigest: getAttributeOrDefault(
        this.el,
        "data-evaluation-digest",
        null
      ),
    };
  },

  handleNavigationEvent(event) {
    if (event.type === "element_focused") {
      this.handleElementFocused(event.focusableId, event.scroll);
    } else if (event.type === "insert_mode_changed") {
      this.handleInsertModeChanged(event.enabled);
    } else if (event.type === "location_report") {
      this.handleLocationReport(event.client, event.report);
    }
  },

  handleCellsEvent(event) {
    if (event.type === "cell_moved") {
      this.handleCellMoved(event.cellId);
    } else if (event.type === "cell_upload") {
      this.handleCellUpload(event.cellId, event.url);
    }
  },

  handleElementFocused(focusableId, scroll) {
    if (this.props.cellId === focusableId) {
      this.isFocused = true;
      this.el.setAttribute("data-js-focused", "");
      if (scroll) {
        smoothlyScrollToElement(this.el);
      }
    } else if (this.isFocused) {
      this.isFocused = false;
      this.el.removeAttribute("data-js-focused");
    }
  },

  handleCellEditorCreated(tag, liveEditor) {
    this.liveEditors[tag] = liveEditor;

    this.updateInsertModeAvailability();

    if (liveEditor === this.currentEditor()) {
      // Once the editor is created, reflect the current insert mode state
      this.maybeFocusCurrentEditor(true);
    }

    liveEditor.onBlur(() => {
      // Prevent from blurring unless the state changes. For example
      // when we move cell using buttons the editor should keep focus
      if (this.isFocused && this.insertMode) {
        this.currentEditor().focus();
      }
    });

    liveEditor.onCursorSelectionChange((selection) => {
      this.broadcastSelection(selection);
    });

    if (tag === "primary") {
      // Setup markdown rendering
      if (this.props.type === "markdown") {
        const markdownContainer = this.el.querySelector(
          `[data-element="markdown-container"]`
        );
        const markdown = new Markdown(
          markdownContainer,
          liveEditor.getSource(),
          {
            baseUrl: this.props.sessionPath,
            emptyText: "Empty markdown cell",
          }
        );

        liveEditor.onChange((newSource) => {
          markdown.setContent(newSource);
        });
      }

      // Setup change indicator
      if (isEvaluable(this.props.type)) {
        this.updateChangeIndicator();

        liveEditor.onChange((newSource) => {
          this.updateChangeIndicator();
        });

        this.handleEvent(
          `evaluation_finished:${this.props.cellId}`,
          ({ code_error }) => {
            liveEditor.setCodeErrorMarker(code_error);
          }
        );
      }
    }
  },

  handleCellEditorRemoved(tag) {
    delete this.liveEditors[tag];
  },

  currentEditor() {
    return this.liveEditors[this.currentEditorTag()];
  },

  currentEditorTag() {
    if (this.props.type === "smart") {
      const isSourceTab = this.el.hasAttribute("data-js-source-visible");
      return isSourceTab ? "primary" : "secondary";
    }

    return "primary";
  },

  updateInsertModeAvailability() {
    this.el.toggleAttribute(
      "data-js-insert-mode-disabled",
      !this.currentEditor()
    );
  },

  maybeFocusCurrentEditor(scroll = false) {
    if (this.isFocused && this.insertMode) {
      this.currentEditor().focus();

      if (scroll) {
        // If the element is being scrolled to, focus interrupts it,
        // so ensure the scrolling continues.
        smoothlyScrollToElement(this.el);
      }

      this.broadcastSelection();
    }
  },

  updateChangeIndicator() {
    const cellStatus = this.el.querySelector(`[data-element="cell-status"]`);
    const indicator =
      cellStatus &&
      cellStatus.querySelector(`[data-element="change-indicator"]`);

    if (indicator && this.props.evaluationDigest) {
      const source = this.liveEditors.primary.getSource();
      const digest = md5Base64(source);
      const changed = this.props.evaluationDigest !== digest;
      cellStatus.toggleAttribute("data-js-changed", changed);
    }
  },

  handleInsertModeChanged(insertMode) {
    if (this.isFocused && !this.insertMode && insertMode) {
      this.insertMode = insertMode;

      if (this.currentEditor()) {
        this.currentEditor().focus();

        // The insert mode may be enabled as a result of clicking the editor,
        // in which case we want to wait until editor handles the click and
        // sets new cursor position. To achieve this, we simply put this task
        // at the end of event loop, ensuring the editor mousedown handler is
        // executed first
        setTimeout(() => {
          scrollIntoView(document.activeElement, {
            scrollMode: "if-needed",
            behavior: "smooth",
            block: "center",
          });
        }, 0);

        this.broadcastSelection();
      }
    } else if (this.insertMode && !insertMode) {
      this.insertMode = insertMode;

      if (this.currentEditor()) {
        this.currentEditor().blur();
      }
    }
  },

  handleCellMoved(cellId) {
    if (this.isFocused && cellId === this.props.cellId) {
      smoothlyScrollToElement(this.el);
    }
  },

  handleCellUpload(cellId, url) {
    const liveEditor = this.liveEditors.primary;

    if (!liveEditor) {
      return;
    }

    if (this.props.cellId === cellId) {
      const markdown = `![](${url})`;
      liveEditor.insert(markdown);
    }
  },

  handleLocationReport(client, report) {
    Object.entries(this.liveEditors).forEach(([tag, liveEditor]) => {
      if (
        this.props.cellId === report.focusableId &&
        report.selection &&
        report.selection.tag === tag
      ) {
        liveEditor.updateUserSelection(
          client,
          report.selection.editorSelection
        );
      } else {
        liveEditor.removeUserSelection(client);
      }
    });
  },

  broadcastSelection(editorSelection = null) {
    editorSelection =
      editorSelection || this.currentEditor().editor.getSelection();

    const tag = this.currentEditorTag();

    // Report new selection only if this cell is in insert mode
    if (this.isFocused && this.insertMode) {
      globalPubSub.broadcast("session", {
        type: "cursor_selection_changed",
        focusableId: this.props.cellId,
        selection: { tag, editorSelection },
      });
    }
  },
};

export default Cell;
