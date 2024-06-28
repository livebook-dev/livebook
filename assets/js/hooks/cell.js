import { parseHookProps } from "../lib/attribute";
import Markdown from "../lib/markdown";
import { globalPubsub } from "../lib/pubsub";
import {
  md5Base64,
  smoothlyScrollToElement,
  waitUntilInViewport,
} from "../lib/utils";
import scrollIntoView from "scroll-into-view-if-needed";
import { isEvaluable } from "../lib/notebook";

/**
 * A hook managing a single cell.
 *
 * Manages the collaborative editor, takes care of markdown rendering
 * and focusing the editor when applicable.
 *
 * ## Props
 *
 *   * `cell-id` - id of the cell being edited
 *
 *   * `type` - type of the cell
 *
 *   * `session-path` - root path to the current session
 *
 *   * `evaluation-digest` - digest of the last evaluated cell source
 *
 *   * `smart-cell-js-view-ref` - ref for the JS View, applicable
 *     only to Smart cells
 *
 *   * `allowed-uri-schemes` - a list of additional URI schemes that
 *     should be kept during sanitization. Applicable only to Markdown
 *     cells
 *
 */
const Cell = {
  mounted() {
    this.props = this.getProps();

    this.isFocused = false;
    this.insertMode = false;
    this.liveEditors = {};

    this.updateInsertModeAvailability();

    // Setup action handlers

    if (["code", "smart"].includes(this.props.type)) {
      const amplifyButton = this.el.querySelector(
        `[data-el-amplify-outputs-button]`,
      );
      amplifyButton.addEventListener("click", (event) => {
        this.el.toggleAttribute("data-js-amplified");
      });
    }

    if (this.props.type === "smart") {
      const toggleSourceButton = this.el.querySelector(
        `[data-el-toggle-source-button]`,
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

    this.subscriptions = [
      globalPubsub.subscribe(
        "navigation",
        this.handleNavigationEvent.bind(this),
      ),
      globalPubsub.subscribe("cells", this.handleCellsEvent.bind(this)),
      globalPubsub.subscribe(
        `cells:${this.props.cellId}`,
        this.handleCellEvent.bind(this),
      ),
    ];

    // DOM events

    this._handleViewportResize = this.handleViewportResize.bind(this);
    window.visualViewport.addEventListener(
      "resize",
      this._handleViewportResize,
    );
  },

  disconnected() {
    // Reinitialize on reconnection
    this.el.removeAttribute("id");
  },

  destroyed() {
    this.subscriptions.forEach((subscription) => subscription.destroy());

    window.visualViewport.removeEventListener(
      "resize",
      this._handleViewportResize,
    );
  },

  updated() {
    const prevProps = this.props;
    this.props = this.getProps();

    if (this.props.evaluationDigest !== prevProps.evaluationDigest) {
      this.updateChangeIndicator();
    }
  },

  getProps() {
    return parseHookProps(this.el, [
      "cell-id",
      "type",
      "session-path",
      "evaluation-digest",
      "smart-cell-js-view-ref",
      "allowed-uri-schemes",
    ]);
  },

  handleNavigationEvent(event) {
    if (event.type === "element_focused") {
      this.handleElementFocused(event.focusableId, event.scroll);
    } else if (event.type === "insert_mode_changed") {
      this.handleInsertModeChanged(event.enabled);
    }
  },

  handleCellsEvent(event) {
    if (event.type === "cell_moved") {
      this.handleCellMoved(event.cellId);
    }
  },

  handleCellEvent(event) {
    if (event.type === "dispatch_queue_evaluation") {
      this.handleDispatchQueueEvaluation(event.dispatch);
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
      this.maybeFocusCurrentEditor();
    }

    liveEditor.onBlur(() => {
      // We defer the check to happen after all focus/click events have
      // been processed, in case the state changes as a result
      setTimeout(() => {
        // Prevent from blurring unless the state changes. For example
        // when we move cell using buttons the editor should keep focus
        if (this.isFocused && this.insertMode) {
          this.currentEditor().focus();
        }
      }, 0);
    });

    liveEditor.onFocus(() => {
      // We defer the check to happen after all focus/click events have
      // been processed, in case the state changes as a result
      setTimeout(() => {
        // Prevent from focusing unless the state changes. The editor
        // uses a contenteditable element, and it may accidentally get
        // focus. In Chrome clicking on the right-side of the editor
        // gives it focus
        if (!this.isFocused || !this.insertMode) {
          this.currentEditor().blur();
        }
      }, 0);
    });

    if (tag === "primary") {
      const source = liveEditor.getSource();

      this.el.toggleAttribute("data-js-empty", source === "");

      liveEditor.onChange((newSource) => {
        this.el.toggleAttribute("data-js-empty", newSource === "");
      });

      // Setup markdown rendering
      if (this.props.type === "markdown") {
        const markdownContainer = this.el.querySelector(
          `[data-el-markdown-container]`,
        );
        const markdown = new Markdown(markdownContainer, source, {
          baseUrl: this.props.sessionPath,
          emptyText: "Empty markdown cell",
          allowedUriSchemes: this.props.allowedUriSchemes,
        });

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
          ({ code_markers }) => {
            liveEditor.setCodeMarkers(code_markers);
          },
        );

        this.handleEvent(`start_evaluation:${this.props.cellId}`, () => {
          liveEditor.clearDoctests();
        });

        this.handleEvent(
          `doctest_report:${this.props.cellId}`,
          (doctestReport) => {
            liveEditor.updateDoctests([doctestReport]);
          },
        );

        this.handleEvent(`erase_outputs`, () => {
          liveEditor.setCodeMarkers([]);
          liveEditor.clearDoctests();
        });
      }
    }
  },

  handleCellEditorRemoved(tag) {
    delete this.liveEditors[tag];
  },

  handleViewportResize() {
    if (this.isFocused) {
      this.scrollEditorCursorIntoViewIfNeeded();
    }
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
      !this.currentEditor(),
    );
  },

  maybeFocusCurrentEditor() {
    if (this.isFocused && this.insertMode) {
      this.currentEditor().focus();
    }
  },

  updateChangeIndicator() {
    const cellStatus = this.el.querySelector(`[data-el-cell-status]`);
    const indicator =
      cellStatus && cellStatus.querySelector(`[data-el-change-indicator]`);

    if (indicator && this.props.evaluationDigest) {
      const source = this.liveEditors.primary.getSource();
      const digest = md5Base64(source);
      const changed = this.props.evaluationDigest !== digest;
      this.el.toggleAttribute("data-js-changed", changed);
    }
  },

  handleInsertModeChanged(insertMode) {
    if (this.isFocused && !this.insertMode && insertMode) {
      this.insertMode = insertMode;

      if (this.currentEditor()) {
        this.currentEditor().focus();
        this.scrollEditorCursorIntoViewIfNeeded();
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

  handleDispatchQueueEvaluation(dispatch) {
    if (this.props.type === "smart" && this.props.smartCellJsViewRef) {
      // Ensure the smart cell UI is reflected on the server, before the evaluation
      globalPubsub.broadcast(`js_views:${this.props.smartCellJsViewRef}`, {
        type: "sync",
        callback: dispatch,
      });
    } else {
      dispatch();
    }
  },

  scrollEditorCursorIntoViewIfNeeded() {
    const element = this.currentEditor().getElementAtCursor();

    scrollIntoView(element, {
      scrollMode: "if-needed",
      behavior: "smooth",
      block: "center",
    });
  },
};

export default Cell;
