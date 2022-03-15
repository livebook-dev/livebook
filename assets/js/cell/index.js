import { getAttributeOrDefault, getAttributeOrThrow } from "../lib/attribute";
import Markdown from "../lib/markdown";
import { globalPubSub } from "../lib/pub_sub";
import { md5Base64, smoothlyScrollToElement } from "../lib/utils";
import scrollIntoView from "scroll-into-view-if-needed";
import { isEvaluable } from "../lib/notebook";

/**
 * A hook managing a single cell.
 *
 * Mounts and manages the collaborative editor,
 * takes care of markdown rendering and focusing the editor when applicable.
 *
 * Configuration:
 *
 *   * `data-cell-id` - id of the cell being edited
 *   * `data-type` - type of the cell
 *   * `data-session-path` - root path to the current session
 *   * `data-evaluation-digest` - digest of the last evaluated cell source
 */
const Cell = {
  mounted() {
    this.props = getProps(this);
    this.state = {
      isFocused: false,
      insertMode: false,
      liveEditors: {},
    };

    updateInsertModeAvailability(this);

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
        updateInsertModeAvailability(this);
        maybeFocusCurrentEditor(this);
      });
    }

    // Setup listeners

    this.el.addEventListener("lb:cell:editor_created", (event) => {
      const { tag, liveEditor } = event.detail;
      handleCellEditorCreated(this, tag, liveEditor);
    });

    this.el.addEventListener("lb:cell:editor_removed", (event) => {
      const { tag } = event.detail;
      handleCellEditorRemoved(this, tag);
    });

    // We manually track hover to correctly handle absolute iframe

    this.el.addEventListener("mouseenter", (event) => {
      this.el.setAttribute("data-js-hover", "true");
    });

    this.el.addEventListener("mouseleave", (event) => {
      this.el.removeAttribute("data-js-hover");
    });

    this._unsubscribeFromNavigationEvents = globalPubSub.subscribe(
      "navigation",
      (event) => {
        handleNavigationEvent(this, event);
      }
    );

    this._unsubscribeFromCellsEvents = globalPubSub.subscribe(
      "cells",
      (event) => {
        handleCellsEvent(this, event);
      }
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
    this._unsubscribeFromNavigationEvents();
    this._unsubscribeFromCellsEvents();
  },

  updated() {
    const prevProps = this.props;
    this.props = getProps(this);

    if (this.props.evaluationDigest !== prevProps.evaluationDigest) {
      updateChangeIndicator(this);
    }
  },
};

function getProps(hook) {
  return {
    cellId: getAttributeOrThrow(hook.el, "data-cell-id"),
    type: getAttributeOrThrow(hook.el, "data-type"),
    sessionPath: getAttributeOrThrow(hook.el, "data-session-path"),
    evaluationDigest: getAttributeOrDefault(
      hook.el,
      "data-evaluation-digest",
      null
    ),
  };
}

/**
 * Handles client-side navigation event.
 */
function handleNavigationEvent(hook, event) {
  if (event.type === "element_focused") {
    handleElementFocused(hook, event.focusableId, event.scroll);
  } else if (event.type === "insert_mode_changed") {
    handleInsertModeChanged(hook, event.enabled);
  } else if (event.type === "location_report") {
    handleLocationReport(hook, event.client, event.report);
  }
}

/**
 * Handles client-side cells event.
 */
function handleCellsEvent(hook, event) {
  if (event.type === "cell_moved") {
    handleCellMoved(hook, event.cellId);
  } else if (event.type === "cell_upload") {
    handleCellUpload(hook, event.cellId, event.url);
  }
}

function handleElementFocused(hook, focusableId, scroll) {
  if (hook.props.cellId === focusableId) {
    hook.state.isFocused = true;
    hook.el.setAttribute("data-js-focused", "true");
    if (scroll) {
      smoothlyScrollToElement(hook.el);
    }
  } else if (hook.state.isFocused) {
    hook.state.isFocused = false;
    hook.el.removeAttribute("data-js-focused");
  }
}

function handleCellEditorCreated(hook, tag, liveEditor) {
  hook.state.liveEditors[tag] = liveEditor;

  updateInsertModeAvailability(hook);

  if (liveEditor === currentEditor(hook)) {
    // Once the editor is created, reflect the current insert mode state
    maybeFocusCurrentEditor(hook, true);
  }

  liveEditor.onBlur(() => {
    // Prevent from blurring unless the state changes. For example
    // when we move cell using buttons the editor should keep focus
    if (hook.state.isFocused && hook.state.insertMode) {
      currentEditor(hook).focus();
    }
  });

  liveEditor.onCursorSelectionChange((selection) => {
    broadcastSelection(hook, selection);
  });

  if (tag === "primary") {
    // Setup markdown rendering
    if (hook.props.type === "markdown") {
      const markdownContainer = hook.el.querySelector(
        `[data-element="markdown-container"]`
      );
      const markdown = new Markdown(markdownContainer, liveEditor.getSource(), {
        baseUrl: hook.props.sessionPath,
        emptyText: "Empty markdown cell",
      });

      liveEditor.onChange((newSource) => {
        markdown.setContent(newSource);
      });
    }

    // Setup change indicator
    if (isEvaluable(hook.props.type)) {
      updateChangeIndicator(hook);

      liveEditor.onChange((newSource) => {
        updateChangeIndicator(hook);
      });

      hook.handleEvent(
        `evaluation_finished:${hook.props.cellId}`,
        ({ code_error }) => {
          liveEditor.setCodeErrorMarker(code_error);
        }
      );
    }
  }
}

function handleCellEditorRemoved(hook, tag) {
  delete hook.state.liveEditors[tag];
}

function currentEditor(hook) {
  return hook.state.liveEditors[currentEditorTag(hook)];
}

function currentEditorTag(hook) {
  if (hook.props.type === "smart") {
    const isSourceTab = hook.el.hasAttribute("data-js-source-visible");
    return isSourceTab ? "primary" : "secondary";
  }

  return "primary";
}

function updateInsertModeAvailability(hook) {
  hook.el.toggleAttribute("data-js-insert-mode-disabled", !currentEditor(hook));
}

function maybeFocusCurrentEditor(hook, scroll = false) {
  if (hook.state.isFocused && hook.state.insertMode) {
    currentEditor(hook).focus();

    if (scroll) {
      // If the element is being scrolled to, focus interrupts it,
      // so ensure the scrolling continues.
      smoothlyScrollToElement(hook.el);
    }

    broadcastSelection(hook);
  }
}

function updateChangeIndicator(hook) {
  const cellStatus = hook.el.querySelector(`[data-element="cell-status"]`);
  const indicator =
    cellStatus && cellStatus.querySelector(`[data-element="change-indicator"]`);

  if (indicator && hook.props.evaluationDigest) {
    const source = hook.state.liveEditors.primary.getSource();
    const digest = md5Base64(source);
    const changed = hook.props.evaluationDigest !== digest;
    cellStatus.toggleAttribute("data-js-changed", changed);
  }
}

function handleInsertModeChanged(hook, insertMode) {
  if (hook.state.isFocused && !hook.state.insertMode && insertMode) {
    hook.state.insertMode = insertMode;

    if (currentEditor(hook)) {
      currentEditor(hook).focus();

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

      broadcastSelection(hook);
    }
  } else if (hook.state.insertMode && !insertMode) {
    hook.state.insertMode = insertMode;

    if (currentEditor(hook)) {
      currentEditor(hook).blur();
    }
  }
}

function handleCellMoved(hook, cellId) {
  if (hook.state.isFocused && cellId === hook.props.cellId) {
    smoothlyScrollToElement(hook.el);
  }
}

function handleCellUpload(hook, cellId, url) {
  const liveEditor = hook.state.liveEditors.primary;

  if (!liveEditor) {
    return;
  }

  if (hook.props.cellId === cellId) {
    const markdown = `![](${url})`;
    liveEditor.insert(markdown);
  }
}

function handleLocationReport(hook, client, report) {
  Object.entries(hook.state.liveEditors).forEach(([tag, liveEditor]) => {
    if (
      hook.props.cellId === report.focusableId &&
      report.selection &&
      report.selection.tag === tag
    ) {
      liveEditor.updateUserSelection(client, report.selection.editorSelection);
    } else {
      liveEditor.removeUserSelection(client);
    }
  });
}

function broadcastSelection(hook, editorSelection = null) {
  editorSelection =
    editorSelection || currentEditor(hook).editor.getSelection();

  const tag = currentEditorTag(hook);

  // Report new selection only if this cell is in insert mode
  if (hook.state.isFocused && hook.state.insertMode) {
    globalPubSub.broadcast("session", {
      type: "cursor_selection_changed",
      focusableId: hook.props.cellId,
      selection: { tag, editorSelection },
    });
  }
}

export default Cell;
