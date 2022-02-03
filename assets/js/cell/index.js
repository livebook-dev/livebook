import { getAttributeOrThrow } from "../lib/attribute";
import LiveEditor from "./live_editor";
import Markdown from "./markdown";
import { globalPubSub } from "../lib/pub_sub";
import { md5Base64, smoothlyScrollToElement } from "../lib/utils";
import scrollIntoView from "scroll-into-view-if-needed";

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
 */
const Cell = {
  mounted() {
    this.props = getProps(this);
    this.state = {
      isFocused: false,
      insertMode: false,
      // For text cells (markdown or elixir)
      liveEditor: null,
      markdown: null,
      evaluationDigest: null,
    };

    this.handleEvent(`cell_init:${this.props.cellId}`, (payload) => {
      const { source, revision, evaluation_digest } = payload;

      // Setup markdown rendering
      if (this.props.type === "markdown") {
        const markdownContainer = this.el.querySelector(
          `[data-element="markdown-container"]`
        );
        this.state.markdown = new Markdown(markdownContainer, source, {
          baseUrl: this.props.sessionPath,
          emptyText: "Empty markdown cell",
        });
      }

      // Setup action handlers
      if (this.props.type === "elixir") {
        const amplifyButton = this.el.querySelector(
          `[data-element="amplify-outputs-button"]`
        );
        amplifyButton.addEventListener("click", (event) => {
          this.el.toggleAttribute("data-js-amplified");
        });
      }

      // Setting up an editor takes relatively much synchronous time,
      // so we postpone it, so that other immediate initializations
      // are done first. This is relevant if there are a lot of cells
      setTimeout(() => {
        const editorContainer = this.el.querySelector(
          `[data-element="editor-container"]`
        );
        // Remove the content placeholder.
        editorContainer.firstElementChild.remove();
        // Create an empty container for the editor to be mounted in.
        const editorElement = document.createElement("div");
        editorContainer.appendChild(editorElement);
        // Setup the editor instance.
        this.state.liveEditor = new LiveEditor(
          this,
          editorElement,
          this.props.cellId,
          this.props.type,
          source,
          revision
        );

        // Setup change indicator
        if (this.props.type === "elixir") {
          this.state.evaluationDigest = evaluation_digest;

          const updateChangeIndicator = () => {
            const cellStatus = this.el.querySelector(
              `[data-element="cell-status"]`
            );
            const indicator =
              cellStatus &&
              cellStatus.querySelector(`[data-element="change-indicator"]`);

            if (indicator) {
              const source = this.state.liveEditor.getSource();
              const digest = md5Base64(source);
              const changed = this.state.evaluationDigest !== digest;
              cellStatus.toggleAttribute("data-js-changed", changed);
            }
          };

          updateChangeIndicator();

          this.handleEvent(
            `evaluation_started:${this.props.cellId}`,
            ({ evaluation_digest }) => {
              this.state.evaluationDigest = evaluation_digest;
              updateChangeIndicator();
            }
          );

          this.state.liveEditor.onChange((newSource) => {
            updateChangeIndicator();
          });

          this.handleEvent(
            `evaluation_finished:${this.props.cellId}`,
            ({ code_error }) => {
              this.state.liveEditor.setCodeErrorMarker(code_error);
            }
          );
        }

        // Setup markdown updates
        if (this.props.type === "markdown") {
          this.state.liveEditor.onChange((newSource) => {
            this.state.markdown.setContent(newSource);
          });
        }

        // Once the editor is created, reflect the current state.
        if (this.state.isFocused && this.state.insertMode) {
          this.state.liveEditor.focus();
          // If the element is being scrolled to, focus interrupts it,
          // so ensure the scrolling continues.
          smoothlyScrollToElement(this.el);

          broadcastSelection(this);
        }

        this.state.liveEditor.onBlur(() => {
          // Prevent from blurring unless the state changes.
          // For example when we move cell using buttons
          // the editor should keep focus.
          if (this.state.isFocused && this.state.insertMode) {
            this.state.liveEditor.focus();
          }
        });

        this.state.liveEditor.onCursorSelectionChange((selection) => {
          broadcastSelection(this, selection);
        });
      }, 0);
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

  destroyed() {
    this._unsubscribeFromNavigationEvents();
    this._unsubscribeFromCellsEvents();

    if (this.state.liveEditor) {
      this.state.liveEditor.dispose();
    }
  },

  updated() {
    this.props = getProps(this);
  },
};

function getProps(hook) {
  return {
    cellId: getAttributeOrThrow(hook.el, "data-cell-id"),
    type: getAttributeOrThrow(hook.el, "data-type"),
    sessionPath: getAttributeOrThrow(hook.el, "data-session-path"),
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

function handleInsertModeChanged(hook, insertMode) {
  if (hook.state.isFocused && !hook.state.insertMode && insertMode) {
    hook.state.insertMode = insertMode;

    if (hook.state.liveEditor) {
      hook.state.liveEditor.focus();

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

    if (hook.state.liveEditor) {
      hook.state.liveEditor.blur();
    }
  }
}

function handleCellMoved(hook, cellId) {
  if (hook.state.isFocused && cellId === hook.props.cellId) {
    smoothlyScrollToElement(hook.el);
  }
}

function handleCellUpload(hook, cellId, url) {
  if (!hook.state.liveEditor) {
    return;
  }

  if (hook.props.cellId === cellId) {
    const markdown = `![](${url})`;
    hook.state.liveEditor.insert(markdown);
  }
}

function handleLocationReport(hook, client, report) {
  if (!hook.state.liveEditor) {
    return;
  }

  if (hook.props.cellId === report.focusableId && report.selection) {
    hook.state.liveEditor.updateUserSelection(client, report.selection);
  } else {
    hook.state.liveEditor.removeUserSelection(client);
  }
}

function broadcastSelection(hook, selection = null) {
  selection = selection || hook.state.liveEditor.editor.getSelection();

  // Report new selection only if this cell is in insert mode
  if (hook.state.isFocused && hook.state.insertMode) {
    globalPubSub.broadcast("session", {
      type: "cursor_selection_changed",
      focusableId: hook.props.cellId,
      selection,
    });
  }
}

export default Cell;
