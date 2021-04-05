import { getAttributeOrThrow } from "../lib/attribute";
import LiveEditor from "./live_editor";
import Markdown from "./markdown";
import { globalPubSub } from "../lib/pub_sub";
import { smoothlyScrollToElement } from "../lib/utils";
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
 *   * `data-type` - editor type (i.e. language), either "markdown" or "elixir" is expected
 */
const Cell = {
  mounted() {
    this.props = getProps(this);
    this.state = {
      liveEditor: null,
      isFocused: false,
      insertMode: false,
    };

    this.pushEvent("cell_init", { cell_id: this.props.cellId }, (payload) => {
      const { source, revision } = payload;

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

      // Setup markdown rendering.
      if (this.props.type === "markdown") {
        const markdownContainer = this.el.querySelector(
          `[data-element="markdown-container"]`
        );
        const baseUrl = this.props.sessionPath;
        const markdown = new Markdown(markdownContainer, source, baseUrl);

        this.state.liveEditor.onChange((newSource) => {
          markdown.setContent(newSource);
        });
      }

      // Once the editor is created, reflect the current state.
      if (this.state.isFocused && this.state.insertMode) {
        this.state.liveEditor.focus();
        // If the element is being scrolled to, focus interrupts it,
        // so ensure the scrolling continues.
        smoothlyScrollToElement(this.el);
      }

      this.state.liveEditor.onBlur(() => {
        // Prevent from blurring unless the state changes.
        // For example when we move cell using buttons
        // the editor should keep focus.
        if (this.state.isFocused && this.state.insertMode) {
          this.state.liveEditor.focus();
        }
      });
    });

    this.handleSessionEvent = (event) => handleSessionEvent(this, event);
    globalPubSub.subscribe("session", this.handleSessionEvent);
  },

  destroyed() {
    globalPubSub.unsubscribe("session", this.handleSessionEvent);
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
 * Handles client-side session event.
 */
function handleSessionEvent(hook, event) {
  if (event.type === "cell_focused") {
    handleCellFocused(hook, event.cellId);
  } else if (event.type === "insert_mode_changed") {
    handleInsertModeChanged(hook, event.enabled);
  } else if (event.type === "cell_moved") {
    handleCellMoved(hook, event.cellId);
  } else if (event.type === "cell_upload") {
    handleCellUpload(hook, event.cellId, event.url);
  }
}

function handleCellFocused(hook, cellId) {
  if (hook.props.cellId === cellId) {
    hook.state.isFocused = true;
    hook.el.setAttribute("data-js-focused", "true");
    smoothlyScrollToElement(hook.el);
  } else if (hook.state.isFocused) {
    hook.state.isFocused = false;
    hook.el.removeAttribute("data-js-focused");
  }
}

function handleInsertModeChanged(hook, insertMode) {
  if (hook.state.isFocused) {
    hook.state.insertMode = insertMode;

    if (hook.state.liveEditor) {
      if (hook.state.insertMode) {
        hook.state.liveEditor.focus();
        // The insert mode may be enabled as a result of clicking the editor,
        // in which case we want to wait until editor handles the click
        // and sets new cursor position.
        // To achieve this, we simply put this task at the end of event loop,
        // ensuring all click handlers are executed first.
        setTimeout(() => {
          scrollIntoView(document.activeElement, {
            scrollMode: "if-needed",
            behavior: "smooth",
            block: "center",
          });
        }, 0);
      } else {
        hook.state.liveEditor.blur();
      }
    }
  }
}

function handleCellMoved(hook, cellId) {
  if (hook.state.isFocused && cellId === hook.props.cellId) {
    smoothlyScrollToElement(hook.el);
  }
}

function handleCellUpload(hook, cellId, url) {
  if (hook.props.cellId === cellId) {
    const markdown = `![](${url})`;
    hook.state.liveEditor.insert(markdown);
  }
}

export default Cell;
