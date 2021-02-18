import { getAttributeOrThrow, parseBoolean } from "../lib/attribute";
import { isMacOS, isEditableElement } from "../lib/utils";
import KeyBuffer from "./key_buffer";

/**
 * A hook managing the whole session.
 *
 * Registers event listeners to handle keybindings and focus events.
 *
 * Configuration:
 *
 *   * `data-focused-cell-id` - id of the cell currently being focused
 */
const Session = {
  mounted() {
    this.props = getProps(this);

    // Keybindings
    // Note: make sure to keep the shortcuts help modal up to date.

    const keyBuffer = new KeyBuffer();

    this.handleDocumentKeydown = (event) => {
      if (event.repeat) {
        return;
      }

      const cmd = isMacOS() ? event.metaKey : event.ctrlKey;
      const key = event.key;

      if (this.props.insertMode) {
        keyBuffer.reset();

        if (key === "Escape") {
          this.pushEvent("set_insert_mode", { enabled: false });
        } else if (
          this.props.focusedCellType === "elixir" &&
          cmd &&
          key === "Enter"
        ) {
          cancelEvent(event);
          this.pushEvent("queue_focused_cell_evaluation");
        }
      } else {
        if (isEditableElement(event.target)) {
          keyBuffer.reset();
          return;
        }

        keyBuffer.push(event.key);

        if (keyBuffer.tryMatch(["d", "d"])) {
          this.pushEvent("delete_focused_cell", {});
        } else if (
          this.props.focusedCellType === "elixir" &&
          keyBuffer.tryMatch(["e", "e"])
        ) {
          this.pushEvent("queue_focused_cell_evaluation", {});
        } else if (keyBuffer.tryMatch(["e", "s"])) {
          this.pushEvent("queue_section_cells_evaluation", {});
        } else if (keyBuffer.tryMatch(["e", "j"])) {
          this.pushEvent("queue_child_cells_evaluation", {});
        } else if (keyBuffer.tryMatch(["?"])) {
          this.pushEvent("show_shortcuts", {});
        } else if (key === "i") {
          this.pushEvent("set_insert_mode", { enabled: true });
        } else if (key === "j") {
          this.pushEvent("move_cell_focus", { offset: 1 });
        } else if (key === "k") {
          this.pushEvent("move_cell_focus", { offset: -1 });
        } else if (key === "n") {
          this.pushEvent("insert_cell_below_focused", { type: "elixir" });
        } else if (key === "N") {
          this.pushEvent("insert_cell_above_focused", { type: "elixir" });
        } else if (key === "m") {
          this.pushEvent("insert_cell_below_focused", { type: "markdown" });
        } else if (key === "M") {
          this.pushEvent("insert_cell_above_focused", { type: "markdown" });
        }
      }
    };

    document.addEventListener("keydown", this.handleDocumentKeydown, true);

    // Focus/unfocus a cell when the user clicks somewhere
    this.handleDocumentClick = (event) => {
      // Find the parent with cell id info, if there is one
      const cell = event.target.closest("[data-cell-id]");
      const cellId = cell ? cell.dataset.cellId : null;
      if (cellId !== this.props.focusedCellId) {
        this.pushEvent("focus_cell", { cell_id: cellId });
      }

      // Depending if the click targets editor or not disable/enable insert mode.
      if (cell) {
        const editorContainer = cell.querySelector("[data-editor-container]");
        const editorClicked = editorContainer.contains(event.target);
        this.pushEvent("set_insert_mode", { enabled: editorClicked });
      }
    };

    document.addEventListener("click", this.handleDocumentClick);
  },

  updated() {
    this.props = getProps(this);
  },

  destroyed() {
    document.removeEventListener("keydown", this.handleDocumentKeydown);
    document.removeEventListener("click", this.handleDocumentClick);
  },
};

function getProps(hook) {
  return {
    insertMode: getAttributeOrThrow(hook.el, "data-insert-mode", parseBoolean),
    focusedCellId: getAttributeOrThrow(
      hook.el,
      "data-focused-cell-id",
      (value) => value || null
    ),
    focusedCellType: getAttributeOrThrow(hook.el, "data-focused-cell-type"),
  };
}

function cancelEvent(event) {
  // Cancel any default browser behavior.
  event.preventDefault();
  // Stop event propagation (e.g. so it doesn't reach the editor).
  event.stopPropagation();
}

export default Session;
