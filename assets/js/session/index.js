import { getAttributeOrThrow } from "../lib/attribute";

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
    this.handleDocumentKeydown = (event) => {
      const key = event.key.toLowerCase();
      const shift = event.shiftKey;
      const alt = event.altKey;
      const ctrl = event.ctrlKey;

      if (event.repeat) {
        return;
      }

      if (shift && key === "enter") {
        cancelEvent(event);

        if (this.props.focusedCellType === "elixir") {
          this.pushEvent("queue_focused_cell_evaluation");
        }

        this.pushEvent("move_cell_focus", { offset: 1 });
      } else if (alt && ctrl && key === "enter") {
        cancelEvent(event);

        this.pushEvent("queue_child_cells_evaluation", {});
      } else if (ctrl && key === "enter") {
        cancelEvent(event);

        if (this.props.focusedCellType === "elixir") {
          this.pushEvent("queue_focused_cell_evaluation");
        }

        if (this.props.focusedCellType === "markdown") {
          this.pushEvent("toggle_cell_expanded");
        }
      } else if (alt && key === "j") {
        cancelEvent(event);

        this.pushEvent("move_cell_focus", { offset: 1 });
      } else if (alt && key === "k") {
        cancelEvent(event);

        this.pushEvent("move_cell_focus", { offset: -1 });
      } else if (alt && key === "n") {
        cancelEvent(event);

        if (shift) {
          this.pushEvent("insert_cell_above_focused", { type: "elixir" });
        } else {
          this.pushEvent("insert_cell_below_focused", { type: "elixir" });
        }
      } else if (alt && key === "m") {
        cancelEvent(event);

        if (shift) {
          this.pushEvent("insert_cell_above_focused", { type: "markdown" });
        } else {
          this.pushEvent("insert_cell_below_focused", { type: "markdown" });
        }
      } else if (alt && key === "w") {
        cancelEvent(event);

        this.pushEvent("delete_focused_cell", {}); // TODO: focused:delete_cell ?
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
