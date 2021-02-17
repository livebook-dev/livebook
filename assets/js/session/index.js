import { getAttributeOrThrow } from "../lib/attribute";
import { isMacOS } from "../lib/utils";

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
    this.handleDocumentKeydown = (event) => {
      const cmd = isMacOS() ? event.metaKey : event.ctrlKey;
      const opt = event.altKey;
      const shift = event.shiftKey;
      // Generally it's good to use event.key for layout-independent checks,
      // but when modifiers such as opt/alt/shift are used then event.key
      // value varies (n, N, ń, Ń) and may depend on the system language.
      // The other option is to use event.code that indicates the physical
      // key pressed. The drawback is that the value is irrespective
      // of the keyboard layout used.
      const code = event.code;

      if (event.repeat) {
        return;
      }

      if (shift && code === "Enter") {
        cancelEvent(event);

        if (this.props.focusedCellType === "elixir") {
          this.pushEvent("queue_focused_cell_evaluation");
        }

        this.pushEvent("move_cell_focus", { offset: 1 });
      } else if (cmd && opt && code === "Enter") {
        cancelEvent(event);

        this.pushEvent("queue_child_cells_evaluation", {});
      } else if (cmd && code === "Enter") {
        cancelEvent(event);

        if (this.props.focusedCellType === "elixir") {
          this.pushEvent("queue_focused_cell_evaluation");
        }

        if (this.props.focusedCellType === "markdown") {
          this.pushEvent("toggle_cell_expanded");
        }
      } else if (cmd && code === "KeyJ") {
        cancelEvent(event);

        this.pushEvent("move_cell_focus", { offset: 1 });
      } else if (cmd && code === "KeyK") {
        cancelEvent(event);

        this.pushEvent("move_cell_focus", { offset: -1 });
      } else if (cmd && opt && code === "KeyN") {
        cancelEvent(event);

        if (shift) {
          this.pushEvent("insert_cell_above_focused", { type: "elixir" });
        } else {
          this.pushEvent("insert_cell_below_focused", { type: "elixir" });
        }
      } else if (cmd && opt && code === "KeyM") {
        cancelEvent(event);

        if (shift) {
          this.pushEvent("insert_cell_above_focused", { type: "markdown" });
        } else {
          this.pushEvent("insert_cell_below_focused", { type: "markdown" });
        }
      } else if (cmd && opt && code === "KeyW") {
        cancelEvent(event);

        this.pushEvent("delete_focused_cell", {});
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
