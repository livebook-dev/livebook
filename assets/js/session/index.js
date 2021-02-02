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
      if (event.shiftKey && event.key === "Enter" && !event.repeat) {
        if (this.props.focusedCellId !== null) {
          // If the editor is focused we don't want it to receive the input
          event.preventDefault();
          this.pushEvent("toggle_cell_expanded", {});
        }
      } else if (event.altKey && event.key === "j") {
        event.preventDefault();
        this.pushEvent("move_cell_focus", { offset: 1 });
      } else if (event.altKey && event.key === "k") {
        event.preventDefault();
        this.pushEvent("move_cell_focus", { offset: -1 });
      } else if (event.ctrlKey && event.key === "Enter") {
        event.stopPropagation();
        this.pushEvent("queue_cell_evaluation", {});
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
  }
};

function getProps(hook) {
  return {
    focusedCellId: getAttributeOrThrow(
      hook.el,
      "data-focused-cell-id",
      (value) => value || null
    ),
  };
}

export default Session;
