import { getAttributeOrThrow, parseInteger } from "../lib/attribute";
/**
 * A hook for the output panel.
 */

const OutputPanel = {
  mounted() {
    this.props = this.getProps();
    this.initializeDragAndDrop();
  },
  update() {
    this.props = this.getProps();
  },
  getProps() {
    return {
      phxTarget: getAttributeOrThrow(this.el, "data-phx-target", parseInteger),
    };
  },

  /**
   * Initializes drag and drop event handlers.
   */
  initializeDragAndDrop() {
    let isDragging = false;
    let draggedEl = null;

    const startDragging = (element = null) => {
      if (!isDragging) {
        isDragging = true;
        draggedEl = element;

        this.el.setAttribute("data-js-dragging", "");
      }
    };

    const stopDragging = () => {
      if (isDragging) {
        isDragging = false;
        this.el.removeAttribute("data-js-dragging");
      }
    };

    this.el.addEventListener("dragstart", (event) => {
      startDragging(event.target);
    });

    this.el.addEventListener("dragenter", (event) => {
      startDragging();
    });

    this.el.addEventListener("dragleave", (event) => {
      if (!this.el.contains(event.relatedTarget)) {
        stopDragging();
      }
    });

    this.el.addEventListener("dragover", (event) => {
      event.stopPropagation();
      event.preventDefault();
    });

    this.el.addEventListener("drop", (event) => {
      event.stopPropagation();
      event.preventDefault();

      const insertDropEl = event.target.closest(
        `[data-el-output-panel-row-drop-area]`
      );

      if (insertDropEl && draggedEl) {
        console.log(draggedEl);
        const cellId = getAttributeOrThrow(draggedEl, "data-cell-id");
        const dstRow = getAttributeOrThrow(
          insertDropEl,
          "data-drop-area-row-index",
          parseInteger
        );

        this.pushEventTo(this.props.phxTarget, "handle_move_item_to_new_row", {
          cell_id: cellId,
          row_num: dstRow,
        });
      }
      console.log(event);

      stopDragging();
    });

    this.handleEvent("finish_file_drop", (event) => {
      const inputEl = document.querySelector(
        `#add-file-entry-modal input[type="file"]`
      );

      if (inputEl) {
        inputEl.files = files;
        inputEl.dispatchEvent(new Event("change", { bubbles: true }));
      }
    });
  },
};

export default OutputPanel;
