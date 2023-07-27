import {
  getAttributeOrThrow,
  getAttributeOrDefault,
  parseInteger,
} from "../lib/attribute";
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

      const dstEl = event.target.closest(`[phx-hook="Dropzone"]`);

      const srcEl = draggedEl.closest(`[data-el-output-panel-item]`);

      if (dstEl && srcEl) {
        const cellId = getAttributeOrThrow(srcEl, "data-cell-id");
        let dstRow = getAttributeOrThrow(dstEl, "data-row-index", parseInteger);
        let dstCol = getAttributeOrDefault(
          dstEl,
          "data-col-index",
          null,
          parseInteger
        );

        if (dstCol !== null) {
          // when dropping on the right side, move element one column to the right
          if (event.layerX > dstEl.offsetWidth / 2) dstCol += 1;

          this.pushEventTo(this.props.phxTarget, "handle_move_item", {
            cell_id: cellId,
            row_index: dstRow,
            col_index: dstCol,
          });
        } else {
          this.pushEventTo(
            this.props.phxTarget,
            "handle_move_item_to_new_row",
            {
              cell_id: cellId,
              row_index: dstRow,
            }
          );
        }
      }

      stopDragging();
    });
  },
};

export default OutputPanel;
