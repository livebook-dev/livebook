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
    this.isDragging = false;
    this.draggedEl = null;

    this.el.addEventListener("dragstart", (event) => {
      this.startDragging(event.target);
    });

    this.el.addEventListener("dragend", (event) => {
      this.stopDragging();
    });

    this.el.addEventListener("dragenter", (event) => {
      //console.log("Valid drop area", event);
    });

    this.el.addEventListener("dragleave", (event) => {
      //console.log("Valid drop area left", event);
    });

    this.el.addEventListener("dragover", (event) => {
      event.stopPropagation();
      event.preventDefault();
    });

    this.el.addEventListener("drop", (event) => {
      event.stopPropagation();
      event.preventDefault();

      const dstEl = event.target.closest(`[data-js-dropzone]`);
      console.log(dstEl);

      const srcEl = this.draggedEl.closest(`[data-el-output-panel-item]`);

      if (dstEl && srcEl) {
        const cellId = getAttributeOrThrow(srcEl, "data-cell-id");
        const srcRow = getAttributeOrThrow(
          srcEl,
          "data-row-index",
          parseInteger
        );
        const srcCol = getAttributeOrThrow(
          srcEl,
          "data-col-index",
          parseInteger
        );
        let dstRow = getAttributeOrThrow(dstEl, "data-row-index", parseInteger);
        let dstCol = getAttributeOrDefault(
          dstEl,
          "data-col-index",
          null,
          parseInteger
        );

        if (dstCol !== null) {
          console.log("New position");
          console.log("Rows", srcRow, "->", dstRow);
          console.log("Cols", srcCol, "->", dstCol);
          // when dropping on the right side, move element one column to the right
          if (srcRow !== dstRow && event.layerX > dstEl.offsetWidth / 2)
            dstCol += 1;
          if (srcRow === dstRow && srcCol < dstCol) dstCol += 1;

          this.pushEventTo(this.props.phxTarget, "handle_move_item", {
            cell_id: cellId,
            row_index: dstRow,
            col_index: dstCol,
          });
        } else {
          console.log("New row");
          console.log("Rows", srcRow, "->", dstRow);
          console.log("Cols", srcCol, "->", dstCol);
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
      this.stopDragging();
    });
  },
  update() {
    this.props = this.getProps();
  },
  getProps() {
    return {
      phxTarget: getAttributeOrThrow(this.el, "data-phx-target", parseInteger),
    };
  },
  startDragging(element) {
    if (!this.isDragging) {
      this.isDragging = true;
      this.draggedEl = element;

      this.el.setAttribute("data-js-dragging", "");
    }
  },
  stopDragging() {
    if (this.isDragging) {
      this.isDragging = false;
      this.el.removeAttribute("data-js-dragging");
    }
  },
};

export default OutputPanel;
