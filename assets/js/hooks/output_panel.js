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
    this.srcDropzoneRow = null;
    this.srcDropzoneEl = null;
    this.dragLabel = this.createDragLabel();

    this.el.addEventListener("dragstart", (event) => {
      if (!this.isDragging) {
        this.isDragging = true;
        this.draggedEl = event.target.closest(`[data-el-output-panel-item]`);
        const elRowIndex = getAttributeOrThrow(
          event.target,
          "data-row-index",
          parseInteger
        );
        const elColIndex = getAttributeOrThrow(
          event.target,
          "data-col-index",
          parseInteger
        );
        this.srcDropzoneRow = document.querySelector(
          `[data-el-output-panel-row][data-row-index="${elRowIndex}"]`
        );
        this.srcDropzoneEl = document.getElementById(
          `dropzone-row-${elRowIndex}-col-${elColIndex}`
        );
        this.el.setAttribute("data-js-dragging", "");
        console.log(this.srcDropzoneRow);
        console.log(this.srcDropzoneEl);
        this.srcDropzoneRow.setAttribute("data-js-dragging-source", "");
        this.srcDropzoneEl.setAttribute("data-js-dragging-source", "");

        event.dataTransfer.setDragImage(this.dragLabel, 10, 10);
      }
    });

    this.el.addEventListener("dragend", (event) => {
      this.stopDragging();
    });

    this.el.addEventListener("dragover", (event) => {
      event.stopPropagation();
      event.preventDefault();
    });

    this.el.addEventListener("drop", (event) => {
      event.stopPropagation();
      event.preventDefault();

      const dstEl = event.target.closest(`[phx-hook="OutputPanelDropzone"]`);

      const srcEl = this.draggedEl;

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
      this.stopDragging();
    });
  },
  updated() {
    this.props = this.getProps();
  },
  destroyed() {
    document.body.removeChild(this.dragLabel);
  },
  getProps() {
    return {
      phxTarget: getAttributeOrThrow(this.el, "data-phx-target", parseInteger),
    };
  },
  stopDragging() {
    if (this.isDragging) {
      this.isDragging = false;
      this.el.removeAttribute("data-js-dragging");
      this.srcDropzoneRow.removeAttribute("data-js-dragging-source");
      this.srcDropzoneEl.removeAttribute("data-js-dragging-source");
    }
  },
  createDragLabel() {
    const elem = document.createElement("div");
    elem.classList.add("z-50", "w-24", "h-8", "bg-blue-900", "rounded");
    elem.style.position = "fixed";
    elem.style.left = "-100%";

    // the element must be in the DOM to use it with setDragItem
    document.body.appendChild(elem);
    return elem;
  },
};

export default OutputPanel;
