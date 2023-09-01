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
    this.dragLabel = this.createDragLabel();
    this.srcColEl = null;

    this.el.addEventListener("dragstart", (event) => {
      let row = getAttributeOrThrow(
        event.srcElement,
        "data-row-index",
        parseInteger
      );
      let col = getAttributeOrThrow(
        event.srcElement,
        "data-col-index",
        parseInteger
      );
      this.srcColEl = document.getElementById(`dropzone-row-${row}-col-${col}`);
      this.srcColEl.classList.add("hidden");
      event.target.parentNode.classList.add("hidden");
      event.dataTransfer.setDragImage(this.dragLabel, 10, 10);

      this.startDragging(event.target);
    });

    this.el.addEventListener("dragend", (event) => {
      this.srcColEl.classList.remove("hidden");
      event.target.parentNode.classList.remove("hidden");
      this.stopDragging();
    });

    this.el.addEventListener("dragover", (event) => {
      event.stopPropagation();
      event.preventDefault();
    });

    this.el.addEventListener("drop", (event) => {
      event.stopPropagation();
      event.preventDefault();

      const dstEl = event.target.closest(`[data-js-droppable]`);

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
  createDragLabel() {
    const elem = document.createElement("div");
    elem.classList.add("w-24", "h-8", "bg-blue-900", "rounded");
    elem.style.position = "fixed";
    elem.style.left = "-1000px";
    elem.style.top = "-1000px";

    // the element must be in the DOM to use it with setDragItem
    document.body.appendChild(elem);
    return elem;
  },
};

export default OutputPanel;
