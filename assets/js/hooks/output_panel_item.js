const DRAGGING_ATTR = "data-js-dragging";

/**
 * A hook for an output item in the Output Panel.
 */
const OutputPanelItem = {
  mounted() {
    // since the :hover pseudo-class in CSS is not directly triggerable by JavaScript,
    // we need this to show options menu for iframes on hover.
    this.el.addEventListener("mouseenter", (event) => {
      const optionsEl = this.el.querySelector(
        "[data-el-output-panel-item-options]"
      );
      optionsEl && optionsEl.classList.remove("hidden");
    });

    this.el.addEventListener("mouseleave", (event) => {
      const optionsEl = this.el.querySelector(
        "[data-el-output-panel-item-options]"
      );
      optionsEl && optionsEl.classList.add("hidden");
    });

    this.el.addEventListener("dragenter", (event) => {
      this.el.setAttribute(DRAGGING_ATTR, "");
    });

    this.el.addEventListener("dragleave", (event) => {
      if (!this.el.contains(event.relatedTarget)) {
        this.el.removeAttribute(DRAGGING_ATTR);
      }
    });

    this.el.addEventListener("drop", (event) => {
      this.el.removeAttribute(DRAGGING_ATTR);
    });
  },
};

export default OutputPanelItem;
