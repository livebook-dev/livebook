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
  },
};

export default OutputPanelItem;
