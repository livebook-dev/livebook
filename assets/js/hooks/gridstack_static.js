import "gridstack/dist/gridstack.min.css";
import { GridStack } from "gridstack";

/**
 * A hook for creating app dashboard.
 */
const GridstackStatic = {
  mounted() {
    const self = this;

    const options = {
      staticGrid: true,
      float: true,
      margin: 0,
      cellHeight: "4rem",
    };

    this.grid = GridStack.init(options, this.el);

    this.handleEvent("load_layout", function({ layout }) {
      if (layout) {
        console.log("layout", layout);
        self.grid.load(layout);
        self.mountOutputs();
      }
    });
  },
  loadLayout(layout) {
  },
  mountOutputs() {
    const outputs = document.querySelectorAll("[data-el-output]");
    for (let output of outputs) {
      const item_content = this.el.querySelector(`[gs-id="${output.parentNode.id}"] .grid-stack-item-content`);
      console.log("output", output);
      console.log("content", item_content);
      if (item_content) {
        item_content.prepend(output);
      }
    }
  }
};

export default GridstackStatic;
