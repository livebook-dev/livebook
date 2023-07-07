import "gridstack/dist/gridstack.min.css";
import { GridStack } from "gridstack";

/**
 * A hook for creating app dashboard.
 */
const Gridstack = {
  mounted() {
    const options = {
      acceptWidgets: true,
      float: true
    };
    this.grid = GridStack.init(options, ".grid-stack");
    GridStack.setupDragIn("div[data-el-side-panel] .grid-stack-item", { appendTo: "body" });
  },
};

export default Gridstack;
