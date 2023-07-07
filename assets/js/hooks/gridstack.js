import { getAttributeOrDefault, getAttributeOrThrow } from "../lib/attribute";
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
    let parent = this;

    this.grid.on("change", function(event, items) {
      parent.saveLayout();
    });
    this.handleEvent(
      `load_grid`,
      ({ layout: layout }) => {
        if (layout) {
          console.log(layout);
          parent.loadLayout(layout);
        }
      }
    );
  },
  saveLayout() {
    const layout = this.grid.save();
    console.log("Test");
    console.log(layout);
    this.pushEvent("saved_grid_layout", { layout: layout });
  },
  loadLayout(layout) {
    this.grid.load(layout);
  }
};

export default Gridstack;
