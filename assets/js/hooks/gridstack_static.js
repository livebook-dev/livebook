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

    this.handleEvent(
      `load_grid`,
      ({ layout: layout }) => {
        if (layout) {
          console.log(layout);
          self.loadLayout(layout);
        }
      }
    );
  },
  loadLayout(layout) {
    this.grid.load(layout);
  }
};

export default GridstackStatic;
