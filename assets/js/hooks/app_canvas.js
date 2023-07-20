import "gridstack/dist/gridstack.min.css";
import { GridStack } from "gridstack";

/**
 * A hook for creating app dashboard.
 */
const AppCanvas = {
  mounted() {
    const self = this;

    const options = {
      staticGrid: true,
      float: true,
      margin: 0,
      cellHeight: "4rem",
    };

    this.grid = GridStack.init(options, this.el);

    this.handleEvent("init", ({ payload }) => {
      const grid_items = Object.entries(payload).map(([id, value]) => ({
        id,
        ...value,
      }));
      this.grid.load(grid_items);
      console.log(this.grid.el.children);
      Array.from(this.grid.el.children).forEach((item) => {
        const output_id = `[id^=outputs-${item.id}]`;
        const output_el = document.querySelector(output_id);
        item.firstChild.appendChild(output_el);
      });
    });
  },
};

export default AppCanvas;
