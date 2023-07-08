import { getAttributeOrThrow, parseInteger } from "../lib/attribute";
import "gridstack/dist/gridstack.min.css";
import { GridStack } from "gridstack";

/**
 * A hook for creating app dashboard.
 */
const Gridstack = {
  mounted() {
    const self = this;
    this.props = this.getProps();

    console.log(this.props);
    const options = {
      acceptWidgets: true,
      styleInHead: true,
      float: true,
    };
    this.grid = GridStack.init(options, this.el);

    this.grid.on("change", function(event, items) {
      let new_items = items.reduce((acc, item) => {
        acc[item.id] = {
          x_pos: item.x,
          y_pos: item.y,
          width: item.w,
          height: item.h
        };
        return acc;
      }, {});
      self.pushEventTo(self.props.phxTarget, "items_changed", new_items);
    });

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
  updated() {
    this.props = this.getProps();
  },
  getProps() {
    return {
      phxTarget: getAttributeOrThrow(this.el, "data-phx-target", parseInteger),
    };
  },
  saveLayout() {
    const layout = this.grid.save();
    console.log("Layout saved");
    console.log(layout);
    this.pushEvent("saved_grid_layout", { layout: layout });
  },
  loadLayout(layout) {
    this.grid.load(layout);
  }
};

export default Gridstack;
