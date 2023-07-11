import { getAttributeOrThrow, parseInteger } from "../lib/attribute";
import { globalPubSub } from "../lib/pub_sub";
import "gridstack/dist/gridstack.min.css";
import { GridStack } from "gridstack";

/**
 * A hook for creating app dashboard.
 */
const Gridstack = {
  mounted() {
    console.log("Gridstack mounted");
    const self = this;

    this.visible = false;

    const options = {
      //acceptWidgets: true,
      //removeable: true,
      styleInHead: true,
      float: true,
      resizable: { handles: "all" },
      margin: 0,
      cellHeight: "4rem",
    };

    this.grid = GridStack.init(options, this.el);

    this.grid.on("change", function (event, items) {
      console.log(items);
      let new_items = items.reduce((acc, item) => {
        acc[item.id] = {
          x: item.x,
          y: item.y,
          w: item.w,
          h: item.h,
        };
        return acc;
      }, {});
      self.pushEvent("items_changed", new_items);
      self.repositionIframe();
    });

    this.grid.on("drag", function (event, item) {
      // TODO update iframe position when dragging
      //self.repositionIframe();
    });
  },
  updated() {
    console.log("Gridstack updated", this.grid);
  },
  repositionIframe() {
    globalPubSub.broadcast("js_views", { type: "reposition" });
  },
};

export default Gridstack;
