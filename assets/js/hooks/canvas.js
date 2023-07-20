import { getAttributeOrThrow, parseInteger } from "../lib/attribute";
import { globalPubSub } from "../lib/pub_sub";
import "gridstack/dist/gridstack.min.css";
import { GridStack } from "gridstack";

/**
 * A hook for creating a canvas with gridstackjs.
 */
const Canvas = {
  mounted() {
    this.props = this.getProps();
    console.log("Gridstack mounted");
    const self = this;

    const options = {
      //acceptWidgets: true,
      //removeable: true,
      styleInHead: true,
      float: true,
      resizable: { handles: "all" },
      margin: 0,
      cellHeight: "4rem",
      //handle: ".drag-handle",
    };

    this.grid = GridStack.init(options, this.el);

    this.handleEvent("init", ({ payload }) => {
      const grid_items = Object.entries(payload).map(([id, value]) => ({
        id,
        ...value,
      }));
      console.log("LAYOUT", grid_items);
      this.grid.load(grid_items);
    });

    this.grid.on("added", (event, items) => {
      items.forEach((item) => {
        const output_id = `[id^=outputs-${item.id}]`;
        const output_el = document.querySelector(output_id);
        output_el._notebookLocation = output_el.parentNode;
        item.el.firstChild.appendChild(output_el);
      });
      console.log("ADDED", items);
    });

    this.grid.on("change", function(event, items) {
      console.log("ITEMS changed: ", items);
      let new_items = items.reduce((acc, item) => {
        acc[item.id] = {
          x: item.x,
          y: item.y,
          w: item.w,
          h: item.h,
        };
        return acc;
      }, {});
      self.pushEventTo(self.props.phxTarget, "items_changed", new_items);
      //self.repositionIframe();
    });

    this.grid.on("removed", (event, items) => {
      console.log("REMOVED", event);
    });

    this.grid.on("drag", function(event, item) {
      // TODO update iframe position when dragging
      //self.repositionIframe();
    });
  },
  updated() {
    this.props = this.getProps();
    console.log("Gridstack updated", this.grid);
  },
  getProps() {
    return {
      phxTarget: getAttributeOrThrow(this.el, "data-phx-target", parseInteger),
    };
  },
  repositionIframe() {
    globalPubSub.broadcast("js_views", { type: "reposition" });
  },
};

export default Canvas;
