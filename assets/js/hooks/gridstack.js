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

    this.options = {
      //acceptWidgets: true,
      //removeable: true,
      styleInHead: true,
      float: true,
      resizable: { handles: "all" },
      margin: 0,
      cellHeight: "4rem",
    };

    this.grid = GridStack.init(this.options, this.el);

    document.querySelector("[data-el-app-info-toggle]").addEventListener("click", function(event) {
      self.visible = !self.visible;
      self.toggleMountOutputs();
    });

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
      self.pushEvent("items_changed", new_items);
      globalPubSub.broadcast("js_views", { type: "reposition" });
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

    this.handleEvent("save_layout", function() {
      self.saveLayout();
    });
  },
  updated() {
    console.log("Gridstack updated");
  },
  toggleMountOutputs() {
    if (this.visible) {
      const outputs = document.querySelectorAll("[data-el-output]");
      console.log(outputs);
      for (let output of outputs) {
        // safe origin location for unmounting
        output._origin || (output._origin = output.parentNode);
        const output_id = Number(output.id.split("-")[2]);
        const item_content = this.el.querySelector(`[gs-id="${output_id}"] .grid-stack-item-content`);
        if (item_content) {
          item_content.prepend(output);
        }
      }
    } else {
      const output_containers = this.el.querySelectorAll(`.grid-stack-item .grid-stack-item-content`);
      for (let container of output_containers) {
        const output = container.firstChild;
        if (output) {
          output._origin && output._origin.prepend(output);
        }
      }
    }
    globalPubSub.broadcast("js_views", { type: "reposition" });
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
