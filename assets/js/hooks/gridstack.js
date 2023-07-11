import { getAttributeOrThrow, parseInteger } from "../lib/attribute";
import { globalPubSub } from "../lib/pub_sub";
import "gridstack/dist/gridstack.min.css";
import { GridStack } from "gridstack";

/**
 * A hook for creating app dashboard.
 */
const Gridstack = {
  mounted() {
    this.props = this.getProps();
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

    document
      .querySelector("[data-el-app-info-toggle]")
      .addEventListener("click", function (event) {
        self.visible = !self.visible;
        self.toggleMountOutputs();
      });

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
      self.pushEventTo(self.props.phxTarget, "items_changed", new_items);
      self.repositionIframe();
    });

    this.grid.on("drag", function (event, item) {
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
  toggleMountOutputs() {
    if (this.visible) {
      const outputs = document.querySelectorAll("[data-el-outputs-container]");
      for (let output of outputs) {
        // safe origin location for unmounting
        output._origin || (output._origin = output.parentNode);
        const output_id = output.id.split("-")[1];
        const item_content = this.el.querySelector(
          `[gs-id="${output_id}"] .grid-stack-item-content`
        );
        if (item_content) {
          item_content.prepend(output);
        }
      }
    } else {
      const output_containers = this.el.querySelectorAll(
        `.grid-stack-item .grid-stack-item-content`
      );
      for (let container of output_containers) {
        const output = container.firstChild;
        if (output) {
          output._origin && output._origin.appendChild(output);
        }
      }
    }
    this.repositionIframe();
  },
  repositionIframe() {
    globalPubSub.broadcast("js_views", { type: "reposition" });
  },
};

export default Gridstack;
