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

    if (this.props.externWindow) {
      this.handleBeforeUnloadEvent = this.handleBeforeUnloadEvent.bind(this);
      window.addEventListener("beforeunload", this.handleBeforeUnloadEvent);
      this.getElement("canvas-close-button").addEventListener(
        "click",
        (event) => this.handleCanvasCloseClick()
      );
      this.getElement("canvas-popin-button").addEventListener(
        "click",
        (event) => this.handleCanvasPopinClick()
      );
    }

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
    this.props = this.getProps();
    console.log("Gridstack updated", this.grid);
  },
  getProps() {
    return {
      externWindow: this.el.hasAttribute("data-el-js-extern-window"),
    };
  },
  repositionIframe() {
    globalPubSub.broadcast("js_views", { type: "reposition" });
  },
  handleBeforeUnloadEvent(event) {
    this.sendToParent("popin");
  },
  handleCanvasCloseClick() {
    window.removeEventListener("beforeunload", this.handleBeforeUnloadEvent);
    this.sendToParent("close");
    window.close();
  },
  handleCanvasPopinClick() {
    window.removeEventListener("beforeunload", this.handleBeforeUnloadEvent);
    this.sendToParent("popin");
    window.close();
  },
  getElement(name) {
    return document.querySelector(`[data-el-${name}]`);
  },
  sendToParent(message) {
    window.opener.postMessage(message, window.location.origin);
  },
};

export default Gridstack;
