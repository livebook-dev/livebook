/**
 * A hook for popped out windows.
 */
const PopoutWindow = {
  mounted() {
    this.props = this.getProps();
    console.log("PopoutWindow mounted");
    const self = this;

    this.handleBeforeUnloadEvent = this.handleBeforeUnloadEvent.bind(this);
    window.addEventListener("beforeunload", this.handleBeforeUnloadEvent);
    this.getElement("canvas-close-button").addEventListener("click", (event) =>
      this.handleCanvasCloseClick()
    );
    this.getElement("canvas-popin-button").addEventListener("click", (event) =>
      this.handleCanvasPopinClick()
    );
  },
  updated() {
    this.props = this.getProps();
    console.log("PopoutWindow updated");
  },
  getProps() {
    return {};
  },
  handleBeforeUnloadEvent(event) {
    this.sendToParent("canvas_popin_clicked");
  },
  handleCanvasCloseClick() {
    window.removeEventListener("beforeunload", this.handleBeforeUnloadEvent);
    this.sendToParent("canvas_close_clicked");
    window.close();
  },
  handleCanvasPopinClick() {
    window.removeEventListener("beforeunload", this.handleBeforeUnloadEvent);
    this.sendToParent("canvas_popin_clicked");
    window.close();
  },
  getElement(name) {
    return document.querySelector(`[data-el-${name}]`);
  },
  sendToParent(message) {
    window.opener.postMessage(message, window.location.origin);
  },
};

export default PopoutWindow;
