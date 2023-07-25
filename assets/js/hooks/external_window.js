import { getAttributeOrDefault, parseBoolean } from "../lib/attribute";
/**
 * A hook for external windows.
 */
const ExternalWindow = {
  mounted() {
    this.props = this.getProps();

    if (!this.props.isWindowEmbedded) {
      this.handleBeforeUnloadEvent = this.handleBeforeUnloadEvent.bind(this);
      window.addEventListener("beforeunload", this.handleBeforeUnloadEvent);
      this.getElement("external-window-close-button").addEventListener(
        "click",
        (event) => this.handleExternalWindowCloseClick()
      );
      this.getElement("external-window-popin-button").addEventListener(
        "click",
        (event) => this.handleExternalWindowPopinClick()
      );
    }
  },
  updated() {
    this.props = this.getProps();
  },
  getProps() {
    return {
      isWindowEmbedded: this.el.hasAttribute("data-window-embedded"),
    };
  },
  handleBeforeUnloadEvent(event) {
    this.sendToParent("external_window_popin_clicked");
  },
  handleExternalWindowCloseClick() {
    window.removeEventListener("beforeunload", this.handleBeforeUnloadEvent);
    this.sendToParent("external_window_close_clicked");
  },
  handleExternalWindowPopinClick() {
    window.removeEventListener("beforeunload", this.handleBeforeUnloadEvent);
    this.sendToParent("external_window_popin_clicked");
  },
  getElement(name) {
    return document.querySelector(`[data-el-${name}]`);
  },
  sendToParent(message) {
    window.opener.postMessage(message, window.location.origin);
  },
};

export default ExternalWindow;
