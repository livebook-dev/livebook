import { parseHookProps } from "../lib/attribute";
import { cancelEvent, isEditableElement, isMacOS } from "../lib/utils";

/**
 * A hook for ControlComponent to handle user keyboard interactions.
 *
 * ## Props
 *
 *   * `cell-id` - id of the cell in which the control is rendered
 *
 *   * `default-handlers` - whether keyboard events should be
 *     intercepted and canceled, disabling session shortcuts. Must be
 *     one of "off", "on", or "disable_only"
 *
 *   * `keydown-enabled` - whether keydown events should be listened to
 *
 *   * `keyup-enabled` - whether keyup events should be listened to
 *
 *   * `target` - the target to send live events to
 *
 */
const KeyboardControl = {
  mounted() {
    this.props = this.getProps();

    this._handleDocumentKeyDown = this.handleDocumentKeyDown.bind(this);
    this._handleDocumentKeyUp = this.handleDocumentKeyUp.bind(this);
    this._handleDocumentFocus = this.handleDocumentFocus.bind(this);

    // We intentionally register on window rather than document, to
    // intercept events as early on as possible, even before the
    // session shortcuts
    window.addEventListener("keydown", this._handleDocumentKeyDown, true);
    window.addEventListener("keyup", this._handleDocumentKeyUp, true);
    // Note: the focus event doesn't bubble, so we register for the
    // capture phase
    window.addEventListener("focus", this._handleDocumentFocus, true);
  },

  updated() {
    this.props = this.getProps();
  },

  destroyed() {
    window.removeEventListener("keydown", this._handleDocumentKeyDown, true);
    window.removeEventListener("keyup", this._handleDocumentKeyUp, true);
    window.removeEventListener("focus", this._handleDocumentFocus, true);
  },

  getProps() {
    return parseHookProps(this.el, [
      "cell-id",
      "default-handlers",
      "keydown-enabled",
      "keyup-enabled",
      "target",
    ]);
  },

  handleDocumentKeyDown(event) {
    if (
      this.isKeyboardToggle(event) &&
      !isEditableElement(document.activeElement)
    ) {
      cancelEvent(event);
      this.keyboardEnabled() ? this.disableKeyboard() : this.enableKeyboard();
      return;
    }

    if (this.keyboardEnabled()) {
      if (this.props.defaultHandlers !== "on") {
        cancelEvent(event);
      }

      if (event.repeat) {
        return;
      }

      if (this.props.keydownEnabled) {
        const { key } = event;
        this.pushEventTo(this.props.target, "keydown", { key });
      }
    }
  },

  handleDocumentKeyUp(event) {
    if (this.keyboardEnabled()) {
      if (this.props.defaultHandlers !== "on") {
        cancelEvent(event);
      }

      if (this.props.keyupEnabled) {
        const { key } = event;
        this.pushEventTo(this.props.target, "keyup", { key });
      }
    }
  },

  handleDocumentFocus(event) {
    if (this.props.keydownEnabled && isEditableElement(event.target)) {
      this.disableKeyboard();
    }
  },

  enableKeyboard() {
    if (!this.keyboardEnabled()) {
      this.pushEventTo(this.props.target, "enable_keyboard", {});
    }
  },

  disableKeyboard() {
    if (this.keyboardEnabled()) {
      this.pushEventTo(this.props.target, "disable_keyboard", {});
    }
  },

  keyboardEnabled() {
    return this.props.keydownEnabled || this.props.keyupEnabled;
  },

  isKeyboardToggle(event) {
    if (event.repeat) {
      return false;
    }

    const { metaKey, ctrlKey, key } = event;
    const cmd = isMacOS() ? metaKey : ctrlKey;

    if (cmd && key === "k" && this.isCellFocused()) {
      return (
        !this.keyboardEnabled() ||
        ["on", "disable_only"].includes(this.props.defaultHandlers)
      );
    } else {
      return false;
    }
  },

  isCellFocused() {
    const sessionEl = this.el.closest("[data-el-session]");
    return (
      sessionEl &&
      sessionEl.getAttribute("data-js-focused-id") === this.props.cellId
    );
  },
};

export default KeyboardControl;
