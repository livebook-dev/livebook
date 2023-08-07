import { getAttributeOrThrow, parseBoolean } from "../lib/attribute";
import { cancelEvent, isEditableElement, isMacOS } from "../lib/utils";
import { globalPubSub } from "../lib/pub_sub";

/**
 * A hook for ControlComponent to handle user keyboard interactions.
 *
 * ## Configuration
 *
 *   * `data-cell-id` - id of the cell in which the control is rendered
 *
 *   * `data-default-handlers` - whether keyboard events should be
 *     intercepted and canceled, disabling session shortcuts. Must be
 *     one of "off", "on", or "disable_only"
 *
 *   * `data-keydown-enabled` - whether keydown events should be listened to
 *
 *   * `data-keyup-enabled` - whether keyup events should be listened to
 *
 *   * `data-target` - the target to send live events to
 */
const KeyboardControl = {
  mounted() {
    this.props = this.getProps();
    this.cellFocused = false;

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

    this.unsubscribeFromNavigationEvents = globalPubSub.subscribe(
      "navigation",
      (event) => this.handleNavigationEvent(event)
    );
  },

  updated() {
    this.props = this.getProps();
  },

  destroyed() {
    window.removeEventListener("keydown", this._handleDocumentKeyDown, true);
    window.removeEventListener("keyup", this._handleDocumentKeyUp, true);
    window.removeEventListener("focus", this._handleDocumentFocus, true);
    this.unsubscribeFromNavigationEvents();
  },

  getProps() {
    return {
      cellId: getAttributeOrThrow(this.el, "data-cell-id"),
      defaultHandlers: getAttributeOrThrow(this.el, "data-default-handlers"),
      isKeydownEnabled: getAttributeOrThrow(
        this.el,
        "data-keydown-enabled",
        parseBoolean
      ),
      isKeyupEnabled: getAttributeOrThrow(
        this.el,
        "data-keyup-enabled",
        parseBoolean
      ),
      target: getAttributeOrThrow(this.el, "data-target"),
    };
  },

  handleDocumentKeyDown(event) {
    if (this.isKeyboardToggle(event)) {
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

      if (this.props.isKeydownEnabled) {
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

      if (event.repeat) {
        return;
      }

      if (this.props.isKeyupEnabled) {
        const { key } = event;
        this.pushEventTo(this.props.target, "keyup", { key });
      }
    }
  },

  handleDocumentFocus(event) {
    if (this.props.isKeydownEnabled && isEditableElement(event.target)) {
      this.disableKeyboard();
    }
  },

  handleNavigationEvent(event) {
    if (event.type === "element_focused") {
      this.cellFocused = event.focusableId === this.props.cellId;
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
    return this.props.isKeydownEnabled || this.props.isKeyupEnabled;
  },

  isKeyboardToggle({ key, metaKey, ctrlKey }) {
    const cmd = isMacOS() ? metaKey : ctrlKey;

    if (cmd && key === "k" && this.cellFocused) {
      return (
        !this.keyboardEnabled() ||
        (this.keyboardEnabled() && this.props.defaultHandlers !== "off")
      );
    } else {
      return false;
    }
  },
};

export default KeyboardControl;
