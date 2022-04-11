import { getAttributeOrThrow, parseBoolean } from "../lib/attribute";
import { cancelEvent, isEditableElement } from "../lib/utils";

/**
 * A hook for ControlComponent to handle user keyboard interactions.
 *
 * ## Configuration
 *
 *   * `data-keydown-enabled` - whether keydown events should be intercepted
 *
 *   * `data-keyup-enabled` - whether keyup events should be intercepted
 *
 *   * `data-target` - the target to send live events to
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
    return {
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
    if (this.keyboardEnabled()) {
      cancelEvent(event);
    }

    if (this.props.isKeydownEnabled) {
      if (event.repeat) {
        return;
      }

      const { key } = event;
      this.pushEventTo(this.props.target, "keydown", { key });
    }
  },

  handleDocumentKeyUp(event) {
    if (this.keyboardEnabled()) {
      cancelEvent(event);
    }

    if (this.props.isKeyupEnabled) {
      const { key } = event;
      this.pushEventTo(this.props.target, "keyup", { key });
    }
  },

  handleDocumentFocus(event) {
    if (this.props.isKeydownEnabled && isEditableElement(event.target)) {
      this.pushEventTo(this.props.target, "disable_keyboard", {});
    }
  },

  keyboardEnabled() {
    return this.props.isKeydownEnabled || this.props.isKeyupEnabled;
  },
};

export default KeyboardControl;
