import { getAttributeOrThrow, parseBoolean } from "../lib/attribute";
import { cancelEvent, isEditableElement } from "../lib/utils";

/**
 * A hook for ControlComponent to handle user keyboard interactions.
 *
 * Configuration:
 *
 *   * `data-keydown-enabled` - whether keydown events should be intercepted
 *
 *   * `data-keyup-enabled` - whether keyup events should be intercepted
 *
 *   * `data-target` - the target to send live events to
 */
const KeyboardControl = {
  mounted() {
    this.props = getProps(this);

    this.handleDocumentKeyDown = (event) => {
      handleDocumentKeyDown(this, event);
    };

    // We intentionally register on window rather than document,
    // to intercept clicks as early on as possible, even before
    // the session shortcuts
    window.addEventListener("keydown", this.handleDocumentKeyDown, true);

    this.handleDocumentKeyUp = (event) => {
      handleDocumentKeyUp(this, event);
    };

    window.addEventListener("keyup", this.handleDocumentKeyUp, true);

    this.handleDocumentFocus = (event) => {
      handleDocumentFocus(this, event);
    };

    // Note: the focus event doesn't bubble, so we register for the capture phase
    window.addEventListener("focus", this.handleDocumentFocus, true);
  },

  updated() {
    this.props = getProps(this);
  },

  destroyed() {
    window.removeEventListener("keydown", this.handleDocumentKeyDown, true);
    window.removeEventListener("keyup", this.handleDocumentKeyUp, true);
    window.removeEventListener("focus", this.handleDocumentFocus, true);
  },
};

function getProps(hook) {
  return {
    isKeydownEnabled: getAttributeOrThrow(
      hook.el,
      "data-keydown-enabled",
      parseBoolean
    ),
    isKeyupEnabled: getAttributeOrThrow(
      hook.el,
      "data-keyup-enabled",
      parseBoolean
    ),
    target: getAttributeOrThrow(hook.el, "data-target"),
  };
}

function handleDocumentKeyDown(hook, event) {
  if (keyboardEnabled(hook)) {
    cancelEvent(event);
  }

  if (hook.props.isKeydownEnabled) {
    if (event.repeat) {
      return;
    }

    const key = event.key;
    hook.pushEventTo(hook.props.target, "keydown", { key });
  }
}

function handleDocumentKeyUp(hook, event) {
  if (keyboardEnabled(hook)) {
    cancelEvent(event);
  }

  if (hook.props.isKeyupEnabled) {
    const key = event.key;
    hook.pushEventTo(hook.props.target, "keyup", { key });
  }
}

function handleDocumentFocus(hook, event) {
  if (hook.props.isKeydownEnabled && isEditableElement(event.target)) {
    hook.pushEventTo(hook.props.target, "disable_keyboard", {});
  }
}

function keyboardEnabled(hook) {
  return hook.props.isKeydownEnabled || hook.props.isKeyupEnabled;
}

export default KeyboardControl;
