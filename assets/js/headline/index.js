import { getAttributeOrThrow } from "../lib/attribute";
import { globalPubSub } from "../lib/pub_sub";
import { smoothlyScrollToElement } from "../lib/utils";

/**
 * A hook managing notebook/section headline.
 *
 * Similarly to cells the headline is focus/insert enabled.
 *
 * Configuration:
 *
 *   * `data-focusable-id` - an identifier for the focus/insert navigation
 *   * `data-on-value-change` - name of the event pushed when the user edits heading value
 *   * `data-metadata` - additional value to send with the change event
 */
const Headline = {
  mounted() {
    this.props = getProps(this);
    this.state = {
      isFocused: false,
      insertMode: false,
    };

    const heading = getHeading(this);

    // Make sure only plain text is pasted
    heading.addEventListener("paste", (event) => {
      event.preventDefault();
      const text = event.clipboardData.getData("text/plain").replace("\n", " ");
      document.execCommand("insertText", false, text);
    });

    // Ignore enter
    heading.addEventListener("keydown", (event) => {
      if (event.key === "Enter") {
        event.preventDefault();
      }
    });

    heading.addEventListener("blur", (event) => {
      // Wait for other handlers to complete and if still in insert
      // force focus
      setTimeout(() => {
        if (this.state.isFocused && this.state.insertMode) {
          heading.focus();
          moveSelectionToEnd(heading);
        }
      }, 0);
    });

    this._unsubscribeFromNavigationEvents = globalPubSub.subscribe(
      "navigation",
      (event) => {
        handleNavigationEvent(this, event);
      }
    );
  },

  updated() {
    this.props = getProps(this);
  },

  destroyed() {
    this._unsubscribeFromNavigationEvents();
  },
};

function getProps(hook) {
  return {
    focusableId: getAttributeOrThrow(hook.el, "data-focusable-id"),
    onValueChange: getAttributeOrThrow(hook.el, "data-on-value-change"),
    metadata: getAttributeOrThrow(hook.el, "data-metadata"),
  };
}

function handleNavigationEvent(hook, event) {
  if (event.type === "element_focused") {
    handleElementFocused(hook, event.focusableId, event.scroll);
  } else if (event.type === "insert_mode_changed") {
    handleInsertModeChanged(hook, event.enabled);
  }
}

function handleElementFocused(hook, cellId, scroll) {
  if (hook.props.focusableId === cellId) {
    hook.state.isFocused = true;
    hook.el.setAttribute("data-js-focused", "true");
    if (scroll) {
      smoothlyScrollToElement(hook.el);
    }
  } else if (hook.state.isFocused) {
    hook.state.isFocused = false;
    hook.el.removeAttribute("data-js-focused");
  }
}

function handleInsertModeChanged(hook, insertMode) {
  if (hook.state.isFocused) {
    hook.state.insertMode = insertMode;

    const heading = getHeading(hook);

    if (hook.state.insertMode) {
      // While in insert mode, ignore the incoming changes
      hook.el.setAttribute("phx-update", "ignore");
      heading.setAttribute("contenteditable", "true");
      heading.focus();
      moveSelectionToEnd(heading);
    } else {
      heading.removeAttribute("contenteditable");
      hook.el.removeAttribute("phx-update");
      hook.pushEvent(hook.props.onValueChange, {
        value: headingValue(heading),
        metadata: hook.props.metadata,
      });
    }
  }
}

function getHeading(hook) {
  return hook.el.querySelector(`[data-element="heading"]`);
}

function headingValue(heading) {
  return heading.textContent.trim();
}

function moveSelectionToEnd(heading) {
  const range = document.createRange();
  range.selectNodeContents(heading);
  range.collapse(false);
  const selection = window.getSelection();
  selection.removeAllRanges();
  selection.addRange(range);
}

export default Headline;
