import { getAttributeOrThrow } from "../lib/attribute";
import { globalPubSub } from "../lib/pub_sub";
import { smoothlyScrollToElement } from "../lib/utils";

/**
 * A hook managing notebook/section headline.
 *
 * Similarly to cells the headline is focus/insert enabled.
 *
 * ## Configuration
 *
 *   * `data-focusable-id` - an identifier for the focus/insert
 *     navigation
 *
 *   * `data-on-value-change` - name of the event pushed when the user
 *     edits heading value
 *
 *   * `data-metadata` - additional value to send with the change event
 */
const Headline = {
  mounted() {
    this.props = this.getProps();

    this.isFocused = false;
    this.insertMode = false;

    this.initializeHeadingEl();

    this.unsubscribeFromNavigationEvents = globalPubSub.subscribe(
      "navigation",
      (event) => {
        this.handleNavigationEvent(event);
      }
    );
  },

  updated() {
    this.props = this.getProps();
    this.initializeHeadingEl();
  },

  destroyed() {
    this.unsubscribeFromNavigationEvents();
  },

  getProps() {
    return {
      focusableId: getAttributeOrThrow(this.el, "data-focusable-id"),
      onValueChange: getAttributeOrThrow(this.el, "data-on-value-change"),
      metadata: getAttributeOrThrow(this.el, "data-metadata"),
    };
  },

  initializeHeadingEl() {
    const headingEl = this.el.querySelector(`[data-element="heading"]`);

    if (headingEl === this.headingEl) {
      return;
    }

    this.headingEl = headingEl;

    // Make sure only plain text is pasted
    this.headingEl.addEventListener("paste", (event) => {
      event.preventDefault();
      const text = event.clipboardData.getData("text/plain").replace("\n", " ");
      document.execCommand("insertText", false, text);
    });

    // Ignore enter
    this.headingEl.addEventListener("keydown", (event) => {
      if (event.key === "Enter") {
        event.preventDefault();
      }
    });

    this.headingEl.addEventListener("blur", (event) => {
      // Wait for other handlers to complete and if still in insert
      // mode force focus
      setTimeout(() => {
        if (this.isFocused && this.insertMode) {
          this.headingEl.focus();
          moveSelectionToEnd(this.headingEl);
        }
      }, 0);
    });
  },

  handleNavigationEvent(event) {
    if (event.type === "element_focused") {
      this.handleElementFocused(event.focusableId, event.scroll);
    } else if (event.type === "insert_mode_changed") {
      this.handleInsertModeChanged(event.enabled);
    }
  },

  handleElementFocused(cellId, scroll) {
    if (this.props.focusableId === cellId) {
      this.isFocused = true;
      this.el.setAttribute("data-js-focused", "");
      if (scroll) {
        smoothlyScrollToElement(this.el);
      }
    } else if (this.isFocused) {
      this.isFocused = false;
      this.el.removeAttribute("data-js-focused");
    }
  },

  handleInsertModeChanged(insertMode) {
    if (this.isFocused && !this.insertMode && insertMode) {
      this.insertMode = insertMode;
      // While in insert mode, ignore the incoming changes
      this.el.setAttribute("phx-update", "ignore");
      this.headingEl.setAttribute("contenteditable", "");
      this.headingEl.focus();
      moveSelectionToEnd(this.headingEl);
    } else if (this.insertMode && !insertMode) {
      this.insertMode = insertMode;
      this.headingEl.removeAttribute("contenteditable");
      this.el.removeAttribute("phx-update");
      this.pushEvent(this.props.onValueChange, {
        value: this.headingEl.textContent.trim(),
        metadata: this.props.metadata,
      });
    }
  },
};

function moveSelectionToEnd(element) {
  const range = document.createRange();
  range.selectNodeContents(element);
  range.collapse(false);
  const selection = window.getSelection();
  selection.removeAllRanges();
  selection.addRange(range);
}

export default Headline;
