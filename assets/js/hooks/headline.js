import { parseHookProps } from "../lib/attribute";
import { globalPubsub } from "../lib/pubsub";
import { smoothlyScrollToElement } from "../lib/utils";

/**
 * A hook managing notebook/section headline.
 *
 * Similarly to cells the headline is focus/insert enabled.
 *
 * ## Props
 *
 *   * `id` - an identifier for the focus/insert navigation
 *
 *   * `on-value-change` - name of the event pushed when the user
 *     edits heading value
 *
 *   * `metadata` - additional value to send with the change event
 *
 */
const Headline = {
  mounted() {
    this.props = this.getProps();

    this.isFocused = false;
    this.insertMode = false;

    this.initializeHeadingEl();

    this.subscriptions = [
      globalPubsub.subscribe(
        "navigation:focus_changed",
        ({ focusableId, scroll }) =>
          this.handleElementFocused(focusableId, scroll),
      ),
      globalPubsub.subscribe("navigation:insert_mode_changed", ({ enabled }) =>
        this.handleInsertModeChanged(enabled),
      ),
    ];
  },

  updated() {
    this.props = this.getProps();
    this.initializeHeadingEl();
  },

  destroyed() {
    this.subscriptions.forEach((subscription) => subscription.destroy());
  },

  getProps() {
    return parseHookProps(this.el, ["id", "on-value-change", "metadata"]);
  },

  initializeHeadingEl() {
    const headingEl = this.el.querySelector(`[data-el-heading]`);

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

  handleElementFocused(focusableId, scroll) {
    if (this.props.id === focusableId) {
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
