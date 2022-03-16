import { isEditableElement } from "../lib/utils";

/**
 * A hook used to focus an element whenever it receives LV update.
 */
const FocusOnUpdate = {
  mounted() {
    this.focus();
  },

  updated() {
    if (this.el !== document.activeElement) {
      this.focus();
    }
  },

  focus() {
    if (isEditableElement(document.activeElement)) {
      return;
    }

    this.el.focus();
    this.el.selectionStart = this.el.selectionEnd = this.el.value.length;
    this.el.scrollLeft = this.el.scrollWidth;
  },
};

export default FocusOnUpdate;
