/**
 * A hook used to focus an element whenever it receives LV update.
 */
const FocusOnUpdate = {
  mounted() {
    this.__focus();
  },

  updated() {
    this.__focus();
  },

  __focus() {
    this.el.focus();
    this.el.selectionStart = this.el.selectionEnd = this.el.value.length;
  },
};

export default FocusOnUpdate;
