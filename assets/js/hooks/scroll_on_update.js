/**
 * A hook used to scroll to the bottom of an element
 * whenever it receives LV update.
 */
const ScrollOnUpdate = {
  mounted() {
    this.__scroll();
  },

  updated() {
    this.__scroll();
  },

  __scroll() {
    this.el.scrollTop = this.el.scrollHeight;
  },
};

export default ScrollOnUpdate;
