import { scrollToEnd } from "../lib/utils";

/**
 * A hook used to scroll to the bottom of an element whenever it
 * receives LV update.
 */
const ScrollOnUpdate = {
  mounted() {
    this.scroll();
  },

  updated() {
    this.scroll();
  },

  scroll() {
    scrollToEnd(this.el);
  },
};

export default ScrollOnUpdate;
