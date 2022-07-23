import { settingsStore } from "../lib/settings";

/**
 * A hook used to make the notebook content in full view mode.
 */
const FullView = {
  mounted() {
    this.setWidth();
  },

  updated() {
    this.setWidth();
  },

  setWidth() {
    const settings = settingsStore.get();
    if (settings.notebook_full_width) {
      this.el.classList.remove("max-w-screen-lg");
    }
  },
};

export default FullView;
