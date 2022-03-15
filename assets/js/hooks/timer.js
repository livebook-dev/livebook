import { getAttributeOrThrow } from "../lib/attribute";

const UPDATE_INTERVAL_MS = 100;

/**
 * A hook used to display a counting timer.
 *
 * ## Configuration
 *
 *   * `data-start` - the timestamp to count from
 */
const Timer = {
  mounted() {
    this.props = this.getProps();

    this.interval = setInterval(() => this.updateDOM(), UPDATE_INTERVAL_MS);
  },

  updated() {
    this.props = this.getProps();
    this.updateDOM();
  },

  destroyed() {
    clearInterval(this.interval);
  },

  getProps() {
    return {
      start: getAttributeOrThrow(this.el, "data-start"),
    };
  },

  updateDOM() {
    const elapsedMs = Date.now() - new Date(this.props.start);
    const elapsedSeconds = elapsedMs / 1_000;

    this.el.innerHTML = `${elapsedSeconds.toFixed(1)}s`;
  },
};

export default Timer;
