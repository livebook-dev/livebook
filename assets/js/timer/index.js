import { getAttributeOrThrow } from "../lib/attribute";

const UPDATE_INTERVAL_MS = 100;

/**
 * A hook used to display a timer counting from the moment
 * of mounting.
 */
const Timer = {
  mounted() {
    this.props = getProps(this);

    this.state = {
      start: new Date(this.props.start),
      interval: null,
    };

    this.state.interval = setInterval(() => {
      this.__tick();
    }, UPDATE_INTERVAL_MS);
  },

  destroyed() {
    clearInterval(this.state.interval);
  },

  __tick() {
    const elapsedMs = Date.now() - this.state.start;
    const elapsedSeconds = elapsedMs / 1_000;

    this.el.innerHTML = `${elapsedSeconds.toFixed(1)}s`;
  },
};

function getProps(hook) {
  return {
    start: getAttributeOrThrow(hook.el, "data-start"),
  };
}

export default Timer;
