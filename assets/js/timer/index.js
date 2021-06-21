const UPDATE_INTERVAL_MS = 100;

/**
 * A hook used to display a timer counting from the moment
 * of mounting.
 */
const Timer = {
  mounted() {
    this.state = {
      start: Date.now(),
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

export default Timer;
