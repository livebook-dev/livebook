/**
 * An abstraction for registering and dispatching callbacks.
 */
export default class Emitter {
  constructor() {
    /** @private */
    this.callbacks = [];
  }

  /**
   * A function used to register new listener in the emitter.
   *
   * This is a shorthand for `addListener`.
   */
  get event() {
    return this.addListener.bind(this);
  }

  /**
   * Adds new listener to the emitter.
   *
   * Returns a subscription object that you can destroy in order to
   * unsubscribe.
   */
  addListener(callback) {
    this.callbacks.push(callback);

    return {
      destroy: () => {
        this.removeListener(callback);
      },
    };
  }

  /**
   * Removes a listener from the emitter.
   */
  removeListener(callback) {
    const idx = this.callbacks.indexOf(callback);

    if (idx !== -1) {
      this.callbacks.splice(idx, 1);
    }
  }

  /**
   * Dispatches all listeners with the given arguments.
   */
  dispatch(...args) {
    this.callbacks.forEach((callback) => {
      callback(...args);
    });
  }
}
