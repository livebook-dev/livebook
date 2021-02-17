/**
 * Allows for recording a sequence of keys pressed
 * and matching against that sequence.
 */
class KeyBuffer {
  /**
   * @param {Number} resetTimeout The number of milliseconds to wait after new key is pushed before the buffer is cleared.
   */
  constructor(resetTimeout = 2000) {
    this.resetTimeout = resetTimeout;
    this.buffer = [];
    this.resetTimeoutId = null;
  }

  /**
   * Adds a new key to the buffer and renews the reset timeout.
   */
  push(key) {
    this.buffer.push(key);

    if (this.resetTimeoutId) {
      clearTimeout(this.resetTimeoutId);
    }

    this.resetTimeoutId = setTimeout(() => {
      this.reset();
    }, this.resetTimeout);
  }

  /**
   * Immediately clears the buffer.
   */
  reset() {
    if (this.resetTimeout) {
      clearTimeout(this.resetTimeout);
    }

    this.clearTimeout = null;
    this.buffer = [];
  }

  /**
   * Checks if the given sequence of keys matches the end of buffer.
   *
   * If the match succeeds, the buffer is reset.
   */
  tryMatch(keys) {
    if (keys.length > this.buffer.length) {
      return false;
    }

    const bufferTail = this.buffer.slice(-keys.length);
    const matches = keys.every((key, index) => key === bufferTail[index]);

    if (matches) {
      this.reset();
    }

    return matches;
  }
}

export default KeyBuffer;
