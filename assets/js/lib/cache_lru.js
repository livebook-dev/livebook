/**
 * A Map-based LRU cache.
 */
export default class CacheLRU {
  constructor(size) {
    this.size = size;
    this.cache = new Map();
  }

  get(key) {
    if (this.cache.has(key)) {
      const value = this.cache.get(key);
      // Map keys are stored and iterated in insertion order,
      // so we reinsert on every access
      this.cache.delete(key);
      this.cache.set(key, value);
      return value;
    } else {
      return undefined;
    }
  }

  set(key, value) {
    if (this.cache.has(key)) {
      this.cache.delete(key);
    } else if (this.cache.size === this.size) {
      const oldestKey = this.cache.keys().next().value;
      this.cache.delete(oldestKey);
    }

    this.cache.set(key, value);
  }
}
