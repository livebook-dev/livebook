import PubSub from "./pubsub";

const stores = {};

/**
 * A reactive in-memory store implementation.
 *
 * Stores can be accessed by a global id. Values in a given store can
 * can be watched for change.
 */
export class LiveStore {
  /** @private */
  constructor(id) {
    this.id = id;

    this.pubsub = new PubSub();
    this.map = {};
  }

  /**
   * Creates and registers a global store under the given id.
   */
  static create(id) {
    const store = new LiveStore(id);
    stores[id] = store;
    return store;
  }

  /**
   * Gets a store by the given global id.
   */
  static getStore(id) {
    if (!stores.hasOwnProperty(id)) {
      throw new Error(`No store found for id "${id}"`);
    }

    return stores[id];
  }

  /**
   * Destroys the store.
   *
   * This removes the store from the global register.
   */
  destroy() {
    // Another store may be created and re-registered under the given
    // id, so we unregister only if it points to this store
    if (stores[this.id] === this) {
      delete stores[this.id];
    }
  }

  /**
   * Puts value in the store.
   */
  set(key, value) {
    this.map[key] = value;
    this.pubsub.broadcast(key, value);
  }

  /**
   * Gets value from the store.
   */
  get(key) {
    if (!this.map.hasOwnProperty(key)) {
      throw new Error(`Key "${key}" not found in the store`);
    }

    return this.map[key];
  }

  /**
   * Subscribes to changes for the given key.
   */
  watch(key, callback) {
    return this.pubsub.subscribe(key, callback);
  }
}
