/**
 * A basic pub-sub implementation for client-side communication.
 */
export default class PubSub {
  constructor() {
    this.subscribersByTopic = {};
  }

  /**
   * Links the given function to the given topic.
   *
   * Subsequent calls to `broadcast` with this topic
   * will result in this function being called.
   *
   * Returns a function that unsubscribes
   * as a shorthand for `unsubscribe`.
   */
  subscribe(topic, callback) {
    if (!Array.isArray(this.subscribersByTopic[topic])) {
      this.subscribersByTopic[topic] = [];
    }

    this.subscribersByTopic[topic].push(callback);

    return () => {
      this.unsubscribe(topic, callback);
    };
  }

  /**
   * Unlinks the given function from the given topic.
   *
   * Note that you must pass the same function reference
   * as you passed to `subscribe`.
   */
  unsubscribe(topic, callback) {
    const idx = this.subscribersByTopic[topic].indexOf(callback);

    if (idx !== -1) {
      this.subscribersByTopic[topic].splice(idx, 1);
    }
  }

  /**
   * Calls all functions linked to the given topic
   * and passes `payload` as the argument.
   */
  broadcast(topic, payload) {
    if (Array.isArray(this.subscribersByTopic[topic])) {
      this.subscribersByTopic[topic].forEach((callback) => {
        callback(payload);
      });
    }
  }
}

export const globalPubSub = new PubSub();
