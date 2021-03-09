/**
 * A basic pub-sub implementation for client-side communication.
 */
export default class PubSub {
  constructor() {
    this.subscribersByTopic = {};
  }

  subscribe(topic, callback) {
    if (!Array.isArray(this.subscribersByTopic[topic])) {
      this.subscribersByTopic[topic] = [];
    }

    this.subscribersByTopic[topic].push(callback);
  }

  unsubscribe(topic, callback) {
    const idx = this.subscribersByTopic[topic].indexOf(callback);

    if (idx !== -1) {
      this.subscribersByTopic[topic].splice(idx, 1);
    }
  }

  broadcast(topic, payload) {
    if (Array.isArray(this.subscribersByTopic[topic])) {
      this.subscribersByTopic[topic].forEach((callback) => {
        callback(payload);
      });
    }
  }
}

export const globalPubSub = new PubSub();
