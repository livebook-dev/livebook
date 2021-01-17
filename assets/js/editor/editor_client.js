export default class EditorClient {
  constructor(serverAdapter, editorAdapter, revision) {
    this.serverAdapter = serverAdapter;
    this.editorAdapter = editorAdapter;
    this.revision = revision;
    this.state = new Synchronized(this);

    this.editorAdapter.onDelta((delta) => {
      this.__handleClientDelta(delta);
    });

    this.serverAdapter.onDelta((delta) => {
      this.__handleServerDelta(delta);
    });

    this.serverAdapter.onAcknowledgement(() => {
      this.__handleServerAcknowledgement();
    });
  }

  __handleClientDelta(delta) {
    this.state = this.state.onClientDelta(delta);
  }

  __handleServerDelta(delta) {
    this.revision++;
    this.state = this.state.onServerDelta(delta);
  }

  __handleServerAcknowledgement() {
    this.state = this.state.onServerAcknowledgement();
  }

  applyDelta(delta) {
    this.editorAdapter.applyDelta(delta);
  }

  sendDelta(delta) {
    this.revision++;
    this.serverAdapter.sendDelta(delta, this.revision);
  }
}

/**
 * Client is in this state when there is no delta pending acknowledgement
 * (the client is fully in sync with the server).
 */
class Synchronized {
  constructor(client) {
    this.client = client;
  }

  onClientDelta(delta) {
    this.client.sendDelta(delta);
    return new AwaitingConfirm(this.client, delta);
  }

  onServerDelta(delta) {
    this.client.applyDelta(delta);
    return this;
  }

  onServerAcknowledgement() {
    throw new Error("Unexpected server acknowledgement.");
  }
}

/**
 * Client is in this state when the client sent one delta and waits
 * for an acknowledgement, while there are no other deltas in a buffer.
 */
class AwaitingConfirm {
  constructor(client, awaitedDelta) {
    this.client = client;
    this.awaitedDelta = awaitedDelta;
  }

  onClientDelta(delta) {
    return new AwaitingWithBuffer(this.client, this.awaitedDelta, delta);
  }

  onServerDelta(delta) {
    const deltaPrime = this.awaitedDelta.transform(delta);
    this.client.applyDelta(deltaPrime);
    const awaitedDeltaPrime = delta.transform(this.awaitedDelta);
    return new AwaitingConfirm(this.client, awaitedDeltaPrime);
  }

  onServerAcknowledgement() {
    return new Synchronized(this.client);
  }
}

/**
 * Client is in this state when the client sent one delta and waits
 * for an acknowledgement, while there are more deltas in a buffer.
 */
class AwaitingWithBuffer {
  constructor(client, awaitedDelta, buffer) {
    this.client = client;
    this.awaitedDelta = awaitedDelta;
    this.buffer = buffer;
  }

  onClientDelta(delta) {
    const newBuffer = this.buffer.compose(delta);
    return new AwaitingWithBuffer(this.client, this.awaitedDelta, newBuffer);
  }

  onServerDelta(delta) {
    const deltaPrime = this.awaitedDelta.compose(this.buffer).transform(delta);
    this.client.applyDelta(deltaPrime);
    const awaitedDeltaPrime = delta.transform(this.awaitedDelta);
    const bufferPrime = this.awaitedDelta
      .transform(delta)
      .transform(this.buffer);

    return new AwaitingWithBuffer(this.client, awaitedDeltaPrime, bufferPrime);
  }

  onServerAcknowledgement() {
    this.client.sendDelta(this.buffer);
    return new AwaitingConfirm(this.client, this.buffer);
  }
}
