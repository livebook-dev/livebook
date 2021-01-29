import Delta from "../../lib/delta";

/**
 * Encapsulates logic related to sending/receiving messages from the server.
 *
 * Uses the given hook instance socket for the communication.
 */
export default class HookServerAdapter {
  constructor(hook, cellId) {
    this.hook = hook;
    this.cellId = cellId;
    this._onDelta = null;
    this._onAcknowledgement = null;

    this.hook.handleEvent(`cell_delta:${this.cellId}`, ({ delta }) => {
      this._onDelta && this._onDelta(Delta.fromCompressed(delta));
    });

    this.hook.handleEvent(`cell_acknowledgement:${this.cellId}`, () => {
      this._onAcknowledgement && this._onAcknowledgement();
    });
  }

  /**
   * Registers a callback called whenever a new delta comes from the server.
   */
  onDelta(callback) {
    this._onDelta = callback;
  }

  /**
   * Registers a callback called when delta acknowledgement comes from the server.
   */
  onAcknowledgement(callback) {
    this._onAcknowledgement = callback;
  }

  /**
   * Sends the given delta to the server.
   */
  sendDelta(delta, revision) {
    this.hook.pushEvent("apply_cell_delta", {
      cell_id: this.cellId,
      delta: delta.toCompressed(),
      revision,
    });
  }
}
