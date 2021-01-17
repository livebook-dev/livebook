import Delta from "../lib/delta";

export default class HookAdapter {
  constructor(hook, cellId) {
    this.hook = hook;
    this.cellId = cellId;
    this._onDelta = null;
    this._onAcknowledgement = null;

    this.hook.handleEvent(`cell:${this.cellId}:delta`, (delta) => {
      this._onDelta && this._onDelta(new Delta(delta.ops));
    });

    this.hook.handleEvent(`cell:${this.cellId}:acknowledgement`, () => {
      this._onAcknowledgement && this._onAcknowledgement();
    });
  }

  onDelta(callback) {
    this._onDelta = callback;
  }

  onAcknowledgement(callback) {
    this._onAcknowledgement = callback;
  }

  sendDelta(delta, revision) {
    this.hook.pushEvent("cell_delta", {
      cell_id: this.cellId,
      delta,
      revision,
    });
  }
}
