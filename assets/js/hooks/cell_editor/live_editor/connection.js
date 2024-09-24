import Delta from "../../../lib/delta";
import { EditorSelection } from "@codemirror/state";
import { LiveStore } from "../../../lib/live_store";
import Emitter from "../../../lib/emitter";

/**
 * Encapsulates the editor communication with the server.
 *
 * Uses the given hook instance socket for the communication.
 */
export default class Connection {
  /** @private */
  _onDelta = new Emitter();

  /**
   * Registers a callback called whenever a new delta comes from the server.
   */
  onDelta = this._onDelta.event;

  /** @private */
  _onAcknowledgement = new Emitter();

  /**
   * Registers a callback called when delta acknowledgement comes from the server.
   */
  onAcknowledgement = this._onAcknowledgement.event;

  /** @private */
  _onSelection = new Emitter();

  /**
   * Registers a callback called whenever a new selection report comes from the server.
   */
  onSelection = this._onSelection.event;

  /** @private */
  _onClientsUpdate = new Emitter();

  /**
   * Registers a callback called whenever any remote client changes.
   *
   * The change may be a result of a client leaving, joining or updating
   * their details.
   */
  onClientsUpdate = this._onClientsUpdate.event;

  constructor(hook, cellId, tag) {
    this.hook = hook;
    this.cellId = cellId;
    this.tag = tag;

    this.sessionStore = LiveStore.getStore("session");
    this.handlerByRef = {};

    this.setupCollaborationHandlers();

    this.clientsSubscription = this.sessionStore.watch("clients", (clients) => {
      this._onClientsUpdate.dispatch(clients);
    });

    this.setupIntellisenseHandlers();
  }

  destroy() {
    this.clientsSubscription.destroy();
  }

  /**
   * Returns the list of clients currently connected to the session.
   */
  getClients() {
    return this.sessionStore.get("clients");
  }

  /**
   * Returns the ide of this particular client.
   */
  getClientId() {
    return this.sessionStore.get("clientId");
  }

  /**
   * Sends the given delta to the server.
   */
  sendDelta(delta, selection, revision) {
    this.hook.pushEvent("apply_cell_delta", {
      cell_id: this.cellId,
      tag: this.tag,
      delta: delta.toCompressed(),
      selection: selection && selectionToCompressed(selection),
      revision,
    });
  }

  /**
   * Sends the current client selection to the server along with the
   * current revision it is at.
   */
  sendSelection(selection, revision) {
    this.hook.pushEvent("report_cell_selection", {
      cell_id: this.cellId,
      tag: this.tag,
      selection: selection && selectionToCompressed(selection),
      revision,
    });
  }

  /**
   * Sends an information to the server that the client is at the
   * specified revision.
   *
   * This should be invoked if the client received updates, but is
   * not itself sending any delta at the moment. By sending this
   * messages we make sure the server doesn't accumulate a huge list
   * of deltas unnecessarily.
   */
  sendRevision(revision) {
    this.hook.pushEvent("report_cell_revision", {
      cell_id: this.cellId,
      tag: this.tag,
      revision,
    });
  }

  /** @private */
  setupCollaborationHandlers() {
    this.hook.handleEvent(
      `cell_delta:${this.cellId}:${this.tag}`,
      ({ delta, selection, client_id }) => {
        delta = Delta.fromCompressed(delta);
        selection = selection && selectionFromCompressed(selection);
        this._onDelta.dispatch(delta, selection, client_id);
      },
    );

    this.hook.handleEvent(
      `cell_acknowledgement:${this.cellId}:${this.tag}`,
      () => {
        this._onAcknowledgement.dispatch();
      },
    );

    this.hook.handleEvent(
      `cell_selection:${this.cellId}:${this.tag}`,
      ({ selection, client_id }) => {
        selection = selection && selectionFromCompressed(selection);
        this._onSelection.dispatch(selection, client_id);
      },
    );
  }

  /** @private */
  setupIntellisenseHandlers() {
    // Intellisense requests such as completion or formatting are
    // handled asynchronously by the runtime. The request happens in
    // two steps:
    //
    //   * we send an intellisense request to the LV process and get
    //     a unique reference, under which we store a callback
    //
    //   * once we receive the "intellisense_response" event from the
    //     server, we look up the callback for that reference to
    //     either resolve or reject the promise that was returned to
    //     the caller

    this.hook.handleEvent("intellisense_response", ({ ref, response }) => {
      const handler = this.handlerByRef[ref];

      if (handler) {
        handler(response);
        delete this.handlerByRef[ref];
      }
    });
  }

  /**
   * Makes an intellisense request.
   *
   * The returned promise is either resolved with a valid response or
   * rejected with an intellisense error.
   */
  intellisenseRequest(type, props) {
    return new Promise((resolve, reject) => {
      this.hook.pushEvent(
        "intellisense_request",
        { cell_id: this.cellId, type, ...props },
        ({ ref }) => {
          if (ref) {
            this.handlerByRef[ref] = (response) => {
              if (response) {
                resolve(response);
              } else {
                reject(
                  new IntellisenseError(
                    "No relevant intellisense response for the given parameters",
                  ),
                );
              }
            };
          } else {
            reject(
              new IntellisenseError(
                "Intellisense request could not be completed",
              ),
            );
          }
        },
      );
    });
  }
}

function selectionToCompressed(selection) {
  return selection.ranges.map(({ anchor, head }) => [anchor, head]);
}

function selectionFromCompressed(list) {
  const ranges = list.map(([anchor, head]) =>
    EditorSelection.range(anchor, head),
  );

  return EditorSelection.create(ranges);
}

class IntellisenseError extends Error {
  constructor(message) {
    super(message);
    this.name = "IntellisenseError";
  }
}
