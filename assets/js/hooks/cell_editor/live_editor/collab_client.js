import Emitter from "../../../lib/emitter";
import { pop } from "../../../lib/utils";
import { transformSelection } from "./codemirror/collab";

const REVISION_REPORT_TIMEOUT_MS = 5000;

/**
 * Collaborative editing client.
 *
 * The client subscribes to collaborative updates and exposes APIs for
 * an editor to receive and push local changes. Note that updates are
 * consumed regardless of whether there is an actual editor attached
 * to this client. This makes it possible to mount an editor at any
 * later point without getting out of sync, while keeping track of the
 * current source (for example, to feed markdown renderer).
 *
 * If the client receives deltas, but does not produce any events on
 * its own, it periodically reports the current revision to the server,
 * so that the server can free up the deltas it keeps. This is another
 * aspect that makes it important to keep the fully operational client
 * separate from the editor plugin.
 *
 * ## Synchronization flow
 *
 * When the local editor emits a change, it is converted to a delta,
 * and notified to the client. The clients sends it to the server
 * and waits for an acknowledgement. Until the acknowledgement comes,
 * the client keeps all further edits in a buffer. The server may send
 * either an acknowledgement or other client's delta. It's important
 * to note that the messages come in what the server believes is
 * chronological order, so any delta received before the acknowledgement
 * should be treated as if it happened before our unacknowledged delta.
 * Other client's delta is transformed against the local unacknowledged
 * deltas and applied to the editor.
 */
export default class CollabClient {
  /** @private */
  _onDelta = new Emitter();

  /**
   * Registers a callback called with a new delta is emitted, either
   * by the client or the server.
   *
   * The deltas are transformed, such that applying them one by one
   * keeps the document in sync.
   */
  onDelta = this._onDelta.event;

  /** @private */
  _onPeersChange = new Emitter();

  /**
   * Registers a callback called whenever the clients change.
   *
   * Clients may change as a result of join, leave, details update or
   * a new selection.
   */
  onPeersChange = this._onPeersChange.event;

  constructor(connection, revision) {
    this.connection = connection;
    this.revision = revision;

    this.clientId = connection.getClientId();

    this.peers = {};
    this.updatePeers(connection.getClients());

    this.inflightDelta = null;
    this.bufferDelta = null;
    this.selection = null;
    this.selectionChanged = false;
    this.revisionReportTimeoutId = null;

    this.subscriptions = [
      connection.onDelta(this.handleServerDelta.bind(this)),
      connection.onAcknowledgement(this.handleServerAcknowledgement.bind(this)),
      connection.onSelection(this.handleServerSelection.bind(this)),
      connection.onClientsUpdate(this.handleServerClientsUpdate.bind(this)),
    ];
  }

  destroy() {
    this.subscriptions.forEach((subscription) => subscription.destroy());
  }

  /**
   * Returns the map with currently connected peers.
   */
  getPeers() {
    return this.peers;
  }

  /**
   * Sends a local delta to the server or puts it in the queue.
   *
   * Should be called by the editor, whenever the content is changed
   * by the user.
   */
  handleClientDelta(delta, selection) {
    this.peers = transformPeerSelections(this.peers, delta);

    this.selection = selection;

    if (!this.inflightDelta) {
      this.inflightDelta = delta;
      this.sendDelta();
    } else if (!this.bufferDelta) {
      this.bufferDelta = delta;
    } else {
      this.bufferDelta = this.bufferDelta.compose(delta);
    }

    this._onDelta.dispatch(delta, { remote: false });
    this._onPeersChange.dispatch(this.peers);
  }

  /**
   * Sends a local selection to the server or puts it in the queue.
   *
   * Should be called by the editor, whenever the current selection
   * changes.
   */
  handleClientSelection(selection) {
    this.selection = selection;

    if (!this.inflightDelta) {
      this.sendSelection();
    } else {
      this.selectionChanged = true;
    }
  }

  /** @private */
  handleServerDelta(delta, selection, clientId) {
    this.revision++;

    // The server dictates the order of the deltas, so we consider the
    // incoming delta to have happened first

    let { inflightDelta, bufferDelta } = this;

    if (inflightDelta) {
      [delta, inflightDelta] = [
        inflightDelta.transform(delta, "right"),
        delta.transform(inflightDelta, "left"),
      ];

      selection = selection && transformSelection(selection, inflightDelta);
    }

    if (bufferDelta) {
      [delta, bufferDelta] = [
        bufferDelta.transform(delta, "right"),
        delta.transform(bufferDelta, "left"),
      ];

      selection = selection && transformSelection(selection, bufferDelta);
    }

    this.inflightDelta = inflightDelta;
    this.bufferDelta = bufferDelta;

    this.selection =
      this.selection && transformSelection(this.selection, delta);

    let [peer, peers] = pop(this.peers, clientId);
    peers = transformPeerSelections(peers, delta);

    if (peer) {
      peers[clientId] = new Peer(peer.id, peer.meta, selection);
    }

    this.peers = peers;

    this._onDelta.dispatch(delta, { remote: true });
    this._onPeersChange.dispatch(this.peers);

    // The client received a new delta, so we schedule a request to
    // report the revision, unless the client emits a message soon
    this.maybeScheduleRevisionReport();
  }

  /** @private */
  handleServerAcknowledgement() {
    this.revision++;

    this.inflightDelta = null;

    if (this.bufferDelta) {
      this.inflightDelta = this.bufferDelta;
      this.bufferDelta = null;
      this.sendDelta();
    }
  }

  /** @private */
  handleServerSelection(selection, clientId) {
    if (!this.peers.hasOwnProperty(clientId)) return;

    let { inflightDelta, bufferDelta } = this;

    if (inflightDelta) {
      selection = selection && transformSelection(selection, inflightDelta);
    }

    if (bufferDelta) {
      selection = selection && transformSelection(selection, bufferDelta);
    }

    const peer = this.peers[clientId];

    this.peers = {
      ...this.peers,
      [clientId]: new Peer(peer.id, peer.meta, selection),
    };

    this._onPeersChange.dispatch(this.peers);
  }

  /** @private */
  handleServerClientsUpdate(clients) {
    this.updatePeers(clients);
    this._onPeersChange.dispatch(this.peers);
  }

  /** @private */
  sendDelta() {
    this.connection.sendDelta(
      this.inflightDelta,
      this.selection,
      this.revision,
    );

    this.selectionChanged = false;

    // Cancel the revision report if scheduled, since the client is
    // has just sent the revision along with the delta
    this.maybeCancelRevisionReport();
  }

  /** @private */
  sendSelection() {
    // Only send the selection if there are some peers
    if (Object.keys(this.peers).length > 0) {
      this.connection.sendSelection(this.selection, this.revision);
    }

    this.selectionChanged = false;
  }

  /** @private */
  sendRevision() {
    this.connection.sendRevision(this.revision);
  }

  /** @private */
  updatePeers(clients) {
    const peers = {};

    for (const clientId in clients) {
      if (clientId !== this.clientId) {
        const currentPeer = this.peers[clientId];
        const meta = clients[clientId];
        const selection = currentPeer ? currentPeer.selection : null;
        peers[clientId] = new Peer(clientId, meta, selection);
      }
    }

    this.peers = peers;
  }

  /** @private */
  maybeScheduleRevisionReport() {
    if (!this.inflightDelta && !this.revisionReportTimeoutId) {
      this.revisionReportTimeoutId = setTimeout(() => {
        this.sendRevision();
        this.revisionReportTimeoutId = null;
      }, REVISION_REPORT_TIMEOUT_MS);
    }
  }

  /** @private */
  maybeCancelRevisionReport() {
    if (this.revisionReportTimeoutId !== null) {
      clearTimeout(this.revisionReportTimeoutId);
      this.revisionReportTimeoutId = null;
    }
  }
}

/**
 * Holds information about a collaborative peer, including their
 * selection and details.
 */
export class Peer {
  constructor(id, meta, selection) {
    this.id = id;
    this.meta = meta;
    this.selection = selection;
  }

  eq(other) {
    return (
      this.id === other.id &&
      this.meta === other.meta &&
      (this.selection === other.selection ||
        (this.selection &&
          other.selection &&
          this.selection.eq(other.selection)))
    );
  }
}

function transformPeerSelections(peers, delta) {
  const newPeers = {};

  for (const clientId in peers) {
    const peer = peers[clientId];
    const selection =
      peer.selection && transformSelection(peer.selection, delta);
    newPeers[clientId] = new Peer(peer.id, peer.meta, selection);
  }

  return newPeers;
}
