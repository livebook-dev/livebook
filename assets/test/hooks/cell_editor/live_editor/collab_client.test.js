import CollabClient, {
  Peer,
} from "../../../../js/hooks/cell_editor/live_editor/collab_client";
import Delta from "../../../../js/lib/delta";
import { EditorSelection } from "@codemirror/state";

jest.useFakeTimers();

describe("when synchronized", () => {
  test("sends local delta immediately", () => {
    const connection = buildMockConnection();
    const collabClient = new CollabClient(connection, 0);

    const onDelta = jest.fn();
    collabClient.onDelta(onDelta);

    const delta = new Delta().insert("cat");
    const selection = cursorSelection(3);
    collabClient.handleClientDelta(delta, selection);

    expect(connection.sendDelta).toHaveBeenCalledWith(delta, selection, 0);
    expect(onDelta).toHaveBeenCalledWith(delta, { remote: false });
  });

  test("accepts remote delta unchanged", () => {
    const connection = buildMockConnection();
    const collabClient = new CollabClient(connection, 0);

    const onDelta = jest.fn();
    collabClient.onDelta(onDelta);

    const remoteDelta = new Delta().retain(1).insert("dog");
    const remoteSelection = cursorSelection(4);
    getListener(connection, "onDelta")(remoteDelta, remoteSelection, "client2");

    expect(onDelta).toHaveBeenCalledWith(remoteDelta, { remote: true });

    // We are already in sync, so the client reports the revision in 5s
    jest.runOnlyPendingTimers();
    expect(connection.sendRevision).toHaveBeenCalledWith(1);
  });

  test("sends local selection when there are peers", () => {
    const connection = buildMockConnection();
    connection.getClients.mockReturnValue({
      client1: { name: "Jake" },
    });
    connection.getClientId.mockReturnValue("client1");
    const collabClient = new CollabClient(connection, 0);

    const selection = cursorSelection(3);
    collabClient.handleClientSelection(selection);

    expect(connection.sendSelection).not.toHaveBeenCalled();
  });

  test("does not send local selection when there are no peers", () => {
    const connection = buildMockConnection();
    connection.getClients.mockReturnValue({
      client1: { name: "Jake" },
      client2: { name: "Amy" },
    });
    connection.getClientId.mockReturnValue("client1");
    const collabClient = new CollabClient(connection, 0);

    const selection = cursorSelection(3);
    collabClient.handleClientSelection(selection);

    expect(connection.sendSelection).toHaveBeenCalledWith(selection, 0);
  });
});

describe("with inflight delta", () => {
  test("buffers local delta until acknowledgement", () => {
    const connection = buildMockConnection();
    const collabClient = new CollabClient(connection, 0);

    const onDelta = jest.fn();
    collabClient.onDelta(onDelta);

    const delta = new Delta().insert("cat");
    const selection = cursorSelection(3);
    collabClient.handleClientDelta(delta, selection);

    const delta2 = new Delta().retain(5).insert("jumps");
    const selection2 = cursorSelection(10);
    collabClient.handleClientDelta(delta2, selection2);
    expect(onDelta).toHaveBeenCalledWith(delta2, { remote: false });

    expect(connection.sendDelta.mock.calls).toHaveLength(1);
    getListener(connection, "onAcknowledgement")();
    expect(connection.sendDelta.mock.calls).toHaveLength(2);

    expect(connection.sendDelta).toHaveBeenCalledWith(delta2, selection2, 1);
  });

  test("transforms remote delta againast inflight delta", () => {
    const connection = buildMockConnection();
    const collabClient = new CollabClient(connection, 0);

    const onDelta = jest.fn();
    collabClient.onDelta(onDelta);

    const delta = new Delta().insert("cat");
    const selection = cursorSelection(3);
    collabClient.handleClientDelta(delta, selection);

    const remoteDelta = new Delta().retain(1).insert("dog");
    const remoteSelection = cursorSelection(4);
    getListener(connection, "onDelta")(remoteDelta, remoteSelection, "client2");

    const transformedDelta = new Delta().retain(4).insert("dog");
    expect(onDelta).toHaveBeenCalledWith(transformedDelta, { remote: true });
  });
});

describe("with buffer delta", () => {
  test("merges subsequent local deltas into the buffer until acknowledgement", () => {
    const connection = buildMockConnection();
    const collabClient = new CollabClient(connection, 0);

    const onDelta = jest.fn();
    collabClient.onDelta(onDelta);

    const delta = new Delta().insert("cat");
    const selection = cursorSelection(3);
    collabClient.handleClientDelta(delta, selection);

    const delta2 = new Delta().retain(5).insert("jumps");
    const selection2 = cursorSelection(10);
    collabClient.handleClientDelta(delta2, selection2);
    expect(onDelta).toHaveBeenCalledWith(delta2, { remote: false });

    const delta3 = new Delta().retain(15).insert("high");
    const selection3 = cursorSelection(19);
    collabClient.handleClientDelta(delta3, selection3);
    expect(onDelta).toHaveBeenCalledWith(delta3, { remote: false });

    expect(connection.sendDelta.mock.calls).toHaveLength(1);
    getListener(connection, "onAcknowledgement")();
    expect(connection.sendDelta.mock.calls).toHaveLength(2);

    const bufferDelta = new Delta()
      .retain(5)
      .insert("jumps")
      .retain(5)
      .insert("high");
    const bufferSelection = cursorSelection(19);
    expect(connection.sendDelta).toHaveBeenCalledWith(
      bufferDelta,
      bufferSelection,
      1,
    );
  });

  test("transforms remote delta and buffer", () => {
    const connection = buildMockConnection();
    const collabClient = new CollabClient(connection, 0);

    const onDelta = jest.fn();
    collabClient.onDelta(onDelta);

    const delta = new Delta().insert("cat");
    const selection = cursorSelection(3);
    collabClient.handleClientDelta(delta, selection);

    const delta2 = new Delta().retain(5).insert("jumps");
    const selection2 = cursorSelection(10);
    collabClient.handleClientDelta(delta2, selection2);
    expect(onDelta).toHaveBeenCalledWith(delta2, { remote: false });

    const remoteDelta = new Delta()
      .retain(1)
      .insert("dog")
      .retain(10)
      .insert("fox");
    const remoteSelection = cursorSelection(4);
    getListener(connection, "onDelta")(remoteDelta, remoteSelection, "client2");

    const transformedDelta = new Delta()
      // Transformed againast inflight
      .retain(4)
      .insert("dog")
      // Transformed against buffer
      .retain(15)
      .insert("fox");
    expect(onDelta).toHaveBeenCalledWith(transformedDelta, { remote: true });

    expect(connection.sendDelta.mock.calls).toHaveLength(1);
    getListener(connection, "onAcknowledgement")();
    expect(connection.sendDelta.mock.calls).toHaveLength(2);

    // Transformed againast remote delta
    const bufferDelta = new Delta().retain(8).insert("jumps");
    const bufferSelection = cursorSelection(13);
    expect(connection.sendDelta).toHaveBeenCalledWith(
      bufferDelta,
      bufferSelection,
      2,
    );
  });
});

describe("peers", () => {
  test("transforms peer selections against local delta", () => {
    const connection = buildMockConnection();
    connection.getClients.mockReturnValue({
      client1: { name: "Jake" },
      client2: { name: "Amy" },
    });
    connection.getClientId.mockReturnValue("client1");
    const collabClient = new CollabClient(connection, 0);

    const onPeersChange = jest.fn();
    collabClient.onPeersChange(onPeersChange);

    const remoteSelection = cursorSelection(4);
    getListener(connection, "onSelection")(remoteSelection, "client2");

    expect(onPeersChange).toHaveBeenCalledWith({
      client2: new Peer("client2", { name: "Amy" }, cursorSelection(4)),
    });

    const delta = new Delta().insert("cat");
    const selection = cursorSelection(3);
    collabClient.handleClientDelta(delta, selection);

    expect(onPeersChange).toHaveBeenCalledWith({
      client2: new Peer("client2", { name: "Amy" }, cursorSelection(7)),
    });
  });

  test("transforms peer selections against remote delta", () => {
    const connection = buildMockConnection();
    connection.getClients.mockReturnValue({
      client1: { name: "Jake" },
      client2: { name: "Amy" },
    });
    connection.getClientId.mockReturnValue("client1");
    const collabClient = new CollabClient(connection, 0);

    const onPeersChange = jest.fn();
    collabClient.onPeersChange(onPeersChange);

    const remoteSelection = cursorSelection(4);
    getListener(connection, "onSelection")(remoteSelection, "client2");

    expect(onPeersChange).toHaveBeenCalledWith({
      client2: new Peer("client2", { name: "Amy" }, cursorSelection(4)),
    });

    const remoteDelta2 = new Delta().retain(1).insert("dog");
    const remoteSelection2 = cursorSelection(4);
    getListener(connection, "onDelta")(
      remoteDelta2,
      remoteSelection2,
      "client3",
    );

    expect(onPeersChange).toHaveBeenCalledWith({
      client2: new Peer("client2", { name: "Amy" }, cursorSelection(7)),
    });
  });

  test("dispatches peers change on meta change", () => {
    const connection = buildMockConnection();
    connection.getClients.mockReturnValue({
      client1: { name: "Jake" },
      client2: { name: "Amy" },
    });
    connection.getClientId.mockReturnValue("client1");
    const collabClient = new CollabClient(connection, 0);

    const onPeersChange = jest.fn();
    collabClient.onPeersChange(onPeersChange);

    const newClients = {
      client1: { name: "Jake" },
      client2: { name: "Amy Santiago" },
    };
    connection.getClients.mockReturnValue(newClients);
    getListener(connection, "onClientsUpdate")(newClients);

    expect(onPeersChange).toHaveBeenCalledWith({
      client2: new Peer("client2", { name: "Amy Santiago" }, null),
    });
  });
});

function buildMockConnection() {
  return {
    onDelta: jest.fn(),
    onAcknowledgement: jest.fn(),
    onSelection: jest.fn(),
    onClientsUpdate: jest.fn(),
    destroy: jest.fn(),
    getClients: jest.fn(),
    getClientId: jest.fn(),
    sendDelta: jest.fn(),
    sendSelection: jest.fn(),
    sendRevision: jest.fn(),
    intellisenseRequest: jest.fn(),
  };
}

function getListener(connection, property) {
  const [callback] = connection[property].mock.calls[0];
  return callback;
}

function cursorSelection(pos) {
  return EditorSelection.create([EditorSelection.cursor(pos)]);
}
