import { Socket } from "phoenix";

const csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");

const socket = new Socket("/socket", { params: { _csrf_token: csrfToken } });

let channel = null;

/**
 * Returns channel used for all JS views in the current session.
 */
export function getChannel(sessionId) {
  if (!channel) {
    socket.connect();
    channel = socket.channel("js_view", { session_id: sessionId });
    channel.join();
  }

  return channel;
}

/**
 * Leaves the JS views channel tied to the current session.
 */
export function leaveChannel() {
  if (channel) {
    channel.leave();
    channel = null;
    socket.disconnect();
  }
}

// Encoding/decoding of channel payloads

export function transportEncode(meta, payload) {
  if (
    Array.isArray(payload) &&
    payload[1] &&
    payload[1].constructor === ArrayBuffer
  ) {
    const [info, buffer] = payload;
    return encode([meta, info], buffer);
  } else {
    return { root: [meta, payload] };
  }
}

export function transportDecode(raw) {
  if (raw.constructor === ArrayBuffer) {
    const [[meta, info], buffer] = decode(raw);
    return [meta, [info, buffer]];
  } else {
    const {
      root: [meta, payload],
    } = raw;
    return [meta, payload];
  }
}

const HEADER_LENGTH = 4;

function encode(meta, buffer) {
  const encoder = new TextEncoder();
  const metaArray = encoder.encode(JSON.stringify(meta));

  const raw = new ArrayBuffer(
    HEADER_LENGTH + metaArray.byteLength + buffer.byteLength
  );
  const view = new DataView(raw);

  view.setUint32(0, metaArray.byteLength);
  new Uint8Array(raw, HEADER_LENGTH, metaArray.byteLength).set(metaArray);
  new Uint8Array(raw, HEADER_LENGTH + metaArray.byteLength).set(
    new Uint8Array(buffer)
  );

  return raw;
}

function decode(raw) {
  const view = new DataView(raw);
  const metaArrayLength = view.getUint32(0);

  const metaArray = new Uint8Array(raw, HEADER_LENGTH, metaArrayLength);
  const buffer = raw.slice(HEADER_LENGTH + metaArrayLength);

  const decoder = new TextDecoder();
  const meta = JSON.parse(decoder.decode(metaArray));

  return [meta, buffer];
}
