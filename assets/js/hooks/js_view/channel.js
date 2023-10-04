import { Socket } from "phoenix";
import { decodeAnnotatedBuffer, encodeAnnotatedBuffer } from "../../lib/codec";

const csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");

const socket = new Socket(window.LIVEBOOK_BASE_URL_PATH + "/socket", {
  params: { _csrf_token: csrfToken },
});

let channel = null;

/**
 * Returns channel used for all JS views in the current session.
 */
export function getChannel(sessionToken) {
  if (!channel) {
    socket.connect();
    channel = socket.channel("js_view", { session_token: sessionToken });
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
    return encodeAnnotatedBuffer([meta, info], buffer);
  } else {
    return { root: [meta, payload] };
  }
}

export function transportDecode(raw) {
  if (raw.constructor === ArrayBuffer) {
    const [[meta, info], buffer] = decodeAnnotatedBuffer(raw);
    return [meta, [info, buffer]];
  } else {
    const {
      root: [meta, payload],
    } = raw;
    return [meta, payload];
  }
}
