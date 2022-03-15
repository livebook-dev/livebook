import { Socket } from "phoenix";
import { getAttributeOrThrow, parseInteger } from "../lib/attribute";
import { randomToken, sha256Base64 } from "../lib/utils";

/**
 * A hook used to render a runtime-connected JavaScript view.
 *
 * JavaScript view is an abstraction for extending Livebook with
 * custom capabilities. In particular, it is the primary building
 * block for defining custom interactive output types, such as plots
 * and maps.
 *
 * The JavaScript is defined by the user, so we sandbox the script
 * execution inside an iframe.
 *
 * The hook connects to a dedicated channel, sending the token and
 * view ref in an initial message. It expects `init:<ref>` message
 * with `{ data }` payload, the data is then used in the initial call
 * to the custom JS module.
 *
 * Then, a number of `event:<ref>` with `{ event, payload }` payload
 * can be sent. The `event` is forwarded to the initialized component.
 *
 * Configuration:
 *
 *   * `data-ref` - a unique identifier used as messages scope
 *
 *   * `data-assets-base-path` - the path to resolve all relative paths
 *     against in the iframe
 *
 *   * `data-js-path` - a relative path for the initial view-specific
 *     JS module
 *
 *   * `data-session-token` - token is sent in the "connect" message
 *     to the channel
 *
 *   * `data-session-id` - the identifier of the session that this
 *     view belongs go
 *
 *   * `data-iframe-local-port` - the local port where the iframe is
 *     served
 *
 */
const JSView = {
  mounted() {
    this.props = getProps(this);
    this.state = {
      childToken: randomToken(),
      childReadyPromise: null,
      childReady: false,
      iframe: null,
      channelUnsubscribe: null,
      errorContainer: null,
    };

    const channel = getChannel(this.props.sessionId);

    // When cells/sections are reordered, morphdom detaches and attaches
    // the relevant elements in the DOM. JS view is generally rendered
    // inside cells, so when reordering happens it becomes temporarily
    // detached from the DOM and attaching it back would cause the iframe
    // to reload. This behaviour is expected, as discussed in (1). Reloading
    // that frequently is inefficient and also clears the iframe state,
    // which makes is very undesired in our case. To solve this, we insert
    // the iframe higher in the DOM tree, so that it's never affected by
    // reordering. Then, we insert a placeholder element in the output to
    // take up the expected space and we use absolute positioning to place
    // the iframe exactly over that placeholder. We set up observers to
    // track the changes in placeholder's position/size and we keep the
    // absolute iframe in sync.
    //
    // (1): https://github.com/whatwg/html/issues/5484

    const iframePlaceholder = document.createElement("div");
    this.el.appendChild(iframePlaceholder);

    const iframe = document.createElement("iframe");
    iframe.className = "w-full h-0 absolute z-[1]";
    this.state.iframe = iframe;

    this.disconnectObservers = bindIframeSize(iframe, iframePlaceholder);

    // Emulate mouse enter and leave on the placeholder. Note that we
    // intentionally use bubbling to notify all parents that may have
    // listeners on themselves

    iframe.addEventListener("mouseenter", (event) => {
      iframePlaceholder.dispatchEvent(
        new MouseEvent("mouseenter", { bubbles: true })
      );
    });

    iframe.addEventListener("mouseleave", (event) => {
      iframePlaceholder.dispatchEvent(
        new MouseEvent("mouseleave", { bubbles: true })
      );
    });

    // Register message chandler to communicate with the iframe

    function postMessage(message) {
      iframe.contentWindow.postMessage(message, "*");
    }

    this.state.childReadyPromise = new Promise((resolve, reject) => {
      this.handleWindowMessage = (event) => {
        if (event.source === iframe.contentWindow) {
          handleChildMessage(event.data);
        }
      };

      window.addEventListener("message", this.handleWindowMessage);

      const handleChildMessage = (message) => {
        if (message.type === "ready" && !this.state.childReady) {
          const assetsBaseUrl =
            window.location.origin + this.props.assetsBasePath;
          postMessage({
            type: "readyReply",
            token: this.state.childToken,
            baseUrl: assetsBaseUrl,
            jsPath: this.props.jsPath,
          });
          this.state.childReady = true;
          resolve();
        } else {
          // Note: we use a random token to authorize child messages
          // and do our best to make this token unavailable for the
          // injected script on the child side. In the worst case scenario,
          // the script manages to extract the token and can then send
          // any of those messages, so we can treat this as a possible
          // surface for attacks. In this case the most "critical" actions
          // are shortcuts, neither of which is particularly dangerous.
          if (message.token !== this.state.childToken) {
            throw new Error("Token mismatch");
          }

          if (message.type === "resize") {
            iframePlaceholder.style.height = `${message.height}px`;
            iframe.style.height = `${message.height}px`;
          } else if (message.type === "domEvent") {
            // Replicate the child events on the current element,
            // so that they are detected upstream in the session hook
            const event = replicateDomEvent(message.event);
            this.el.dispatchEvent(event);
          } else if (message.type === "event") {
            const { event, payload } = message;
            const raw = transportEncode([event, this.props.ref], payload);
            channel.push("event", raw);
          }
        }
      };

      const replicateDomEvent = (event) => {
        if (event.type === "focus") {
          return new FocusEvent("focus");
        } else if (event.type === "mousedown") {
          return new MouseEvent("mousedown", { bubbles: true });
        } else if (event.type === "keydown") {
          return new KeyboardEvent(event.type, event.props);
        }
      };
    });

    // Load the iframe content
    const iframesEl = document.querySelector(
      `[data-element="js-view-iframes"]`
    );
    initializeIframeSource(iframe, this.props.iframePort).then(() => {
      iframesEl.appendChild(iframe);
    });

    // Event handlers

    const initRef = channel.on(`init:${this.props.ref}`, (raw) => {
      const [, payload] = transportDecode(raw);

      this.state.childReadyPromise.then(() => {
        postMessage({ type: "init", data: payload });
      });
    });

    const eventRef = channel.on(`event:${this.props.ref}`, (raw) => {
      const [[event], payload] = transportDecode(raw);

      this.state.childReadyPromise.then(() => {
        postMessage({ type: "event", event, payload });
      });
    });

    const errorRef = channel.on(`error:${this.props.ref}`, ({ message }) => {
      if (!this.state.errorContainer) {
        this.state.errorContainer = document.createElement("div");
        this.state.errorContainer.classList.add("error-box", "mb-4");
        this.el.prepend(this.state.errorContainer);
      }

      this.state.errorContainer.textContent = message;
    });

    this.state.channelUnsubscribe = () => {
      channel.off(`init:${this.props.ref}`, initRef);
      channel.off(`event:${this.props.ref}`, eventRef);
      channel.off(`error:${this.props.ref}`, errorRef);
    };

    channel.push("connect", {
      session_token: this.props.sessionToken,
      ref: this.props.ref,
    });
  },

  updated() {
    this.props = getProps(this);
  },

  destroyed() {
    window.removeEventListener("message", this.handleWindowMessage);
    this.disconnectObservers();
    this.state.iframe.remove();

    const channel = getChannel(this.props.sessionId, { create: false });

    if (channel) {
      this.state.channelUnsubscribe();
      channel.push("disconnect", { ref: this.props.ref });
    }
  },
};

function getProps(hook) {
  return {
    ref: getAttributeOrThrow(hook.el, "data-ref"),
    assetsBasePath: getAttributeOrThrow(hook.el, "data-assets-base-path"),
    jsPath: getAttributeOrThrow(hook.el, "data-js-path"),
    sessionToken: getAttributeOrThrow(hook.el, "data-session-token"),
    sessionId: getAttributeOrThrow(hook.el, "data-session-id"),
    iframePort: getAttributeOrThrow(
      hook.el,
      "data-iframe-local-port",
      parseInteger
    ),
  };
}

const csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");
const socket = new Socket("/socket", { params: { _csrf_token: csrfToken } });

let channel = null;

/**
 * Returns channel used for all JS views in the current session.
 */
function getChannel(sessionId, { create = true } = {}) {
  if (!channel && create) {
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

/**
 * Sets up observers to resize and reposition the iframe
 * whenever the placeholder moves.
 */
function bindIframeSize(iframe, iframePlaceholder) {
  const notebookEl = document.querySelector(`[data-element="notebook"]`);
  const notebookContentEl = notebookEl.querySelector(
    `[data-element="notebook-content"]`
  );

  function repositionIframe() {
    if (iframePlaceholder.offsetParent === null) {
      // When the placeholder is hidden, we hide the iframe as well
      iframe.classList.add("hidden");
    } else {
      iframe.classList.remove("hidden");
      const notebookBox = notebookEl.getBoundingClientRect();
      const placeholderBox = iframePlaceholder.getBoundingClientRect();
      const top = placeholderBox.top - notebookBox.top + notebookEl.scrollTop;
      iframe.style.top = `${top}px`;
      const left =
        placeholderBox.left - notebookBox.left + notebookEl.scrollLeft;
      iframe.style.left = `${left}px`;
      iframe.style.height = `${placeholderBox.height}px`;
      iframe.style.width = `${placeholderBox.width}px`;
    }
  }

  // Most placeholder position changes are accompanied by changes to the
  // notebook content element height (adding cells, inserting newlines
  // in the editor, etc). On the other hand, toggling the sidebar or
  // resizing the window changes the width, however the notebook
  // content element doesn't span full width, so this change may not
  // be detected, that's why we observe the full-width parent element
  const resizeObserver = new ResizeObserver((entries) => repositionIframe());
  resizeObserver.observe(notebookContentEl);
  resizeObserver.observe(notebookEl);

  // On lower level cell/section reordering is applied as element
  // removal followed by insert, consequently the intersection
  // between the placeholder and notebook content changes (becomes
  // none for a brief moment)
  const intersectionObserver = new IntersectionObserver(
    (entries) => repositionIframe(),
    { root: notebookContentEl }
  );
  intersectionObserver.observe(iframePlaceholder);

  return () => {
    resizeObserver.disconnect();
    intersectionObserver.disconnect();
  };
}

// Loading iframe using `srcdoc` disables cookies and browser APIs,
// such as camera and microphone (1), the same applies to `src` with
// data URL, so we need to load the iframe through a regular request.
// Since the iframe is sandboxed we also need `allow-same-origin`.
// Additionally, we cannot load the iframe from the same origin as
// the app, because using `allow-same-origin` together with `allow-scripts`
// would be insecure (2). Consequently, we need to load the iframe
// from a different origin.
//
// When running Livebook on https:// we load the iframe from another
// https:// origin. On the other hand, when running on http:// we want
// to load the iframe from http:// as well, otherwise the browser could
// block asset requests from the https:// iframe to http:// Livebook.
// However, external http:// content is not considered a secure context (3),
// which implies no access to user media. Therefore, instead of using
// http://livebook.space we use another localhost endpoint. Note that
// this endpoint has a different port than the Livebook web app, that's
// because we need separate origins, as outlined above.
//
// To ensure integrity of the loaded content we manually verify the
// checksum against the expected value.
//
// (1): https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia#document_source_security
// (2): https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe#attr-sandbox
// (3): https://developer.mozilla.org/en-US/docs/Web/Security/Secure_Contexts

const IFRAME_SHA256 = "+uJyGu0Ey7uVV7WwRwg7GyjwCkMNRBnyNc25iGFpYXc=";

function getIframeUrl(iframePort) {
  return window.location.protocol === "https:"
    ? "https://livebook.space/iframe/v2.html"
    : `http://${window.location.hostname}:${iframePort}/iframe/v2.html`;
}

function initializeIframeSource(iframe, iframePort) {
  const iframeUrl = getIframeUrl(iframePort);

  return verifyIframeSource(iframeUrl).then(() => {
    iframe.sandbox =
      "allow-scripts allow-same-origin allow-downloads allow-modals";
    iframe.allow =
      "accelerometer; ambient-light-sensor; camera; display-capture; encrypted-media; geolocation; gyroscope; microphone; midi; usb; xr-spatial-tracking";
    iframe.src = iframeUrl;
  });
}

let iframeVerificationPromise = null;

function verifyIframeSource(iframeUrl) {
  if (!iframeVerificationPromise) {
    iframeVerificationPromise = fetch(iframeUrl)
      .then((response) => response.text())
      .then((html) => {
        if (sha256Base64(html) !== IFRAME_SHA256) {
          throw new Error(
            "The loaded iframe content doesn't have the expected checksum"
          );
        }
      });
  }

  return iframeVerificationPromise;
}

// Encoding/decoding of channel payloads

function transportEncode(meta, payload) {
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

function transportDecode(raw) {
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

export default JSView;
