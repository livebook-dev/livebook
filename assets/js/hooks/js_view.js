import { getAttributeOrThrow, parseInteger } from "../lib/attribute";
import { isElementHidden, randomId, randomToken } from "../lib/utils";
import { globalPubSub } from "../lib/pub_sub";
import {
  getChannel,
  transportDecode,
  transportEncode,
} from "./js_view/channel";
import { initializeIframeSource } from "./js_view/iframe";

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
 * ## Configuration
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
 *   * `data-timeout-message` - the message to show when the initial
 *     data does not load
 *
 */
const JSView = {
  mounted() {
    this.props = this.getProps();

    this.id = randomId();
    this.childToken = randomToken();
    this.childReadyPromise = null;
    this.childReady = false;
    this.initReceived = false;
    this.syncCallbackQueue = [];
    this.pongCallbackQueue = [];

    this.initTimeout = setTimeout(() => this.handleInitTimeout(), 2_000);

    this.channel = getChannel(this.props.sessionId);

    this.removeIframe = this.createIframe();

    // Setup child communication
    this.childReadyPromise = new Promise((resolve, reject) => {
      this._handleWindowMessage = (event) => {
        if (event.source === this.iframe.contentWindow) {
          this.handleChildMessage(event.data, resolve);
        }
      };

      window.addEventListener("message", this._handleWindowMessage);
    });

    this.hiddenInput = document.createElement("input");
    this.hiddenInput.style.display = "none";
    this.el.appendChild(this.hiddenInput);

    this.loadIframe();

    // Channel events

    const initRef = this.channel.on(
      `init:${this.props.ref}:${this.id}`,
      (raw) => {
        const [, payload] = transportDecode(raw);
        this.handleServerInit(payload);
      }
    );

    const eventRef = this.channel.on(`event:${this.props.ref}`, (raw) => {
      const [[event], payload] = transportDecode(raw);
      this.handleServerEvent(event, payload);
    });

    const errorRef = this.channel.on(
      `error:${this.props.ref}`,
      ({ message, init }) => {
        this.handleServerError(message, init);
      }
    );

    const pongRef = this.channel.on(`pong:${this.props.ref}`, () => {
      this.handleServerPong();
    });

    this.unsubscribeFromChannelEvents = () => {
      this.channel.off(`init:${this.props.ref}:${this.id}`, initRef);
      this.channel.off(`event:${this.props.ref}`, eventRef);
      this.channel.off(`error:${this.props.ref}`, errorRef);
      this.channel.off(`pong:${this.props.ref}`, pongRef);
    };

    this.unsubscribeFromJSViewEvents = globalPubSub.subscribe(
      `js_views:${this.props.ref}`,
      (event) => this.handleJSViewEvent(event)
    );

    this.channel.push(
      "connect",
      {
        session_token: this.props.sessionToken,
        ref: this.props.ref,
        id: this.id,
      },
      // If the client is very busy with executing JS we may reach the
      // default timeout of 10s, so we increase it
      30_000
    );
  },

  updated() {
    this.props = this.getProps(this);
  },

  destroyed() {
    window.removeEventListener("message", this._handleWindowMessage);

    this.removeIframe();

    this.unsubscribeFromChannelEvents();
    this.channel.push("disconnect", { ref: this.props.ref });

    this.unsubscribeFromJSViewEvents();
  },

  getProps() {
    return {
      ref: getAttributeOrThrow(this.el, "data-ref"),
      assetsBasePath: getAttributeOrThrow(this.el, "data-assets-base-path"),
      jsPath: getAttributeOrThrow(this.el, "data-js-path"),
      sessionToken: getAttributeOrThrow(this.el, "data-session-token"),
      sessionId: getAttributeOrThrow(this.el, "data-session-id"),
      iframePort: getAttributeOrThrow(
        this.el,
        "data-iframe-local-port",
        parseInteger
      ),
      timeoutMessage: getAttributeOrThrow(this.el, "data-timeout-message"),
    };
  },

  createIframe() {
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

    this.iframePlaceholder = document.createElement("div");
    this.el.appendChild(this.iframePlaceholder);

    this.iframe = document.createElement("iframe");
    this.iframe.className = "w-full h-0 absolute z-[1]";

    const notebookEl = document.querySelector(`[data-el-notebook]`);
    const notebookContentEl = notebookEl.querySelector(
      `[data-el-notebook-content]`
    );

    // Most placeholder position changes are accompanied by changes to the
    // notebook content element height (adding cells, inserting newlines
    // in the editor, etc). On the other hand, toggling the sidebar or
    // resizing the window changes the width, however the notebook
    // content element doesn't span full width, so this change may not
    // be detected, that's why we observe the full-width parent element
    const resizeObserver = new ResizeObserver((entries) => {
      this.repositionIframe();
    });
    resizeObserver.observe(notebookContentEl);
    resizeObserver.observe(notebookEl);

    // On lower level cell/section reordering is applied as element
    // removal followed by insert, consequently the intersection
    // between the placeholder and notebook content changes (becomes
    // none for a brief moment)
    const intersectionObserver = new IntersectionObserver(
      (entries) => {
        this.repositionIframe();
      },
      { root: notebookContentEl }
    );
    intersectionObserver.observe(this.iframePlaceholder);

    // Emulate mouse enter and leave on the placeholder. Note that we
    // intentionally use bubbling to notify all parents that may have
    // listeners on themselves

    this.iframe.addEventListener("mouseenter", (event) => {
      this.iframePlaceholder.dispatchEvent(
        new MouseEvent("mouseenter", { bubbles: true })
      );
    });

    this.iframe.addEventListener("mouseleave", (event) => {
      this.iframePlaceholder.dispatchEvent(
        new MouseEvent("mouseleave", { bubbles: true })
      );
    });

    return () => {
      resizeObserver.disconnect();
      intersectionObserver.disconnect();
      this.iframe.remove();
    };
  },

  repositionIframe() {
    const { iframe, iframePlaceholder } = this;
    const notebookEl = document.querySelector(`[data-el-notebook]`);

    if (isElementHidden(iframePlaceholder)) {
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
  },

  loadIframe() {
    const iframesEl = document.querySelector(`[data-el-js-view-iframes]`);
    initializeIframeSource(this.iframe, this.props.iframePort).then(() => {
      iframesEl.appendChild(this.iframe);
    });
  },

  handleChildMessage(message, onReady) {
    if (message.type === "ready" && !this.childReady) {
      const assetsBaseUrl = window.location.origin + this.props.assetsBasePath;

      this.postMessage({
        type: "readyReply",
        token: this.childToken,
        baseUrl: assetsBaseUrl,
        jsPath: this.props.jsPath,
      });

      this.childReady = true;
      onReady();
    } else {
      // Note: we use a random token to authorize child messages
      // and do our best to make this token unavailable for the
      // injected script on the child side. In the worst case scenario,
      // the script manages to extract the token and can then send
      // any of those messages, so we can treat this as a possible
      // surface for attacks. In this case the most "critical" actions
      // are shortcuts, neither of which is particularly dangerous.
      if (message.token !== this.childToken) {
        throw new Error("Token mismatch");
      }

      if (message.type === "resize") {
        this.iframePlaceholder.style.height = `${message.height}px`;
        this.iframe.style.height = `${message.height}px`;
      } else if (message.type === "domEvent") {
        // Replicate the child events on the current element,
        // so that they are detected upstream in the session hook
        const event = this.replicateDomEvent(message.event);
        if (message.isTargetEditable) {
          this.hiddenInput.dispatchEvent(event);
        } else {
          this.el.dispatchEvent(event);
        }
      } else if (message.type === "event") {
        const { event, payload } = message;
        const raw = transportEncode([event, this.props.ref], payload);
        this.channel.push("event", raw);
      } else if (message.type === "syncReply") {
        this.pongCallbackQueue.push(this.syncCallbackQueue.shift());
        this.channel.push("ping", { ref: this.props.ref });
      }
    }
  },

  postMessage(message) {
    this.iframe.contentWindow.postMessage(message, "*");
  },

  replicateDomEvent(event) {
    if (event.type === "focus") {
      return new FocusEvent("focus");
    } else if (event.type === "mousedown") {
      return new MouseEvent("mousedown", { bubbles: true });
    } else if (event.type === "keydown") {
      return new KeyboardEvent(event.type, event.props);
    }
  },

  handleInitTimeout() {
    this.initTimeoutContainer = document.createElement("div");
    this.initTimeoutContainer.classList.add("info-box");
    this.el.prepend(this.initTimeoutContainer);
    this.initTimeoutContainer.textContent = this.props.timeoutMessage;
  },

  clearInitTimeout() {
    clearTimeout(this.initTimeout);

    if (this.initTimeoutContainer) {
      this.initTimeoutContainer.remove();
    }
  },

  handleServerInit(payload) {
    this.clearInitTimeout();
    this.initReceived = true;

    this.childReadyPromise.then(() => {
      this.postMessage({ type: "init", data: payload });
    });
  },

  handleServerEvent(event, payload) {
    if (!this.initReceived) {
      return;
    }

    this.childReadyPromise.then(() => {
      this.postMessage({ type: "event", event, payload });
    });
  },

  handleServerError(message, init) {
    if (init) {
      this.clearInitTimeout();
    }

    if (!this.errorContainer) {
      this.errorContainer = document.createElement("div");
      this.errorContainer.classList.add("error-box", "mb-4");
      this.el.prepend(this.errorContainer);
    }

    this.errorContainer.textContent = message;
  },

  handleServerPong() {
    const callback = this.pongCallbackQueue.shift();
    callback();
  },

  handleJSViewEvent(event) {
    if (event.type === "sync") {
      // First, we invoke optional synchronization callback in the iframe,
      // that may send any deferred UI changes to the server. Then, we
      // do a ping to synchronize with the server
      this.syncCallbackQueue.push(event.callback);
      this.postMessage({ type: "sync" });
    }
  },
};

export default JSView;
