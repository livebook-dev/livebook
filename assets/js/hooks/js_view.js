import {
  getAttributeOrDefault,
  getAttributeOrThrow,
  parseInteger,
} from "../lib/attribute";
import {
  isElementHidden,
  isElementVisibleInViewport,
  randomId,
  randomToken,
} from "../lib/utils";
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
 *   * `data-assets-base-path` - the base path to fetch assets from
 *     within the iframe (and resolve all relative paths against)
 *
 *   * `data-assets-cdn-url` - a URL to CDN location to fetch assets
 *     from. Only used if specified and the entrypoint script can be
 *     successfully accessed, also only when Livebook runs on https
 *
 *   * `data-js-path` - a relative path for the initial view-specific
 *     JS module
 *
 *   * `data-session-token` - a session-specific token passed when
 *     joining the JS view channel
 *
 *   * `data-connect-token` - a JS view specific token passed in the
 *     "connect" message to the channel
 *
 *   * `data-iframe-local-port` - the local port where the iframe is
 *     served
 *
 *   * `data-iframe-url` - an optional location to load the iframe from
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

    this.channel = getChannel(this.props.sessionToken);

    this.iframeActions = this.createIframe();

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

    this.iframeActions.visibilityPromise.then(() => {
      this.loadIframe();
    });

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
        connect_token: this.props.connectToken,
        ref: this.props.ref,
        id: this.id,
      },
      // If the client is very busy with executing JS we may reach the
      // default timeout of 10s, so we increase it
      30_000
    );

    this.unsubscribeFromCellEvents = globalPubSub.subscribe(
      "navigation",
      (event) => this.handleNavigationEvent(event)
    );
  },

  updated() {
    this.props = this.getProps(this);
  },

  disconnected() {
    // Reinitialize on reconnection
    this.el.removeAttribute("id");
  },

  destroyed() {
    window.removeEventListener("message", this._handleWindowMessage);

    this.iframeActions.remove();

    this.unsubscribeFromChannelEvents();
    this.channel.push("disconnect", { ref: this.props.ref });

    this.unsubscribeFromJSViewEvents();
    this.unsubscribeFromCellEvents();
  },

  getProps() {
    return {
      ref: getAttributeOrThrow(this.el, "data-ref"),
      assetsBasePath: getAttributeOrThrow(this.el, "data-assets-base-path"),
      assetsCdnUrl: getAttributeOrDefault(this.el, "data-assets-cdn-url", null),
      jsPath: getAttributeOrThrow(this.el, "data-js-path"),
      sessionToken: getAttributeOrThrow(this.el, "data-session-token"),
      connectToken: getAttributeOrThrow(this.el, "data-connect-token"),
      iframePort: getAttributeOrThrow(
        this.el,
        "data-iframe-local-port",
        parseInteger
      ),
      iframeUrl: getAttributeOrDefault(this.el, "data-iframe-url", null),
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

    // On certain events, like section/cell moved, a global event is
    // dispatched to trigger reposition. This way we don't need to
    // use deep MutationObserver, which would be expensive, especially
    // with code editor
    const unsubscribeFromJSViewsEvents = globalPubSub.subscribe(
      "js_views",
      (event) => {
        if (event.type === "reposition") {
          this.repositionIframe();
        }
      }
    );

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

    // We detect when the placeholder enters viewport and becomes visible,
    // based on that we can load the iframe contents lazily

    let viewportIntersectionObserver = null;

    const visibilityPromise = new Promise((resolve, reject) => {
      if (isElementVisibleInViewport(this.iframePlaceholder)) {
        resolve();
      } else {
        viewportIntersectionObserver = new IntersectionObserver((entries) => {
          if (isElementVisibleInViewport(this.iframePlaceholder)) {
            viewportIntersectionObserver.disconnect();
            resolve();
          }
        });
        viewportIntersectionObserver.observe(this.iframePlaceholder);
      }
    });

    // Reflect focus based on whether there is a focused parent, this
    // is later synced on "element_focused" events
    this.iframe.toggleAttribute(
      "data-js-focused",
      !!this.el.closest(`[data-js-focused]`)
    );

    // Cleanup

    const remove = () => {
      resizeObserver.disconnect();
      unsubscribeFromJSViewsEvents();
      viewportIntersectionObserver && viewportIntersectionObserver.disconnect();
      this.iframe.remove();
      this.iframePlaceholder.remove();
    };

    return { visibilityPromise, remove };
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
    initializeIframeSource(
      this.iframe,
      this.props.iframePort,
      this.props.iframeUrl
    ).then(() => {
      iframesEl.appendChild(this.iframe);
    });
  },

  handleChildMessage(message, onReady) {
    if (message.type === "ready" && !this.childReady) {
      this.getAssetsBaseUrl().then((assetsBaseUrl) => {
        this.postMessage({
          type: "readyReply",
          token: this.childToken,
          baseUrl: assetsBaseUrl,
          jsPath: this.props.jsPath,
        });

        this.childReady = true;
        onReady();
      });
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
      } else if (message.type == "selectSecret") {
        this.pushEvent("select_secret", {
          js_view_ref: this.props.ref,
          preselect_name: message.preselectName,
          options: message.options,
        });
      }
    }
  },

  getAssetsBaseUrl() {
    // Livebook may be running behind an authentication proxy, in
    // which case the internal assets URL is not accessible from
    // within the iframe (served from a different origin). To
    // workaround this, we fallback to a CDN for the assets if
    // available for the given package.
    return cachedPublicEndpointCheck().then((isPublicAccessible) => {
      if (!isPublicAccessible && this.props.assetsCdnUrl) {
        return this.props.assetsCdnUrl;
      } else {
        return window.location.origin + this.props.assetsBasePath;
      }
    });
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
    } else if (event.type == "secretSelected") {
      this.postMessage({
        type: "secretSelected",
        secretName: event.secretName,
      });
    }
  },

  handleNavigationEvent(event) {
    if (event.type === "element_focused") {
      // If a parent focusable element is focused, mirror the attribute
      // to the iframe element. This way if we need to apply style rules
      // (such as opacity) to focused elements, we can target the iframe
      // elements placed elsewhere in the DOM

      const focusableEl = this.el.closest(`[data-focusable-id]`);
      const focusableId = focusableEl ? focusableEl.dataset.focusableId : null;

      this.iframe.toggleAttribute(
        "data-js-focused",
        focusableId === event.focusableId
      );
    }
  },
};

/**
 * Checks if Livebook public endpoint is accessible without auth cookies.
 *
 * Returns a promise that resolves to a boolean. The request is sent only
 * once and the response is cached.
 */
function cachedPublicEndpointCheck() {
  cachedPublicEndpointCheck.promise =
    cachedPublicEndpointCheck.promise ||
    fetch("/public/health")
      .then((response) => response.status === 200)
      .catch((error) => false);

  return cachedPublicEndpointCheck.promise;
}

export default JSView;
