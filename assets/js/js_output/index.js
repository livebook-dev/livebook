import { getAttributeOrThrow } from "../lib/attribute";
import { randomToken } from "../lib/utils";

import iframeHtml from "./iframe.html";

/**
 * A hook used to render JS-enabled cell output.
 *
 * The JavaScript is defined by the user, so we sandbox the script
 * execution inside an iframe.
 *
 * The hook expects `js_output:<id>:init` event with `{ data }` payload,
 * the data is then used in the initial call to the custom JS module.
 *
 * Then, a number of `js_output:<id>:event` with `{ event }` payload can
 * be sent. The `event` is forwarded to the initialized component.
 *
 * Configuration:
 *
 *   * `data-id` - a unique identifier used as messages scope
 *
 *   * `data-assets-base-url` - the URL to resolve all relative paths
 *     against in the iframe
 *
 *   * `data-js-path` - a relative path for the initial output-specific
 *     JS module
 *
 */
const JSOutput = {
  mounted() {
    this.props = getProps(this);
    this.state = {
      token: randomToken(),
      childReadyPromise: null,
      childReady: false,
      iframe: null,
    };

    const iframePlaceholder = document.createElement("div");
    const iframe = document.createElement("iframe");
    this.state.iframe = iframe;

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
          postMessage({
            type: "readyReply",
            token: this.state.token,
            baseUrl: this.props.assetsBaseUrl,
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
          if (message.token !== this.state.token) {
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
            this.pushEvent("event", { event, payload });
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

    // When cells/sections are reordered, morphdom detaches and attaches
    // the relevant elements in the DOM. Consequently the output element
    // becomes temporarily detached from the DOM and attaching it back
    // would cause the iframe to reload. This behaviour is expected, see
    // https://github.com/whatwg/html/issues/5484 for more details. Reloading
    // that frequently is inefficient and also clears the iframe state,
    // which makes is very undesired in our case. To solve this, we insert
    // the iframe higher in the DOM tree, so that it's never affected by
    // reordering. Then, we insert a placeholder element in the output to
    // take up the expected space and we use absolute positioning to place
    // the iframe exactly over that placeholder. We set up observers to
    // track the changes in placeholder's position/size and we keep the
    // absolute iframe in sync.

    const notebookEl = document.querySelector(`[data-element="notebook"]`);
    const notebookContentEl = notebookEl.querySelector([
      `[data-element="notebook-content"]`,
    ]);
    const iframesEl = notebookEl.querySelector(
      `[data-element="output-iframes"]`
    );

    iframe.className = "w-full h-0 absolute z-[1]";
    // Note that we use `srcdoc`, so the iframe has the same origin as the
    // parent. For this reason we intentionally don't use allow-same-origin,
    // as it would allow the iframe to effectively access the parent window.
    iframe.sandbox = "allow-scripts";
    iframe.srcdoc = iframeHtml;

    iframesEl.appendChild(iframe);
    this.el.appendChild(iframePlaceholder);

    function repositionIframe() {
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

    // Most output position changes are accompanied by changes to the
    // notebook content element (adding cells, inserting newlines in
    // the editor, etc)
    this.resizeObserver = new ResizeObserver((entries) => repositionIframe());
    this.resizeObserver.observe(notebookContentEl);

    // On lower level cell/section reordering is applied as element
    // removal followed by insert, consequently the intersection
    // between the output and notebook content changes (becomes none
    // for a brief moment)
    this.intersectionObserver = new IntersectionObserver(
      (entries) => repositionIframe(),
      { root: notebookContentEl }
    );
    this.intersectionObserver.observe(iframePlaceholder);

    // Event handlers

    this.handleEvent(`js_output:${this.props.id}:init`, ({ data }) => {
      this.state.childReadyPromise.then(() => {
        postMessage({ type: "init", data });
      });
    });

    this.handleEvent(
      `js_output:${this.props.id}:event`,
      ({ event, payload }) => {
        this.state.childReadyPromise.then(() => {
          postMessage({ type: "event", event, payload });
        });
      }
    );
  },

  updated() {
    this.props = getProps(this);
  },

  destroyed() {
    window.removeEventListener("message", this.handleWindowMessage);
    this.resizeObserver.disconnect();
    this.intersectionObserver.disconnect();
    this.state.iframe.remove();
  },
};

function getProps(hook) {
  return {
    id: getAttributeOrThrow(hook.el, "data-id"),
    assetsBaseUrl: getAttributeOrThrow(hook.el, "data-assets-base-url"),
    jsPath: getAttributeOrThrow(hook.el, "data-js-path"),
  };
}

export default JSOutput;
