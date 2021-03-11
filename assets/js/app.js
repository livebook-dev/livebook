import "../css/app.css";

import "phoenix_html";
import { Socket } from "phoenix";
import NProgress from "nprogress";
import { LiveSocket } from "phoenix_live_view";
import ContentEditable from "./content_editable";
import Cell from "./cell";
import Session from "./session";
import FocusOnUpdate from "./focus_on_update";
import ScrollOnUpdate from "./scroll_on_update";
import VirtualizedLines from "./virtualized_lines";

const Hooks = {
  ContentEditable,
  Cell,
  Session,
  FocusOnUpdate,
  ScrollOnUpdate,
  VirtualizedLines,
};

const csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");

const liveSocket = new LiveSocket("/live", Socket, {
  params: { _csrf_token: csrfToken },
  hooks: Hooks,
  dom: {
    onBeforeElUpdated(from, to) {
      // Keep element attributes starting with data-js-
      // which we set on the client.
      for (const attr of from.attributes) {
        if (attr.name.startsWith("data-js-")) {
          to.setAttribute(attr.name, attr.value);
        }
      }
    },
  },
});

// Show progress bar on live navigation and form submits
window.addEventListener("phx:page-loading-start", (info) => NProgress.start());
window.addEventListener("phx:page-loading-stop", (info) => NProgress.done());

// connect if there are any LiveViews on the page
liveSocket.connect();

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;
