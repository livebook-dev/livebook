import "../css/app.css";
import "remixicon/fonts/remixicon.css";
import "katex/dist/katex.min.css";

import "@fontsource/inter";
import "@fontsource/inter/500.css";
import "@fontsource/inter/600.css";
import "@fontsource/jetbrains-mono";

import "phoenix_html";
import { Socket } from "phoenix";
import topbar from "topbar";
import { LiveSocket } from "phoenix_live_view";
import ContentEditable from "./content_editable";
import Cell from "./cell";
import Session from "./session";
import FocusOnUpdate from "./focus_on_update";
import ScrollOnUpdate from "./scroll_on_update";
import VirtualizedLines from "./virtualized_lines";
import Menu from "./menu";
import morphdomCallbacks from "./morphdom_callbacks";

const hooks = {
  ContentEditable,
  Cell,
  Session,
  FocusOnUpdate,
  ScrollOnUpdate,
  VirtualizedLines,
  Menu,
};

const csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");

const liveSocket = new LiveSocket("/live", Socket, {
  params: { _csrf_token: csrfToken },
  hooks: hooks,
  dom: morphdomCallbacks,
});

// Show progress bar on live navigation and form submits
topbar.config({
  barColors: { 0: "#b2c1ff" },
  shadowColor: "rgba(0, 0, 0, .3)",
});
window.addEventListener("phx:page-loading-start", () => topbar.show());
window.addEventListener("phx:page-loading-stop", () => topbar.hide());

// connect if there are any LiveViews on the page
liveSocket.connect();

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;
