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
import Headline from "./headline";
import Cell from "./cell";
import Session from "./session";
import FocusOnUpdate from "./focus_on_update";
import ScrollOnUpdate from "./scroll_on_update";
import VirtualizedLines from "./virtualized_lines";
import UserForm from "./user_form";
import EditorSettings from "./editor_settings";
import VegaLite from "./vega_lite";
import Timer from "./timer";
import MarkdownRenderer from "./markdown_renderer";
import Highlight from "./highlight";
import DragAndDrop from "./drag_and_drop";
import PasswordToggle from "./password_toggle";
import KeyboardControl from "./keyboard_control";
import morphdomCallbacks from "./morphdom_callbacks";
import JSOutput from "./js_output";
import { loadUserData } from "./lib/user";
import { settingsStore } from "./lib/settings";

const hooks = {
  Headline,
  Cell,
  Session,
  FocusOnUpdate,
  ScrollOnUpdate,
  VirtualizedLines,
  UserForm,
  EditorSettings,
  VegaLite,
  Timer,
  MarkdownRenderer,
  Highlight,
  DragAndDrop,
  PasswordToggle,
  KeyboardControl,
  JSOutput,
};

const csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");

const liveSocket = new LiveSocket("/live", Socket, {
  params: (liveViewName) => {
    return {
      _csrf_token: csrfToken,
      // Pass the most recent user data to the LiveView in `connect_params`
      user_data: loadUserData(),
    };
  },
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

// Handling custom events dispatched with JS.dispatch/3

window.addEventListener("lb:focus", (event) => {
  event.target.focus();
});

window.addEventListener("lb:set_value", (event) => {
  event.target.value = event.detail.value;
});

if ("clipboard" in navigator) {
  window.addEventListener("lb:clipcopy", (event) => {
    const text = event.target.textContent;
    navigator.clipboard.writeText(text);
  });
}

// Other global handlers

window.addEventListener("contextmenu", (event) => {
  const target = event.target.closest("[data-contextmenu-trigger-click]");

  if (target) {
    event.preventDefault();
    target.dispatchEvent(new Event("click", { bubbles: true }));
  }
});

// Global configuration

settingsStore.getAndSubscribe((settings) => {
  document.body.setAttribute("data-editor-theme", settings.editor_theme);
});
