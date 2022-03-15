import "../css/app.css";
import "remixicon/fonts/remixicon.css";
import "katex/dist/katex.min.css";
import "@fontsource/inter";
import "@fontsource/inter/500.css";
import "@fontsource/inter/600.css";
import "@fontsource/jetbrains-mono";

import "phoenix_html";
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import topbar from "topbar";

import hooks from "./hooks";
import { morphdomOptions } from "./dom";
import { loadUserData } from "./lib/user";
import { settingsStore } from "./lib/settings";

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
  dom: morphdomOptions,
});

// Show progress bar on live navigation and form submits
topbar.config({
  barColors: { 0: "#b2c1ff" },
  shadowColor: "rgba(0, 0, 0, .3)",
});

let topBarScheduled = null;

window.addEventListener("phx:page-loading-start", () => {
  if (!topBarScheduled) {
    topBarScheduled = setTimeout(() => topbar.show(), 200);
  }
});

window.addEventListener("phx:page-loading-stop", () => {
  clearTimeout(topBarScheduled);
  topBarScheduled = null;
  topbar.hide();
});

// Handling custom events dispatched with JS.dispatch/3

window.addEventListener("lb:focus", (event) => {
  // The element may be about to show up via JS.show, which wraps the
  // change in requestAnimationFrame, so we do the same to make sure
  // the focus is applied only after we change the element visibility
  requestAnimationFrame(() => {
    event.target.focus();
  });
});

window.addEventListener("lb:set_value", (event) => {
  event.target.value = event.detail.value;
});

window.addEventListener("lb:check", (event) => {
  event.target.checked = true;
});

window.addEventListener("lb:uncheck", (event) => {
  event.target.checked = false;
});

window.addEventListener("lb:set_text", (event) => {
  event.target.textContent = event.detail.value;
});

window.addEventListener("lb:clipcopy", (event) => {
  if ("clipboard" in navigator) {
    const text = event.target.textContent;
    navigator.clipboard.writeText(text);
  } else {
    alert(
      "Sorry, your browser does not support clipboard copy.\nThis generally requires a secure origin — either HTTPS or localhost."
    );
  }
});

window.addEventListener("lb:session_list:on_selection_change", () => {
  const anySessionSelected = !!document.querySelector(
    "[name='session_ids[]']:checked"
  );
  const disconnect = document.querySelector(
    "#edit-sessions [name='disconnect']"
  );
  const closeAll = document.querySelector("#edit-sessions [name='close_all']");
  disconnect.disabled = !anySessionSelected;
  closeAll.disabled = !anySessionSelected;
});

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

// Connect if there are any LiveViews on the page
liveSocket.connect();

// Expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000) // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;
