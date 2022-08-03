import "../css/app.css";
import "remixicon/fonts/remixicon.css";
import "katex/dist/katex.min.css";
import "@fontsource/inter";
import "@fontsource/inter/500.css";
import "@fontsource/inter/600.css";
import "@fontsource/red-hat-text";
import "@fontsource/jetbrains-mono";

import "phoenix_html";
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";

import hooks from "./hooks";
import { morphdomOptions } from "./dom";
import { loadUserData } from "./lib/user";
import { settingsStore } from "./lib/settings";
import { registerTopbar, registerGlobalEventHandlers } from "./events";

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
registerTopbar();

// Handle custom events dispatched with JS.dispatch/3
registerGlobalEventHandlers();

// Reflect global configuration in attributes to enable CSS rules
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
