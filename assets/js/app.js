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
import { loadAppAuthToken } from "./lib/app";
import { settingsStore } from "./lib/settings";
import {
  registerTopbar,
  registerGlobalEventHandlers,
  disableZoomOnInputFocus,
} from "./events";
import { cookieOptions } from "./lib/utils";
import {
  loadConfirmOptOutIds,
  registerGlobalEventHandlersForConfirm,
} from "./confirm";

function connect() {
  const csrfToken = document
    .querySelector("meta[name='csrf-token']")
    .getAttribute("content");

  const liveSocket = new LiveSocket(
    window.LIVEBOOK_BASE_URL_PATH + "/live",
    Socket,
    {
      params: (liveViewName) => {
        return {
          _csrf_token: csrfToken,
          // Pass the most recent user data to the LiveView in `connect_params`
          user_data: loadUserData(),
          app_auth_token: loadAppAuthToken(),
          confirm_opt_out_ids: loadConfirmOptOutIds(),
        };
      },
      hooks: hooks,
      dom: morphdomOptions,
    }
  );

  // Show progress bar on live navigation and form submits
  registerTopbar();

  // Handle custom events dispatched with JS.dispatch/3
  registerGlobalEventHandlers();

  registerGlobalEventHandlersForConfirm();

  disableZoomOnInputFocus();

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
}

// When Livebook runs in a cross-origin iframe the browser may restrict access
// to cookies. Without cookies access, the session is not stored, so CSRF tokens
// are invalid. Consequently, LV keeps reloading the page, as we try to connect
// the socket with invalid token. To work around this we tell the user to open
// Livebook outside the iframe.
//
// The behaviour varies across browsers and browsing modes (regular and private).
// A few examples (at the time of writing):
//
//   * Safari by default blocks all cross-origin cookies. This is controlled by
//     the "Prevent cross-site tracking" option
//
//   * Chrome in incognito mode blocks all cross-origin cookies, can be relaxed
//     on per-site basis
//
//   * Firefox implements state partitioning (1) and it is enabled for storage
//     by default since Firefox 103 (2). With storage partitioning, the embedded
//     site gets a separate storage bucket scoped by the top-level origin, so
//     the site generally works as expected
//
//   * Brave also implements storage partitioning (3)
//
// To detect whether cookies are allowed, we check if we can programmatically
// set a cookie.
//
// Also see the proposal (4), which may streamline this in the future.
//
// (1): https://developer.mozilla.org/en-US/docs/Web/Privacy/State_Partitioning#state_partitioning
// (2): https://www.mozilla.org/en-US/firefox/103.0/releasenotes
// (3): https://brave.com/privacy-updates/7-ephemeral-storage
// (4): https://github.com/privacycg/CHIPS

if (hasCookiesAccess()) {
  connect();
} else {
  const overlayEl = document.createElement("div");

  overlayEl.innerHTML = `
    <div class="fixed top-0 bottom-0 left-0 right-0 z-[1000] px-4 py-8 bg-gray-900/95 flex justify-center items-center">
      <div class="max-w-[600px] w-full flex flex-col">
        <div class="text-xl text-gray-100 font-medium">
          Action required
        </div>
        <div class="mt-3 text-sm text-gray-300">
          It looks like Livebook does not have access to cookies. This usually happens when
          it runs in an iframe. To make sure the app is fully functional open it in a new
          tab directly. Alternatively you can relax security settings for this page to allow
          third-party cookies.
        </div>
        <div class="mt-6">
          <a id="open-app" class="button-base button-blue" target="_blank">
            Open app
          </a>
        </div>
      </div>
    </div>
  `;

  overlayEl.querySelector("#open-app").href = window.location;

  document.body.appendChild(overlayEl);
}

function hasCookiesAccess() {
  document.cookie = `lb:probe_cookie=;path=/${cookieOptions()}`;

  return document.cookie
    .split("; ")
    .some((cookie) => cookie.startsWith(`lb:probe_cookie=`));
}
