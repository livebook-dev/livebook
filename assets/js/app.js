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
import { registerTopbar, registerGlobalEventHandlers } from "./events";

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
// to cookies. This is the case in Safari with the "Prevent cross-site tracking"
// option enabled, which is the default. Without cookies access, the session
// is not stored, so CSRF tokens are invalid. Consequently, LV keeps reloading
// the page, as we try to connect the socket with invalid token. To work around
// this we need to ask to explicitly grant access to cookies, as outlined in (1).
//
// (1): https://developer.mozilla.org/en-US/docs/Web/API/Storage_Access_API

if (document.hasStorageAccess) {
  document.hasStorageAccess().then((hasStorageAccess) => {
    if (hasStorageAccess) {
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
              it runs in an iframe. To make the app functional you need to grant Livebook access
              to its cookies explicitly.
            </div>
            <div>
              <button id="grant-access" class="mt-6 button-base button-blue">
                Grant access
              </button>
            </div>
          </div>
        </div>
      `;

      document.body.appendChild(overlayEl);

      const grantAccessButtonEl = overlayEl.querySelector("#grant-access");

      grantAccessButtonEl.addEventListener("click", (event) => {
        document
          .requestStorageAccess()
          .then(() => window.location.reload())
          .catch(() => console.log("Access to storage denied"));
      });
    }
  });
} else {
  connect();
}
