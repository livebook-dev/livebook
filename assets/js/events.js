import topbar from "topbar";

export function registerTopbar() {
  topbar.config({
    barColors: { 0: "#b2c1ff" },
    shadowColor: "rgba(0, 0, 0, .3)",
  });

  window.addEventListener("phx:page-loading-start", () => {
    topbar.show(500);
  });

  window.addEventListener("phx:page-loading-stop", () => {
    topbar.hide();
  });
}

export function registerGlobalEventHandlers() {
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
      if (event.detail.content) {
        navigator.clipboard.writeText(event.detail.content);
      } else if (event.target.tagName === "INPUT") {
        navigator.clipboard.writeText(event.target.value);
      } else {
        navigator.clipboard.writeText(event.target.textContent);
      }
    } else {
      alert(
        "Sorry, your browser does not support clipboard copy.\nThis generally requires a secure origin — either HTTPS or localhost."
      );
    }
  });

  window.addEventListener("phx:lb:exec_js", (event) => {
    const selector = event.detail.to || "body";

    document.querySelectorAll(selector).forEach((element) => {
      window.liveSocket.execJS(element, event.detail.js);
    });
  });

  window.addEventListener("lb:session_list:on_selection_change", () => {
    const anySessionSelected = !!document.querySelector(
      "[name='session_ids[]']:checked"
    );
    const disconnect = document.querySelector(
      "#edit-sessions [name='disconnect']"
    );
    const closeAll = document.querySelector(
      "#edit-sessions [name='close_all']"
    );
    disconnect.parentElement.classList.toggle(
      "pointer-events-none",
      !anySessionSelected
    );
    disconnect.parentElement.classList.toggle(
      "opacity-50",
      !anySessionSelected
    );
    closeAll.parentElement.classList.toggle(
      "pointer-events-none",
      !anySessionSelected
    );
    closeAll.parentElement.classList.toggle("opacity-50", !anySessionSelected);
  });

  window.addEventListener("contextmenu", (event) => {
    const target = event.target.closest("[data-contextmenu-trigger-click]");

    if (target) {
      event.preventDefault();
      // LV dispatches phx-click to the target of the preceding mousedown event
      target.dispatchEvent(new Event("mousedown", { bubbles: true }));
      target.dispatchEvent(new Event("click", { bubbles: true }));
    }
  });

  // Ignore submit events on elements with phx-nosubmit
  window.addEventListener(
    "submit",
    (event) => {
      if (event.target.hasAttribute("phx-nosubmit")) {
        event.preventDefault();
        event.stopPropagation();
      }
    },
    { capture: true }
  );
}

/**
 * Disables the auto-zoom behavior when focusing an input on a touch device.
 *
 * It is important that this should not prevent users from manually
 * zooming if they wish. There isn't a portable solution to this
 * problem, so this hook is a no-op if the detected device is not
 * known to behave well.
 *
 * See: https://stackoverflow.com/questions/2989263/disable-auto-zoom-in-input-text-tag-safari-on-iphone
 */
export function disableZoomOnInputFocus() {
  const isWebKit = /AppleWebKit/.test(navigator.userAgent);
  const isTouchScreen =
    "ontouchstart" in window || navigator.maxTouchPoints > 0;

  if (isWebKit && isTouchScreen) {
    const viewportTag = document.querySelector("meta[name='viewport']");

    if (viewportTag) {
      viewportTag.content += ", maximum-scale=1.0";
    }
  }
}
