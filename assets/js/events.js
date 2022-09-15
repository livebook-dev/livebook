import topbar from "topbar";

export function registerTopbar() {
  topbar.config({
    barColors: { 0: "#b2c1ff" },
    shadowColor: "rgba(0, 0, 0, .3)",
  });

  let topBarScheduled = null;

  window.addEventListener("phx:page-loading-start", () => {
    if (!topBarScheduled) {
      topBarScheduled = setTimeout(() => topbar.show(), 500);
    }
  });

  window.addEventListener("phx:page-loading-stop", () => {
    clearTimeout(topBarScheduled);
    topBarScheduled = null;
    topbar.hide();
  });
}

export function registerGlobalEventHandlers() {
  const handleFocus = (event) => {
    // The element may be about to show up via JS.show, which wraps the
    // change in requestAnimationFrame, so we do the same to make sure
    // the focus is applied only after we change the element visibility
    requestAnimationFrame(() => {
      const { detail, target } = event;
      const { to } = detail || {};
      const $target = to
        ? document.querySelector(to)
        : target;
      $target.focus();
    });
  };

  window.addEventListener("lb:focus", handleFocus);
  window.addEventListener("phx:focus", handleFocus);

  window.addEventListener("phx:select_range", (event) => {
    requestAnimationFrame(() => {
      const { detail } = event;
      const $el = document.querySelector(detail.to);
      if (detail.params) {
        $el.setSelectionRange(...detail.params);
      } else {
        $el.select();
      }
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
    const closeAll = document.querySelector(
      "#edit-sessions [name='close_all']"
    );
    disconnect.disabled = !anySessionSelected;
    closeAll.disabled = !anySessionSelected;
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
}
