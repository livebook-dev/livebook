const callbacks = {
  onBeforeElUpdated(from, to) {
    // Keep element attributes starting with data-js-
    // which we set on the client.
    for (const attr of from.attributes) {
      if (attr.name.startsWith("data-js-")) {
        to.setAttribute(attr.name, attr.value);
      }
    }
  },
  onNodeAdded(node) {
    if (node.nodeType === Node.ELEMENT_NODE) {
      if (node.getAttribute("data-element") === "menu-toggle") {
        initializeMenuToggle(node);
      }
    }
  },
};

function initializeMenuToggle(element) {
  element.addEventListener("click", (event) => {
    const menu = element.nextElementSibling;

    if (menu.getAttribute("data-element") === "menu") {
      if (menu.hasAttribute("data-js-shown")) {
        menu.removeAttribute("data-js-shown");
      } else {
        menu.setAttribute("data-js-shown", "true");
        // Postpone callback registration until the current click finishes bubbling.
        setTimeout(() => {
          document.addEventListener(
            "click",
            (event) => {
              menu.removeAttribute("data-js-shown");
            },
            { once: true }
          );
        }, 0);
      }
    }
  });
}

export default callbacks;
