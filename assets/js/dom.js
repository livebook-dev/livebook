export const morphdomOptions = {
  onBeforeElUpdated(from, to) {
    // Keep element attributes starting with data-js-
    // which we set on the client.
    for (const attr of from.attributes) {
      if (attr.name.startsWith("data-js-")) {
        to.setAttribute(attr.name, attr.value);
      }

      if (attr.name === "data-keep-attribute") {
        if (from.hasAttribute(attr.value)) {
          to.setAttribute(attr.value, from.getAttribute(attr.value));
        } else {
          to.removeAttribute(attr.value);
        }
      }
    }
  },

  onNodeAdded(node) {
    // Mimic autofocus for dynamically inserted elements
    if (node.nodeType === Node.ELEMENT_NODE && node.hasAttribute("autofocus")) {
      node.focus();

      if (node.setSelectionRange && node.value) {
        const lastIndex = node.value.length;
        node.setSelectionRange(lastIndex, lastIndex);
      }
    }
  },
};
