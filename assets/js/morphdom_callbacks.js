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
};

export default callbacks;
