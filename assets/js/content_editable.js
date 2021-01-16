/**
 * A hook used on [contenteditable] elements to update the specified
 * attribute with the element text.
 *
 * Configuration:
 *
 *   * `data-update-attribute` - the name of the attribute to update when content changes
 */
const ContentEditable = {
  mounted() {
    this.attribute = this.el.dataset.updateAttribute;

    this.__updateAttribute();

    // Set the specified attribute on every content change
    this.el.addEventListener("input", (event) => {
      this.__updateAttribute();
    });

    // Make sure only plain text is pasted
    this.el.addEventListener("paste", (event) => {
      event.preventDefault();
      const text = event.clipboardData.getData("text/plain");
      document.execCommand("insertText", false, text);
    });
  },

  updated() {
    // The element has been re-rendered so we have to add the attribute back
    this.__updateAttribute();
  },

  __updateAttribute() {
    const value = this.el.innerText.trim();
    this.el.setAttribute(this.attribute, value);
  },
};

export default ContentEditable;
