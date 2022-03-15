const DRAGGING_ATTR = "data-js-dragging";

/**
 * A hook used to highlight drop zone when dragging a file.
 */
const Dropzone = {
  mounted() {
    this.el.addEventListener("dragenter", (event) => {
      this.el.setAttribute(DRAGGING_ATTR, "");
    });

    this.el.addEventListener("dragleave", (event) => {
      if (!this.el.contains(event.relatedTarget)) {
        this.el.removeAttribute(DRAGGING_ATTR);
      }
    });

    this.el.addEventListener("drop", (event) => {
      this.el.removeAttribute(DRAGGING_ATTR);
    });
  },
};

export default Dropzone;
