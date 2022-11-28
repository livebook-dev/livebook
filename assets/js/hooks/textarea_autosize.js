const BORDER_HEIGHT = 1;

/**
 * A hook that automatically matches textarea height to its content.
 */
const TextareaAutosize = {
  mounted() {
    this.autosize();

    this.el.addEventListener("input", (event) => {
      this.autosize();
    });
  },

  updated() {
    this.autosize();
  },

  autosize() {
    this.el.style.height = "0px";
    this.el.style.height = `${this.el.scrollHeight + 2 * BORDER_HEIGHT}px`;
  },
};

export default TextareaAutosize;
