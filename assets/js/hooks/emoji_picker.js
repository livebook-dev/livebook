import { createPicker } from "picmo";

/**
 * A hook for the emoji picker input.
 */
const EmojiPicker = {
  mounted() {
    const rootElement = this.el.querySelector("[data-emoji-container]");
    const preview = this.el.querySelector("[data-emoji-preview]");
    const input = this.el.querySelector("[data-emoji-input]");
    const button = this.el.querySelector("[data-emoji-button]");

    const pickerOptions = {
      rootElement,
      showSearch: false,
      showPreview: false,
    };

    const picker = createPicker(pickerOptions);

    picker.addEventListener("emoji:select", ({ emoji }) => {
      preview.innerHTML = emoji;
      input.value = emoji;
      rootElement.classList.toggle("hidden");
    });

    button.addEventListener("click", (_) => {
      rootElement.classList.toggle("hidden");
    });
  },
};

export default EmojiPicker;
