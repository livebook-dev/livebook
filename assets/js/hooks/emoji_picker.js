import { createPicker } from "picmo";

/**
 * A hook for the emoji picker input.
 */
const EmojiPicker = {
  mounted() {
    const rootElement = document.querySelector("#emoji-picker-container");
    const preview = document.querySelector("#emoji-preview");
    const input = document.querySelector(".emoji-picker-input");

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

    this.el.addEventListener("click", (_) => {
      rootElement.classList.toggle("hidden");
    });
  },
};

export default EmojiPicker;
