import { createPopup } from "@picmo/popup-picker";

/**
 * A hook for the emoji picker input.
 */
const EmojiPicker = {
  mounted() {
    const button = this.el.querySelector("[data-emoji-button]");
    const preview = this.el.querySelector("[data-emoji-preview]");
    const input = this.el.querySelector("[data-emoji-input]");

    const picker = createPopup(
      { showPreview: false },
      {
        triggerElement: button,
        referenceElement: button,
        position: "bottom",
      }
    );

    picker.addEventListener("emoji:select", ({ emoji }) => {
      preview.innerHTML = emoji;
      input.value = emoji;
    });

    button.addEventListener("click", (_) => {
      picker.toggle();
    });
  },
};

export default EmojiPicker;
