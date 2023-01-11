import { EmojiButton } from "@joeattardi/emoji-button";

/**
 * A hook for the emoji picker input.
 */
const EmojiPicker = {
  mounted() {
    const picker = new EmojiButton();
    const preview = document.querySelector("#emoji-preview");
    const input = document.querySelector(".emoji-picker-input");

    picker.on("emoji", (selection) => {
      preview.innerHTML = selection.emoji;
      input.value = selection.emoji;
    });

    this.el.addEventListener("click", (_) => {
      picker.togglePicker(this.el);
    });
  },
};

export default EmojiPicker;
