import { getAttributeOrThrow } from "../lib/attribute";

/**
 * A hook adding click handler that copies text from the target
 * element to clipboard.
 *
 * Configuration:
 *
 *   * `data-target-id` - HTML id of the element whose `innerText` is copied
 */
const ClipCopy = {
  mounted() {
    this.props = getProps(this);

    this.el.addEventListener("click", (event) => {
      const target = document.getElementById(this.props.targetId);

      if (target) {
        const text = target.innerText;

        if ("clipboard" in navigator) {
          navigator.clipboard.writeText(text);
        }
      }
    });
  },

  updated() {
    this.props = getProps(this);
  },
};

function getProps(hook) {
  return {
    targetId: getAttributeOrThrow(hook.el, "data-target-id"),
  };
}

export default ClipCopy;
