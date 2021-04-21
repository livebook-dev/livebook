/**
 * A hook controlling a toggleable menu.
 *
 * The element should have two children:
 *
 *   * one annotated with `data-toggle` being a clickable element
 *
 *   * one annotated with `data-content` with menu content
 */
const Menu = {
  mounted() {
    const toggleElement = this.el.querySelector("[data-toggle]");

    if (!toggleElement) {
      throw new Error("Menu must have a child with data-toggle attribute");
    }

    const contentElement = this.el.querySelector("[data-content]");

    if (!contentElement) {
      throw new Error("Menu must have a child with data-content attribute");
    }

    toggleElement.addEventListener("click", (event) => {
      if (this.el.hasAttribute("data-js-open")) {
        this.el.removeAttribute("data-js-open");
      } else {
        this.el.setAttribute("data-js-open", "true");
        // Postpone callback registration until the current click finishes bubbling.
        setTimeout(() => {
          document.addEventListener(
            "click",
            (event) => {
              this.el.removeAttribute("data-js-open");
            },
            { once: true }
          );
        }, 0);
      }
    });
  },
};

export default Menu;
