import { getAttributeOrDefault, parseBoolean } from "../lib/attribute";

/**
 * A hook controlling a toggleable menu.
 *
 * Configuration:
 *
 *   * `data-primary` - a boolean indicating whether to open on the primary
 *     click or the secondary click (like right mouse button click). Defaults to `true`.
 *
 * The element should have two children:
 *
 *   * one annotated with `data-toggle` being a clickable element
 *
 *   * one annotated with `data-content` with menu content
 */
const Menu = {
  mounted() {
    this.props = getProps(this);

    const toggleElement = this.el.querySelector("[data-toggle]");

    if (!toggleElement) {
      throw new Error("Menu must have a child with data-toggle attribute");
    }

    const contentElement = this.el.querySelector("[data-content]");

    if (!contentElement) {
      throw new Error("Menu must have a child with data-content attribute");
    }

    if (this.props.primary) {
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
    } else {
      toggleElement.addEventListener("contextmenu", (event) => {
        event.preventDefault();

        if (this.el.hasAttribute("data-js-open")) {
          this.el.removeAttribute("data-js-open");
        } else {
          this.el.setAttribute("data-js-open", "true");
          // Postpone callback registration until the current click finishes bubbling.
          setTimeout(() => {
            const handler = (event) => {
              this.el.removeAttribute("data-js-open");
              document.removeEventListener("click", handler);
              document.removeEventListener("contextmenu", handler);
            };

            document.addEventListener("click", handler);
            document.addEventListener("contextmenu", handler);
          }, 0);
        }
      });
    }
  },
};

function getProps(hook) {
  return {
    primary: getAttributeOrDefault(
      hook.el,
      "data-primary",
      "true",
      parseBoolean
    ),
  };
}

export default Menu;
