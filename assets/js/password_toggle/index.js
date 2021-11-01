/**
 * A hook used to toggle password's input visibility via icon button.
 */

const VISIBLE_ICON = "ri-eye-off-line";
const OBSCURED_ICON = "ri-eye-line";

const PasswordToggle = {
  mounted() {
    this.visible = false;

    this.input = this.el.querySelector("input");
    this.iconButton = this.el.querySelector("i");

    this.iconButton.onclick = () => {
      this.visible = !this.visible;
      this._updateState();
    };
  },

  updated() {
    this._updateState();
  },

  _updateState() {
    if (this.visible) {
      this.input.type = "text";
      this.iconButton.classList.remove(OBSCURED_ICON);
      this.iconButton.classList.add(VISIBLE_ICON);
    } else {
      this.input.type = "password";
      this.iconButton.classList.remove(VISIBLE_ICON);
      this.iconButton.classList.add(OBSCURED_ICON);
    }
  },
};

export default PasswordToggle;
