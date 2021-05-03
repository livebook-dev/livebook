import { storeUserData } from "../lib/user";

/**
 * A hook for the user profile form.
 *
 * On submit this hook saves the new data into cookie.
 * This cookie serves as a backup and can be used to restore
 * user data if the server is restarted.
 */
const UserForm = {
  mounted() {
    this.el.addEventListener("submit", (event) => {
      const name = this.el.data_name.value;
      const hex_color = this.el.data_hex_color.value;
      storeUserData({ name, hex_color });
    });
  },
};

export default UserForm;
