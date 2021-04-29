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
      const color = this.el.data_color.value;
      storeUserData({ name, color });
    });
  },
};

function storeUserData(userData) {
  const value = JSON.stringify(userData);
  setCookie("user_data", value, 157680000); // 5 years
}

function setCookie(key, value, maxAge) {
  const cookie = `${key}=${value};max-age=${maxAge};path=/`;
  document.cookie = cookie;
}

export default UserForm;
