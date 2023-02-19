import { storeAppAuthToken } from "../lib/app";

/**
 * A hook for the app auth page.
 */
const AppAuth = {
  mounted() {
    this.handleEvent("persist_app_auth", ({ slug, token }) => {
      storeAppAuthToken(slug, token);
      this.pushEvent("app_auth_persisted");
    });
  },
};

export default AppAuth;
