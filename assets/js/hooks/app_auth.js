import { storeAppAuthToken } from "../lib/app";

const AppAuth = {
  mounted() {
    this.handleEvent("persist_app_auth", ({ slug, token }) => {
      storeAppAuthToken(slug, token);
      this.pushEvent("app_auth_persisted");
    });
  },
};

export default AppAuth;
