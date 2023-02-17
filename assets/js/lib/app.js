import { load, store } from "./storage";

const APP_AUTH_TOKEN_PREFIX = "app_auth_token:";

export function storeAppAuthToken(slug, token) {
  store(APP_AUTH_TOKEN_PREFIX + slug, token);
}

export function loadAppAuthToken() {
  const path = window.location.pathname;

  if (path.startsWith("/apps/")) {
    const slug = path.split("/")[2];
    const token = load(APP_AUTH_TOKEN_PREFIX + slug);

    if (token) {
      return token;
    }
  }

  return null;
}
