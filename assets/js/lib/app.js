import { load, store } from "./storage";

const APP_AUTH_TOKEN_PREFIX = "app_auth_token:";

export function storeAppAuthToken(slug, token) {
  store(APP_AUTH_TOKEN_PREFIX + slug, token);
}

export function loadAppAuthToken() {
  const element = document.querySelector(`[data-app-slug]`);

  if (element) {
    const slug = element.getAttribute("data-app-slug");
    const token = load(APP_AUTH_TOKEN_PREFIX + slug);

    if (token) {
      return token;
    }
  }

  return null;
}
