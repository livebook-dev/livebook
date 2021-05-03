import { decodeBase64, encodeBase64 } from "./utils";

const USER_DATA_COOKIE = "user_data";

/**
 * Stores user data in the `"user_data"` cookie.
 */
export function storeUserData(userData) {
  const json = JSON.stringify(userData);
  const encoded = encodeBase64(json);
  setCookie(USER_DATA_COOKIE, encoded, 157_680_000); // 5 years
}

/**
 * Loads user data from the `"user_data"` cookie.
 */
export function loadUserData() {
  const encoded = getCookieValue(USER_DATA_COOKIE);
  if (encoded) {
    const json = decodeBase64(encoded);
    return JSON.parse(json);
  } else {
    return null;
  }
}

function getCookieValue(key) {
  const cookie = document.cookie
    .split("; ")
    .find((cookie) => cookie.startsWith(`${key}=`));

  if (cookie) {
    const value = cookie.replace(`${key}=`, "");
    return value;
  } else {
    return null;
  }
}

function setCookie(key, value, maxAge) {
  const cookie = `${key}=${value};max-age=${maxAge};path=/`;
  document.cookie = cookie;
}
