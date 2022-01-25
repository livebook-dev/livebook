import md5 from "crypto-js/md5";
import encBase64 from "crypto-js/enc-base64";

export function isMacOS() {
  return /(Mac|iPhone|iPod|iPad)/i.test(navigator.platform);
}

export function isEditableElement(element) {
  return (
    element.matches && element.matches("input, textarea, [contenteditable]")
  );
}

export function clamp(n, x, y) {
  return Math.min(Math.max(n, x), y);
}

export function getLineHeight(element) {
  const computedStyle = window.getComputedStyle(element);
  const lineHeight = parseInt(computedStyle.lineHeight, 10);

  if (Number.isNaN(lineHeight)) {
    const clone = element.cloneNode();
    clone.innerHTML = "<br>";
    element.appendChild(clone);
    const singleLineHeight = clone.clientHeight;
    clone.innerHTML = "<br><br>";
    const doubleLineHeight = clone.clientHeight;
    element.removeChild(clone);
    const lineHeight = doubleLineHeight - singleLineHeight;
    return lineHeight;
  } else {
    return lineHeight;
  }
}

export function selectElementContent(element) {
  const selection = window.getSelection();
  const range = document.createRange();
  range.selectNodeContents(element);
  selection.removeAllRanges();
  selection.addRange(range);
}

export function smoothlyScrollToElement(element) {
  const { height } = element.getBoundingClientRect();

  if (height < window.innerHeight) {
    element.scrollIntoView({ behavior: "smooth", block: "center" });
  } else {
    element.scrollIntoView({ behavior: "smooth", block: "start" });
  }
}

/**
 * Transforms a UTF8 string into base64 encoding.
 */
export function encodeBase64(string) {
  return btoa(unescape(encodeURIComponent(string)));
}

/**
 * Transforms base64 encoding into UTF8 string.
 */
export function decodeBase64(binary) {
  return decodeURIComponent(escape(atob(binary)));
}

/**
 * Generates a random string.
 */
export function randomId() {
  return randomString(24);
}

/**
 * Generates a random long string.
 */
export function randomToken() {
  return randomString(40);
}

function randomString(byteSize) {
  const array = new Uint8Array(byteSize);
  crypto.getRandomValues(array);
  const byteString = String.fromCharCode(...array);
  return btoa(byteString);
}

/**
 * Calculates MD5 of the given string and returns
 * the base64 encoded binary.
 */
export function md5Base64(string) {
  return md5(string).toString(encBase64);
}

/**
 * A simple throttle version that ensures
 * the given function is called at most once
 * within the given time window.
 */
export function throttle(fn, windowMs) {
  let ignore = false;

  return (...args) => {
    if (!ignore) {
      fn(...args);
      ignore = true;
      setTimeout(() => {
        ignore = false;
      }, windowMs);
    }
  };
}

export function setFavicon(name) {
  let link = document.querySelector(`[rel="icon"]`);

  if (!link) {
    link = document.createElement("link");
    link.rel = "icon";
    document.head.appendChild(link);
  }

  link.href = `/${name}.svg`;
}

export function findChildOrThrow(element, selector) {
  const child = element.querySelector(selector);

  if (!child) {
    throw new Error(
      `expected a child matching ${selector}, but none was found`
    );
  }

  return child;
}

export function cancelEvent(event) {
  // Cancel any default browser behavior.
  event.preventDefault();
  // Stop event propagation (e.g. so it doesn't reach the editor).
  event.stopPropagation();
}

const htmlEscapes = {
  "&": "&amp;",
  "<": "&lt;",
  ">": "&gt;",
  '"': "&quot;",
  "'": "&#39",
};

/**
 * Transforms the given string to a HTML-safe value.
 */
export function escapeHtml(string) {
  return (string || "").replace(/[&<>"']/g, (char) => htmlEscapes[char]);
}
