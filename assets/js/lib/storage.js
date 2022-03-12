const PREFIX = "livebook:";

/**
 * Loads value from local storage.
 */
export function load(key) {
  try {
    const json = localStorage.getItem(PREFIX + key);

    if (json) {
      return JSON.parse(json);
    }
  } catch (error) {
    console.error(
      `Failed to load from local storage, reason: ${error.message}`
    );
  }

  return undefined;
}

/**
 * Stores value in local storage.
 *
 * The value is serialized as JSON.
 */
export function store(key, value) {
  try {
    const json = JSON.stringify(value);
    localStorage.setItem(PREFIX + key, json);
  } catch (error) {
    console.error(`Failed to write to local storage, reason: ${error.message}`);
  }
}
