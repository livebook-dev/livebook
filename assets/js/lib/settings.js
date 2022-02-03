const SETTINGS_KEY = "livebook:settings";

export const EDITOR_FONT_SIZE = {
  normal: 14,
  large: 16,
};

export const EDITOR_THEME = {
  default: "default",
  highContrast: "highContrast",
};

const DEFAULT_SETTINGS = {
  editor_auto_completion: true,
  editor_auto_signature: true,
  editor_font_size: EDITOR_FONT_SIZE.normal,
  editor_theme: EDITOR_THEME.default,
};

/**
 * Stores local configuration and persists it across browser sessions.
 */
class SettingsStore {
  constructor() {
    this._subscribers = [];
    this._settings = DEFAULT_SETTINGS;

    this._loadSettings();
  }

  /**
   * Returns the current settings.
   */
  get() {
    return this._settings;
  }

  /**
   * Stores new settings.
   *
   * The given attributes are merged into the current settings.
   */
  update(newSettings) {
    const prevSettings = this._settings;
    this._settings = { ...this._settings, ...newSettings };
    this._subscribers.forEach((callback) =>
      callback(this._settings, prevSettings)
    );
    this._storeSettings();
  }

  /**
   * Registers to settings changes.
   *
   * The given function is called immediately with the current
   * settings and then on every change.
   */
  getAndSubscribe(callback) {
    this._subscribers.push(callback);
    callback(this._settings);
  }

  _loadSettings() {
    try {
      const json = localStorage.getItem(SETTINGS_KEY);

      if (json) {
        const settings = JSON.parse(json);
        this._settings = { ...this._settings, ...settings };
      }
    } catch (error) {
      console.error(`Failed to load local settings, reason: ${error.message}`);
    }
  }

  _storeSettings() {
    try {
      const json = JSON.stringify(this._settings);
      localStorage.setItem(SETTINGS_KEY, json);
    } catch (error) {
      console.error(`Failed to store local settings, reason: ${error.message}`);
    }
  }
}

export const settingsStore = new SettingsStore();
