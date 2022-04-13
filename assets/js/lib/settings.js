import { load, store } from "./storage";

const SETTINGS_KEY = "settings";

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
  editor_markdown_word_wrap: true,
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
    const settings = load(SETTINGS_KEY);

    if (settings) {
      this._settings = { ...this._settings, ...settings };
    }
  }

  _storeSettings() {
    store(SETTINGS_KEY, this._settings);
  }
}

export const settingsStore = new SettingsStore();
