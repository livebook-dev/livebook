import { load, store } from "./storage";

const SETTINGS_KEY = "settings";

export const EDITOR_FONT_SIZE = {
  normal: 14,
  large: 16,
};

export const EDITOR_THEME = {
  default: "default",
  light: "light",
};

const DEFAULT_SETTINGS = {
  editor_auto_completion: true,
  editor_auto_signature: true,
  editor_font_size: EDITOR_FONT_SIZE.normal,
  editor_theme: EDITOR_THEME.default,
  editor_markdown_word_wrap: true,
  custom_view_show_section: true,
  custom_view_show_markdown: true,
  custom_view_show_output: true,
  custom_view_spotlight: false,
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
   *
   * Returns a function that unsubscribes as a shorthand for
   * `unsubscribe`.
   */
  getAndSubscribe(callback) {
    this._subscribers.push(callback);
    callback(this._settings);

    return () => {
      this.unsubscribe(callback);
    };
  }

  /**
   * Unsubscribes the given function from updates.
   *
   * Note that you must pass the same function reference as you
   * passed to `subscribe`.
   */
  unsubscribe(callback) {
    const index = this._subscribers.indexOf(callback);

    if (index !== -1) {
      this._subscribers.splice(index, 1);
    }
  }

  _loadSettings() {
    const settings = load(SETTINGS_KEY);

    if (settings) {
      // Rewrite settings for backward compatibility
      if (!Object.values(EDITOR_THEME).includes(settings.editor_theme)) {
        delete settings.editor_theme;
      }

      this._settings = { ...this._settings, ...settings };
    }
  }

  _storeSettings() {
    store(SETTINGS_KEY, this._settings);
  }
}

export const settingsStore = new SettingsStore();
