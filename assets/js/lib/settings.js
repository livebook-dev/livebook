import Emitter from "./emitter";
import { load, store } from "./storage";

const SETTINGS_KEY = "settings";

export const EDITOR_FONT_SIZE = {
  normal: 14,
  large: 16,
};

export const EDITOR_MODE = {
  default: "default",
  emacs: "emacs",
  vim: "vim",
};

export const EDITOR_THEME = {
  default: "default",
  light: "light",
};

const DEFAULTSETTINGS = {
  editor_auto_completion: true,
  editor_auto_signature: true,
  editor_auto_close_brackets: true,
  editor_font_size: EDITOR_FONT_SIZE.normal,
  editor_theme: EDITOR_THEME.default,
  editor_ligatures: false,
  editor_markdown_word_wrap: true,
  editor_mode: EDITOR_MODE.default,
  custom_view_show_section: true,
  custom_view_show_markdown: true,
  custom_view_show_code: true,
  custom_view_show_output: true,
  custom_view_spotlight: false,
};

/**
 * Stores local configuration and persists it across browser sessions.
 */
class SettingsStore {
  /** @private */
  _onChange = new Emitter();

  constructor() {
    this.settings = DEFAULTSETTINGS;

    this.loadSettings();
  }

  /**
   * Returns the current settings.
   */
  get() {
    return this.settings;
  }

  /**
   * Stores new settings.
   *
   * The given attributes are merged into the current settings.
   */
  update(newSettings) {
    const prevSettings = this.settings;
    this.settings = { ...this.settings, ...newSettings };
    this._onChange.dispatch(this.settings, prevSettings);
    this.storeSettings();
  }

  /**
   * Registers to settings changes.
   *
   * The given function is called immediately with the current
   * settings and then on every change.
   *
   * Returns a subscription object with `destroy` method that
   * unsubscribes from changes.
   */
  getAndSubscribe(callback) {
    callback(this.settings);
    return this._onChange.addListener(callback);
  }

  /** @private */
  loadSettings() {
    const settings = load(SETTINGS_KEY);

    if (settings) {
      // Rewrite settings for backward compatibility
      if (!Object.values(EDITOR_THEME).includes(settings.editor_theme)) {
        delete settings.editor_theme;
      }

      this.settings = { ...this.settings, ...settings };
    }
  }

  /** @private */
  storeSettings() {
    store(SETTINGS_KEY, this.settings);
  }
}

export const settingsStore = new SettingsStore();
