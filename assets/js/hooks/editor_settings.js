import { settingsStore, EDITOR_FONT_SIZE, EDITOR_THEME } from "../lib/settings";

/**
 * A hook for the editor settings.
 *
 * Those settings are user-specific and only relevant on the client
 * side, so we store them locally in the browser storage, so that
 * they are persisted across application runs.
 */
const EditorSettings = {
  mounted() {
    const settings = settingsStore.get();

    const editorAutoCompletionCheckbox = this.el.querySelector(
      `[name="editor_auto_completion"][value="true"]`,
    );
    const editorAutoSignatureCheckbox = this.el.querySelector(
      `[name="editor_auto_signature"][value="true"]`,
    );
    const editorAutoCloseBracketsCheckbox = this.el.querySelector(
      `[name="editor_auto_close_brackets"][value="true"]`,
    );
    const editorFontSizeCheckbox = this.el.querySelector(
      `[name="editor_font_size"][value="true"]`,
    );
    const editorLigaturesCheckbox = this.el.querySelector(
      `[name="editor_ligatures"][value="true"]`,
    );
    const editorLightThemeCheckbox = this.el.querySelector(
      `[name="editor_light_theme"][value="true"]`,
    );
    const editorMarkdownWordWrapCheckbox = this.el.querySelector(
      `[name="editor_markdown_word_wrap"][value="true"]`,
    );
    const editorMode = this.el.querySelector(`select[name="editor_mode"]`);

    editorAutoCompletionCheckbox.checked = settings.editor_auto_completion;
    editorAutoSignatureCheckbox.checked = settings.editor_auto_signature;
    editorAutoCloseBracketsCheckbox.checked =
      settings.editor_auto_close_brackets;
    editorFontSizeCheckbox.checked =
      settings.editor_font_size === EDITOR_FONT_SIZE.large ? true : false;
    editorLigaturesCheckbox.checked = settings.editor_ligatures;
    editorLightThemeCheckbox.checked =
      settings.editor_theme === EDITOR_THEME.light ? true : false;
    editorMarkdownWordWrapCheckbox.checked = settings.editor_markdown_word_wrap;
    editorMode.value = settings.editor_mode;

    editorAutoCompletionCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ editor_auto_completion: event.target.checked });
    });

    editorAutoSignatureCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ editor_auto_signature: event.target.checked });
    });

    editorAutoCloseBracketsCheckbox.addEventListener("change", (event) => {
      settingsStore.update({
        editor_auto_close_brackets: event.target.checked,
      });
    });

    editorFontSizeCheckbox.addEventListener("change", (event) => {
      settingsStore.update({
        editor_font_size: event.target.checked
          ? EDITOR_FONT_SIZE.large
          : EDITOR_FONT_SIZE.normal,
      });
    });

    editorLigaturesCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ editor_ligatures: event.target.checked });
    });

    editorLightThemeCheckbox.addEventListener("change", (event) => {
      settingsStore.update({
        editor_theme: event.target.checked
          ? EDITOR_THEME.light
          : EDITOR_THEME.default,
      });
    });

    editorMarkdownWordWrapCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ editor_markdown_word_wrap: event.target.checked });
    });

    editorMode.addEventListener("change", (event) => {
      settingsStore.update({ editor_mode: event.target.value });
    });
  },
};

export default EditorSettings;
