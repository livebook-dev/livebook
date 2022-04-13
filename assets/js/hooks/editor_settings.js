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
      `[name="editor_auto_completion"][value="true"]`
    );
    const editorAutoSignatureCheckbox = this.el.querySelector(
      `[name="editor_auto_signature"][value="true"]`
    );
    const editorFontSizeCheckbox = this.el.querySelector(
      `[name="editor_font_size"][value="true"]`
    );
    const editorHighContrastCheckbox = this.el.querySelector(
      `[name="editor_high_contrast"][value="true"]`
    );
    const editorMarkdownWordWrapCheckbox = this.el.querySelector(
      `[name="editor_markdown_word_wrap"][value="true"]`
    );

    editorAutoCompletionCheckbox.checked = settings.editor_auto_completion;
    editorAutoSignatureCheckbox.checked = settings.editor_auto_signature;
    editorFontSizeCheckbox.checked =
      settings.editor_font_size === EDITOR_FONT_SIZE.large ? true : false;
    editorHighContrastCheckbox.checked =
      settings.editor_theme === EDITOR_THEME.highContrast ? true : false;
    editorMarkdownWordWrapCheckbox.checked = settings.editor_markdown_word_wrap;

    editorAutoCompletionCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ editor_auto_completion: event.target.checked });
    });

    editorAutoSignatureCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ editor_auto_signature: event.target.checked });
    });

    editorFontSizeCheckbox.addEventListener("change", (event) => {
      settingsStore.update({
        editor_font_size: event.target.checked
          ? EDITOR_FONT_SIZE.large
          : EDITOR_FONT_SIZE.normal,
      });
    });

    editorHighContrastCheckbox.addEventListener("change", (event) => {
      settingsStore.update({
        editor_theme: event.target.checked
          ? EDITOR_THEME.highContrast
          : EDITOR_THEME.default,
      });
    });

    editorMarkdownWordWrapCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ editor_markdown_word_wrap: event.target.checked });
    });
  },
};

export default EditorSettings;
