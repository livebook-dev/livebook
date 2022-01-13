import {
  loadLocalSettings,
  storeLocalSettings,
  EDITOR_FONT_SIZE,
} from "../lib/settings";

/**
 * A hook for the editor settings.
 *
 * Those settings are user-specific and only relevant on the client
 * side, so we store them locally in the browser storage, so that
 * they are persisted across application runs.
 */
const EditorSettings = {
  mounted() {
    const settings = loadLocalSettings();

    const editorAutoCompletionCheckbox = this.el.querySelector(
      `[name="editor_auto_completion"][value="true"]`
    );
    const editorAutoSignatureCheckbox = this.el.querySelector(
      `[name="editor_auto_signature"][value="true"]`
    );
    const editorFontSizeCheckbox = this.el.querySelector(
      `[name="editor_font_size"][value="true"]`
    );

    editorAutoCompletionCheckbox.checked = settings.editor_auto_completion;
    editorAutoSignatureCheckbox.checked = settings.editor_auto_signature;
    editorFontSizeCheckbox.checked =
      settings.editor_font_size === EDITOR_FONT_SIZE.large ? true : false;

    editorAutoCompletionCheckbox.addEventListener("change", (event) => {
      storeLocalSettings({ editor_auto_completion: event.target.checked });
    });

    editorAutoSignatureCheckbox.addEventListener("change", (event) => {
      storeLocalSettings({ editor_auto_signature: event.target.checked });
    });

    editorFontSizeCheckbox.addEventListener("change", (event) => {
      storeLocalSettings({
        editor_font_size: event.target.checked
          ? EDITOR_FONT_SIZE.large
          : EDITOR_FONT_SIZE.normal,
      });
    });
  },
};

export default EditorSettings;
