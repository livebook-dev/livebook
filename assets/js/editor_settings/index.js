import { loadLocalSettings, storeLocalSettings } from "../lib/settings";

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

    editorAutoCompletionCheckbox.checked = settings.editor_auto_completion;
    editorAutoSignatureCheckbox.checked = settings.editor_auto_signature;

    editorAutoCompletionCheckbox.addEventListener("change", (event) => {
      storeLocalSettings({ editor_auto_completion: event.target.checked });
    });

    editorAutoSignatureCheckbox.addEventListener("change", (event) => {
      storeLocalSettings({ editor_auto_signature: event.target.checked });
    });
  },
};

export default EditorSettings;
