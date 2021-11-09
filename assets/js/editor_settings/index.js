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

    const autoCompletionCheckbox = this.el.querySelector(
      `[name="auto_completion"][value="true"]`
    );
    const autoSignatureCheckbox = this.el.querySelector(
      `[name="auto_signature"][value="true"]`
    );

    autoCompletionCheckbox.checked = settings.auto_completion;
    autoSignatureCheckbox.checked = settings.auto_signature;

    autoCompletionCheckbox.addEventListener("change", (event) => {
      storeLocalSettings({ auto_completion: event.target.checked });
    });

    autoSignatureCheckbox.addEventListener("change", (event) => {
      storeLocalSettings({ auto_signature: event.target.checked });
    });
  },
};

export default EditorSettings;
