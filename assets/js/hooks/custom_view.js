import { settingsStore } from "../lib/settings";

/**
 * A hook for the custom view settings.
 */
const CustomViewSettings = {
  mounted() {
    const settings = settingsStore.get();

    const customSectionCheckbox = this.el.querySelector(
      `[name="custom_section"][value="true"]`
    );
    const customMarkdownCheckbox = this.el.querySelector(
      `[name="custom_markdown"][value="true"]`
    );
    const customResultsCheckbox = this.el.querySelector(
      `[name="custom_results"][value="true"]`
    );
    const customOutputCheckbox = this.el.querySelector(
      `[name="custom_output"][value="true"]`
    );
    const customSpotlightCheckbox = this.el.querySelector(
      `[name="custom_spotlight"][value="true"]`
    );

    customSectionCheckbox.checked = settings.custom_section;
    customMarkdownCheckbox.checked = settings.custom_markdown;
    customResultsCheckbox.checked = settings.custom_results;
    customOutputCheckbox.checked = settings.custom_output;
    customSpotlightCheckbox.checked = settings.custom_spotlight;

    customSectionCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ custom_section: event.target.checked });
    });
    customMarkdownCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ custom_markdown: event.target.checked });
    });
    customResultsCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ custom_results: event.target.checked });
    });
    customOutputCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ custom_output: event.target.checked });
    });
    customSpotlightCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ custom_spotlight: event.target.checked });
    });
  },
};

export default CustomViewSettings;
