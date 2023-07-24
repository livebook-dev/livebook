import { settingsStore } from "../lib/settings";

/**
 * A hook for the custom view settings.
 */
const CustomViewSettings = {
  mounted() {
    const settings = settingsStore.get();

    const customSectionCheckbox = this.el.querySelector(
      `[name="show_section"][value="true"]`
    );
    const customMarkdownCheckbox = this.el.querySelector(
      `[name="show_markdown"][value="true"]`
    );
    const customOutputCheckbox = this.el.querySelector(
      `[name="show_output"][value="true"]`
    );
    const customSpotlightCheckbox = this.el.querySelector(
      `[name="spotlight"][value="true"]`
    );

    customSectionCheckbox.checked = settings.custom_view_show_section;
    customMarkdownCheckbox.checked = settings.custom_view_show_markdown;
    customOutputCheckbox.checked = settings.custom_view_show_output;
    customSpotlightCheckbox.checked = settings.custom_view_spotlight;

    customSectionCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ custom_view_show_section: event.target.checked });
    });
    customMarkdownCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ custom_view_show_markdown: event.target.checked });
    });
    customOutputCheckbox.addEventListener("change", (event) => {
      settingsStore.update({ custom_view_show_output: event.target.checked });
    });
    customSpotlightCheckbox.addEventListener("change", (event) => {
      settingsStore.update({
        custom_view_spotlight: event.target.checked,
      });
    });
  },
};

export default CustomViewSettings;
