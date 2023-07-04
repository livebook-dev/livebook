import { getAttributeOrDefault, getAttributeOrThrow } from "../lib/attribute";

/**
 * A hook for client-preprocessed time input.
 */
const UtcTimeInput = {
  mounted() {
    this.props = this.getProps();
    this.updateAttrs();

    this.el.addEventListener("blur", (event) => {
      const value = this.timeLocalToUtc(this.el.value);
      this.pushEventTo(this.props.phxTarget, "change", { html_value: value });
    });
  },

  updated() {
    this.props = this.getProps();
    this.updateAttrs();
  },

  getProps() {
    return {
      utcValue: getAttributeOrDefault(this.el, "data-utc-value", null),
      utcMin: getAttributeOrDefault(this.el, "data-utc-min", null),
      utcMax: getAttributeOrDefault(this.el, "data-utc-max", null),
      phxTarget: getAttributeOrThrow(this.el, "data-phx-target"),
    };
  },

  updateAttrs() {
    this.el.value = this.timeUtcToLocal(this.props.utcValue);
    this.el.min = this.timeUtcToLocal(this.props.utcMin);
    this.el.max = this.timeUtcToLocal(this.props.utcMax);
  },

  timeUtcToLocal(time) {
    if (!time) return null;

    const date = new Date();
    date.setUTCHours(...time.split(":"));
    const hours = date.getHours().toString().padStart(2, "0");
    const minutes = date.getMinutes().toString().padStart(2, "0");
    const seconds = date.getSeconds().toString().padStart(2, "0");

    return `${hours}:${minutes}:${seconds}`;
  },

  timeLocalToUtc(time) {
    if (!time) return null;

    const date = new Date();
    date.setHours(...time.split(":"));
    const hours = date.getUTCHours().toString().padStart(2, "0");
    const minutes = date.getUTCMinutes().toString().padStart(2, "0");
    const seconds = date.getUTCSeconds().toString().padStart(2, "0");

    return `${hours}:${minutes}:${seconds}`;
  },
};

export default UtcTimeInput;
