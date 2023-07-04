import { getAttributeOrDefault, getAttributeOrThrow } from "../lib/attribute";

/**
 * A hook for client-preprocessed datetime input.
 */
const UtcDateTimeInput = {
  mounted() {
    this.props = this.getProps();
    this.updateAttrs();

    this.el.addEventListener("blur", (event) => {
      const value = this.datetimeLocalToUtc(this.el.value);
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
    this.el.value = this.datetimeUtcToLocal(this.props.utcValue);
    this.el.min = this.datetimeUtcToLocal(this.props.utcMin);
    this.el.max = this.datetimeUtcToLocal(this.props.utcMax);
  },

  datetimeUtcToLocal(datetime) {
    if (!datetime) return null;

    const date = new Date(datetime + "Z");

    const year = date.getFullYear().toString();
    const month = (date.getMonth() + 1).toString().padStart(2, "0");
    const day = date.getDate().toString().padStart(2, "0");
    const hours = date.getHours().toString().padStart(2, "0");
    const minutes = date.getMinutes().toString().padStart(2, "0");
    const seconds = date.getSeconds().toString().padStart(2, "0");

    return `${year}-${month}-${day}T${hours}:${minutes}:${seconds}`;
  },

  datetimeLocalToUtc(datetime) {
    if (!datetime) return null;

    const date = new Date(datetime);

    const year = date.getUTCFullYear().toString();
    const month = (date.getUTCMonth() + 1).toString().padStart(2, "0");
    const day = date.getUTCDate().toString().padStart(2, "0");
    const hours = date.getUTCHours().toString().padStart(2, "0");
    const minutes = date.getUTCMinutes().toString().padStart(2, "0");
    const seconds = date.getUTCSeconds().toString().padStart(2, "0");

    return `${year}-${month}-${day}T${hours}:${minutes}:${seconds}`;
  },
};

export default UtcDateTimeInput;
