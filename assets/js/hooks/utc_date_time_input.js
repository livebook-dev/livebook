import { getAttributeOrThrow, parseInteger } from "../lib/attribute";

/**
 * A hook for client-preprocessed datetime and time input.
 *
 */
const UtcDateTimeInput = {
  mounted() {
    this.props = this.getProps();
    this.el.addEventListener("input", (event) => {
      event.stopPropagation();
      event.preventDefault();
    });
    this.el.addEventListener("change", (event) => {
      event.stopPropagation();
      event.preventDefault();
      switch (this.props.type) {
        case "datetime-local":
          let utcDate = new Date(this.el.value).toISOString();
          this.pushEventTo(this.props.phxTarget, "change", {
            html_value: utcDate,
          });
          break;
        case "time":
          let utcTime = new Date(
            new Date().setHours(
              ...this.el.value.split(":").map((num) => parseInt(num))
            )
          ).toISOString();
          this.pushEventTo(this.props.phxTarget, "change", {
            html_value: utcTime.slice(11, -1),
          });
          break;
      }
    });
  },
  updated() {
    this.props = this.getProps();
  },
  getProps() {
    return {
      type: getAttributeOrThrow(this.el, "type"),
      phxTarget: getAttributeOrThrow(this.el, "phx-target", parseInteger),
    };
  },
};

export default UtcDateTimeInput;
