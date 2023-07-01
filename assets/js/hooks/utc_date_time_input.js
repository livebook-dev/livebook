import { getAttributeOrThrow, parseInteger } from "../lib/attribute";

/**
 * A hook for client-preprocessed datetime and time input.
 *
 */
const UtcDateTimeInput = {
  mounted() {
    this.props = this.getProps();
    this.updateValue();

    this.el.addEventListener("change", (event) => {
      switch (this.props.type) {
        case "datetime-local":
          let utcDate = this.datetime_local_to_utc(this.el.value);
          this.pushEventTo(this.props.phxTarget, "change", {
            html_value: utcDate,
          });
          break;
        case "time":
          let utcTime = this.time_local_to_utc(this.el.value + ":00");
          this.pushEventTo(this.props.phxTarget, "change", {
            html_value: utcTime,
          });
          break;
      }
    });
  },
  updated() {
    this.props = this.getProps();
    this.updateValue();
  },
  getProps() {
    return {
      type: getAttributeOrThrow(this.el, "type"),
      utcValue: getAttributeOrThrow(this.el, "data-utc-value"),
      phxTarget: getAttributeOrThrow(this.el, "phx-target"),
    };
  },
  updateValue() {
    switch (this.props.type) {
      case "datetime-local":
        this.el.value = this.datetime_utc_to_local(this.props.utcValue);
        break;
      case "time":
        this.el.value = this.time_utc_to_local(this.props.utcValue);
        break;
    }
  },
  datetime_utc_to_local(datetime) {
    let localDate = new Date(datetime + "Z");
    let year = localDate.getFullYear().toString();
    let month = (localDate.getMonth() + 1).toString().padStart(2, "0");
    let day = localDate.getDate().toString().padStart(2, "0");
    let hours = localDate.getHours().toString().padStart(2, "0");
    let minutes = localDate.getMinutes().toString().padStart(2, "0");
    let seconds = localDate.getSeconds().toString().padStart(2, "0");
    return (
      year +
      "-" +
      month +
      "-" +
      day +
      "T" +
      hours +
      ":" +
      minutes +
      ":" +
      seconds
    );
  },
  datetime_local_to_utc(datetime) {
    return new Date(datetime).toISOString();
  },
  time_utc_to_local(time) {
    let localDate = new Date();
    localDate.setUTCHours(...time.split(":"));
    let hours = localDate.getHours().toString().padStart(2, "0");
    let minutes = localDate.getMinutes().toString().padStart(2, "0");
    let seconds = localDate.getSeconds().toString().padStart(2, "0");
    return hours + ":" + minutes + ":" + seconds;
  },
  time_local_to_utc(time) {
    let utcTime = new Date(
      new Date().setHours(...time.split(":").map((num) => parseInt(num)))
    ).toISOString();
    return utcTime.slice(11, -5);
  },
};

export default UtcDateTimeInput;
