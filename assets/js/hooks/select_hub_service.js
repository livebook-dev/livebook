import { getAttributeOrThrow } from "../lib/attribute";

/**
 * A hook for the hub service form.
 *
 * On selecting the hub service card, this hook emits
 * an event to the backend so we can update the page's
 * state.
 */
const SelectHubService = {
  mounted() {
    this.el.addEventListener("click", (_) => {
      const service = getAttributeOrThrow(this.el, "id")
      this.pushEvent("select_hub_service", { value: service });
    });
  }
}

export default SelectHubService;
