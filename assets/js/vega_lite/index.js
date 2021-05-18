import vegaEmbed from "vega-embed";
import { getAttributeOrThrow } from "../lib/attribute";

/**
 * A hook used to render graphics according to the given
 * Vega-Lite specification.
 *
 * The hook expects a `vega_lite:<id>` event with `{ spec }` payload,
 * where `spec` is the graphic definition as an object.
 *
 * Configuration:
 *
 *   * `data-id` - plot id
 *
 */
const VegaLite = {
  mounted() {
    this.props = getProps(this);
    this.state = {
      container: null,
    };

    this.state.container = document.createElement("div");
    this.el.appendChild(this.state.container);

    this.handleEvent(`vega_lite:${this.props.id}`, ({ spec }) => {
      vegaEmbed(this.state.container, spec, {});
    });
  },

  updated() {
    this.props = getProps(this);
  },
};

function getProps(hook) {
  return {
    id: getAttributeOrThrow(hook.el, "data-id"),
  };
}

export default VegaLite;
