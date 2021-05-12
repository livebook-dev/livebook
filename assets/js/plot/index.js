import vegaEmbed from "vega-embed";
import { getAttributeOrThrow } from "../lib/attribute";

/**
 * A hook used to render a plot using Vega-Lite based
 * on the received JSON definition.
 *
 * The hook expects a `plot:<id>` event with `{ spec }` payload,
 * where the spec is the plot definition as an object.
 *
 * Configuration:
 *
 *   * `data-id` - plot id
 *
 */
const Plot = {
  mounted() {
    this.props = getProps(this);
    this.state = {
      container: null,
    };

    this.state.container = document.createElement("div");
    this.el.appendChild(this.state.container);

    this.handleEvent(`plot:${this.props.id}`, ({ spec }) => {
      vegaEmbed(this.state.container, spec, {
        // theme: "ggplot2",
      });
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

export default Plot;
