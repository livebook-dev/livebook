import * as vega from "vega";
import vegaEmbed from "vega-embed";
import { getAttributeOrThrow } from "../lib/attribute";
import { throttle } from "../lib/utils";

// See https://github.com/vega/vega-lite/blob/b61b13c2cbd4ecde0448544aff6cdaea721fd22a/src/compile/data/assemble.ts#L228-L231
const DEFAULT_DATASET_NAME = "source_0";

/**
 * A hook used to render graphics according to the given
 * Vega-Lite specification.
 *
 * The hook expects a `vega_lite:<id>:init` event with `{ spec }` payload,
 * where `spec` is the graphic definition as an object.
 *
 * Later `vega_lite:<id>:push` events may be sent with `{ data, dataset, window }` payload,
 * to dynamically update the underlying data.
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
      viewPromise: null,
    };

    this.state.container = document.createElement("div");
    this.el.appendChild(this.state.container);

    this.handleEvent(`vega_lite:${this.props.id}:init`, ({ spec }) => {
      if (!spec.data) {
        spec.data = { values: [] };
      }

      this.state.viewPromise = vegaEmbed(this.state.container, spec, {})
        .then((result) => result.view)
        .catch((error) => {
          const message = `Failed to render the given Vega-Lite specification, got the following error:\n\n    ${error.message}\n\nMake sure to check for typos.`;

          this.state.container.innerHTML = `
            <div class="text-red-600 whitespace-pre-wrap">${message}</div>
          `;
        });
    });

    const throttledResize = throttle((view) => view.resize(), 1_000);

    this.handleEvent(
      `vega_lite:${this.props.id}:push`,
      ({ data, dataset, window }) => {
        dataset = dataset || DEFAULT_DATASET_NAME;

        this.state.viewPromise.then((view) => {
          const currentData = view.data(dataset);
          const changeset = buildChangeset(currentData, data, window);
          // Schedule resize after the run finishes
          throttledResize(view);
          view.change(dataset, changeset).run();
        });
      }
    );
  },

  updated() {
    this.props = getProps(this);
  },

  destroyed() {
    if (this.state.viewPromise) {
      this.state.viewPromise.then((view) => view.finalize());
    }
  },
};

function getProps(hook) {
  return {
    id: getAttributeOrThrow(hook.el, "data-id"),
  };
}

function buildChangeset(currentData, newData, window) {
  if (window === 0) {
    return vega.changeset().remove(currentData);
  } else if (window) {
    const toInsert = newData.slice(-window);
    const freeSpace = Math.max(window - toInsert.length, 0);
    const toRemove = currentData.slice(0, -freeSpace);

    return vega.changeset().remove(toRemove).insert(toInsert);
  } else {
    return vega.changeset().insert(newData);
  }
}

export default VegaLite;
