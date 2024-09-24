import { ViewPlugin } from "@codemirror/view";
import {
  Annotation,
  Transaction,
  Facet,
  combineConfig,
  EditorSelection,
} from "@codemirror/state";
import Delta, { isDelete, isInsert, isRetain } from "../../../../lib/delta";

const collabConfig = Facet.define({
  combine(configs) {
    return combineConfig(configs, {});
  },
});

const remoteTransaction = Annotation.define();

const collabPlugin = ViewPlugin.fromClass(
  class {
    constructor(view) {
      const { collabClient } = view.state.facet(collabConfig);

      this.collabClient = collabClient;

      this.deltaSubscription = collabClient.onDelta((delta, { remote }) => {
        if (!remote) return;

        // Note that we explicitly transform the current selection,
        // rather than relying on the implicit editor mapping. It is
        // important that we use the same transformation logic as we
        // do for transforming remote user selections, so that all
        // selections stay consistent.

        return view.dispatch({
          changes: deltaToChanges(delta),
          selection: transformSelection(view.state.selection, delta),
          annotations: [
            Transaction.addToHistory.of(false),
            Transaction.remote.of(true),
            remoteTransaction.of(true),
          ],
          filter: false,
        });
      });
    }

    update(update) {
      // Skip changes dispatched by ourselves
      const isRemoteChange = update.transactions.some((tr) =>
        tr.annotation(remoteTransaction),
      );

      if (isRemoteChange) return;

      if (update.docChanged) {
        const delta = changesToDelta(update.changes);
        const selection = currentSelection(update);
        this.collabClient.handleClientDelta(delta, selection);
      } else if (
        update.focusChanged ||
        !update.state.selection.eq(update.startState.selection)
      ) {
        const selection = currentSelection(update);
        this.collabClient.handleClientSelection(selection);
      }
    }

    destroy() {
      this.deltaSubscription.destroy();
    }
  },
);

export function deltaToChanges(delta) {
  const specs = [];
  let index = 0;

  for (const op of delta.ops) {
    if (isRetain(op)) {
      index += op.retain;
    }

    if (isInsert(op)) {
      specs.push({ from: index, to: index, insert: op.insert });
    }

    if (isDelete(op)) {
      specs.push({ from: index, to: index + op.delete });
      index += op.delete;
    }
  }

  return specs;
}

export function changesToDelta(changes) {
  const deltas = [];

  changes.iterChanges((fromA, toA, fromB, toB, inserted) => {
    const delta = new Delta();

    if (fromA) {
      delta.retain(fromA);
    }

    if (fromA !== toA) {
      delta.delete(toA - fromA);
    }

    const text = inserted.toString();

    if (text) {
      delta.insert(text);
    }

    deltas.push(delta);
  });

  // All deltas represent changes to the same text. We want to compose
  // them starting from later ranges to earlier ranges, so that the
  // accumulated delta does not invalidate the positions in the deltas
  // we add next
  return deltas.reverse().reduce((delta1, delta2) => delta1.compose(delta2));
}

function currentSelection(update) {
  if (!update.view.hasFocus) {
    return null;
  }

  return update.state.selection;
}

export function transformSelection(selection, delta) {
  const ranges = selection.ranges.map((range) =>
    EditorSelection.range(
      delta.transformPosition(range.anchor),
      delta.transformPosition(range.head),
    ),
  );

  return EditorSelection.create(ranges, selection.mainIndex);
}

/**
 * Returns an extension that attaches the editor to the collaborative
 * client implementation.
 *
 * With this extension, the editor reports own changes and applies
 * changes received from the server.
 */
export function collab(collabClient) {
  return [collabPlugin, collabConfig.of({ collabClient })];
}
