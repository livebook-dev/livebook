import {
  EditorView,
  ViewPlugin,
  Decoration,
  WidgetType,
} from "@codemirror/view";
import { RangeSet, combineConfig, Facet } from "@codemirror/state";

const baseTheme = EditorView.baseTheme({
  ".cm-peerCursor": {
    position: "relative",
    display: "inline",
    // We set z-index, so that if there are other widgets, the cursor
    // is on top and gets the hover event
    zIndex: "1",
  },

  ".cm-peerCursorCaret": {
    position: "absolute",
    left: "0",
    top: "0",
    bottom: "0",
    height: "100%",
    width: "2px",
  },

  ".cm-peerCursorLabel": {
    position: "absolute",
    left: "0",
    top: "0",
    transform: "translateY(-100%)",
    whiteSpace: "nowrap",
    padding: "1px 8px",
    fontSize: "12px",
    color: "#f8fafc",
    visibility: "hidden",
    transitionProperty: "visibility",
    transitionDuration: "0s",
    transitionDelay: "1.5s",
  },

  ".cm-peerCursor .cm-peerCursorLabel:hover": {
    visibility: "visible",
  },

  ".cm-peerCursor .cm-peerCursorCaret:hover + .cm-peerCursorLabel": {
    visibility: "visible",
    transitionDelay: "0s",
  },

  // When in the first line, we want to display cursor and label in
  // the same line, because it cannot overflow the editor box. Content
  // overflowing the editor is hidden, because .cm-scroller uses
  // `overflow-x: scroll`, which forces `overflow-y: hidden`.
  ".cm-peerCursor.cm-peerCursor-inline .cm-peerCursorLabel": {
    marginLeft: "4px",
    transform: "none",
  },
});

const collabMarkersConfig = Facet.define({
  combine(configs) {
    return combineConfig(configs, {});
  },
});

const collabMarkersPlugin = ViewPlugin.fromClass(
  class {
    constructor(view) {
      const { collabClient } = view.state.facet(collabMarkersConfig);

      this.peers = collabClient.getPeers();

      this.decorations = RangeSet.of(
        Object.values(this.peers).flatMap(decorationsForPeer),
        true,
      );

      this.peersSubscription = collabClient.onPeersChange((peers) => {
        const prevPeers = this.peers;

        if (prevPeers === peers) return;

        const keepPeers = new Set();
        const addPeers = [];

        for (const clientId in peers) {
          const peer = peers[clientId];
          const prevPeer = prevPeers[clientId];

          if (prevPeer && peer.eq(prevPeer)) {
            keepPeers.add(clientId);
          } else {
            addPeers.push(peer);
          }
        }

        this.decorations = this.decorations.update({
          filter: (from, to, decoration) => keepPeers.has(decoration.spec.id),
          add: addPeers.flatMap(decorationsForPeer),
          sort: true,
        });

        this.peers = peers;

        // Dispatch a view update to re-render the decorations. Note
        // that peers may change synchronously as a result of local
        // text change (via transformation), so we defer the update
        // to the next event cycle to make sure we don't dispatch
        // update during an existing update
        setTimeout(() => {
          view.update([]);
        }, 0);
      });
    }

    destroy() {
      this.peersSubscription.destroy();
    }
  },
  { decorations: (plugin) => plugin.decorations },
);

class CursorWidget extends WidgetType {
  constructor(cursorPos, color, label) {
    super();

    this.cursorPos = cursorPos;
    this.color = color;
    this.label = label;
  }

  toDOM(view) {
    const node = document.createElement("div");
    node.classList.add("cm-peerCursor");

    const cursorLineNumber = view.state.doc.lineAt(this.cursorPos).number;
    if (cursorLineNumber === 1) {
      node.classList.add("cm-peerCursor-inline");
    }

    const cursorNode = document.createElement("div");
    cursorNode.classList.add("cm-peerCursorCaret");
    cursorNode.style.backgroundColor = this.color;

    const labelNode = document.createElement("div");
    labelNode.classList.add("cm-peerCursorLabel");
    labelNode.textContent = this.label;
    labelNode.style.backgroundColor = this.color;

    node.appendChild(cursorNode);
    node.appendChild(labelNode);

    return node;
  }

  eq(other) {
    return (
      other.cursorPos === this.cursorPos &&
      other.color === this.color &&
      other.label === this.label
    );
  }
}

function decorationsForPeer(peer) {
  const {
    id,
    selection,
    meta: { hex_color, name },
  } = peer;

  if (!selection) return [];

  const backgroundDecoration = Decoration.mark({
    class: "cm-peerSelection",
    attributes: { style: `background-color: ${hex_color}30` },
    id,
  });

  const selectionDecorationRanges = selection.ranges
    .filter((selectionRange) => !selectionRange.empty)
    .map(({ from, to }) => backgroundDecoration.range(from, to));

  const cursorDecorationRanges = selection.ranges.map((selectionRange) => {
    const cursorPos = selectionRange.head;

    return Decoration.widget({
      widget: new CursorWidget(cursorPos, hex_color, name),
      id,
    }).range(cursorPos);
  });

  return selectionDecorationRanges.concat(cursorDecorationRanges);
}

/**
 * Returns an extension that adds cursor and selection markers for
 * collaborative peers.
 */
export function collabMarkers(collabClient) {
  return [
    collabMarkersPlugin,
    collabMarkersConfig.of({ collabClient }),
    baseTheme,
  ];
}
