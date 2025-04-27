import LiveEditor from "./cell_editor/live_editor";
import Connection from "./cell_editor/live_editor/connection";
import { parseHookProps } from "../lib/attribute";
import { waitUntilInViewport } from "../lib/utils";

const CellEditor = {
  mounted() {
    this.props = this.getProps();

    this.handleEvent(
      `cell_editor_init:${this.props.cellId}:${this.props.tag}`,
      ({ source, revision, doctest_reports, code_markers }) => {
        const editorContainer = this.el.querySelector(
          `[data-el-editor-container]`,
        );

        const editorEl = document.createElement("div");
        editorContainer.appendChild(editorEl);

        this.connection = new Connection(
          this,
          this.props.cellId,
          this.props.tag,
        );

        this.liveEditor = new LiveEditor(
          editorEl,
          this.connection,
          source,
          revision,
          this.props.language,
          this.props.intellisense,
          this.props.readOnly,
        );

        this.liveEditor.setCodeMarkers(code_markers);
        this.liveEditor.updateDoctests(doctest_reports);

        const skeletonEl = editorContainer.querySelector(`[data-el-skeleton]`);

        // Replace the skeleton with initial source, so that the
        // whole page is still searchable, before the editors are
        // lazily mounted. This also ensures the scroll has an
        // accurate length right away.
        const sourceEl = document.createElement("div");

        sourceEl.classList.add(
          "whitespace-pre",
          "overflow-x-auto",
          "text-editor",
          "font-editor",
          "px-12",
        );
        sourceEl.textContent = source;
        skeletonEl.replaceChildren(sourceEl);

        this.liveEditor.onMount(() => {
          // Remove the content placeholder
          skeletonEl.remove();
        });

        this.el.dispatchEvent(
          new CustomEvent("lb:cell:editor_created", {
            detail: { tag: this.props.tag, liveEditor: this.liveEditor },
            bubbles: true,
          }),
        );

        this.visibility = waitUntilInViewport(this.el, {
          root: document.querySelector("[data-el-notebook]"),
          proximity: 2000,
        });

        // We mount the editor lazily once it enters the viewport
        this.visibility.promise.then(() => {
          if (!this.liveEditor.isMounted()) {
            this.liveEditor.mount();
          }
        });
      },
    );
  },

  disconnected() {
    // When disconnected, this client is no longer seen by the server
    // and misses all collaborative changes. On reconnection we want
    // to clean up and mount a fresh hook, which we force by ensuring
    // the DOM id doesn't match
    this.el.removeAttribute("id");
    // The container element has phx-update="ignore", so we want to
    // make sure it is also replaced
    this.el.querySelector(`[data-el-editor-container]`).removeAttribute("id");
  },

  updated() {
    const prevProps = this.props;
    this.props = this.getProps();

    if (
      this.props.language !== prevProps.language ||
      this.props.intellisense !== prevProps.intellisense
    ) {
      this.liveEditor.setLanguage(this.props.language, this.props.intellisense);
    }
  },

  destroyed() {
    if (this.connection) {
      this.connection.destroy();
    }

    if (this.liveEditor) {
      this.el.dispatchEvent(
        new CustomEvent("lb:cell:editor_removed", {
          detail: { tag: this.props.tag },
          bubbles: true,
        }),
      );
      this.liveEditor.destroy();
    }

    if (this.visibility) {
      this.visibility.cancel();
    }
  },

  getProps() {
    return parseHookProps(this.el, [
      "cell-id",
      "tag",
      "language",
      "intellisense",
      "read-only",
    ]);
  },
};

export default CellEditor;
