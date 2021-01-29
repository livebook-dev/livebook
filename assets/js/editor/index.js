import monaco from "../cell/live_editor/monaco";
import EditorClient from "../cell/live_editor/editor_client";
import MonacoEditorAdapter from "../cell/live_editor/monaco_editor_adapter";
import HookServerAdapter from "./hook_server_adapter";
import {
  getAttributeOrThrow,
  parseBoolean,
  parseInteger,
} from "../lib/attribute";

/**
 * A hook managing an editable cell.
 *
 * Mounts a Monaco Editor and provides real-time collaboration mechanism
 * by sending all changes as `Delta` objects to the server
 * and handling such objects sent by other clients.
 *
 * Configuration:
 *
 *   * `data-cell-id` - id of the cell being edited
 *   * `data-type` - editor type (i.e. language), either "markdown" or "elixir" is expected
 *   * `data-hidden` - whether this editor is currently hidden
 *   * `data-active` - whether this editor is currently the active one
 *
 * Additionally the root element should have a direct `div` child
 * with `data-source` and `data-revision` providing the initial values.
 */
const Editor = {
  mounted() {
    this.props = getProps(this);

    this.editorContainer = this.el.querySelector("div");

    if (!this.editorContainer) {
      throw new Error("Editor Hook root element should have a div child");
    }

    // Remove the content placeholder
    this.editorContainer.firstElementChild.remove();

    this.__mountEditor();

    const source = getAttributeOrThrow(this.editorContainer, "data-source");
    const revision = getAttributeOrThrow(
      this.editorContainer,
      "data-revision",
      parseInteger
    );

    this.editor.getModel().setValue(source);

    new EditorClient(
      new HookServerAdapter(this, this.props.cellId),
      new MonacoEditorAdapter(this.editor),
      revision
    );
  },

  updated() {
    const prevProps = this.props;
    this.props = getProps(this);

    if (prevProps.isHidden && !this.props.isHidden) {
      // If the editor was created as hidden it didn't get the chance
      // to properly adjust to the available space, so trigger it now.
      this.__adjustEditorLayout();
    }

    if (!prevProps.isActive && this.props.isActive) {
      this.editor.focus();
    }

    if (prevProps.isActive && !this.props.isActive) {
      if (this.editor.hasTextFocus()) {
        document.activeElement.blur();
      }
    }
  },

  __mountEditor() {
    this.editor = monaco.editor.create(this.editorContainer, {
      language: this.props.type,
      value: "",
      scrollbar: {
        vertical: "hidden",
        handleMouseWheel: false,
      },
      minimap: {
        enabled: false,
      },
      overviewRulerLanes: 0,
      scrollBeyondLastLine: false,
      quickSuggestions: false,
      renderIndentGuides: false,
      occurrencesHighlight: false,
      renderLineHighlight: "none",
      theme: "custom",
    });

    this.editor.getModel().updateOptions({
      tabSize: 2,
    });

    this.editor.updateOptions({
      autoIndent: true,
      tabSize: 2,
      formatOnType: true,
    });

    this.editor.onDidContentSizeChange(() => this.__adjustEditorLayout());
    this.__adjustEditorLayout();

    window.addEventListener("resize", (event) => {
      this.editor.layout();
    });
  },

  __adjustEditorLayout() {
    // Dynamically adjust editor height to the content, see https://github.com/microsoft/monaco-editor/issues/794
    const contentHeight = this.editor.getContentHeight();
    this.editorContainer.style.height = `${contentHeight}px`;
    this.editor.layout();
  },
};

function getProps(hook) {
  return {
    cellId: getAttributeOrThrow(hook.el, "data-cell-id"),
    type: getAttributeOrThrow(hook.el, "data-type"),
    isHidden: getAttributeOrThrow(hook.el, "data-hidden", parseBoolean),
    isActive: getAttributeOrThrow(hook.el, "data-active", parseBoolean),
  };
}

export default Editor;
