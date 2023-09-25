import LiveEditor from "./cell_editor/live_editor";
import {
  getAttributeOrDefault,
  getAttributeOrThrow,
} from "../lib/attribute";

import monaco from "./cell_editor/live_editor/monaco";

const InlineAIComponent = {
  mounted() {

    this.props = this.getProps();

    const editors = monaco.editor.getEditors() 
    this._editor = editors.find(editor => 
      document.getElementById("cell-" + this.props.cellId).contains(editor.getDomNode())
    );


    const editor = this._editor;
    console.log(this._editor.getId())

    var selection = this._editor.getSelection()
    selection = selection.setStartPosition(selection.startLineNumber, 1)
    selection = selection.setEndPosition(selection.endLineNumber, 1000)
    this._editor.setSelection(selection)
    var selectedText = this._editor.getModel().getValueInRange(selection)


    
    const formElement = this.el.querySelector('form');
    formElement.addEventListener("submit", (event) => {
      event.preventDefault();      

      const prompt = formElement.querySelector('input').value;

      // TODO it probably makes more sense to get the cell content on the server side, as 
      // we also want content from all other cells in the livebook
      this.pushEventTo(this.el, "request_inline_ai_completion",
      {selectedCode: selectedText, cellContent: editor.getModel().getValue(), prompt: prompt })
    });

    
    console.log("selected text", selectedText)

    const widgetElement = this.el
    console.log(widgetElement)

    // const position = this._editor.getPosition()
    const position = new monaco.Position(selection.startLineNumber-1, 1)

    
    
    // selection.getStartPosition()
    

    
    // var originalModel = monaco.editor.createModel(
    //   selectedText,
    //   "text/plain"
    // );
    // var modifiedModel = monaco.editor.createModel(
    //   "just some text\nabcz\nzzzzefgh\nSome more text\nThis line is removed on the left.",
    //   "text/plain"
    // );
    
    // var diffDom = document.getElementById("uniqueid")

    // var diffEditor = monaco.editor.createDiffEditor(
    //   diffDom,
    //   {
    //     // You can optionally disable the resizing
    //     enableSplitViewResizing: false,
    
    //     // Render the diff inline
    //     renderSideBySide: false,
    //   }
    // );
    // diffEditor.setModel({
    //   original: originalModel,
    //   modified: modifiedModel,
    // });

    // console.log(diffDom)

    

    // Define the content widget
    const contentWidget = {
      getId: function () {
        return 'my.content.widget';
      },
      getDomNode: function () {
        return widgetElement;
        // return diffDom;
      },
      getPosition: function () {
        return {
          position: position,
          preference: [monaco.editor.ContentWidgetPositionPreference.ABOVE]
        };
      }
    };

    // Add the content widget to the editor
    this._editor.addOverlayWidget(contentWidget);


    this._editor.changeViewZones((changeAccessor) => {
      this._viewZone = changeAccessor.addZone({
        afterLineNumber: position.lineNumber,
        // Placeholder for all lines and additional padding
        heightInPx: 60,
        domNode: document.createElement("div"),
        onDomNodeTop: (top) => {
          widgetElement.style.top = `${top}px`;
          widgetElement.style.position = "absolute";

          const marginWidth = this._editor
            .getDomNode()
            .querySelector(".margin-view-overlays").offsetWidth;

          const column = 1;
          widgetElement.style.paddingLeft = `calc(${marginWidth}px + ${column}ch)`;

        },

        onComputedHeight: (height) => {
          widgetElement.style.height = `${height}px`;
        },
      });
    });

    console.log(this._editor)
    
    var aiResponse = ""
    this.handleEvent(
      `inline_ai_token_received:${this.props.cellId}`,
      ({chunk}) => {

        // first chunk
        // if(aiResponse == "") {
        //   this._editor.trigger(monaco.KeyCode.Backspace, 'deleteLeft')
        // }

        aiResponse += chunk
        console.log(aiResponse)
        // this._editor.trigger('keyboard', 'type', {text: chunk});
        this._editor.executeEdits('my-source', [{
          identifier: { major: 1, minor: 1 },
          range: editor.getSelection(),
          text: chunk,
          forceMoveMarkers: true
        }]);                
      }
    );
  },

  disconnected() {
    // When disconnected, this client is no longer seen by the server
    // and misses all collaborative changes. On reconnection we want
    // to clean up and mount a fresh hook, which we force by ensuring
    // the DOM id doesn't match
    // this.el.removeAttribute("id");
  },

  destroyed() {
    // if (this.liveEditor) {
    //   this.el.dispatchEvent(
    //     new CustomEvent("lb:cell:editor_removed", {
    //       detail: { tag: this.props.tag },
    //       bubbles: true,
    //     })
    //   );
    //   this.liveEditor.dispose();
    // }
  },

  getProps() {
    return {
      cellId: getAttributeOrThrow(this.el, "data-cell-id"),
      // TODO understand how tags work - something to do with events?
      // tag: getAttributeOrThrow(this.el, "data-tag"),
    };
  },
};

export default InlineAIComponent;
