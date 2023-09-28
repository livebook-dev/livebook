import monaco from "./monaco";

/**
 * AI helper
 *
 * - Pressing cmd + k opens the ai helper
 * - pressing the x in the corner or pressing escape in the helper closes it
 * - Helper is shown as an overlay widget _above_ the cursor or beginning of current selection
 * - The editor content is pushed down accordingly (and pushed down further if the AiHelper grows)
 * - Responses are streamed straight back into the monaco editor and replace the selection (probably not ideal)
 * - The previous content can be restored by pressing cmd + backspace
 * - The suggestions can be accepted with cmd + return
 * - pressing alt+return starts an in-line chat
 */
export default class AiHelper {
  constructor(editor, hook, cellId) {
    window.liveSocket.editor = editor;
    // TODO is the convention for these vars to have an underscore or no?
    this._cellId = cellId;
    this._hook = hook;
    this._editor = editor;

    // TODO resize textarea if it contains more than one row
    const widgetNode = this.renderWidget();
    const selection = this.makeSelection();

    const position = new monaco.Position(selection.startLineNumber-1, 1);

    this._overlayWidget = {
      getId: () => `livebook.ai_helper.overlay.${this.cellId}`,
      getDomNode: () => widgetNode,
      getPosition: () => {
        return {
            position: position,
            preference: [monaco.editor.ContentWidgetPositionPreference.ABOVE]
        };
      },
    };

    this._editor.addOverlayWidget(this._overlayWidget);

    this.setViewZone();
    this.focusTextarea();
    this.addEventListeners();

  }

  renderWidget() {
    const widgetNode = document.createElement("div");
    widgetNode.className = "w-full p-3"
    widgetNode.innerHTML = `
      <div class="w-full border border-gray-500 rounded-md p-3">
        <div class="absolute top-3 right-3 z-50 p-1">
          <button class="close-button bg-transparent focus:outline-none">
            <svg fill="none" viewBox="0 0 24 24" class="stroke-slate-300 hover:stroke-slate-50 h-4 w-4">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path>
            </svg>
          </button>
        </div>
        <textarea class="resize-none h-6 w-full bg-transparent" placeholder="New code instructions"></textarea>
        <div class="footer text-xs text-gray-600">
        ⏎ to generate code, Esc to cancel
        </div>
      </div>
    `;
    return widgetNode;
  }

  // modifies current selection to be complete lines and inserts an
  // empty line if there is no selection
  makeSelection() {
    let selection = this._editor.getSelection();
    
    // If no code is selected we insert a new empty row below the current one
    // and show the widget above that
    if(selection.isEmpty()) {

        if(selection.startLineNumber == 1 ){
            return selection;
        }

        // TODO what is 'my-source' in this context?...
        // TODO if we cancel out of the ai overlay, we need to remove the line we inserted
      this._editor.executeEdits('my-source', [{
            identifier: { major: 1, minor: 1 },
            range: new monaco.Range(selection.startLineNumber, Infinity, selection.startLineNumber, Infinity),
            text: "\n",
            forceMoveMarkers: true
          }]);
          
        selection = new monaco.Selection(selection.startLineNumber+1, 1, selection.startLineNumber+1, 1)

    } else {
        selection = selection.setStartPosition(selection.startLineNumber, 1);
        selection = selection.setEndPosition(selection.endLineNumber+1, 1);
    }
    this._editor.setSelection(selection);
    return selection;
  }

  getCodeBeforeSelection() {
    return this._editor.getModel().getValueInRange(
        new monaco.Range(1, 1, this._editor.getSelection().startLineNumber, 1)
    )
  }

  getCodeAfterSelection() {
    return this._editor.getModel().getValueInRange(
        new monaco.Range(this._editor.getSelection().endLineNumber, 1, Infinity, Infinity)
    );
  }

  getSelectedText() {
    return this._editor.getModel().getValueInRange(this._editor.getSelection())
  }


  addEventListeners() {

    const widgetNode = this._overlayWidget.getDomNode();

    widgetNode.querySelector('.close-button').addEventListener('click', () => {
      this.dispose();
    });

    widgetNode.querySelector('textarea').addEventListener('keydown', (event) => {
      if (event.key === 'Escape') {
        this.dispose();
      }
      else if (event.key === 'Enter' && !event.ctrlKey && !event.metaKey && !event.altKey) {
        event.preventDefault();

        this.requestAiCompletion();
      }
    });

    // store reference to listeners so we can remove them in the dispose function
    this._hookListeners = {
      stream: this._hook.handleEvent(`ai_helper_token_stream:${this._cellId}`, ({token}) => {
        // TODO what is 'my-source' meant to be?
        this._editor.executeEdits('my-source', [{
          identifier: { major: 1, minor: 1 },
          range: this._editor.getSelection(),
          text: token,
          forceMoveMarkers: true
        }]);
      }),

      end: this._hook.handleEvent(`ai_helper_token_stream_end:${this._cellId}`, () => {
        alert("ent")
      })
    }  
  }

  isOpen() {
    return document.body.contains(this._overlayWidget.getDomNode());  
  }

  focusTextarea() { this._overlayWidget.getDomNode().querySelector("textarea").focus(); }

  setViewZone() {

    const position = this._overlayWidget.getPosition().position;
    const widgetNode = this._overlayWidget.getDomNode();

    this._editor.changeViewZones((changeAccessor) => {
        this._viewZone = changeAccessor.addZone({
          afterLineNumber: position.lineNumber,
          // Placeholder for all lines and additional padding
          heightInPx: widgetNode.offsetHeight,
          domNode: document.createElement("div"),
          onDomNodeTop: (top) => {
            widgetNode.style.top = `${top}px`;
  
            const marginWidth = this._editor
              .getDomNode()
              .querySelector(".margin-view-overlays").offsetWidth;
  
            widgetNode.style.paddingLeft = `calc(${marginWidth}px + ${0}ch)`;
          },
          onComputedHeight: (height) => {
            widgetNode.style.height = `${height}px`;
          },
        });
      });
  }

  requestAiCompletion() {

    const aiCompletionRequest = { 
            cell_id: this._cellId,
            code_before_selection: this.getCodeBeforeSelection(),
            code_after_selection: this.getCodeAfterSelection(),
            selected_code: this.getSelectedText(),
            prompt: this._overlayWidget.getDomNode().querySelector("textarea").value
    };
    console.log("AI Completion request: ", aiCompletionRequest);
    
    this._hook.pushEvent(
        "ai_helper.request_completion",
        aiCompletionRequest,
        () => {})
  }

  dispose() {
    this._editor.removeOverlayWidget(this._overlayWidget);
    this._editor.changeViewZones((changeAccessor) => {
      changeAccessor.removeZone(this._viewZone);
    });

    // TODO is this the right way to do this? Feels pretty janky.
    // But if I don't clean up the listener on the hook I end up adding one listener each time
    // the ai helper is opened
    Object.keys(this._hookListeners).forEach(key => {
      this._hook.removeHandleEvent(this._hookListeners[key]);
    });
    this._hookListeners = {};

  }
}
