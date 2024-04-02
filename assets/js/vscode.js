export function registerVsCodeEventHandlers() {
  window.document.addEventListener('keydown', e => {
    /**
     * Workaround for copy / paste not working within vscode iframe on mac
     * See: https://github.com/microsoft/vscode/issues/129178#issuecomment-1674169530
     */
    if ((e.ctrlKey || e.metaKey) && e.code === "KeyC") {
      document.execCommand("copy");
    } else if ((e.ctrlKey || e.metaKey) && e.code === "KeyX") {
      document.execCommand("cut");
    } else if ((e.ctrlKey || e.metaKey) && e.code === "KeyV") {
      document.execCommand("paste");
    }
    /**
     * Send keydown events to the parent window to allow for vscode keybindings to work
     * See: https://github.com/microsoft/vscode/issues/65452#issuecomment-586036474
     */
    window.parent.postMessage(JSON.stringify({
      type: e.type,
      altKey: e.altKey,
      code: e.code,
      ctrlKey: e.ctrlKey,
      isComposing: e.isComposing,
      key: e.key,
      location: e.location,
      metaKey: e.metaKey,
      repeat: e.repeat,
      shiftKey: e.shiftKey
    }), '*')
  })
}