export function registerVsCodeEventHandlers() {
  /**
   * Send keydown events to the parent window to allow for vscode keybindings to work
   * See: https://github.com/microsoft/vscode/issues/65452#issuecomment-586036474
   */
  window.document.addEventListener('keydown', e => {
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