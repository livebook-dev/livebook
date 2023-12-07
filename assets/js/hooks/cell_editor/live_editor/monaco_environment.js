window.MonacoEnvironment = {
  // Certain browser extensions are Monaco-aware, so we expose it on
  // the window object
  globalAPI: true,
  getWorkerUrl(_workerId, label) {
    if (label === "json") {
      return "/assets/language/json/json.worker.js";
    }

    return "/assets/editor/editor.worker.js";
  },
};
