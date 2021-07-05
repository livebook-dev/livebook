import {
  isMacOS,
  isEditableElement,
  clamp,
  selectElementContent,
  smoothlyScrollToElement,
} from "../lib/utils";
import KeyBuffer from "./key_buffer";
import { globalPubSub } from "../lib/pub_sub";
import monaco from "../cell/live_editor/monaco";

/**
 * A hook managing the whole session.
 *
 * Serves as a coordinator handling all the global session events.
 * Note that each cell has its own hook, so that LV keeps track
 * of cells being added/removed from the DOM. We do however need
 * to communicate between this global hook and cells and for
 * that we use a simple local pubsub that the hooks subscribe to.
 *
 * ## Shortcuts
 *
 * This hook registers session shortcut handlers,
 * see `LivebookWeb.SessionLive.ShortcutsComponent`
 * for the complete list of available shortcuts.
 *
 * ## Navigation
 *
 * This hook handles focusing cells and moving the focus around,
 * this is done purely on the client side because it is event-intensive
 * and specific to this client only. The UI changes are handled by
 * setting `data-js-*` attributes and using CSS accordingly (see assets/css/js_interop.css).
 * Navigation changes are also broadcasted to all cell hooks via PubSub.
 *
 * ## Location tracking and following
 *
 * Location describes where the given client is within
 * the notebook (in which cell, and where specifically in that cell).
 * When multiple clients are connected, they report own location to
 * each other whenever it changes. We then each the location to show
 * cursor and selection indicators.
 *
 * Additionally the current user may follow another client from the clients list.
 * In such case, whenever a new location comes from that client we move there automatically
 * (i.e. we focus the same cells to effectively mimic how the followed client moves around).
 *
 * Initially we load basic information about connected clients using `"session_init"`
 * and then update this information whenever clients join/leave/update.
 * This way location reports include only client pid, as we already have
 * the necessary hex_color/name locally.
 */
const Session = {
  mounted() {
    this.state = {
      focusedCellId: null,
      focusedSectionId: null,
      focusedCellType: null,
      insertMode: false,
      keyBuffer: new KeyBuffer(),
      clientsMap: {},
      lastLocationReportByClientPid: {},
      followedClientPid: null,
    };

    // Load initial data

    this.pushEvent("session_init", {}, ({ clients }) => {
      clients.forEach((client) => {
        this.state.clientsMap[client.pid] = client;
      });
    });

    // DOM events

    this.handleDocumentKeyDown = (event) => {
      handleDocumentKeyDown(this, event);
    };

    document.addEventListener("keydown", this.handleDocumentKeyDown, true);

    this.handleDocumentMouseDown = (event) => {
      handleDocumentMouseDown(this, event);
    };

    document.addEventListener("mousedown", this.handleDocumentMouseDown);

    this.handleDocumentDoubleClick = (event) => {
      handleDocumentDoubleClick(this, event);
    };

    document.addEventListener("dblclick", this.handleDocumentDoubleClick);

    getSectionsList().addEventListener("click", (event) => {
      handleSectionsListClick(this, event);
    });

    getClientsList().addEventListener("click", (event) => {
      handleClientsListClick(this, event);
    });

    getSectionsListToggle().addEventListener("click", (event) => {
      toggleSectionsList(this);
    });

    getClientsListToggle().addEventListener("click", (event) => {
      toggleClientsList(this);
    });

    getNotebook().addEventListener("scroll", (event) => {
      updateSectionListHighlight();
    });

    getCellIndicators().addEventListener("click", (event) => {
      handleCellIndicatorsClick(this, event);
    });

    window.addEventListener(
      "phx:page-loading-stop",
      () => {
        focusCellFromUrl(this);
      },
      { once: true }
    );

    // DOM setup

    updateSectionListHighlight();
    focusNotebookNameIfNew();

    // Server events

    this.handleEvent("cell_inserted", ({ cell_id: cellId }) => {
      handleCellInserted(this, cellId);
    });

    this.handleEvent(
      "cell_deleted",
      ({ cell_id: cellId, sibling_cell_id: siblingCellId }) => {
        handleCellDeleted(this, cellId, siblingCellId);
      }
    );

    this.handleEvent("cell_restored", ({ cell_id: cellId }) => {
      handleCellRestored(this, cellId);
    });

    this.handleEvent("cell_moved", ({ cell_id }) => {
      handleCellMoved(this, cell_id);
    });

    this.handleEvent("section_inserted", ({ section_id }) => {
      handleSectionInserted(this, section_id);
    });

    this.handleEvent("section_deleted", ({ section_id }) => {
      handleSectionDeleted(this, section_id);
    });

    this.handleEvent("section_moved", ({ section_id }) => {
      handleSectionMoved(this, section_id);
    });

    this.handleEvent("cell_upload", ({ cell_id, url }) => {
      handleCellUpload(this, cell_id, url);
    });

    this.handleEvent("client_joined", ({ client }) => {
      handleClientJoined(this, client);
    });

    this.handleEvent("client_left", ({ client_pid }) => {
      handleClientLeft(this, client_pid);
    });

    this.handleEvent("clients_updated", ({ clients }) => {
      handleClientsUpdated(this, clients);
    });

    this.handleEvent(
      "location_report",
      ({ client_pid, cell_id, selection }) => {
        const report = {
          cellId: cell_id,
          selection: decodeSelection(selection),
        };

        handleLocationReport(this, client_pid, report);
      }
    );

    this._unsubscribeFromSessionEvents = globalPubSub.subscribe(
      "session",
      (event) => {
        handleSessionEvent(this, event);
      }
    );
  },

  destroyed() {
    this._unsubscribeFromSessionEvents();

    document.removeEventListener("keydown", this.handleDocumentKeyDown);
    document.removeEventListener("mousedown", this.handleDocumentMouseDown);
    document.removeEventListener("dblclick", this.handleDocumentDoubleClick);
  },
};

/**
 * Data of a specific LV client.
 *
 * @typedef Client
 * @type {Object}
 * @property {String} pid
 * @property {String} hex_color
 * @property {String} name
 */

/**
 * A report of the current location sent by one of the other clients.
 *
 * @typedef LocationReport
 * @type {Object}
 * @property {String|null} cellId
 * @property {monaco.Selection|null} selection
 */

// DOM event handlers

/**
 * Handles session keybindings.
 *
 * Make sure to keep the shortcuts help modal up to date.
 */
function handleDocumentKeyDown(hook, event) {
  if (event.repeat) {
    return;
  }

  const cmd = isMacOS() ? event.metaKey : event.ctrlKey;
  const alt = event.altKey;
  const key = event.key;
  const keyBuffer = hook.state.keyBuffer;

  if (hook.state.insertMode) {
    keyBuffer.reset();

    if (key === "Escape") {
      const monacoInputOpen = !!event.target.closest(".monaco-inputbox");

      const activeDescendant = event.target.getAttribute(
        "aria-activedescendant"
      );
      const completionBoxOpen =
        activeDescendant && activeDescendant.includes("suggest");

      // Ignore Escape if it's supposed to close some Monaco input
      // (like the find/replace box), or the completion box.
      if (!monacoInputOpen && !completionBoxOpen) {
        escapeInsertMode(hook);
      }
    } else if (cmd && key === "Enter" && !alt) {
      cancelEvent(event);
      if (hook.state.focusedCellType === "elixir") {
        queueFocusedCellEvaluation(hook);
      }
    } else if (cmd && key === "s") {
      cancelEvent(event);
      saveNotebook(hook);
    }
  } else {
    // Ignore inputs and notebook/section title fields
    if (isEditableElement(event.target)) {
      keyBuffer.reset();
      return;
    }

    keyBuffer.push(event.key);

    if (cmd && key === "s") {
      cancelEvent(event);
      saveNotebook(hook);
    } else if (keyBuffer.tryMatch(["d", "d"])) {
      deleteFocusedCell(hook);
    } else if (
      hook.state.focusedCellType === "elixir" &&
      (keyBuffer.tryMatch(["e", "e"]) || (cmd && key === "Enter"))
    ) {
      queueFocusedCellEvaluation(hook);
    } else if (keyBuffer.tryMatch(["e", "a"])) {
      queueAllCellsEvaluation(hook);
    } else if (keyBuffer.tryMatch(["e", "s"])) {
      queueFocusedSectionEvaluation(hook);
    } else if (keyBuffer.tryMatch(["e", "j"])) {
      queueChildCellsEvaluation(hook);
    } else if (keyBuffer.tryMatch(["s", "s"])) {
      toggleSectionsList(hook);
    } else if (keyBuffer.tryMatch(["s", "u"])) {
      toggleClientsList(hook);
    } else if (keyBuffer.tryMatch(["s", "r"])) {
      showNotebookRuntimeSettings(hook);
    } else if (keyBuffer.tryMatch(["s", "b"])) {
      showBin(hook);
    } else if (keyBuffer.tryMatch(["e", "x"])) {
      cancelFocusedCellEvaluation(hook);
    } else if (keyBuffer.tryMatch(["0", "0"])) {
      restartRuntime(hook);
    } else if (keyBuffer.tryMatch(["?"])) {
      showShortcuts(hook);
    } else if (
      keyBuffer.tryMatch(["i"]) ||
      (event.target === document.body &&
        hook.state.focusedCellId &&
        key === "Enter")
    ) {
      cancelEvent(event);
      enterInsertMode(hook);
    } else if (keyBuffer.tryMatch(["j"])) {
      moveCellFocus(hook, 1);
    } else if (keyBuffer.tryMatch(["k"])) {
      moveCellFocus(hook, -1);
    } else if (keyBuffer.tryMatch(["J"])) {
      moveFocusedCell(hook, 1);
    } else if (keyBuffer.tryMatch(["K"])) {
      moveFocusedCell(hook, -1);
    } else if (keyBuffer.tryMatch(["n"])) {
      insertCellBelowFocused(hook, "elixir");
    } else if (keyBuffer.tryMatch(["N"])) {
      insertCellAboveFocused(hook, "elixir");
    } else if (keyBuffer.tryMatch(["m"])) {
      insertCellBelowFocused(hook, "markdown");
    } else if (keyBuffer.tryMatch(["M"])) {
      insertCellAboveFocused(hook, "markdown");
    }
  }
}

/**
 * Focuses/blurs a cell when the user clicks somewhere.
 *
 * Note: we use mousedown event to more reliably focus editor
 * (e.g. if the user starts selecting some text within the editor)
 */
function handleDocumentMouseDown(hook, event) {
  // If the click is outside the notebook element, keep the focus as is
  if (!event.target.closest(`[data-element="notebook"]`)) {
    if (hook.state.insertMode) {
      setInsertMode(hook, false);
    }
    return;
  }

  // If click targets a clickable element that awaits mouse up, keep the focus as is
  if (event.target.closest(`a, button`)) {
    // If the pencil icon is clicked, enter insert mode
    if (event.target.closest(`[data-element="enable-insert-mode-button"]`)) {
      setInsertMode(hook, true);
    }

    return;
  }

  // Find the cell element, if one was clicked
  const cell = event.target.closest(`[data-element="cell"]`);
  const cellId = cell ? cell.dataset.cellId : null;
  const insertMode = editableElementClicked(event, cell);

  if (cellId !== hook.state.focusedCellId) {
    setFocusedCell(hook, cellId, !insertMode);
  }

  // Depending on whether the click targets editor disable/enable insert mode
  if (hook.state.insertMode !== insertMode) {
    setInsertMode(hook, insertMode);
  }
}

function editableElementClicked(event, cell) {
  if (cell) {
    const editorContainer = cell.querySelector(
      `[data-element="editor-container"]`
    );
    const input = cell.querySelector(`[data-element="input"]`);
    const editableElement = editorContainer || input;

    return editableElement.contains(event.target);
  }

  return false;
}

/**
 * Enters insert mode when a markdown cell is double-clicked.
 */
function handleDocumentDoubleClick(hook, event) {
  const markdownCell = event.target.closest(
    `[data-element="cell"][data-type="markdown"]`
  );

  if (markdownCell && hook.state.focusedCellId && !hook.state.insertMode) {
    setInsertMode(hook, true);
  }
}

/**
 * Handles section link clicks in the section list.
 */
function handleSectionsListClick(hook, event) {
  const sectionButton = event.target.closest(
    `[data-element="sections-list-item"]`
  );
  if (sectionButton) {
    const sectionId = sectionButton.getAttribute("data-section-id");
    const section = getSectionById(sectionId);
    section.scrollIntoView({ behavior: "smooth", block: "start" });
  }
}

/**
 * Handles client link clicks in the clients list.
 */
function handleClientsListClick(hook, event) {
  const clientListItem = event.target.closest(
    `[data-element="clients-list-item"]`
  );

  if (clientListItem) {
    const clientPid = clientListItem.getAttribute("data-client-pid");

    const clientLink = event.target.closest(`[data-element="client-link"]`);
    if (clientLink) {
      handleClientLinkClick(hook, clientPid);
    }

    const clientFollowToggle = event.target.closest(
      `[data-element="client-follow-toggle"]`
    );
    if (clientFollowToggle) {
      handleClientFollowToggleClick(hook, clientPid, clientListItem);
    }
  }
}

function handleClientLinkClick(hook, clientPid) {
  mirrorClientFocus(hook, clientPid);
}

function handleClientFollowToggleClick(hook, clientPid, clientListItem) {
  const followedClientListItem = document.querySelector(
    `[data-element="clients-list-item"][data-js-followed]`
  );

  if (followedClientListItem) {
    followedClientListItem.removeAttribute("data-js-followed");
  }

  if (clientPid === hook.state.followedClientPid) {
    hook.state.followedClientPid = null;
  } else {
    clientListItem.setAttribute("data-js-followed", "true");
    hook.state.followedClientPid = clientPid;
    mirrorClientFocus(hook, clientPid);
  }
}

function mirrorClientFocus(hook, clientPid) {
  const locationReport = hook.state.lastLocationReportByClientPid[clientPid];

  if (locationReport && locationReport.cellId) {
    setFocusedCell(hook, locationReport.cellId);
  }
}

/**
 * Handles button clicks within cell indicators section.
 */
function handleCellIndicatorsClick(hook, event) {
  const button = event.target.closest(`[data-element="focus-cell-button"]`);
  if (button) {
    const cellId = button.getAttribute("data-target");
    setFocusedCell(hook, cellId);
  }
}

/**
 * Focuses cell based on the given URL.
 */
function focusCellFromUrl(hook) {
  const hash = window.location.hash;

  if (hash.startsWith("#cell-")) {
    const cellId = hash.replace(/^#cell-/, "");
    if (getCellById(cellId)) {
      setFocusedCell(hook, cellId);
    }
  } else {
    // Explicitly scroll to the target element
    // after the loading finishes
    const htmlId = hash.replace(/^#/, "");
    const element = document.getElementById(htmlId);
    if (element) {
      element.scrollIntoView();
    }
  }
}

/**
 * Handles the main notebook area being scrolled.
 */
function updateSectionListHighlight() {
  const currentListItem = document.querySelector(
    `[data-element="sections-list-item"][data-js-is-viewed]`
  );

  if (currentListItem) {
    currentListItem.removeAttribute("data-js-is-viewed");
  }

  // Consider a section being viewed if it is within the top 35% of the screen
  const viewedSection = getSections()
    .reverse()
    .find((section) => {
      const { top } = section.getBoundingClientRect();
      const scrollTop = document.documentElement.scrollTop;
      return top <= scrollTop + window.innerHeight * 0.35;
    });

  if (viewedSection) {
    const sectionId = viewedSection.getAttribute("data-section-id");
    const listItem = document.querySelector(
      `[data-element="sections-list-item"][data-section-id="${sectionId}"]`
    );
    listItem.setAttribute("data-js-is-viewed", "true");
  }
}

// User action handlers (mostly keybindings)

function toggleSectionsList(hook) {
  if (hook.el.getAttribute("data-js-side-panel-content") === "sections-list") {
    hook.el.removeAttribute("data-js-side-panel-content");
  } else {
    hook.el.setAttribute("data-js-side-panel-content", "sections-list");
  }
}

function toggleClientsList(hook) {
  if (hook.el.getAttribute("data-js-side-panel-content") === "clients-list") {
    hook.el.removeAttribute("data-js-side-panel-content");
  } else {
    hook.el.setAttribute("data-js-side-panel-content", "clients-list");
  }
}

function showNotebookRuntimeSettings(hook) {
  hook.pushEvent("show_runtime_settings", {});
}

function showBin(hook) {
  hook.pushEvent("show_bin", {});
}

function saveNotebook(hook) {
  hook.pushEvent("save", {});
}

function deleteFocusedCell(hook) {
  if (hook.state.focusedCellId) {
    hook.pushEvent("delete_cell", { cell_id: hook.state.focusedCellId });
  }
}

function queueFocusedCellEvaluation(hook) {
  if (hook.state.focusedCellId) {
    hook.pushEvent("queue_cell_evaluation", {
      cell_id: hook.state.focusedCellId,
    });
  }
}

function queueAllCellsEvaluation(hook) {
  hook.pushEvent("queue_all_cells_evaluation", {});
}

function queueFocusedSectionEvaluation(hook) {
  if (hook.state.focusedSectionId) {
    hook.pushEvent("queue_section_cells_evaluation", {
      section_id: hook.state.focusedSectionId,
    });
  }
}

function queueChildCellsEvaluation(hook) {
  if (hook.state.focusedCellId) {
    hook.pushEvent("queue_child_cells_evaluation", {
      cell_id: hook.state.focusedCellId,
    });
  }
}

function cancelFocusedCellEvaluation(hook) {
  if (hook.state.focusedCellId) {
    hook.pushEvent("cancel_cell_evaluation", {
      cell_id: hook.state.focusedCellId,
    });
  }
}

function restartRuntime(hook) {
  hook.pushEvent("restart_runtime", {});
}

function showShortcuts(hook) {
  hook.pushEvent("show_shortcuts", {});
}

function enterInsertMode(hook) {
  if (hook.state.focusedCellId) {
    setInsertMode(hook, true);
  }
}

function escapeInsertMode(hook) {
  setInsertMode(hook, false);
}

function moveCellFocus(hook, offset) {
  const cellId = nearbyCellId(hook.state.focusedCellId, offset);
  setFocusedCell(hook, cellId);
}

function moveFocusedCell(hook, offset) {
  if (hook.state.focusedCellId) {
    hook.pushEvent("move_cell", { cell_id: hook.state.focusedCellId, offset });
  }
}

function insertCellBelowFocused(hook, type) {
  if (hook.state.focusedCellId) {
    hook.pushEvent("insert_cell_below", {
      cell_id: hook.state.focusedCellId,
      type,
    });
  } else {
    // If no cell is focused, insert below the last cell
    const cellIds = getCellIds();
    if (cellIds.length > 0) {
      const lastCellId = cellIds[cellIds.length - 1];
      hook.pushEvent("insert_cell_below", { cell_id: lastCellId, type });
    } else {
      insertFirstCell(hook, type);
    }
  }
}

function insertCellAboveFocused(hook, type) {
  if (hook.state.focusedCellId) {
    hook.pushEvent("insert_cell_above", {
      cell_id: hook.state.focusedCellId,
      type,
    });
  } else {
    // If no cell is focused, insert above the first cell
    const cellIds = getCellIds();
    if (cellIds.length > 0) {
      const lastCellId = cellIds[0];
      hook.pushEvent("insert_cell_above", { cell_id: lastCellId, type });
    } else {
      insertFirstCell(hook, type);
    }
  }
}

function insertFirstCell(hook, type) {
  const sectionIds = getSectionIds();

  if (sectionIds.length > 0) {
    hook.pushEvent("insert_cell", {
      section_id: sectionIds[0],
      index: 0,
      type,
    });
  }
}

function setFocusedCell(hook, cellId, scroll = true) {
  hook.state.focusedCellId = cellId;

  if (hook.state.focusedCellId) {
    const cell = getCellById(hook.state.focusedCellId);
    hook.state.focusedCellType = cell.getAttribute("data-type");
    hook.state.focusedSectionId = getSectionIdByCellId(
      hook.state.focusedCellId
    );
  } else {
    hook.state.focusedCellType = null;
    hook.state.focusedSectionId = null;
  }

  globalPubSub.broadcast("cells", { type: "cell_focused", cellId, scroll });

  setInsertMode(hook, false);
}

function setInsertMode(hook, insertModeEnabled) {
  hook.state.insertMode = insertModeEnabled;

  if (insertModeEnabled) {
    hook.el.setAttribute("data-js-insert-mode", "true");
  } else {
    hook.el.removeAttribute("data-js-insert-mode");

    sendLocationReport(hook, {
      cellId: hook.state.focusedCellId,
      selection: null,
    });
  }

  globalPubSub.broadcast("cells", {
    type: "insert_mode_changed",
    enabled: insertModeEnabled,
  });
}

// Server event handlers

function handleCellInserted(hook, cellId) {
  setFocusedCell(hook, cellId);
  if (["markdown", "elixir"].includes(hook.state.focusedCellType)) {
    setInsertMode(hook, true);
  }
}

function handleCellDeleted(hook, cellId, siblingCellId) {
  if (hook.state.focusedCellId === cellId) {
    setFocusedCell(hook, siblingCellId);
  }
}

function handleCellRestored(hook, cellId) {
  setFocusedCell(hook, cellId);
}

function handleCellMoved(hook, cellId) {
  if (hook.state.focusedCellId === cellId) {
    globalPubSub.broadcast("cells", { type: "cell_moved", cellId });

    // The cell may have moved to another section, so update this information.
    hook.state.focusedSectionId = getSectionIdByCellId(
      hook.state.focusedCellId
    );
  }
}

function handleSectionInserted(hook, sectionId) {
  if (hook.state.focusedSectionId) {
    setFocusedCell(hook, null);
  }

  const section = getSectionById(sectionId);
  const nameElement = section.querySelector(`[data-element="section-name"]`);
  nameElement.focus({ preventScroll: true });
  selectElementContent(nameElement);
  smoothlyScrollToElement(nameElement);
}

function handleSectionDeleted(hook, sectionId) {
  if (hook.state.focusedSectionId === sectionId) {
    setFocusedCell(hook, null);
  }
}

function handleSectionMoved(hook, sectionId) {
  const section = getSectionById(sectionId);
  smoothlyScrollToElement(section);
}

function handleCellUpload(hook, cellId, url) {
  if (hook.state.focusedCellId !== cellId) {
    setFocusedCell(hook, cellId);
  }

  if (!hook.state.insertMode) {
    setInsertMode(hook, true);
  }

  globalPubSub.broadcast("cells", { type: "cell_upload", cellId, url });
}

function handleClientJoined(hook, client) {
  hook.state.clientsMap[client.pid] = client;
}

function handleClientLeft(hook, clientPid) {
  const client = hook.state.clientsMap[clientPid];

  if (client) {
    delete hook.state.clientsMap[clientPid];

    broadcastLocationReport(client, { cellId: null, selection: null });

    if (client.pid === hook.state.followedClientPid) {
      hook.state.followedClientPid = null;
    }
  }
}

function handleClientsUpdated(hook, updatedClients) {
  updatedClients.forEach((client) => {
    hook.state.clientsMap[client.pid] = client;
  });
}

function handleLocationReport(hook, clientPid, report) {
  const client = hook.state.clientsMap[clientPid];

  hook.state.lastLocationReportByClientPid[clientPid] = report;

  if (client) {
    broadcastLocationReport(client, report);

    if (
      client.pid === hook.state.followedClientPid &&
      report.cellId !== hook.state.focusedCellId
    ) {
      setFocusedCell(hook, report.cellId);
    }
  }
}

function focusNotebookNameIfNew() {
  const sections = getSections();
  const nameElement = document.querySelector(`[data-element="notebook-name"]`);

  if (sections.length === 0 && nameElement.innerText === "Untitled notebook") {
    nameElement.focus();
    selectElementContent(nameElement);
  }
}

// Session event handlers

function handleSessionEvent(hook, event) {
  if (event.type === "cursor_selection_changed") {
    sendLocationReport(hook, {
      cellId: event.cellId,
      selection: event.selection,
    });
  }
}

/**
 * Broadcast new location report coming from the server to all the cells.
 */
function broadcastLocationReport(client, report) {
  globalPubSub.broadcast("cells", {
    type: "location_report",
    client,
    report,
  });
}

/**
 * Sends local location report to the server.
 */
function sendLocationReport(hook, report) {
  const numberOfClients = Object.keys(hook.state.clientsMap).length;

  // Only send reports if there are other people to send to
  if (numberOfClients > 1) {
    hook.pushEvent("location_report", {
      cell_id: report.cellId,
      selection: encodeSelection(report.selection),
    });
  }
}

function encodeSelection(selection) {
  if (selection === null) return null;

  return [
    selection.selectionStartLineNumber,
    selection.selectionStartColumn,
    selection.positionLineNumber,
    selection.positionColumn,
  ];
}

function decodeSelection(encoded) {
  if (encoded === null) return null;

  const [
    selectionStartLineNumber,
    selectionStartColumn,
    positionLineNumber,
    positionColumn,
  ] = encoded;

  return new monaco.Selection(
    selectionStartLineNumber,
    selectionStartColumn,
    positionLineNumber,
    positionColumn
  );
}

// Helpers

function nearbyCellId(cellId, offset) {
  const cellIds = getCellIds();

  if (cellIds.length === 0) {
    return null;
  }

  const idx = cellIds.indexOf(cellId);

  if (idx === -1) {
    return cellIds[0];
  } else {
    const siblingIdx = clamp(idx + offset, 0, cellIds.length - 1);
    return cellIds[siblingIdx];
  }
}

function getCellIds() {
  const cells = getCells();
  return cells.map((cell) => cell.getAttribute("data-cell-id"));
}

function getCells() {
  return Array.from(document.querySelectorAll(`[data-element="cell"]`));
}

function getCellById(cellId) {
  return document.querySelector(
    `[data-element="cell"][data-cell-id="${cellId}"]`
  );
}

function getSectionIdByCellId(cellId) {
  const cell = document.querySelector(
    `[data-element="cell"][data-cell-id="${cellId}"]`
  );
  const section = cell.closest(`[data-element="section"]`);
  return section.getAttribute("data-section-id");
}

function getSectionIds() {
  const sections = getSections();
  return sections.map((section) => section.getAttribute("data-section-id"));
}

function getSections() {
  return Array.from(document.querySelectorAll(`[data-element="section"]`));
}

function getSectionById(sectionId) {
  return document.querySelector(
    `[data-element="section"][data-section-id="${sectionId}"]`
  );
}

function getSectionsList() {
  return document.querySelector(`[data-element="sections-list"]`);
}

function getClientsList() {
  return document.querySelector(`[data-element="clients-list"]`);
}

function getCellIndicators() {
  return document.querySelector(`[data-element="notebook-indicators"]`);
}

function getNotebook() {
  return document.querySelector(`[data-element="notebook"]`);
}

function getSectionsListToggle() {
  return document.querySelector(`[data-element="sections-list-toggle"]`);
}

function getClientsListToggle() {
  return document.querySelector(`[data-element="clients-list-toggle"]`);
}

function cancelEvent(event) {
  // Cancel any default browser behavior.
  event.preventDefault();
  // Stop event propagation (e.g. so it doesn't reach the editor).
  event.stopPropagation();
}

export default Session;
