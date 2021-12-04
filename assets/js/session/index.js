import {
  isMacOS,
  isEditableElement,
  clamp,
  selectElementContent,
  smoothlyScrollToElement,
  setFavicon,
  cancelEvent,
} from "../lib/utils";
import { getAttributeOrDefault } from "../lib/attribute";
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
 * Configuration:
 *
 *   * `data-autofocus-cell-id` - id of the cell that gets initial
 *     focus once the notebook is loaded
 *
 * ## Shortcuts
 *
 * This hook registers session shortcut handlers,
 * see `LivebookWeb.SessionLive.ShortcutsComponent`
 * for the complete list of available shortcuts.
 *
 * ## Navigation
 *
 * This hook handles focusing section titles, cells and moving the focus around,
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
    this.props = getProps(this);
    this.state = {
      focusedId: null,
      focusedCellType: null,
      insertMode: false,
      keyBuffer: new KeyBuffer(),
      clientsMap: {},
      lastLocationReportByClientPid: {},
      followedClientPid: null,
    };

    // Set initial favicon based on the current status

    setFavicon(faviconForEvaluationStatus(this.props.globalStatus));

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

    this.handleDocumentFocus = (event) => {
      handleDocumentFocus(this, event);
    };

    // Note: the focus event doesn't bubble, so we register for the capture phase
    document.addEventListener("focus", this.handleDocumentFocus, true);

    this.handleDocumentClick = (event) => {
      handleDocumentClick(this, event);
    };

    document.addEventListener("click", this.handleDocumentClick);

    this.handleDocumentDoubleClick = (event) => {
      handleDocumentDoubleClick(this, event);
    };

    document.addEventListener("dblclick", this.handleDocumentDoubleClick);

    getSectionsList().addEventListener("click", (event) => {
      handleSectionsListClick(this, event);
      handleCellIndicatorsClick(this, event);
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

    getRuntimeInfoToggle().addEventListener("click", (event) => {
      toggleRuntimeInfo(this);
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
        initializeFocus(this);
      },
      { once: true }
    );

    // DOM setup

    updateSectionListHighlight();

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
      ({ client_pid, focusable_id, selection }) => {
        const report = {
          focusableId: focusable_id,
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

  updated() {
    const prevProps = this.props;
    this.props = getProps(this);

    if (this.props.globalStatus !== prevProps.globalStatus) {
      setFavicon(faviconForEvaluationStatus(this.props.globalStatus));
    }
  },

  destroyed() {
    this._unsubscribeFromSessionEvents();

    document.removeEventListener("keydown", this.handleDocumentKeyDown, true);
    document.removeEventListener("mousedown", this.handleDocumentMouseDown);
    document.removeEventListener("focus", this.handleDocumentFocus, true);
    document.removeEventListener("click", this.handleDocumentClick);
    document.removeEventListener("dblclick", this.handleDocumentDoubleClick);

    setFavicon("favicon");
  },
};

function getProps(hook) {
  return {
    autofocusCellId: getAttributeOrDefault(
      hook.el,
      "data-autofocus-cell-id",
      null
    ),
    globalStatus: getAttributeOrDefault(hook.el, "data-global-status", null),
  };
}

function faviconForEvaluationStatus(evaluationStatus) {
  if (evaluationStatus === "evaluating") return "favicon-evaluating";
  if (evaluationStatus === "stale") return "favicon-stale";
  return "favicon";
}

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
 * @property {String|null} focusableId
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

      const editor = event.target.closest(".monaco-editor.focused");

      const completionBoxOpen = !!(
        editor &&
        editor.querySelector(".editor-widget.parameter-hints-widget.visible")
      );
      const signatureDetailsOpen = !!(
        editor && editor.querySelector(".editor-widget.suggest-widget.visible")
      );

      // Ignore Escape if it's supposed to close some Monaco input
      // (like the find/replace box), or an intellisense widget
      if (!monacoInputOpen && !completionBoxOpen && !signatureDetailsOpen) {
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
    // Ignore keystrokes on input fields
    if (isEditableElement(event.target)) {
      keyBuffer.reset();

      // Use Escape for universal blur
      if (key === "Escape") {
        event.target.blur();
      }

      return;
    }

    keyBuffer.push(event.key);

    if (cmd && key === "s") {
      cancelEvent(event);
      saveNotebook(hook);
    } else if (keyBuffer.tryMatch(["d", "d"])) {
      deleteFocusedCell(hook);
    } else if (keyBuffer.tryMatch(["e", "e"]) || (cmd && key === "Enter")) {
      if (hook.state.focusedCellType === "elixir") {
        queueFocusedCellEvaluation(hook);
      }
    } else if (keyBuffer.tryMatch(["e", "a"])) {
      queueAllCellsEvaluation(hook);
    } else if (keyBuffer.tryMatch(["e", "s"])) {
      queueFocusedSectionEvaluation(hook);
    } else if (keyBuffer.tryMatch(["s", "s"])) {
      toggleSectionsList(hook);
    } else if (keyBuffer.tryMatch(["s", "u"])) {
      toggleClientsList(hook);
    } else if (keyBuffer.tryMatch(["s", "r"])) {
      toggleRuntimeInfo(hook);
    } else if (keyBuffer.tryMatch(["s", "b"])) {
      showBin(hook);
    } else if (keyBuffer.tryMatch(["e", "x"])) {
      cancelFocusedCellEvaluation(hook);
    } else if (keyBuffer.tryMatch(["0", "0"])) {
      restartRuntime(hook);
    } else if (keyBuffer.tryMatch(["Escape", "Escape"])) {
      setFocusedEl(hook, null);
    } else if (keyBuffer.tryMatch(["?"])) {
      showShortcuts(hook);
    } else if (
      keyBuffer.tryMatch(["i"]) ||
      (event.target === document.body &&
        hook.state.focusedId &&
        key === "Enter")
    ) {
      cancelEvent(event);
      enterInsertMode(hook);
    } else if (keyBuffer.tryMatch(["j"])) {
      moveFocus(hook, 1);
    } else if (keyBuffer.tryMatch(["k"])) {
      moveFocus(hook, -1);
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

  // Find the focusable element, if one was clicked
  const focusableEl = event.target.closest(`[data-focusable-id]`);
  const focusableId = focusableEl ? focusableEl.dataset.focusableId : null;
  const insertMode = editableElementClicked(event, focusableEl);

  if (focusableId !== hook.state.focusedId) {
    setFocusedEl(hook, focusableId, { scroll: false, focusElement: false });
  }

  // If a cell action is clicked, keep the insert mode as is
  if (event.target.closest(`[data-element="actions"]`)) {
    return;
  }

  // Depending on whether the click targets editor or input disable/enable insert mode
  if (hook.state.insertMode !== insertMode) {
    setInsertMode(hook, insertMode);
  }
}

function editableElementClicked(event, element) {
  if (element) {
    const editableElement = element.querySelector(
      `[data-element="editor-container"], [data-element="heading"]`
    );
    return editableElement.contains(event.target);
  }

  return false;
}

/**
 * Focuses a focusable element if the user "tab"s anywhere into it.
 */
function handleDocumentFocus(hook, event) {
  const focusableEl = event.target.closest(`[data-focusable-id]`);

  if (focusableEl) {
    const focusableId = focusableEl.dataset.focusableId;

    if (focusableId !== hook.state.focusedId) {
      setFocusedEl(hook, focusableId, { scroll: false, focusElement: false });
    }
  }
}

/**
 * Enters insert mode when markdown edit action is clicked.
 */
function handleDocumentClick(hook, event) {
  if (event.target.closest(`[data-element="enable-insert-mode-button"]`)) {
    setInsertMode(hook, true);
  }
}

/**
 * Enters insert mode when a markdown cell is double-clicked.
 */
function handleDocumentDoubleClick(hook, event) {
  const markdownCell = event.target.closest(
    `[data-element="cell"][data-type="markdown"]`
  );

  if (markdownCell && hook.state.focusedId && !hook.state.insertMode) {
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

  if (locationReport && locationReport.focusableId) {
    setFocusedEl(hook, locationReport.focusableId);
  }
}

/**
 * Handles button clicks within cell indicators section.
 */
function handleCellIndicatorsClick(hook, event) {
  const button = event.target.closest(`[data-element="focus-cell-button"]`);
  if (button) {
    const cellId = button.getAttribute("data-target");
    setFocusedEl(hook, cellId);
  }
}

/**
 * Focuses cell or any other element based on the current
 * URL and hook attributes.
 */
function initializeFocus(hook) {
  const hash = window.location.hash;

  if (hash) {
    const htmlId = hash.replace(/^#/, "");
    const element = document.getElementById(htmlId);

    if (element) {
      const focusableEl = element.closest("[data-focusable-id]");

      if (focusableEl) {
        setFocusedEl(hook, focusableEl.dataset.focusableId);
      } else {
        // Explicitly scroll to the target element
        // after the loading finishes
        element.scrollIntoView();
      }
    }
  } else if (hook.props.autofocusCellId) {
    setFocusedEl(hook, hook.props.autofocusCellId, { scroll: false });
    setInsertMode(hook, true);
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
  toggleSidePanelContent(hook, "sections-list");
}

function toggleClientsList(hook) {
  toggleSidePanelContent(hook, "clients-list");
}

function toggleRuntimeInfo(hook) {
  toggleSidePanelContent(hook, "runtime-info");
}

function toggleSidePanelContent(hook, name) {
  if (hook.el.getAttribute("data-js-side-panel-content") === name) {
    hook.el.removeAttribute("data-js-side-panel-content");
  } else {
    hook.el.setAttribute("data-js-side-panel-content", name);
  }
}

function showBin(hook) {
  hook.pushEvent("show_bin", {});
}

function saveNotebook(hook) {
  hook.pushEvent("save", {});
}

function deleteFocusedCell(hook) {
  if (hook.state.focusedId && isCell(hook.state.focusedId)) {
    hook.pushEvent("delete_cell", { cell_id: hook.state.focusedId });
  }
}

function queueFocusedCellEvaluation(hook) {
  if (hook.state.focusedId && isCell(hook.state.focusedId)) {
    hook.pushEvent("queue_cell_evaluation", {
      cell_id: hook.state.focusedId,
    });
  }
}

function queueAllCellsEvaluation(hook) {
  hook.pushEvent("queue_all_cells_evaluation", {});
}

function queueFocusedSectionEvaluation(hook) {
  if (hook.state.focusedId) {
    const sectionId = getSectionIdByFocusableId(hook.state.focusedId);

    if (sectionId) {
      hook.pushEvent("queue_section_cells_evaluation", {
        section_id: sectionId,
      });
    }
  }
}

function cancelFocusedCellEvaluation(hook) {
  if (hook.state.focusedId && isCell(hook.state.focusedId)) {
    hook.pushEvent("cancel_cell_evaluation", {
      cell_id: hook.state.focusedId,
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
  if (hook.state.focusedId) {
    setInsertMode(hook, true);
  }
}

function escapeInsertMode(hook) {
  setInsertMode(hook, false);
}

function moveFocus(hook, offset) {
  const focusableId = nearbyFocusableId(hook.state.focusedId, offset);
  setFocusedEl(hook, focusableId);
}

function moveFocusedCell(hook, offset) {
  if (hook.state.focusedId && isCell(hook.state.focusedId)) {
    hook.pushEvent("move_cell", { cell_id: hook.state.focusedId, offset });
  }
}

function insertCellBelowFocused(hook, type) {
  if (hook.state.focusedId) {
    insertCellBelowFocusableId(hook, hook.state.focusedId, type);
  } else {
    const focusableIds = getFocusableIds();
    if (focusableIds.length > 0) {
      insertCellBelowFocusableId(
        hook,
        focusableIds[focusableIds.length - 1],
        type
      );
    }
  }
}

function insertCellAboveFocused(hook, type) {
  if (hook.state.focusedId) {
    const prevFocusableId = nearbyFocusableId(hook.state.focusedId, -1);
    insertCellBelowFocusableId(hook, prevFocusableId, type);
  } else {
    const focusableIds = getFocusableIds();
    if (focusableIds.length > 0) {
      insertCellBelowFocusableId(hook, focusableIds[0], type);
    }
  }
}

function insertCellBelowFocusableId(hook, focusableId, type) {
  if (isCell(focusableId)) {
    hook.pushEvent("insert_cell_below", { type, cell_id: focusableId });
  } else if (isSection(focusableId)) {
    hook.pushEvent("insert_cell_below", { type, section_id: focusableId });
  } else if (isNotebook(focusableId)) {
    const sectionIds = getSectionIds();
    if (sectionIds.length > 0) {
      hook.pushEvent("insert_cell_below", { type, section_id: sectionIds[0] });
    }
  }
}

function setFocusedEl(
  hook,
  focusableId,
  { scroll = true, focusElement = true } = {}
) {
  hook.state.focusedId = focusableId;

  if (focusableId) {
    const el = getFocusableEl(focusableId);

    if (isCell(focusableId)) {
      hook.state.focusedCellType = el.getAttribute("data-type");
    }

    if (focusElement) {
      // Focus the primary content in the focusable element, this is important for screen readers
      const contentEl =
        el.querySelector(`[data-element="cell-body"]`) ||
        el.querySelector(`[data-element="heading"]`) ||
        el;
      contentEl.focus({ preventScroll: true });
    }
  } else {
    hook.state.focusedCellType = null;
  }

  globalPubSub.broadcast("navigation", {
    type: "element_focused",
    focusableId: focusableId,
    scroll,
  });

  setInsertMode(hook, false);
}

function setInsertMode(hook, insertModeEnabled) {
  hook.state.insertMode = insertModeEnabled;

  if (insertModeEnabled) {
    hook.el.setAttribute("data-js-insert-mode", "true");
  } else {
    hook.el.removeAttribute("data-js-insert-mode");

    sendLocationReport(hook, {
      focusableId: hook.state.focusedId,
      selection: null,
    });
  }

  globalPubSub.broadcast("navigation", {
    type: "insert_mode_changed",
    enabled: insertModeEnabled,
  });
}

// Server event handlers

function handleCellInserted(hook, cellId) {
  setFocusedEl(hook, cellId);
  if (["markdown", "elixir"].includes(hook.state.focusedCellType)) {
    setInsertMode(hook, true);
  }
}

function handleCellDeleted(hook, cellId, siblingCellId) {
  if (hook.state.focusedId === cellId) {
    setFocusedEl(hook, siblingCellId);
  }
}

function handleCellRestored(hook, cellId) {
  setFocusedEl(hook, cellId);
}

function handleCellMoved(hook, cellId) {
  if (hook.state.focusedId === cellId) {
    globalPubSub.broadcast("cells", { type: "cell_moved", cellId });
  }
}

function handleSectionInserted(hook, sectionId) {
  const section = getSectionById(sectionId);
  const headlineEl = section.querySelector(`[data-element="section-headline"]`);
  const { focusableId } = headlineEl.dataset;
  setFocusedEl(hook, focusableId);
  setInsertMode(hook, true);
  selectElementContent(document.activeElement);
}

function handleSectionDeleted(hook, sectionId) {
  // Clear focus if the element no longer exists
  if (hook.state.focusedId && !getFocusableEl(hook.state.focusedId)) {
    setFocusedEl(hook, null);
  }
}

function handleSectionMoved(hook, sectionId) {
  const section = getSectionById(sectionId);
  smoothlyScrollToElement(section);
}

function handleCellUpload(hook, cellId, url) {
  if (hook.state.focusedId !== cellId) {
    setFocusedEl(hook, cellId);
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

    broadcastLocationReport(client, { focusableId: null, selection: null });

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
      report.focusableId !== hook.state.focusedId
    ) {
      setFocusedEl(hook, report.focusableId);
    }
  }
}

// Session event handlers

function handleSessionEvent(hook, event) {
  if (event.type === "cursor_selection_changed") {
    sendLocationReport(hook, {
      focusableId: event.focusableId,
      selection: event.selection,
    });
  }
}

/**
 * Broadcast new location report coming from the server to all the cells.
 */
function broadcastLocationReport(client, report) {
  globalPubSub.broadcast("navigation", {
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
      focusable_id: report.focusableId,
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

function nearbyFocusableId(focusableId, offset) {
  const focusableIds = getFocusableIds();

  if (focusableIds.length === 0) {
    return null;
  }

  const idx = focusableIds.indexOf(focusableId);

  if (idx === -1) {
    return focusableIds[0];
  } else {
    const siblingIdx = clamp(idx + offset, 0, focusableIds.length - 1);
    return focusableIds[siblingIdx];
  }
}

function isCell(focusableId) {
  const el = getFocusableEl(focusableId);
  return el.dataset.element === "cell";
}

function isSection(focusableId) {
  const el = getFocusableEl(focusableId);
  return el.dataset.element === "section-headline";
}

function isNotebook(focusableId) {
  const el = getFocusableEl(focusableId);
  return el.dataset.element === "notebook-headline";
}

function getFocusableEl(focusableId) {
  return document.querySelector(`[data-focusable-id="${focusableId}"]`);
}

function getFocusableIds() {
  const elements = Array.from(document.querySelectorAll(`[data-focusable-id]`));
  return elements.map((el) => el.getAttribute("data-focusable-id"));
}

function getSectionIdByFocusableId(focusableId) {
  const el = getFocusableEl(focusableId);
  const section = el.closest(`[data-element="section"]`);
  return section && section.getAttribute("data-section-id");
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

function getRuntimeInfoToggle() {
  return document.querySelector(`[data-element="runtime-info-toggle"]`);
}

export default Session;
