import {
  isMacOS,
  isEditableElement,
  clamp,
  selectElementContent,
  smoothlyScrollToElement,
  setFavicon,
  cancelEvent,
  isElementInViewport,
} from "../lib/utils";
import { getAttributeOrDefault } from "../lib/attribute";
import KeyBuffer from "../lib/key_buffer";
import { globalPubSub } from "../lib/pub_sub";
import monaco from "./cell_editor/live_editor/monaco";
import { leaveChannel } from "./js_view/channel";
import { isDirectlyEditable, isEvaluable } from "../lib/notebook";

/**
 * A hook managing the whole session.
 *
 * Serves as a coordinator handling all the global session events.
 * Note that each cell has its own hook, so that LV keeps track of
 * cells being added/removed from the DOM. We do however need to
 * communicate between this global hook and cells and for that we
 * use a simple local pubsub that the hooks subscribe to.
 *
 * ## Configuration
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
 * This hook handles focusing section titles, cells and moving the
 * focus around, this is done purely on the client side because it is
 * event-intensive and specific to this client only. The UI changes
 * are handled by setting `data-js-*` attributes and applying CSS
 * accordingly (see assets/css/js_interop.css). Navigation changes
 * are also broadcasted to all cell hooks via PubSub.
 *
 * ## Location tracking and following
 *
 * Location describes where the given client is within the notebook
 * (in which cell, and where specifically in that cell). When multiple
 * clients are connected, they report own location to each other
 * whenever it changes. We then each the location to show cursor and
 * selection indicators.
 *
 * Additionally the current user may follow another client from the
 * clients list. In such case, whenever a new location comes from that
 * client we move there automatically (i.e. we focus the same cells
 * to effectively mimic how the followed client moves around).
 *
 * Initially we load basic information about connected clients using
 * the `"session_init"` event and then update this information whenever
 * clients join/leave/update. This way location reports include only
 * client pid, as we already have the necessary hex_color/name locally.
 */
const Session = {
  mounted() {
    this.props = this.getProps();

    this.focusedId = null;
    this.insertMode = false;
    this.keyBuffer = new KeyBuffer();
    this.clientsMap = {};
    this.lastLocationReportByClientPid = {};
    this.followedClientPid = null;

    setFavicon(this.faviconForEvaluationStatus(this.props.globalStatus));

    this.updateSectionListHighlight();

    // DOM events

    this._handleDocumentKeyDown = this.handleDocumentKeyDown.bind(this);
    this._handleDocumentKeyDown = this.handleDocumentKeyDown.bind(this);
    this._handleDocumentMouseDown = this.handleDocumentMouseDown.bind(this);
    this._handleDocumentFocus = this.handleDocumentFocus.bind(this);
    this._handleDocumentClick = this.handleDocumentClick.bind(this);
    this._handleDocumentDoubleClick = this.handleDocumentDoubleClick.bind(this);

    document.addEventListener("keydown", this._handleDocumentKeyDown, true);
    document.addEventListener("mousedown", this._handleDocumentMouseDown);
    // Note: the focus event doesn't bubble, so we register for the capture phase
    document.addEventListener("focus", this._handleDocumentFocus, true);
    document.addEventListener("click", this._handleDocumentClick);
    document.addEventListener("dblclick", this._handleDocumentDoubleClick);

    this.getElement("sections-list").addEventListener("click", (event) => {
      this.handleSectionsListClick(event);
      this.handleCellIndicatorsClick(event);
    });

    this.getElement("clients-list").addEventListener("click", (event) =>
      this.handleClientsListClick(event)
    );

    this.getElement("sections-list-toggle").addEventListener("click", (event) =>
      this.toggleSectionsList()
    );

    this.getElement("clients-list-toggle").addEventListener("click", (event) =>
      this.toggleClientsList()
    );

    this.getElement("runtime-info-toggle").addEventListener("click", (event) =>
      this.toggleRuntimeInfo()
    );

    this.getElement("notebook").addEventListener("scroll", (event) =>
      this.updateSectionListHighlight()
    );

    this.getElement("notebook-indicators").addEventListener("click", (event) =>
      this.handleCellIndicatorsClick(event)
    );

    window.addEventListener(
      "phx:page-loading-stop",
      () => {
        this.initializeFocus();
      },
      { once: true }
    );

    // Server events

    this.handleEvent("session_init", ({ clients }) => {
      clients.forEach((client) => {
        this.clientsMap[client.pid] = client;
      });
    });

    this.handleEvent("cell_inserted", ({ cell_id: cellId }) => {
      this.handleCellInserted(cellId);
    });

    this.handleEvent(
      "cell_deleted",
      ({ cell_id: cellId, sibling_cell_id: siblingCellId }) => {
        this.handleCellDeleted(cellId, siblingCellId);
      }
    );

    this.handleEvent("cell_restored", ({ cell_id: cellId }) => {
      this.handleCellRestored(cellId);
    });

    this.handleEvent("cell_moved", ({ cell_id }) => {
      this.handleCellMoved(cell_id);
    });

    this.handleEvent("section_inserted", ({ section_id }) => {
      this.handleSectionInserted(section_id);
    });

    this.handleEvent("section_deleted", ({ section_id }) => {
      this.handleSectionDeleted(section_id);
    });

    this.handleEvent("section_moved", ({ section_id }) => {
      this.handleSectionMoved(section_id);
    });

    this.handleEvent("cell_upload", ({ cell_id, url }) => {
      this.handleCellUpload(cell_id, url);
    });

    this.handleEvent("client_joined", ({ client }) => {
      this.handleClientJoined(client);
    });

    this.handleEvent("client_left", ({ client_pid }) => {
      this.handleClientLeft(client_pid);
    });

    this.handleEvent("clients_updated", ({ clients }) => {
      this.handleClientsUpdated(clients);
    });

    this.handleEvent(
      "location_report",
      ({ client_pid, focusable_id, selection }) => {
        const report = {
          focusableId: focusable_id,
          selection: this.decodeSelection(selection),
        };

        this.handleLocationReport(client_pid, report);
      }
    );

    this.unsubscribeFromSessionEvents = globalPubSub.subscribe(
      "session",
      (event) => {
        this.handleSessionEvent(event);
      }
    );
  },

  updated() {
    const prevProps = this.props;
    this.props = this.getProps();

    if (this.props.globalStatus !== prevProps.globalStatus) {
      setFavicon(this.faviconForEvaluationStatus(this.props.globalStatus));
    }
  },

  disconnected() {
    // Reinitialize on reconnection
    this.el.removeAttribute("id");
  },

  destroyed() {
    this.unsubscribeFromSessionEvents();

    document.removeEventListener("keydown", this._handleDocumentKeyDown, true);
    document.removeEventListener("mousedown", this._handleDocumentMouseDown);
    document.removeEventListener("focus", this._handleDocumentFocus, true);
    document.removeEventListener("click", this._handleDocumentClick);
    document.removeEventListener("dblclick", this._handleDocumentDoubleClick);

    setFavicon("favicon");

    leaveChannel();
  },

  getProps() {
    return {
      autofocusCellId: getAttributeOrDefault(
        this.el,
        "data-autofocus-cell-id",
        null
      ),
      globalStatus: getAttributeOrDefault(this.el, "data-global-status", null),
    };
  },

  faviconForEvaluationStatus(evaluationStatus) {
    if (evaluationStatus === "evaluating") return "favicon-evaluating";
    if (evaluationStatus === "stale") return "favicon-stale";
    return "favicon";
  },

  // DOM event handlers

  /**
   * Handles session keybindings.
   *
   * Make sure to keep the shortcuts help modal up to date.
   */
  handleDocumentKeyDown(event) {
    if (event.repeat) {
      return;
    }

    const cmd = isMacOS() ? event.metaKey : event.ctrlKey;
    const alt = event.altKey;
    const shift = event.shiftKey;
    const key = event.key;
    const keyBuffer = this.keyBuffer;

    if (this.insertMode) {
      keyBuffer.reset();

      if (key === "Escape") {
        // Ignore Escape if it's supposed to close an editor widget
        if (!this.escapesMonacoWidget(event)) {
          this.escapeInsertMode();
        }
      } else if (cmd && shift && !alt && key === "Enter") {
        cancelEvent(event);
        this.queueFullCellsEvaluation(true);
      } else if (cmd && !alt && key === "Enter") {
        cancelEvent(event);
        if (isEvaluable(this.focusedCellType())) {
          this.queueFocusedCellEvaluation();
        }
      } else if (cmd && key === "s") {
        cancelEvent(event);
        this.saveNotebook();
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
        this.saveNotebook();
      } else if (keyBuffer.tryMatch(["d", "d"])) {
        this.deleteFocusedCell();
      } else if (cmd && shift && !alt && key === "Enter") {
        this.queueFullCellsEvaluation(true);
      } else if (keyBuffer.tryMatch(["e", "a"])) {
        this.queueFullCellsEvaluation(false);
      } else if (
        keyBuffer.tryMatch(["e", "e"]) ||
        (cmd && !alt && key === "Enter")
      ) {
        if (isEvaluable(this.focusedCellType())) {
          this.queueFocusedCellEvaluation();
        }
      } else if (keyBuffer.tryMatch(["e", "s"])) {
        this.queueFocusedSectionEvaluation();
      } else if (keyBuffer.tryMatch(["s", "s"])) {
        this.toggleSectionsList();
      } else if (keyBuffer.tryMatch(["s", "u"])) {
        this.toggleClientsList();
      } else if (keyBuffer.tryMatch(["s", "r"])) {
        this.toggleRuntimeInfo();
      } else if (keyBuffer.tryMatch(["s", "b"])) {
        this.showBin();
      } else if (keyBuffer.tryMatch(["s", "d"])) {
        this.showDependencySearch();
      } else if (keyBuffer.tryMatch(["e", "x"])) {
        this.cancelFocusedCellEvaluation();
      } else if (keyBuffer.tryMatch(["0", "0"])) {
        this.reconnectRuntime();
      } else if (keyBuffer.tryMatch(["Escape", "Escape"])) {
        this.setFocusedEl(null);
      } else if (keyBuffer.tryMatch(["?"])) {
        this.showShortcuts();
      } else if (
        keyBuffer.tryMatch(["i"]) ||
        (event.target === document.body && this.focusedId && key === "Enter")
      ) {
        cancelEvent(event);
        if (this.isInsertModeAvailable()) {
          this.enterInsertMode();
        }
      } else if (keyBuffer.tryMatch(["j"])) {
        this.moveFocus(1);
      } else if (keyBuffer.tryMatch(["k"])) {
        this.moveFocus(-1);
      } else if (keyBuffer.tryMatch(["J"])) {
        this.moveFocusedCell(1);
      } else if (keyBuffer.tryMatch(["K"])) {
        this.moveFocusedCell(-1);
      } else if (keyBuffer.tryMatch(["n"])) {
        this.insertCellBelowFocused("code");
      } else if (keyBuffer.tryMatch(["N"])) {
        this.insertCellAboveFocused("code");
      } else if (keyBuffer.tryMatch(["m"])) {
        this.insertCellBelowFocused("markdown");
      } else if (keyBuffer.tryMatch(["M"])) {
        this.insertCellAboveFocused("markdown");
      }
    }
  },

  escapesMonacoWidget(event) {
    // Escape pressed in an editor input
    if (event.target.closest(".monaco-inputbox")) {
      return true;
    }

    const editor = event.target.closest(".monaco-editor.focused");

    if (!editor) {
      return false;
    }

    // Completion box open
    if (editor.querySelector(".editor-widget.parameter-hints-widget.visible")) {
      return true;
    }

    // Signature details open
    if (editor.querySelector(".editor-widget.suggest-widget.visible")) {
      return true;
    }

    // Multi-cursor selection enabled
    if (editor.querySelectorAll(".cursor").length > 1) {
      return true;
    }

    return false;
  },

  /**
   * Focuses/blurs a cell when the user clicks somewhere.
   *
   * Note: we use mousedown event to more reliably focus editor
   * (e.g. if the user starts selecting some text within the editor)
   */
  handleDocumentMouseDown(event) {
    // If the click is outside the notebook element, keep the focus as is
    if (!event.target.closest(`[data-el-notebook]`)) {
      if (this.insertMode) {
        this.setInsertMode(false);
      }
      return;
    }

    // When clicking an insert button, keep focus and insert mode as is
    if (event.target.closest(`[data-el-insert-buttons] button`)) {
      return;
    }

    // Find the focusable element, if one was clicked
    const focusableEl = event.target.closest(`[data-focusable-id]`);
    const focusableId = focusableEl ? focusableEl.dataset.focusableId : null;
    const insertMode = this.editableElementClicked(event, focusableEl);

    if (focusableId !== this.focusedId) {
      this.setFocusedEl(focusableId, { scroll: false, focusElement: false });
    }

    // If a cell action is clicked, keep the insert mode as is
    if (event.target.closest(`[data-el-actions]`)) {
      return;
    }

    // Depending on whether the click targets editor or input disable/enable insert mode
    if (this.insertMode !== insertMode) {
      this.setInsertMode(insertMode);
    }
  },

  editableElementClicked(event, focusableEl) {
    if (focusableEl) {
      const editableElement = event.target.closest(
        `[data-el-editor-container], [data-el-heading]`
      );
      return editableElement && focusableEl.contains(editableElement);
    }

    return false;
  },

  /**
   * Focuses a focusable element if the user "tab"s anywhere into it.
   */
  handleDocumentFocus(event) {
    const focusableEl =
      event.target.closest && event.target.closest(`[data-focusable-id]`);

    if (focusableEl) {
      const focusableId = focusableEl.dataset.focusableId;

      if (focusableId !== this.focusedId) {
        this.setFocusedEl(focusableId, { scroll: false, focusElement: false });
      }
    }
  },

  /**
   * Enters insert mode when markdown edit action is clicked.
   */
  handleDocumentClick(event) {
    if (event.target.closest(`[data-el-enable-insert-mode-button]`)) {
      this.setInsertMode(true);
    }
  },

  /**
   * Enters insert mode when a markdown cell is double-clicked.
   */
  handleDocumentDoubleClick(event) {
    const cell = event.target.closest(`[data-el-cell]`);
    const type = cell && cell.getAttribute("data-type");

    if (
      type &&
      ["markdown", "setup"].includes(type) &&
      this.focusedId &&
      !this.insertMode
    ) {
      this.setInsertMode(true);
    }
  },

  /**
   * Handles section link clicks in the section list.
   */
  handleSectionsListClick(event) {
    const sectionButton = event.target.closest(`[data-el-sections-list-item]`);
    if (sectionButton) {
      const sectionId = sectionButton.getAttribute("data-section-id");
      const section = this.getSectionById(sectionId);
      section.scrollIntoView({ behavior: "smooth", block: "start" });
    }
  },

  /**
   * Handles client link clicks in the clients list.
   */
  handleClientsListClick(event) {
    const clientListItem = event.target.closest(`[data-el-clients-list-item]`);

    if (clientListItem) {
      const clientPid = clientListItem.getAttribute("data-client-pid");

      const clientLink = event.target.closest(`[data-el-client-link]`);
      if (clientLink) {
        this.handleClientLinkClick(clientPid);
      }

      const clientFollowToggle = event.target.closest(
        `[data-el-client-follow-toggle]`
      );
      if (clientFollowToggle) {
        this.handleClientFollowToggleClick(clientPid, clientListItem);
      }
    }
  },

  handleClientLinkClick(clientPid) {
    this.mirrorClientFocus(clientPid);
  },

  handleClientFollowToggleClick(clientPid, clientListItem) {
    const followedClientListItem = this.el.querySelector(
      `[data-el-clients-list-item][data-js-followed]`
    );

    if (followedClientListItem) {
      followedClientListItem.removeAttribute("data-js-followed");
    }

    if (clientPid === this.followedClientPid) {
      this.followedClientPid = null;
    } else {
      clientListItem.setAttribute("data-js-followed", "");
      this.followedClientPid = clientPid;
      this.mirrorClientFocus(clientPid);
    }
  },

  mirrorClientFocus(clientPid) {
    const locationReport = this.lastLocationReportByClientPid[clientPid];

    if (locationReport && locationReport.focusableId) {
      this.setFocusedEl(locationReport.focusableId);
    }
  },

  /**
   * Handles button clicks within cell indicators section.
   */
  handleCellIndicatorsClick(event) {
    const button = event.target.closest(`[data-el-focus-cell-button]`);
    if (button) {
      const cellId = button.getAttribute("data-target");
      this.setFocusedEl(cellId);
    }
  },

  /**
   * Focuses cell or any other element based on the current
   * URL and hook attributes.
   */
  initializeFocus() {
    const hash = window.location.hash;

    if (hash) {
      const htmlId = hash.replace(/^#/, "");
      const element = document.getElementById(htmlId);

      if (element) {
        const focusableEl = element.closest("[data-focusable-id]");

        if (focusableEl) {
          this.setFocusedEl(focusableEl.dataset.focusableId);
        } else {
          // Explicitly scroll to the target element
          // after the loading finishes
          element.scrollIntoView();
        }
      }
    } else if (this.props.autofocusCellId) {
      this.setFocusedEl(this.props.autofocusCellId, { scroll: false });
      this.setInsertMode(true);
    }
  },

  /**
   * Handles the main notebook area being scrolled.
   */
  updateSectionListHighlight() {
    const currentListItem = this.el.querySelector(
      `[data-el-sections-list-item][data-js-is-viewed]`
    );

    if (currentListItem) {
      currentListItem.removeAttribute("data-js-is-viewed");
    }

    // Consider a section being viewed if it is within the top 35% of the screen
    const viewedSection = this.getSections()
      .reverse()
      .find((section) => {
        const { top } = section.getBoundingClientRect();
        const scrollTop = document.documentElement.scrollTop;
        return top <= scrollTop + window.innerHeight * 0.35;
      });

    if (viewedSection) {
      const sectionId = viewedSection.getAttribute("data-section-id");
      const listItem = this.el.querySelector(
        `[data-el-sections-list-item][data-section-id="${sectionId}"]`
      );
      listItem.setAttribute("data-js-is-viewed", "");
    }
  },

  // User action handlers (mostly keybindings)

  toggleSectionsList() {
    this.toggleSidePanelContent("sections-list");
  },

  toggleClientsList() {
    this.toggleSidePanelContent("clients-list");
  },

  toggleRuntimeInfo() {
    this.toggleSidePanelContent("runtime-info");
  },

  toggleSidePanelContent(name) {
    if (this.el.getAttribute("data-js-side-panel-content") === name) {
      this.el.removeAttribute("data-js-side-panel-content");
    } else {
      this.el.setAttribute("data-js-side-panel-content", name);
    }
  },

  showBin() {
    const actionEl = this.el.querySelector(`[data-btn-show-bin]`);
    actionEl && actionEl.click();
  },

  showDependencySearch() {
    this.setFocusedEl("setup");

    const actionEl = this.el.querySelector(`[data-btn-dependency-search]`);
    actionEl && actionEl.click();
  },

  saveNotebook() {
    this.pushEvent("save", {});
  },

  deleteFocusedCell() {
    if (this.focusedId && this.isCell(this.focusedId)) {
      this.pushEvent("delete_cell", { cell_id: this.focusedId });
    }
  },

  queueFocusedCellEvaluation() {
    if (this.focusedId && this.isCell(this.focusedId)) {
      this.pushEvent("queue_cell_evaluation", {
        cell_id: this.focusedId,
      });
    }
  },

  queueFullCellsEvaluation(includeFocused) {
    const forcedCellIds =
      includeFocused && this.focusedId && this.isCell(this.focusedId)
        ? [this.focusedId]
        : [];

    this.pushEvent("queue_full_evaluation", {
      forced_cell_ids: forcedCellIds,
    });
  },

  queueFocusedSectionEvaluation() {
    if (this.focusedId) {
      const sectionId = this.getSectionIdByFocusableId(this.focusedId);

      if (sectionId) {
        this.pushEvent("queue_section_evaluation", {
          section_id: sectionId,
        });
      }
    }
  },

  cancelFocusedCellEvaluation() {
    if (this.focusedId && this.isCell(this.focusedId)) {
      this.pushEvent("cancel_cell_evaluation", {
        cell_id: this.focusedId,
      });
    }
  },

  reconnectRuntime() {
    this.pushEvent("reconnect_runtime", {});
  },

  showShortcuts() {
    const actionEl = this.el.querySelector(`[data-btn-show-shortcuts]`);
    actionEl && actionEl.click();
  },

  isInsertModeAvailable() {
    if (!this.focusedId) {
      return false;
    }

    const el = this.getFocusableEl(this.focusedId);

    return (
      !this.isCell(this.focusedId) ||
      !el.hasAttribute("data-js-insert-mode-disabled")
    );
  },

  enterInsertMode() {
    if (this.focusedId) {
      this.setInsertMode(true);
    }
  },

  escapeInsertMode() {
    this.setInsertMode(false);
  },

  moveFocus(offset) {
    const focusableId = this.nearbyFocusableId(this.focusedId, offset);
    this.setFocusedEl(focusableId);
  },

  moveFocusedCell(offset) {
    if (this.focusedId && this.isCell(this.focusedId)) {
      this.pushEvent("move_cell", { cell_id: this.focusedId, offset });
    }
  },

  insertCellBelowFocused(type) {
    if (this.focusedId) {
      this.insertCellBelowFocusableId(this.focusedId, type);
    } else {
      const focusableIds = this.getFocusableIds();
      if (focusableIds.length > 0) {
        this.insertCellBelowFocusableId(
          focusableIds[focusableIds.length - 1],
          type
        );
      }
    }
  },

  insertCellAboveFocused(type) {
    if (this.focusedId) {
      const prevFocusableId = this.nearbyFocusableId(this.focusedId, -1);
      this.insertCellBelowFocusableId(prevFocusableId, type);
    } else {
      const focusableIds = this.getFocusableIds();
      if (focusableIds.length > 0) {
        this.insertCellBelowFocusableId(focusableIds[0], type);
      }
    }
  },

  insertCellBelowFocusableId(focusableId, type) {
    if (this.isCell(focusableId)) {
      this.pushEvent("insert_cell_below", { type, cell_id: focusableId });
    } else if (this.isSection(focusableId)) {
      this.pushEvent("insert_cell_below", { type, section_id: focusableId });
    } else if (this.isNotebook(focusableId)) {
      const sectionIds = this.getSectionIds();
      if (sectionIds.length > 0) {
        this.pushEvent("insert_cell_below", {
          type,
          section_id: sectionIds[0],
        });
      }
    }
  },

  setFocusedEl(focusableId, { scroll = true, focusElement = true } = {}) {
    this.focusedId = focusableId;

    if (focusableId) {
      const el = this.getFocusableEl(focusableId);

      if (focusElement) {
        // Focus the primary content in the focusable element, this is important for screen readers
        const contentEl =
          el.querySelector(`[data-el-cell-body]`) ||
          el.querySelector(`[data-el-heading]`) ||
          el;
        contentEl.focus({ preventScroll: true });
      }
    }

    globalPubSub.broadcast("navigation", {
      type: "element_focused",
      focusableId: focusableId,
      scroll,
    });

    this.setInsertMode(false);
  },

  setInsertMode(insertModeEnabled) {
    this.insertMode = insertModeEnabled;

    if (insertModeEnabled) {
      this.el.setAttribute("data-js-insert-mode", "");
    } else {
      this.el.removeAttribute("data-js-insert-mode");

      this.sendLocationReport({
        focusableId: this.focusedId,
        selection: null,
      });
    }

    globalPubSub.broadcast("navigation", {
      type: "insert_mode_changed",
      enabled: insertModeEnabled,
    });
  },

  // Server event handlers

  handleCellInserted(cellId) {
    this.setFocusedEl(cellId);
    if (isDirectlyEditable(this.focusedCellType())) {
      this.setInsertMode(true);
    }
  },

  handleCellDeleted(cellId, siblingCellId) {
    if (this.focusedId === cellId) {
      this.setFocusedEl(siblingCellId);
    }
  },

  handleCellRestored(cellId) {
    this.setFocusedEl(cellId);
  },

  handleCellMoved(cellId) {
    if (this.focusedId === cellId) {
      globalPubSub.broadcast("cells", { type: "cell_moved", cellId });
    }
  },

  handleSectionInserted(sectionId) {
    const section = this.getSectionById(sectionId);
    const headlineEl = section.querySelector(`[data-el-section-headline]`);
    const { focusableId } = headlineEl.dataset;
    this.setFocusedEl(focusableId);
    this.setInsertMode(true);
    selectElementContent(document.activeElement);
  },

  handleSectionDeleted(sectionId) {
    // Clear focus if the element no longer exists
    if (this.focusedId && !this.getFocusableEl(this.focusedId)) {
      this.setFocusedEl(null);
    }
  },

  handleSectionMoved(sectionId) {
    const section = this.getSectionById(sectionId);
    smoothlyScrollToElement(section);
  },

  handleCellUpload(cellId, url) {
    if (this.focusedId !== cellId) {
      this.setFocusedEl(cellId);
    }

    if (!this.insertMode) {
      this.setInsertMode(true);
    }

    globalPubSub.broadcast("cells", { type: "cell_upload", cellId, url });
  },

  handleClientJoined(client) {
    this.clientsMap[client.pid] = client;
  },

  handleClientLeft(clientPid) {
    const client = this.clientsMap[clientPid];

    if (client) {
      delete this.clientsMap[clientPid];

      this.broadcastLocationReport(client, {
        focusableId: null,
        selection: null,
      });

      if (client.pid === this.followedClientPid) {
        this.followedClientPid = null;
      }
    }
  },

  handleClientsUpdated(updatedClients) {
    updatedClients.forEach((client) => {
      this.clientsMap[client.pid] = client;
    });
  },

  handleLocationReport(clientPid, report) {
    const client = this.clientsMap[clientPid];

    this.lastLocationReportByClientPid[clientPid] = report;

    if (client) {
      this.broadcastLocationReport(client, report);

      if (
        client.pid === this.followedClientPid &&
        report.focusableId !== this.focusedId
      ) {
        this.setFocusedEl(report.focusableId);
      }
    }
  },

  // Session event handlers

  handleSessionEvent(event) {
    if (event.type === "cursor_selection_changed") {
      this.sendLocationReport({
        focusableId: event.focusableId,
        selection: event.selection,
      });
    }
  },

  /**
   * Broadcast new location report coming from the server to all the cells.
   */
  broadcastLocationReport(client, report) {
    globalPubSub.broadcast("navigation", {
      type: "location_report",
      client,
      report,
    });
  },

  /**
   * Sends local location report to the server.
   */
  sendLocationReport(report) {
    const numberOfClients = Object.keys(this.clientsMap).length;

    // Only send reports if there are other people to send to
    if (numberOfClients > 1) {
      this.pushEvent("location_report", {
        focusable_id: report.focusableId,
        selection: this.encodeSelection(report.selection),
      });
    }
  },

  encodeSelection(selection) {
    if (selection === null) return null;

    const { tag, editorSelection } = selection;

    return [
      tag,
      editorSelection.selectionStartLineNumber,
      editorSelection.selectionStartColumn,
      editorSelection.positionLineNumber,
      editorSelection.positionColumn,
    ];
  },

  decodeSelection(encoded) {
    if (encoded === null) return null;

    const [
      tag,
      selectionStartLineNumber,
      selectionStartColumn,
      positionLineNumber,
      positionColumn,
    ] = encoded;

    const editorSelection = new monaco.Selection(
      selectionStartLineNumber,
      selectionStartColumn,
      positionLineNumber,
      positionColumn
    );

    return { tag, editorSelection };
  },

  // Helpers

  focusedCellType() {
    if (this.focusedId && this.isCell(this.focusedId)) {
      const el = this.getFocusableEl(this.focusedId);
      return el.getAttribute("data-type");
    } else {
      return null;
    }
  },

  nearbyFocusableId(focusableId, offset) {
    const focusableIds = this.getFocusableIds();

    if (focusableIds.length === 0) {
      return null;
    }

    const idx = focusableIds.indexOf(focusableId);

    if (idx === -1) {
      const focusableElInViewport =
        this.getFocusableEls().find(isElementInViewport);

      if (focusableElInViewport) {
        return focusableElInViewport.getAttribute("data-focusable-id");
      }

      return focusableIds[0];
    } else {
      const siblingIdx = clamp(idx + offset, 0, focusableIds.length - 1);
      return focusableIds[siblingIdx];
    }
  },

  isCell(focusableId) {
    const el = this.getFocusableEl(focusableId);
    return el.dataset.element === "cell";
  },

  isSection(focusableId) {
    const el = this.getFocusableEl(focusableId);
    return el.dataset.element === "section-headline";
  },

  isNotebook(focusableId) {
    const el = this.getFocusableEl(focusableId);
    return el.dataset.element === "notebook-headline";
  },

  getFocusableEl(focusableId) {
    return this.el.querySelector(`[data-focusable-id="${focusableId}"]`);
  },

  getFocusableEls() {
    return Array.from(this.el.querySelectorAll(`[data-focusable-id]`));
  },

  getFocusableIds() {
    return this.getFocusableEls().map((el) =>
      el.getAttribute("data-focusable-id")
    );
  },

  getSectionIdByFocusableId(focusableId) {
    const el = this.getFocusableEl(focusableId);
    const section = el.closest(`[data-el-section]`);
    return section && section.getAttribute("data-section-id");
  },

  getSectionIds() {
    const sections = this.getSections();
    return sections.map((section) => section.getAttribute("data-section-id"));
  },

  getSections() {
    return Array.from(this.el.querySelectorAll(`[data-el-section]`));
  },

  getSectionById(sectionId) {
    return this.el.querySelector(
      `[data-el-section][data-section-id="${sectionId}"]`
    );
  },

  getElement(name) {
    return this.el.querySelector(`[data-el-${name}]`);
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
 * @property {String|null} focusableId
 * @property {monaco.Selection|null} selection
 */

export default Session;
