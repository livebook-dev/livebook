import {
  isMacOS,
  isEditableElement,
  clamp,
  selectElementContent,
  smoothlyScrollToElement,
  setFavicon,
  cancelEvent,
  isElementInViewport,
  isElementHidden,
} from "../lib/utils";
import { getAttributeOrDefault } from "../lib/attribute";
import KeyBuffer from "../lib/key_buffer";
import { globalPubSub } from "../lib/pub_sub";
import monaco from "./cell_editor/live_editor/monaco";
import { leaveChannel } from "./js_view/channel";
import { isDirectlyEditable, isEvaluable } from "../lib/notebook";
import { settingsStore } from "../lib/settings";

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
 * client id, as we already have the necessary hex_color/name locally.
 */
const Session = {
  mounted() {
    this.props = this.getProps();

    this.focusedId = null;
    this.insertMode = false;
    this.view = null;
    this.viewOptions = null;
    this.keyBuffer = new KeyBuffer();
    this.clientsMap = {};
    this.lastLocationReportByClientId = {};
    this.followedClientId = null;

    setFavicon(this.faviconForEvaluationStatus(this.props.globalStatus));

    this.updateSectionListHighlight();

    // DOM events

    this._handleDocumentKeyDown = this.handleDocumentKeyDown.bind(this);
    this._handleDocumentMouseDown = this.handleDocumentMouseDown.bind(this);
    this._handleDocumentFocus = this.handleDocumentFocus.bind(this);
    this._handleDocumentClick = this.handleDocumentClick.bind(this);

    document.addEventListener("keydown", this._handleDocumentKeyDown, true);
    document.addEventListener("mousedown", this._handleDocumentMouseDown);
    // Note: the focus event doesn't bubble, so we register for the capture phase
    document.addEventListener("focus", this._handleDocumentFocus, true);
    document.addEventListener("click", this._handleDocumentClick);

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

    this.getElement("secrets-list-toggle").addEventListener("click", (event) =>
      this.toggleSecretsList()
    );

    this.getElement("runtime-info-toggle").addEventListener("click", (event) =>
      this.toggleRuntimeInfo()
    );

    this.getElement("app-info-toggle").addEventListener("click", (event) =>
      this.toggleAppInfo()
    );

    this.getElement("files-list-toggle").addEventListener("click", (event) =>
      this.toggleFilesList()
    );

    this.getElement("notebook").addEventListener("scroll", (event) =>
      this.updateSectionListHighlight()
    );

    this.getElement("notebook-indicators").addEventListener("click", (event) =>
      this.handleCellIndicatorsClick(event)
    );

    this.getElement("views").addEventListener("click", (event) => {
      this.handleViewsClick(event);
    });

    this.getElement("section-toggle-collapse-all-button").addEventListener(
      "click",
      (event) => this.toggleCollapseAllSections()
    );

    this.initializeDragAndDrop();

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
        this.clientsMap[client.id] = client;
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

    this.handleEvent("client_left", ({ client_id }) => {
      this.handleClientLeft(client_id);
    });

    this.handleEvent("clients_updated", ({ clients }) => {
      this.handleClientsUpdated(clients);
    });

    this.handleEvent(
      "secret_selected",
      ({ select_secret_ref, secret_name }) => {
        this.handleSecretSelected(select_secret_ref, secret_name);
      }
    );

    this.handleEvent(
      "location_report",
      ({ client_id, focusable_id, selection }) => {
        const report = {
          focusableId: focusable_id,
          selection: this.decodeSelection(selection),
        };

        this.handleLocationReport(client_id, report);
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

    // If we reconnect, a new hook is mounted and it becomes responsible
    // for leaving the channel when destroyed
    this.keepChannel = true;
  },

  destroyed() {
    this.unsubscribeFromSessionEvents();

    document.removeEventListener("keydown", this._handleDocumentKeyDown, true);
    document.removeEventListener("mousedown", this._handleDocumentMouseDown);
    document.removeEventListener("focus", this._handleDocumentFocus, true);
    document.removeEventListener("click", this._handleDocumentClick);

    setFavicon("favicon");

    if (!this.keepChannel) {
      leaveChannel();
    }
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
    if (evaluationStatus === "errored") return "favicon-errored";
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

    // Universal shortcuts (ignore editable elements in cell output)
    if (
      !(
        isEditableElement(event.target) &&
        event.target.closest(`[data-el-outputs-container]`)
      )
    ) {
      if (cmd && shift && !alt && key === "Enter") {
        cancelEvent(event);
        this.queueFullCellsEvaluation(true);
        return;
      } else if (!cmd && shift && !alt && key === "Enter") {
        cancelEvent(event);
        if (isEvaluable(this.focusedCellType())) {
          this.queueFocusedCellEvaluation();
        }
        this.moveFocus(1);
        return;
      } else if (cmd && !alt && key === "Enter") {
        cancelEvent(event);
        if (isEvaluable(this.focusedCellType())) {
          this.queueFocusedCellEvaluation();
        }
        return;
      } else if (cmd && key === "s") {
        cancelEvent(event);
        this.saveNotebook();
        return;
      } else if (cmd || alt) {
        return;
      }
    }

    if (this.insertMode) {
      keyBuffer.reset();

      if (key === "Escape") {
        // Ignore Escape if it's supposed to close an editor widget
        if (!this.escapesMonacoWidget(event)) {
          this.escapeInsertMode();
        }
      }
      // Ignore keystrokes on input fields
    } else if (isEditableElement(event.target)) {
      keyBuffer.reset();

      // Use Escape for universal blur
      if (key === "Escape") {
        event.target.blur();
      }
    } else {
      keyBuffer.push(event.key);

      if (keyBuffer.tryMatch(["d", "d"])) {
        this.deleteFocusedCell();
      } else if (keyBuffer.tryMatch(["e", "a"])) {
        this.queueFullCellsEvaluation(false);
      } else if (keyBuffer.tryMatch(["e", "e"])) {
        if (isEvaluable(this.focusedCellType())) {
          this.queueFocusedCellEvaluation();
        }
      } else if (keyBuffer.tryMatch(["e", "s"])) {
        this.queueFocusedSectionEvaluation();
      } else if (keyBuffer.tryMatch(["s", "s"])) {
        this.toggleSectionsList();
      } else if (keyBuffer.tryMatch(["s", "e"])) {
        this.toggleSecretsList();
      } else if (keyBuffer.tryMatch(["s", "a"])) {
        this.toggleAppInfo();
      } else if (keyBuffer.tryMatch(["s", "u"])) {
        this.toggleClientsList();
      } else if (keyBuffer.tryMatch(["s", "f"])) {
        this.toggleFilesList();
      } else if (keyBuffer.tryMatch(["s", "r"])) {
        this.toggleRuntimeInfo();
      } else if (keyBuffer.tryMatch(["s", "b"])) {
        this.showBin();
      } else if (keyBuffer.tryMatch(["s", "p"])) {
        this.showPackageSearch();
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
        (event.target.matches(
          `body, [data-el-cell-body], [data-el-heading], [data-focusable-id]`
        ) &&
          this.focusedId &&
          key === "Enter")
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
        if (!this.view || this.viewOptions.showMarkdown) {
          this.insertCellBelowFocused("markdown");
        }
      } else if (keyBuffer.tryMatch(["M"])) {
        if (!this.view || this.viewOptions.showMarkdown) {
          this.insertCellAboveFocused("markdown");
        }
      } else if (keyBuffer.tryMatch(["v", "z"])) {
        this.toggleView("code-zen");
      } else if (keyBuffer.tryMatch(["v", "p"])) {
        this.toggleView("presentation");
      } else if (keyBuffer.tryMatch(["v", "c"])) {
        this.toggleView("custom");
      } else if (keyBuffer.tryMatch(["c"])) {
        if (!this.view || this.viewOptions.showSection) {
          this.toggleCollapseSection();
        }
      } else if (keyBuffer.tryMatch(["C"])) {
        if (!this.view || this.viewOptions.showSection) {
          this.toggleCollapseAllSections();
        }
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
    if (
      // If the click is outside the notebook element, keep the focus as is
      !event.target.closest(`[data-el-notebook]`) ||
      // If the click is inside the custom doctest editor widget, keep the focus as is
      event.target.closest(`.doctest-details-widget`)
    ) {
      if (this.insertMode) {
        this.setInsertMode(false);
      }
      return;
    }

    // When clicking an insert button, keep focus and insert mode as is.
    // This is relevant for markdown cells, since we show the markdown
    // preview in insert mode and exiting insert mode on mousedown would
    // result in layout shift, so mouseup would happen outside the target
    // button and the click would be ignored
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

    if (event.target.closest(`[data-btn-package-search]`) && this.insertMode) {
      this.setInsertMode(false);
    }

    const evalButton = event.target.closest(
      `[data-el-queue-cell-evaluation-button]`
    );
    if (evalButton) {
      const cellId = evalButton.getAttribute("data-cell-id");
      const disableDependenciesCache = evalButton.hasAttribute(
        "data-disable-dependencies-cache"
      );
      this.queueCellEvaluation(cellId, disableDependenciesCache);
    }

    const hash = window.location.hash;

    if (hash) {
      const htmlId = hash.replace(/^#/, "");
      const hashEl = document.getElementById(htmlId);

      // Remove hash from the URL when the user clicks somewhere else on the page
      if (!hashEl.contains(event.target) && !event.target.closest(`a`)) {
        history.pushState(
          null,
          document.title,
          window.location.pathname + window.location.search
        );
      }
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
      const clientId = clientListItem.getAttribute("data-client-id");

      const clientLink = event.target.closest(`[data-el-client-link]`);
      if (clientLink) {
        this.handleClientLinkClick(clientId);
      }

      const clientFollowToggle = event.target.closest(
        `[data-el-client-follow-toggle]`
      );
      if (clientFollowToggle) {
        this.handleClientFollowToggleClick(clientId, clientListItem);
      }
    }
  },

  handleClientLinkClick(clientId) {
    this.mirrorClientFocus(clientId);
  },

  handleClientFollowToggleClick(clientId, clientListItem) {
    const followedClientListItem = this.el.querySelector(
      `[data-el-clients-list-item][data-js-followed]`
    );

    if (followedClientListItem) {
      followedClientListItem.removeAttribute("data-js-followed");
    }

    if (clientId === this.followedClientId) {
      this.followedClientId = null;
    } else {
      clientListItem.setAttribute("data-js-followed", "");
      this.followedClientId = clientId;
      this.mirrorClientFocus(clientId);
    }
  },

  mirrorClientFocus(clientId) {
    const locationReport = this.lastLocationReportByClientId[clientId];

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
      const hashEl = document.getElementById(htmlId);

      if (hashEl) {
        const focusableEl = hashEl.closest("[data-focusable-id]");

        if (focusableEl) {
          this.setFocusedEl(focusableEl.dataset.focusableId);
        } else {
          // Explicitly scroll to the target element
          // after the loading finishes
          hashEl.scrollIntoView();
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

  /**
   * Initializes drag and drop event handlers.
   */
  initializeDragAndDrop() {
    let isDragging = false;
    let draggedEl = null;
    let files = null;

    const startDragging = (element = null) => {
      if (!isDragging) {
        isDragging = true;
        draggedEl = element;

        const type = element ? "internal" : "external";
        this.el.setAttribute("data-js-dragging", type);

        if (type === "external") {
          this.toggleFilesList(true);
        }
      }
    };

    const stopDragging = () => {
      if (isDragging) {
        isDragging = false;
        this.el.removeAttribute("data-js-dragging");
      }
    };

    this.el.addEventListener("dragstart", (event) => {
      startDragging(event.target);
    });

    this.el.addEventListener("dragenter", (event) => {
      startDragging();
    });

    this.el.addEventListener("dragleave", (event) => {
      if (!this.el.contains(event.relatedTarget)) {
        stopDragging();
      }
    });

    this.el.addEventListener("dragover", (event) => {
      event.stopPropagation();
      event.preventDefault();
    });

    this.el.addEventListener("drop", (event) => {
      event.stopPropagation();
      event.preventDefault();

      const insertDropEl = event.target.closest(`[data-el-insert-drop-area]`);
      const filesDropEl = event.target.closest(`[data-el-files-drop-area]`);

      if (insertDropEl) {
        const sectionId = insertDropEl.getAttribute("data-section-id") || null;
        const cellId = insertDropEl.getAttribute("data-cell-id") || null;

        if (event.dataTransfer.files.length > 0) {
          files = event.dataTransfer.files;

          this.pushEvent("handle_file_drop", {
            section_id: sectionId,
            cell_id: cellId,
          });
        } else if (draggedEl && draggedEl.matches("[data-el-file-entry]")) {
          const fileEntryName = draggedEl.getAttribute("data-name");

          this.pushEvent("insert_file", {
            file_entry_name: fileEntryName,
            section_id: sectionId,
            cell_id: cellId,
          });
        }
      } else if (filesDropEl) {
        if (event.dataTransfer.files.length > 0) {
          files = event.dataTransfer.files;
          this.pushEvent("handle_file_drop", {});
        }
      }

      stopDragging();
    });

    this.handleEvent("finish_file_drop", (event) => {
      const inputEl = document.querySelector(
        `#add-file-entry-modal input[type="file"]`
      );

      if (inputEl) {
        inputEl.files = files;
        inputEl.dispatchEvent(new Event("change", { bubbles: true }));
      }
    });
  },

  // User action handlers (mostly keybindings)

  toggleSectionsList(force = null) {
    this.toggleSidePanelContent("sections-list", force);
  },

  toggleClientsList(force = null) {
    this.toggleSidePanelContent("clients-list", force);
  },

  toggleSecretsList(force = null) {
    this.toggleSidePanelContent("secrets-list", force);
  },

  toggleAppInfo(force = null) {
    this.toggleSidePanelContent("app-info", force);
  },

  toggleFilesList(force = null) {
    this.toggleSidePanelContent("files-list", force);
  },

  toggleRuntimeInfo(force = null) {
    this.toggleSidePanelContent("runtime-info", force);
  },

  toggleSidePanelContent(name, force = null) {
    const shouldOpen =
      force === null
        ? this.el.getAttribute("data-js-side-panel-content") !== name
        : force;

    if (shouldOpen) {
      this.el.setAttribute("data-js-side-panel-content", name);
    } else {
      this.el.removeAttribute("data-js-side-panel-content");
    }
  },

  showBin() {
    const actionEl = this.el.querySelector(`[data-btn-show-bin]`);
    actionEl && actionEl.click();
  },

  showPackageSearch() {
    this.setFocusedEl("setup");

    const actionEl = this.el.querySelector(`[data-btn-package-search]`);
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

  queueCellEvaluation(cellId, disableDependenciesCache) {
    this.dispatchQueueEvaluation(() => {
      this.pushEvent("queue_cell_evaluation", {
        cell_id: cellId,
        disable_dependencies_cache: disableDependenciesCache,
      });
    });
  },

  queueFocusedCellEvaluation() {
    if (this.focusedId && this.isCell(this.focusedId)) {
      this.dispatchQueueEvaluation(() => {
        this.pushEvent("queue_cell_evaluation", { cell_id: this.focusedId });
      });
    }
  },

  queueFullCellsEvaluation(includeFocused) {
    const forcedCellIds =
      includeFocused && this.focusedId && this.isCell(this.focusedId)
        ? [this.focusedId]
        : [];

    this.dispatchQueueEvaluation(() => {
      this.pushEvent("queue_full_evaluation", {
        forced_cell_ids: forcedCellIds,
      });
    });
  },

  queueFocusedSectionEvaluation() {
    if (this.focusedId) {
      const sectionId = this.getSectionIdByFocusableId(this.focusedId);

      if (sectionId) {
        this.dispatchQueueEvaluation(() => {
          this.pushEvent("queue_section_evaluation", {
            section_id: sectionId,
          });
        });
      }
    }
  },

  dispatchQueueEvaluation(dispatch) {
    if (isEvaluable(this.focusedCellType())) {
      // If an evaluable cell is focused, we forward the evaluation
      // request to that cell, so it can synchronize itself before
      // sending the request to the server
      globalPubSub.broadcast(`cells:${this.focusedId}`, {
        type: "dispatch_queue_evaluation",
        dispatch,
      });
    } else {
      dispatch();
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
      // If the element is inside collapsed section, expand that section
      if (!this.isSection(focusableId)) {
        const sectionId = this.getSectionIdByFocusableId(focusableId);

        if (sectionId) {
          const section = this.getSectionById(sectionId);
          section.removeAttribute("data-js-collapsed");
        }
      }

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

  handleViewsClick(event) {
    const button = event.target.closest(`[data-el-view-toggle]`);

    if (button) {
      const view = button.getAttribute("data-el-view-toggle");
      this.toggleView(view);
    }
  },

  toggleView(view) {
    if (view === this.view) {
      this.unsetView();

      if (view === "custom") {
        this.unsubscribeCustomViewFromSettings();
      }
    } else if (view === "code-zen") {
      this.setView(view, {
        showSection: false,
        showMarkdown: false,
        showOutput: true,
        spotlight: false,
      });
    } else if (view === "presentation") {
      this.setView(view, {
        showSection: true,
        showMarkdown: true,
        showOutput: true,
        spotlight: true,
      });
    } else if (view === "custom") {
      this.unsubscribeCustomViewFromSettings = settingsStore.getAndSubscribe(
        (settings) => {
          this.setView(view, {
            showSection: settings.custom_view_show_section,
            showMarkdown: settings.custom_view_show_markdown,
            showOutput: settings.custom_view_show_output,
            spotlight: settings.custom_view_spotlight,
          });
        }
      );

      this.pushEvent("open_custom_view_settings");
    }

    // If nothing is focused, use the first cell in the viewport
    const focusedId = this.focusedId || this.nearbyFocusableId(null, 0);

    if (focusedId) {
      const visibleId = this.ensureVisibleFocusableEl(focusedId);

      if (visibleId !== this.focused) {
        this.setFocusedEl(visibleId, { scroll: false });
      }

      if (visibleId) {
        this.getFocusableEl(visibleId).scrollIntoView({ block: "center" });
      }
    }
  },

  setView(view, options) {
    this.view = view;
    this.viewOptions = options;

    this.el.setAttribute("data-js-view", view);

    this.el.toggleAttribute("data-js-hide-section", !options.showSection);
    this.el.toggleAttribute("data-js-hide-markdown", !options.showMarkdown);
    this.el.toggleAttribute("data-js-hide-output", !options.showOutput);
    this.el.toggleAttribute("data-js-spotlight", options.spotlight);
  },

  unsetView() {
    this.view = null;
    this.viewOptions = null;

    this.el.removeAttribute("data-js-view");

    this.el.removeAttribute("data-js-hide-section");
    this.el.removeAttribute("data-js-hide-markdown");
    this.el.removeAttribute("data-js-hide-output");
    this.el.removeAttribute("data-js-spotlight");
  },

  toggleCollapseSection() {
    if (this.focusedId) {
      const sectionId = this.getSectionIdByFocusableId(this.focusedId);

      if (sectionId) {
        const section = this.getSectionById(sectionId);

        if (section.hasAttribute("data-js-collapsed")) {
          section.removeAttribute("data-js-collapsed");
        } else {
          section.setAttribute("data-js-collapsed", "");
          this.setFocusedEl(sectionId, { scroll: true });
        }
      }
    }
  },

  toggleCollapseAllSections() {
    const allCollapsed = this.getSections().every((section) =>
      section.hasAttribute("data-js-collapsed")
    );

    this.getSections().forEach((section) => {
      section.toggleAttribute("data-js-collapsed", !allCollapsed);
    });

    if (this.focusedId) {
      const focusedSectionId = this.getSectionIdByFocusableId(this.focusedId);

      if (focusedSectionId) {
        this.setFocusedEl(focusedSectionId, { scroll: true });
      }
    }
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
      if (this.view) {
        const visibleSiblingId = this.ensureVisibleFocusableEl(siblingCellId);
        this.setFocusedEl(visibleSiblingId);
      } else {
        this.setFocusedEl(siblingCellId);
      }
    }
  },

  handleCellRestored(cellId) {
    this.setFocusedEl(cellId);
  },

  handleCellMoved(cellId) {
    this.repositionJSViews();

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
    this.repositionJSViews();

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
    this.clientsMap[client.id] = client;
  },

  handleClientLeft(clientId) {
    const client = this.clientsMap[clientId];

    if (client) {
      delete this.clientsMap[clientId];

      this.broadcastLocationReport(client, {
        focusableId: null,
        selection: null,
      });

      if (client.id === this.followedClientId) {
        this.followedClientId = null;
      }
    }
  },

  handleClientsUpdated(updatedClients) {
    updatedClients.forEach((client) => {
      this.clientsMap[client.id] = client;
    });
  },

  handleSecretSelected(select_secret_ref, secretName) {
    globalPubSub.broadcast(`js_views:${select_secret_ref}`, {
      type: "secretSelected",
      secretName,
    });
  },

  handleLocationReport(clientId, report) {
    const client = this.clientsMap[clientId];

    this.lastLocationReportByClientId[clientId] = report;

    if (client) {
      this.broadcastLocationReport(client, report);

      if (
        client.id === this.followedClientId &&
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

  repositionJSViews() {
    globalPubSub.broadcast("js_views", { type: "reposition" });
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

  ensureVisibleFocusableEl(cellId) {
    const focusableEl = this.getFocusableEl(cellId);
    const allFocusableEls = Array.from(
      this.el.querySelectorAll(`[data-focusable-id]`)
    );
    const idx = allFocusableEls.indexOf(focusableEl);
    const visibleSibling = [
      ...allFocusableEls.slice(idx, -1),
      ...allFocusableEls.slice(0, idx).reverse(),
    ].find((el) => !isElementHidden(el));

    return visibleSibling && visibleSibling.getAttribute("data-focusable-id");
  },

  isCell(focusableId) {
    const el = this.getFocusableEl(focusableId);
    return el.hasAttribute("data-el-cell");
  },

  isSection(focusableId) {
    const el = this.getFocusableEl(focusableId);
    return el.hasAttribute("data-el-section-headline");
  },

  isNotebook(focusableId) {
    const el = this.getFocusableEl(focusableId);
    return el.hasAttribute("data-el-notebook-headline");
  },

  getFocusableEl(focusableId) {
    return this.el.querySelector(`[data-focusable-id="${focusableId}"]`);
  },

  getFocusableEls() {
    return Array.from(this.el.querySelectorAll(`[data-focusable-id]`)).filter(
      (el) => !isElementHidden(el)
    );
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
 * @property {String} id
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
