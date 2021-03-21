import {
  isMacOS,
  isEditableElement,
  clamp,
  selectElementContent,
  smoothlyScrollToElement,
} from "../lib/utils";
import KeyBuffer from "./key_buffer";
import { globalPubSub } from "../lib/pub_sub";

/**
 * A hook managing the whole session.
 *
 * Handles keybindings, focus changes and insert mode changes.
 */
const Session = {
  mounted() {
    this.state = {
      focusedCellId: null,
      focusedSectionId: null,
      focusedCellType: null,
      insertMode: false,
      keyBuffer: new KeyBuffer(),
    };

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

    getSectionList().addEventListener("click", (event) => {
      handleSectionListClick(this, event);
    });

    updateSectionListHighlight();

    getNotebook().addEventListener("scroll", (event) => {
      updateSectionListHighlight();
    });

    document
      .querySelector(`[data-element="sections-panel-toggle"]`)
      .addEventListener("click", (event) => {
        this.el.toggleAttribute("data-js-sections-panel-expanded");
      });

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

    this.handleEvent("cell_moved", ({ cell_id: cellId }) => {
      handleCellMoved(this, cellId);
    });

    this.handleEvent("section_inserted", ({ section_id: sectionId }) => {
      handleSectionInserted(this, sectionId);
    });

    this.handleEvent("section_deleted", ({ section_id: sectionId }) => {
      handleSectionDeleted(this, sectionId);
    });
  },

  destroyed() {
    document.removeEventListener("keydown", this.handleDocumentKeyDown);
    document.removeEventListener("mousedown", this.handleDocumentMouseDown);
    document.removeEventListener("dblclick", this.handleDocumentDoubleClick);
  },
};

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
  const key = event.key;
  const keyBuffer = hook.state.keyBuffer;

  if (hook.state.insertMode) {
    keyBuffer.reset();

    if (key === "Escape") {
      // Ignore Escape if it's supposed to close some Monaco input (like the find/replace box)
      if (!event.target.closest(".monaco-inputbox")) {
        escapeInsertMode(hook);
      }
    } else if (cmd && key === "Enter") {
      cancelEvent(event);
      if (hook.state.focusedCellType === "elixir") {
        queueFocusedCellEvaluation(hook);
      }
    }
  } else {
    // Ignore inputs and notebook/section title fields
    if (isEditableElement(event.target)) {
      keyBuffer.reset();
      return;
    }

    keyBuffer.push(event.key);

    if (keyBuffer.tryMatch(["d", "d"])) {
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
    } else if (keyBuffer.tryMatch(["e", "x"])) {
      cancelFocusedCellEvaluation(hook);
    } else if (keyBuffer.tryMatch(["?"])) {
      showShortcuts(hook);
    } else if (keyBuffer.tryMatch(["i"])) {
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
  // If click targets cell actions, keep the focus as is
  if (event.target.closest(`[data-element="actions"]`)) {
    // If the pencil icon is clicked, enter insert mode
    if (event.target.closest(`[data-element="enable-insert-mode-button"]`)) {
      setInsertMode(hook, true);
    }

    return;
  }

  // Find the cell element, if one was clicked
  const cell = event.target.closest(`[data-element="cell"]`);
  const cellId = cell ? cell.dataset.cellId : null;
  if (cellId !== hook.state.focusedCellId) {
    setFocusedCell(hook, cellId);
  }

  // Depending on whether the click targets editor disable/enable insert mode
  if (cell) {
    const editorContainer = cell.querySelector(
      `[data-element="editor-container"]`
    );
    const editorClicked = editorContainer.contains(event.target);
    const insertMode = editorClicked;
    if (hook.state.insertMode !== insertMode) {
      setInsertMode(hook, insertMode);
    }
  }
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
function handleSectionListClick(hook, event) {
  const sectionButton = event.target.closest(
    `[data-element="section-list-item"]`
  );
  if (sectionButton) {
    const sectionId = sectionButton.getAttribute("data-section-id");
    const section = getSectionById(sectionId);
    section.scrollIntoView({ behavior: "smooth", block: "start" });
  }
}

/**
 * Handles the main notebook area being scrolled.
 */
function updateSectionListHighlight() {
  const currentListItem = document.querySelector(
    `[data-element="section-list-item"][data-js-is-viewed]`
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
      `[data-element="section-list-item"][data-section-id="${sectionId}"]`
    );
    listItem.setAttribute("data-js-is-viewed", "true");
  }
}

// User action handlers (mostly keybindings)

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

function setFocusedCell(hook, cellId) {
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

  globalPubSub.broadcast("session", { type: "cell_focused", cellId });

  setInsertMode(hook, false);
}

function setInsertMode(hook, insertModeEnabled) {
  hook.state.insertMode = insertModeEnabled;

  if (insertModeEnabled) {
    hook.el.setAttribute("data-js-insert-mode", "true");
  } else {
    hook.el.removeAttribute("data-js-insert-mode");
  }

  globalPubSub.broadcast("session", {
    type: "insert_mode_changed",
    enabled: insertModeEnabled,
  });
}

// Server event handlers

function handleCellInserted(hook, cellId) {
  setFocusedCell(hook, cellId);
  setInsertMode(hook, true);
}

function handleCellDeleted(hook, cellId, siblingCellId) {
  if (hook.state.focusedCellId === cellId) {
    setFocusedCell(hook, siblingCellId);
  }
}

function handleCellMoved(hook, cellId) {
  if (hook.state.focusedCellId === cellId) {
    globalPubSub.broadcast("session", { type: "cell_moved", cellId });

    // The cell may have moved to another section, so update this information.
    hook.state.focusedSectionId = getSectionIdByCellId(
      hook.state.focusedCellId
    );
  }
}

function handleSectionInserted(hook, sectionId) {
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

function getSectionList() {
  return document.querySelector(`[data-element="section-list"]`);
}

function getNotebook() {
  return document.querySelector(`[data-element="notebook"]`);
}

function cancelEvent(event) {
  // Cancel any default browser behavior.
  event.preventDefault();
  // Stop event propagation (e.g. so it doesn't reach the editor).
  event.stopPropagation();
}

export default Session;
