const Session = {
  mounted() {
    this.previousFocusedCellId = this.__focusedCellId();

    document.addEventListener("keydown", (event) => {
      if (event.shiftKey && event.key === 'Enter' && !event.repeat) {
        if (this.__focusedCellId() !== null) {
          // If the editor is focused we don't want it to receive \n input
          event.preventDefault();
          this.pushEvent('toggle_cell_expanded', {});
        }
      }

      if (event.altKey && event.key === "j") {
        event.preventDefault();
        this.pushEvent('move_cell_focus', { offset: 1 });
      }

      if (event.altKey && event.key === "k") {
        event.preventDefault();
        this.pushEvent('move_cell_focus', { offset: -1 });
      }
    });

    document.addEventListener("click", event => {
      // TODO: any cleaner way? (even editor has this kind of data attr, so that's far from perfect!)
      const cell = event.target.closest("[data-cell-id]");
      const cellId = cell ? cell.dataset.cellId : null;
      if (cellId !== this.__focusedCellId()) {
        this.pushEvent('focus_cell', { cell_id: cellId });
      }
    });
  },

  updated() {
    // TODO: also focus elixir editor on this

    const focusedCellId = this.__focusedCellId();

    if (focusedCellId && focusedCellId !== history.previousFocusedCellId) {
      const cell = this.el.querySelector(`#cell-${focusedCellId}`);
      cell.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }

    this.previousFocusedCellId = focusedCellId;
  },

  __focusedCellId() {
    return this.el.dataset.focusedCellId || null;
  }
};

export default Session;
