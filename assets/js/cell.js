const Cell = {
  mounted() {
    this.cellId = this.el.dataset.cellId;
    this.type = this.el.dataset.type;

    document.addEventListener("keydown", (event) => {
      if (this.type === 'markdown' && this.__isFocused()) {
        if (event.shiftKey && event.key === 'Enter' && !event.repeat) {
          // If the editor is focused we don't want it to receive \n input
          event.preventDefault();
          if (this.__isExpanded()) {
            this.pushEvent('unexpand_cell', { cell_id: this.cellId });
          } else {
            this.pushEvent('expand_cell', { cell_id: this.cellId });
          }
        }
      }
    });

    this.el.addEventListener("click", (event) => {
      if (!this.__isFocused()) {
        this.pushEvent('focus_cell', { cell_id: this.cellId });
      }
    });
  },

  __isFocused() {
    return this.el.dataset.focused === "true";
  },

  __isExpanded() {
    return this.el.dataset.expanded === "true";
  }
};

export default Cell;
