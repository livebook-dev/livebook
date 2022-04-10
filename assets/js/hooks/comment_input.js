/**
 * A hook for the comment input.
 *
 * On submit this hook clears the text input, which is not done
 * automatically when using keyboard shortcuts ('Enter' key).
 */
const CommentInput = {
  mounted() {
    this.el.addEventListener("keydown", (event) => {
      if ("Enter" === event.key && !event.shiftKey) {
        const value = this.el.value;

        // send server event to add a cell comment
        value &&
          this.pushEvent("add_cell_comment", {
            value,
            ctrl_key: event.ctrlKey,
            cell_view_id: this.cellViewId(),
          });

        // clear input
        this.el.value = "";
      }
    });
  },

  cellViewId() {
    return this.el.getAttribute("cell_view_id");
  },
};

export default CommentInput;
