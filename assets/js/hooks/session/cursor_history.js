/**
 * Allows for recording a sequence of focused cells with the focused line
 * and navigate inside this stack.
 */
export default class CursorHistory {
  /** @private */
  entries = [];

  /** @private */
  index = -1;

  /**
   * Adds a new cell to the stack.
   *
   * If the stack length is greater than the stack limit,
   * it will remove the oldest entries.
   */
  push(cellId, line, offset) {
    const entry = { cellId, line, offset };

    if (this.isSameCell(cellId)) {
      this.entries[this.index] = entry;
    } else {
      if (this.entries[this.index + 1] !== undefined) {
        this.entries = this.entries.slice(0, this.index + 1);
      }

      this.entries.push(entry);
      this.index++;
    }

    if (this.entries.length > 20) {
      this.entries.shift();
      this.index--;
    }
  }

  /**
   * Removes all matching cells with given id from the stack.
   */
  removeAllFromCell(cellId) {
    // We need to make sure the last entry from history
    // doesn't belong to the given cell id that we need
    // to remove from the entries list.
    let cellIdCount = 0;

    for (let i = 0; i <= this.index; i++) {
      const entry = this.get(i);
      if (entry.cellId === cellId) cellIdCount++;
    }

    this.entries = this.entries.filter((entry) => entry.cellId !== cellId);
    this.index = this.index - cellIdCount;
    if (this.index === -1 && this.entries.length > 0) this.index = 0;
  }

  /**
   * Checks if the current stack is available to navigate back.
   */
  canGoBack() {
    return this.canGetFromHistory(-1);
  }

  /**
   * Navigates back in the current stack.
   *
   * If the navigation succeeds, it will return the entry from current index.
   * Otherwise, returns null;
   */
  goBack() {
    return this.getFromHistory(-1);
  }

  /**
   * Checks if the current stack is available to navigate forward.
   */
  canGoForward() {
    return this.canGetFromHistory(1);
  }

  /**
   * Navigates forward in the current stack.
   *
   * If the navigation succeeds, it will return the entry from current index.
   * Otherwise, returns null;
   */
  goForward() {
    return this.getFromHistory(1);
  }

  /**
   * Gets the entry in the current stack.
   *
   * If the stack have at least one entry, it will return the entry from current index.
   * Otherwise, returns null;
   */
  getCurrent() {
    return this.get(this.index);
  }

  /** @private **/
  getEntries() {
    return this.entries;
  }

  /** @private **/
  get(index) {
    if (this.entries.length <= 0) return null;
    return this.entries[index];
  }

  /** @private **/
  getFromHistory(direction) {
    if (!this.canGetFromHistory(direction)) return null;

    this.index = Math.max(0, this.index + direction);
    return this.entries[this.index];
  }

  /** @private **/
  canGetFromHistory(direction) {
    if (this.entries.length === 0) return false;

    const index = this.index + direction;
    return 0 <= index && index < this.entries.length;
  }

  /** @private **/
  isSameCell(cellId) {
    const lastEntry = this.get(this.index);
    return lastEntry !== null && cellId === lastEntry.cellId;
  }
}
