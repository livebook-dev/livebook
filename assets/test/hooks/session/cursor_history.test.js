import CursorHistory from "../../../js/hooks/session/cursor_history";

test("goBack returns when there's at least one cell", () => {
  const cursorHistory = new CursorHistory();
  const entry = { cellId: Math.random().toString(36), line: 1, offset: 1 };

  expect(cursorHistory.canGoBack()).toBe(false);
  expect(cursorHistory.getCurrent()).toStrictEqual(null);

  cursorHistory.push(entry.cellId, entry.line, entry.offset);

  expect(cursorHistory.canGoBack()).toBe(false);
  expect(cursorHistory.getCurrent()).toStrictEqual(entry);
});

describe("push", () => {
  test("does not add duplicated cells", () => {
    const cursorHistory = new CursorHistory();
    const entry = { cellId: Math.random().toString(36), line: 1, offset: 1 };

    expect(cursorHistory.canGoBack()).toBe(false);

    cursorHistory.push(entry.cellId, entry.line, entry.offset);
    cursorHistory.push(entry.cellId, entry.line, 2);
    expect(cursorHistory.canGoBack()).toBe(false);

    cursorHistory.push(entry.cellId, 2, entry.offset);
    expect(cursorHistory.canGoBack()).toBe(false);
  });

  test("removes oldest entries and keep it with a maximum of 20 entries", () => {
    const cursorHistory = new CursorHistory();

    for (let i = 0; i <= 19; i++) {
      const value = i + 1;
      cursorHistory.push(`123${value}`, value, 1);
    }

    cursorHistory.push("231", 1, 1);

    // Navigates to the bottom of the stack
    for (let i = 0; i <= 18; i++) {
      cursorHistory.goBack();
    }

    expect(cursorHistory.getCurrent()).toStrictEqual({
      cellId: "1232",
      offset: 1,
      line: 2,
    });
  });

  test("rewrites the subsequent cells if go back and saves a new cell", () => {
    const cursorHistory = new CursorHistory();
    expect(cursorHistory.canGoBack()).toBe(false);

    cursorHistory.push(Math.random().toString(36), 1, 1);
    cursorHistory.push(Math.random().toString(36), 1, 1);
    cursorHistory.push(Math.random().toString(36), 2, 1);

    expect(cursorHistory.canGoBack()).toBe(true);

    // Go back to cell id 456
    cursorHistory.goBack();
    expect(cursorHistory.canGoBack()).toBe(true);

    // Go back to cell id 123
    cursorHistory.goBack();
    expect(cursorHistory.canGoBack()).toBe(false);

    // Removes the subsequent cells from stack
    // and adds this cell to the stack
    cursorHistory.push(Math.random().toString(36), 1, 1);
    expect(cursorHistory.canGoForward()).toBe(false);
  });
});

test("removeAllFromCell removes the cells with given id", () => {
  const cursorHistory = new CursorHistory();
  const cellId = "123456789";

  cursorHistory.push("123", 1, 1);
  cursorHistory.push("456", 1, 1);
  cursorHistory.push(cellId, 1, 1);
  cursorHistory.push("1234", 1, 1);
  cursorHistory.push(cellId, 1, 1);
  cursorHistory.push("8901", 1, 1);
  cursorHistory.push(cellId, 1, 1);

  cursorHistory.removeAllFromCell(cellId);
  expect(cursorHistory.canGoForward()).toBe(false);
  expect(cursorHistory.getCurrent()).toStrictEqual({
    cellId: "8901",
    line: 1,
    offset: 1,
  });
});
