import CursorHistory from "../../../js/hooks/session/cursor_history";

test("goBack returns when there's at least one cell", () => {
  const cursorHistory = new CursorHistory();
  const expectedEntry = { cellId: "123", line: "1", column: "1" };

  expect(cursorHistory.canGoBack()).toBe(false);

  cursorHistory.entries = [expectedEntry];
  cursorHistory.index = 0;

  expect(cursorHistory.index).toBe(0);
  expect(cursorHistory.canGoBack()).toBe(true);
  expect(cursorHistory.goBack()).toStrictEqual(expectedEntry);
});

describe("push", () => {
  test("does not add duplicated cells", () => {
    const cursorHistory = new CursorHistory();
    const entry = { cellId: "123", line: "1", column: "1" };

    expect(cursorHistory.index).toBe(-1);

    cursorHistory.entries = [entry];
    cursorHistory.index = 0;

    cursorHistory.push(entry.cellId, entry.line, "2");
    expect(cursorHistory.index).toBe(0);

    cursorHistory.push(entry.cellId, "2", entry.column);
    expect(cursorHistory.index).toBe(0);
  });

  test("removes oldest entries and keep it with a maximum of 20 entries", () => {
    const cursorHistory = new CursorHistory();

    for (let i = 0; i <= 19; i++) {
      const value = (i + 1).toString();
      cursorHistory.push(`123${value}`, value, "1");
    }

    expect(cursorHistory.index).toBe(19);

    cursorHistory.push("231", "1", "1");

    expect(cursorHistory.index).toBe(19);
    expect(cursorHistory.entries[0]).toStrictEqual({
      cellId: "1232",
      column: "1",
      line: "2",
    });
  });

  test("rewrites the subsequent cells if go back and saves a new cell", () => {
    const cursorHistory = new CursorHistory();
    expect(cursorHistory.canGoBack()).toBe(false);

    cursorHistory.entries = [
      { cellId: "123", line: "1", column: "1" },
      { cellId: "456", line: "1", column: "1" },
      { cellId: "789", line: "2", column: "1" },
    ];
    cursorHistory.index = 2;

    expect(cursorHistory.canGoBack()).toBe(true);

    // Go back to cell id 456
    cursorHistory.goBack();
    expect(cursorHistory.index).toBe(1);

    // Go back to cell id 123
    cursorHistory.goBack();
    expect(cursorHistory.index).toBe(0);

    // Removes the subsequent cells from stack
    // and adds the cell id 999 to the stack
    cursorHistory.push("999", "1", "1");
    expect(cursorHistory.index).toBe(1);
  });
});

test("removeAllFromCell removes the cells with given id", () => {
  const cursorHistory = new CursorHistory();
  const cellId = "123456789";

  cursorHistory.entries = [
    { cellId: "123", line: "1", column: "1" },
    { cellId: "456", line: "1", column: "1" },
    { cellId: cellId, line: "1", column: "1" },
    { cellId: "1234", line: "1", column: "1" },
    { cellId: cellId, line: "1", column: "1" },
    { cellId: "8901", line: "1", column: "1" },
    { cellId: cellId, line: "1", column: "1" },
  ];
  cursorHistory.index = 6;

  cursorHistory.removeAllFromCell(cellId);
  expect(cursorHistory.index).toBe(3);
});
