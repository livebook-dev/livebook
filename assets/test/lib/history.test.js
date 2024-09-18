import History from "../../js/lib/history";

test("goBack returns when there's at least one cell", () => {
  const history = new History();
  const expectedEntry = { cellId: "123", line: "1" };

  expect(history.canGoBack()).toBe(false);

  history.entries = [expectedEntry];
  history.index = 0;

  expect(history.index).toBe(0);
  expect(history.canGoBack()).toBe(true);
  expect(history.goBack()).toStrictEqual(expectedEntry);
});

describe("saveCell", () => {
  test("does not add duplicated cells", () => {
    const history = new History();
    const entry = { cellId: "123", line: "1" };

    expect(history.index).toBe(-1);

    history.entries = [entry];
    history.index = 0;

    history.saveCell(entry.cellId, entry.line);
    expect(history.index).toBe(0);

    // It will only add a new cell if the line isn't the same
    // as the last entry from the stack
    history.saveCell(entry.cellId, "2");
    expect(history.index).toBe(1);
  });

  test("removes oldest entries and keep it with a maximum of 20 entries", () => {
    const history = new History();

    for (let i = 0; i <= 19; i++) history.saveCell("123", (i + 1).toString());

    expect(history.index).toBe(19);

    history.saveCell("231", "1");

    expect(history.index).toBe(19);
    expect(history.entries[0]).toStrictEqual({ cellId: "123", line: "2" });
  });

  test("rewrites the subsequent cells if go back and saves a new cell", () => {
    const history = new History();
    expect(history.canGoBack()).toBe(false);

    history.entries = [
      { cellId: "123", line: "1" },
      { cellId: "456", line: "1" },
      { cellId: "789", line: "2" },
    ];
    history.index = 2;

    expect(history.canGoBack()).toBe(true);

    // Go back to cell id 456
    history.goBack();
    expect(history.index).toBe(1);

    // Go back to cell id 123
    history.goBack();
    expect(history.index).toBe(0);

    // Removes the subsequent cells from stack
    // and adds the cell id 999 to the stack
    history.saveCell("999", "1");
    expect(history.index).toBe(1);
  });
});

test("removeAllFromCell removes the cells with given id", () => {
  const history = new History();
  const cellId = "123456789";

  history.entries = [
    { cellId: "123", line: "1" },
    { cellId: "456", line: "1" },
    { cellId: cellId, line: "1" },
    { cellId: "1234", line: "1" },
    { cellId: cellId, line: "1" },
    { cellId: "8901", line: "1" },
    { cellId: cellId, line: "1" },
  ];
  history.index = 6;

  history.removeAllFromCell(cellId);
  expect(history.index).toBe(3);
});
