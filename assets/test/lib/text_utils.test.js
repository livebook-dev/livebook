import {
  functionCallCodeUntilCursor,
  replacedSuffixLength,
} from "../../js/lib/text_utils";

test("replacedSuffixLength", () => {
  expect(replacedSuffixLength("to_string(", "")).toEqual(0);
  expect(replacedSuffixLength("to_string(", "length")).toEqual(0);
  expect(replacedSuffixLength("length", "length")).toEqual(6);
  expect(replacedSuffixLength("x = ~", "~r")).toEqual(1);
  expect(replacedSuffixLength("Enum.ma", "map")).toEqual(2);
  expect(replacedSuffixLength("Enum.ma", "map_reduce")).toEqual(2);
});

describe("functionCallCodeUntilCursor", () => {
  test("unclosed call", () => {
    const lines = ["Enum.map([1, 2], )"];
    expect(functionCallCodeUntilCursor(lines, 0, 13)).toEqual("Enum.map([1,");
    expect(functionCallCodeUntilCursor(lines, 0, 17)).toEqual(
      "Enum.map([1, 2],"
    );
  });

  test("unclosed multiline call", () => {
    const lines = ["Enum.map(", "  [1, 2],", "  fn", ")"];
    expect(functionCallCodeUntilCursor(lines, 0, 10)).toEqual("Enum.map(");
    expect(functionCallCodeUntilCursor(lines, 1, 6)).toEqual(
      "Enum.map(\n  [1,"
    );
    expect(functionCallCodeUntilCursor(lines, 2, 4)).toEqual(
      "Enum.map(\n  [1, 2],\n  f"
    );
  });

  test("returns a single line when all cells are closed", () => {
    const lines = ["Enum.map([1, 2], fun)", "length([])", "length []"];
    expect(functionCallCodeUntilCursor(lines, 1, 4)).toEqual("len");
    expect(functionCallCodeUntilCursor(lines, 1, 11)).toEqual("length([])");
    expect(functionCallCodeUntilCursor(lines, 2, 10)).toEqual("length []");
  });
});
