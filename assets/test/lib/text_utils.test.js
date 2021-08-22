import { replacedSuffixLength } from "../../js/highlight/text_utils";

test("replacedSuffixLength", () => {
  expect(replacedSuffixLength("to_string(", "")).toEqual(0);
  expect(replacedSuffixLength("to_string(", "length")).toEqual(0);
  expect(replacedSuffixLength("length", "length")).toEqual(6);
  expect(replacedSuffixLength("x = ~", "~r")).toEqual(1);
  expect(replacedSuffixLength("Enum.ma", "map")).toEqual(2);
  expect(replacedSuffixLength("Enum.ma", "map_reduce")).toEqual(2);
});
