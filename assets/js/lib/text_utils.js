/**
 * Returns length of suffix in `string` that should be replaced
 * with `newSuffix` to avoid duplication.
 */
export function replacedSuffixLength(string, newSuffix) {
  let suffix = newSuffix;

  while (!string.endsWith(suffix)) {
    suffix = suffix.slice(0, -1);
  }

  return suffix.length;
}
