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

/**
 * Given lines and cursor position returns a code snippet with
 * unclosed parenthesised call.
 *
 * If there is no unclosed call, the current line is returned
 * instead.
 */
export function functionCallCodeUntilCursor(
  lines,
  cursorLineIdx,
  cursorColumn
) {
  const currentLine = lines[cursorLineIdx].slice(0, cursorColumn - 1);

  let openings = characterCount(currentLine, "(");
  let closings = characterCount(currentLine, ")");
  let lineIdx = cursorLineIdx;

  while (lineIdx > 0 && openings <= closings) {
    lineIdx--;
    const line = lines[lineIdx];
    openings += characterCount(line, "(");
    closings += characterCount(line, ")");
  }

  if (openings > closings) {
    return lines.slice(lineIdx, cursorLineIdx).concat([currentLine]).join("\n");
  } else {
    return currentLine;
  }
}

function characterCount(string, char) {
  return string.split(char).length - 1;
}
