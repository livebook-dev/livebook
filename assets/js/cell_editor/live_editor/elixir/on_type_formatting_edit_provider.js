import monaco from "../monaco";

/**
 * Defines custom auto-formatting behavior for Elixir.
 *
 * The provider is triggered when the user makes edits
 * and it may instruct the editor to apply some additional changes.
 */
const ElixirOnTypeFormattingEditProvider = {
  autoFormatTriggerCharacters: ["\n"],
  provideOnTypeFormattingEdits(model, position, char, options, token) {
    if (char === "\n") {
      return closingEndTextEdits(model, position);
    }

    return [];
  },
};

function closingEndTextEdits(model, position) {
  const lines = model.getLinesContent();
  const lineIndex = position.lineNumber - 1;
  const line = lines[lineIndex];
  const prevLine = lines[lineIndex - 1];
  const prevIndentation = indentation(prevLine);

  if (shouldInsertClosingEnd(lines, lineIndex)) {
    // If this is the last line or the line is not empty,
    // we have to insert a newline at the current position.
    // Otherwise we prefer to explicitly insert the closing end
    // in the next line, as it preserves current cursor position.
    const shouldInsertInNextLine =
      position.lineNumber < lines.length && isBlank(line);

    // If the next line is not available for inserting,
    // we could insert `\nend` but this moves the cursor,
    // so for now we just don't insert `end` at all
    // For more context see https://github.com/livebook-dev/livebook/issues/152
    if (!shouldInsertInNextLine) {
      return [];
    }

    const textEdit = insertClosingEndTextEdit(
      position,
      prevIndentation,
      shouldInsertInNextLine
    );

    return [textEdit];
  }

  return [];
}

function shouldInsertClosingEnd(lines, lineIndex) {
  const prevLine = lines[lineIndex - 1];
  const prevIndentation = indentation(prevLine);
  const prevTokens = tokens(prevLine);

  if (
    last(prevTokens) === "do" ||
    (prevTokens.includes("fn") && last(prevTokens) === "->")
  ) {
    const nextLineWithSameIndentation = lines
      .slice(lineIndex + 1)
      .filter((line) => !isBlank(line))
      .find((line) => indentation(line) === prevIndentation);

    if (nextLineWithSameIndentation) {
      const [firstToken] = tokens(nextLineWithSameIndentation);

      if (["after", "else", "catch", "rescue", "end"].includes(firstToken)) {
        return false;
      }
    }

    return true;
  }

  return false;
}

function insertClosingEndTextEdit(
  position,
  indentation,
  shouldInsertInNextLine
) {
  if (shouldInsertInNextLine) {
    return {
      range: new monaco.Range(
        position.lineNumber + 1,
        1,
        position.lineNumber + 1,
        1
      ),
      text: `${indentation}end\n`,
    };
  } else {
    return {
      range: new monaco.Range(
        position.lineNumber,
        position.column,
        position.lineNumber,
        position.column
      ),
      text: `\n${indentation}end`,
    };
  }
}

function indentation(line) {
  const [indentation] = line.match(/^\s*/);
  return indentation;
}

function tokens(line) {
  return line.replace(/#.*/, "").match(/->|[\w:]+/g) || [];
}

function last(list) {
  return list[list.length - 1];
}

function isBlank(string) {
  return string.trim() === "";
}

export default ElixirOnTypeFormattingEditProvider;
