/**
 * Delta is a format used to represent a set of changes introduced to a text document.
 *
 * See `Livebook.Delta` for more details.
 *
 * An implementation of the full Delta specification is available
 * in the official quill-delta package (https://github.com/quilljs/delta)
 * licensed under MIT. Our version is based on that package,
 * but simplified to match our non rich-text use case.
 */
export default class Delta {
  constructor(ops = []) {
    this.ops = ops;
  }

  /**
   * Appends a retain operation.
   */
  retain(length) {
    if (length <= 0) {
      return this;
    }

    return this.append({ retain: length });
  }

  /**
   * Appends an insert operation.
   */
  insert(text) {
    if (text === "") {
      return this;
    }

    return this.append({ insert: text });
  }

  /**
   * Appends a delete operation.
   */
  delete(length) {
    if (length <= 0) {
      return this;
    }

    return this.append({ delete: length });
  }

  /**
   * Appends the given operation.
   *
   * See `Livebook.Delta.append/2` for more details.
   */
  append(op) {
    if (this.ops.length === 0) {
      this.ops.push(op);
      return this;
    }

    const lastOp = this.ops.pop();

    // Insert and delete are commutative, so we always make sure
    // to put insert first to preserve the canonical form.
    if (isInsert(op) && isDelete(lastOp)) {
      return this.append(op).append(lastOp);
    }

    if (isInsert(op) && isInsert(lastOp)) {
      this.ops.push({ insert: lastOp.insert + op.insert });
      return this;
    }

    if (isDelete(op) && isDelete(lastOp)) {
      this.ops.push({ delete: lastOp.delete + op.delete });
      return this;
    }

    if (isRetain(op) && isRetain(lastOp)) {
      this.ops.push({ retain: lastOp.retain + op.retain });
      return this;
    }

    this.ops.push(lastOp, op);
    return this;
  }

  /**
   * Returns a new delta that is equivalent to applying the operations of this delta,
   * followed by operations of the given delta.
   */
  compose(other) {
    const thisIter = new Iterator(this.ops);
    const otherIter = new Iterator(other.ops);
    const delta = new Delta();

    while (thisIter.hasNext() || otherIter.hasNext()) {
      if (isInsert(otherIter.peek())) {
        delta.append(otherIter.next());
      } else if (isDelete(thisIter.peek())) {
        delta.append(thisIter.next());
      } else {
        const length = Math.min(thisIter.peekLength(), otherIter.peekLength());
        const thisOp = thisIter.next(length);
        const otherOp = otherIter.next(length);

        if (isRetain(otherOp)) {
          // Either retain or insert, so just apply it.
          delta.append(thisOp);

          // Other op should be delete, we could be an insert or retain
          // Insert + delete cancels out
        } else if (isDelete(otherOp) && isRetain(thisOp)) {
          delta.append(otherOp);
        }
      }
    }

    return delta._trim();
  }

  /**
   * Transform the given delta against this delta's operations. Returns a new delta.
   *
   * The method takes a `priority` argument indicates which delta
   * is considered to have happened first and is used for conflict resolution.
   *
   * See `Livebook.Delta.Transformation` for more details.
   */
  transform(other, priority) {
    if (priority !== "left" && priority !== "right") {
      throw new Error(
        `Invalid priority "${priority}", should be either "left" or "right"`
      );
    }

    const thisIter = new Iterator(this.ops);
    const otherIter = new Iterator(other.ops);
    const delta = new Delta();

    while (thisIter.hasNext() || otherIter.hasNext()) {
      if (
        isInsert(thisIter.peek()) &&
        (!isInsert(otherIter.peek()) || priority === "left")
      ) {
        const insertLength = operationLength(thisIter.next());
        delta.retain(insertLength);
      } else if (isInsert(otherIter.peek())) {
        delta.append(otherIter.next());
      } else {
        const length = Math.min(thisIter.peekLength(), otherIter.peekLength());
        const thisOp = thisIter.next(length);
        const otherOp = otherIter.next(length);

        if (isDelete(thisOp)) {
          // Our delete either makes their delete redundant or removes their retain
          continue;
        } else if (isDelete(otherOp)) {
          delta.append(otherOp);
        } else {
          // We retain either their retain or insert
          delta.retain(length);
        }
      }
    }

    return delta._trim();
  }

  _trim() {
    if (this.ops.length > 0 && isRetain(this.ops[this.ops.length - 1])) {
      this.ops.pop();
    }

    return this;
  }

  /**
   * Converts the delta to a compact representation, suitable for sending over the network.
   */
  toCompressed() {
    return this.ops.map((op) => {
      if (isInsert(op)) {
        return op.insert;
      } else if (isRetain(op)) {
        return op.retain;
      } else if (isDelete(op)) {
        return -op.delete;
      }

      throw new Error(`Invalid operation ${op}`);
    });
  }

  /**
   * Builds a new delta from the given compact representation.
   */
  static fromCompressed(list) {
    return list.reduce((delta, compressedOp) => {
      if (typeof compressedOp === "string") {
        return delta.insert(compressedOp);
      } else if (typeof compressedOp === "number" && compressedOp >= 0) {
        return delta.retain(compressedOp);
      } else if (typeof compressedOp === "number" && compressedOp < 0) {
        return delta.delete(-compressedOp);
      }

      throw new Error(`Invalid compressed operation ${compressedOp}`);
    }, new this());
  }

  /**
   * Returns the result of applying the delta to the given string.
   */
  applyToString(string) {
    let newString = "";
    let index = 0;

    this.ops.forEach((op) => {
      if (isRetain(op)) {
        newString += string.slice(index, index + op.retain);
        index += op.retain;
      }

      if (isInsert(op)) {
        newString += op.insert;
      }

      if (isDelete(op)) {
        index += op.delete;
      }
    });

    newString += string.slice(index);

    return newString;
  }
}

/**
 * Operations iterator simplifying the implementation of the delta methods above.
 *
 * Allows for iterating over operation slices by specifying the desired length.
 */
class Iterator {
  constructor(ops) {
    this.ops = ops;
    this.index = 0;
    this.offset = 0;
  }

  hasNext() {
    return this.peekLength() < Infinity;
  }

  next(length = Infinity) {
    const nextOp = this.ops[this.index];

    if (nextOp) {
      const offset = this.offset;
      const opLength = operationLength(nextOp);

      if (length >= opLength - offset) {
        length = opLength - offset;
        this.index += 1;
        this.offset = 0;
      } else {
        this.offset += length;
      }

      if (isDelete(nextOp)) {
        return { delete: length };
      } else if (isRetain(nextOp)) {
        return { retain: length };
      } else if (isInsert(nextOp)) {
        return { insert: nextOp.insert.substr(offset, length) };
      }
    } else {
      return { retain: length };
    }
  }

  peek() {
    return this.ops[this.index] || { retain: Infinity };
  }

  peekLength() {
    if (this.ops[this.index]) {
      return operationLength(this.ops[this.index]) - this.offset;
    } else {
      return Infinity;
    }
  }
}

function operationLength(op) {
  if (isInsert(op)) {
    return op.insert.length;
  }

  if (isRetain(op)) {
    return op.retain;
  }

  if (isDelete(op)) {
    return op.delete;
  }
}

export function isInsert(op) {
  return typeof op.insert === "string";
}

export function isRetain(op) {
  return typeof op.retain === "number";
}

export function isDelete(op) {
  return typeof op.delete === "number";
}
