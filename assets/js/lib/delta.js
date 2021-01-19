/**
 * Delta represents a set of changes introduced to a text document.
 *
 * Delta is suitable for Operational Transformation and is hence
 * our primary building block in collaborative text editing.
 * For a detailed write-up see https://quilljs.com/docs/delta.
 * The specification covers rich-text editing, while we only
 * need to work on plain-text, so we use a strict subset of
 * the specification, where each operation is either:
 *
 *   * `{ retain: number }` - move by the given number of characters
 *   * `{ insert: string }` - insert the given text at the current position
 *   * `{ delete: number }` - delete the given number of characters starting from the current position
 *
 * This class provides a number of methods for creating and transforming a delta.
 *
 * The implementation based on https://github.com/quilljs/delta
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
   * To maintain the canonical form (uniqueness of representation)
   * this method follows two rules:
   *
   *   * insert always goes before delete
   *   * operations of the same type are merged into a single operation
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

    return delta.__trim();
  }

  /**
   * Transform the given delta against this delta's operations. Returns a new delta.
   *
   * This is the core idea behind Operational Transformation.

   * Let's mark this delta as A and the `other` delta as B
   * and assume they represent changes applied at the same time
   * to the same text state. If the current text state reflects
   * delta A being applied, we would like to apply delta B,
   * but to preserve its intent we need consider the changes made by A.
   * We can obtain a new delta B' by transforming it against the delta A,
   * so that does effectively what the original delta B meant to do.
   * In our case that would be `A.transform(B, "right")`.
   *
   * Transformation should work both ways satisfying the property (assuming B is considered to happen first):
   *
   * `A.concat(A.transform(B, "right")) = B.concat(B.transform(A, "left"))`
   *
   * In Git analogy this can be thought of as rebasing branch B onto branch A.
   *
   * The method takes a `priority` argument that indicates which delta should be
   * considered "first" and win ties, should be either "left" or "right".
   */
  transform(other, priority) {
    if (priority !== "left" && priority !== "right") {
      throw new Error(`Invalid priority "${priority}", should be either "left" or "right"`)
    }

    const thisIter = new Iterator(this.ops);
    const otherIter = new Iterator(other.ops);
    const delta = new Delta();

    while (thisIter.hasNext() || otherIter.hasNext()) {
      if (isInsert(thisIter.peek()) && (!isInsert(otherIter.peek()) || priority === "left")) {
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

    return delta.__trim();
  }

  __trim() {
    if (this.ops.length > 0 && isRetain(this.ops[this.ops.length - 1])) {
      this.ops.pop();
    }

    return this;
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
      return { retain: Infinity };
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

function isInsert(op) {
  return typeof op.insert === "string";
}

function isRetain(op) {
  return typeof op.retain === "number";
}

function isDelete(op) {
  return typeof op.delete === "number";
}
