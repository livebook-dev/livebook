export default class Delta {
  constructor(ops = []) {
    this.ops = ops;
  }

  retain(length) {
    if (length <= 0) {
      return this;
    }

    return this.append({ retain: length });
  }

  insert(text) {
    if (text === "") {
      return this;
    }

    return this.append({ insert: text });
  }

  delete(length) {
    if (length <= 0) {
      return this;
    }

    return this.append({ delete: length });
  }

  append(op) {
    if (this.ops.length === 0) {
      this.ops.push(op);
      return this;
    }

    const lastOp = this.ops.pop();

    // Insert and delete are commutative, so we always make sure
    // to put insert first to preserve the canonical form (uniqueness of representation).
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

    return delta.trim();
  }

  transform(other) {
    const thisIter = new Iterator(this.ops);
    const otherIter = new Iterator(other.ops);
    const delta = new Delta();

    while (thisIter.hasNext() || otherIter.hasNext()) {
      if (isInsert(thisIter.peek()) && !isInsert(otherIter.peek())) {
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

    return delta.trim();
  }

  trim() {
    if (this.ops.length > 0 && isRetain(this.ops[this.ops.length - 1])) {
      this.ops.pop();
    }

    return this;
  }
}

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

export function operationLength(op) {
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
