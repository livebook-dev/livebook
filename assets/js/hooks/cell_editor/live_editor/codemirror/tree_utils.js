/**
 * Finds the closest node with the given name (parent or self).
 */
export function closestNode(node, names) {
  while (node) {
    if (names.includes(node.type.name)) return node;

    node = node.parent;
  }

  return null;
}

/**
 * Goes up the tree using the given path.
 *
 * Path is a list of parent node names, from innermost to outermost.
 * Returns the alst node on the path, but only if the path matches
 * exactly.
 */
export function ancestorNode(node, path) {
  let i = 0;

  while (i < path.length && node.parent) {
    if (node.parent.type.name !== path[i]) return null;

    node = node.parent;
    i++;
  }

  return i === path.length && node ? node : null;
}
