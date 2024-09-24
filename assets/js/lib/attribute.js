export function parseHookProps(element, names) {
  const props = {};

  for (const name of names) {
    const attr = `data-p-${name}`;

    if (!element.hasAttribute(attr)) {
      throw new Error(
        `Missing attribute "${attr}" on element <${element.tagName}:${element.id}>`,
      );
    }

    const value = element.getAttribute(attr);
    props[kebabToCamelCase(name)] = JSON.parse(value);
  }

  return props;
}

function kebabToCamelCase(name) {
  const [part, ...parts] = name.split("-");

  return [
    part,
    ...parts.map((part) => part.charAt(0).toUpperCase() + part.slice(1)),
  ].join("");
}
