export function isMacOS() {
  return /(Mac|iPhone|iPod|iPad)/i.test(navigator.platform);
}

export function isEditableElement(element) {
  return (
    ["input", "textarea"].includes(element.tagName.toLowerCase()) ||
    element.contentEditable === "true"
  );
}
