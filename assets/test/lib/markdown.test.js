import { test, expect, vi } from "vitest";
import morphdom from "morphdom";
import { renderMermaid } from "../../js/lib/markdown/mermaid";
import Markdown from "../../js/lib/markdown";

vi.mock("morphdom", () => ({ default: vi.fn() }));
vi.mock("katex/contrib/copy-tex", () => ({}));
vi.mock("../../js/lib/markdown/mermaid", () => ({ renderMermaid: vi.fn() }));

test("setContent ignores outdated renders that finish late", async () => {
  // The first render waits for a slow mermaid graph, while the second
  // render gets the graph immediately (for example, from cache)
  let resolveSlowGraph;
  renderMermaid
    .mockReturnValueOnce(new Promise((resolve) => (resolveSlowGraph = resolve)))
    .mockResolvedValueOnce("<svg></svg>");

  const container = {};
  const markdown = new Markdown(
    container,
    "```mermaid\ngraph TD;\n```\n\nfirst",
  );
  markdown.setContent("```mermaid\ngraph TD;\n```\n\nsecond");

  await vi.waitFor(() => expect(morphdom).toHaveBeenCalledTimes(1));
  resolveSlowGraph("<svg></svg>");
  await new Promise((resolve) => setTimeout(resolve, 50));

  const [, lastHtml] = morphdom.mock.lastCall;
  expect(lastHtml).toContain("second");
});
