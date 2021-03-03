defmodule Livebook.LiveMarkdown do
  @moduledoc false

  # Notebook file format used by Livebook.
  #
  # The format is based off of Markdown and preserves compatibility,
  # in the sense that every LiveMarkdown file is a valid Markdown file.
  # LiveMarkdown uses HTML comments for storing metadata, so a Markdown standard
  # supporting that is assumed. Not every Markdown file is a valid LiveMarkdown file,
  # but may be converted to such by applying tiny changes, which the import function does.
  #
  # Currently the format is straightforward and specifies the following:
  #
  # 1. The file should have a leading *Heading 1* holding the notebook name.
  # 2. Every *Heading 2* starts a new section.
  # 3. Every Elixir code block maps to an Elixir cell.
  # 4. Adjacent regular Markdown text maps to a Markdown cell.
  # 5. Comments of the form `<!-- livebook:json_object -->` hold metadata
  #    any apply to the element they directly precede (e.g. an Elixir cell).
  #    Such comments may appear anywhere, for instance `<!-- livebook:{"force_markdown":true} -->`
  #    forces the next Markdown block to be treated as prat of Markdown cell (even if it's Elixir code block).
  #
  # ## Example
  #
  # Here's an example LiveMarkdown file:
  #
  #     # My Notebook
  #
  #     ## Section 1
  #
  #     Make sure to install:
  #
  #     * Erlang
  #     * Elixir
  #     * PostgreSQL
  #
  #     <!-- livebook:{"readonly":true} -->
  #
  #     ```elixir
  #     Enum.to_list(1..10)
  #     ```
  #
  #     This is it for this section.
  #
  #     ## Section 2
  #
  #     ```elixir
  #     # More Elixir code
  #     ```
  #
  # This file defines a notebook named *My Notebook* with two sections.
  # The first section includes 3 cells and the second section includes 1 Elixir cell.

  def extension(), do: ".livemd"
end
