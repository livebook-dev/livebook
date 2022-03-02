defmodule Livebook.LiveMarkdown do
  @moduledoc false

  # Notebook file format used by Livebook.
  #
  # The format is based off of Markdown and preserves compatibility,
  # in the sense that every LiveMarkdown file is a valid Markdown file.
  # LiveMarkdown uses HTML comments for storing metadata, so a Markdown
  # standard supporting such comments is assumed. Not every Markdown file
  # is a valid LiveMarkdown file, but may be converted to such by applying
  # tiny changes, which the import function does.
  #
  # Currently the format is straightforward and specifies the following:
  #
  #   1. The file should have a leading *Heading 1* holding the notebook name
  #
  #   2. Every *Heading 2* starts a new section
  #
  #   3. Every Elixir code block maps to a Code cell
  #
  #   4. Adjacent regular Markdown text maps to a Markdown cell
  #
  #   5. Comments of the form `<!-- livebook:json_object -->` hold Livebook data
  #      and may be one of the following:
  #
  #      * a description of a notebook object that cannot be naturally encoded
  #        using Markdown, in such case the JSON contains a "livebook_object" field
  #
  #      * a metadata that may appear anywhere and applies to the element
  #        it directly precedes, recognised metadatas are:
  #
  #        - `{"force_markdown":true}` - an annotation forcing the next Markdown
  #          block to be treated as part of Markdown cell (relevant for Elixir code
  #          blocks, which otherwise are interpreted as Code cells)
  #
  #        - `{"break_markdown":true}` - an annotation splitting the markdown content
  #          into separate Markdown cells
  #
  #        - `{"output":true}` - an annotation marking a code snippet as cell output
  #
  #        - section metadata, recognised keys `branch_parent_index`
  #
  #        - cell metadata, recognised keys: `disable_formatting`
  #
  #   6. Any comments before the leading heading are kept.
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
  # The first section includes 3 cells and the second section includes 1 Code cell.

  @doc """
  The file extension used by Live Markdown files.
  """
  def extension(), do: ".livemd"

  @doc """
  Converts the given notebook into a Markdown document.

  ## Options

    * `:include_outputs` - whether to render cell outputs.
      Only textual outputs are included. Defaults to the
      value of `:persist_outputs` notebook attribute.
  """
  @spec notebook_to_livemd(Notebook.t(), keyword()) :: String.t()
  defdelegate notebook_to_livemd(notebook, opts \\ []), to: Livebook.LiveMarkdown.Export

  @doc """
  Converts the given Markdown document into a notebook data structure.

  Returns the notebook structure and a list of informative messages/warnings
  related to the imported input.
  """
  @spec notebook_from_livemd(String.t()) :: {Notebook.t(), list(String.t())}
  defdelegate notebook_from_livemd(markdown), to: Livebook.LiveMarkdown.Import
end
