defmodule Livebook.LiveMarkdown do
  @moduledoc false

  # File format used to store Livebook notebooks.
  #
  # The format is a subset of Markdown, which means that every Live
  # Markdown is valid Markdown. Live Markdown uses HTML comments for
  # storing certain metadata, so we assume a Markdown standard that
  # supports comments. On the other hand, not every Markdown is a
  # valid Live Markdown file, however it can be imported as such by
  # applying tiny changes and assumptions.
  #
  # Currently the Live Markdown format imposes the following rules:
  #
  #   1. The file should have a leading *Heading 1* which contains
  #      the notebook name.
  #
  #   2. Every *Heading 2* starts a new section.
  #
  #   3. Every Elixir/Erlang code block represents a Code cell.
  #
  #   4. Adjacent regular Markdown blocks represents a Markdown cell.
  #
  #   5. Comments of the format `<!-- livebook:json -->` may appear
  #      anywhere in the file and hold Livebook specific data. The
  #      data should be treated as opaque, but is either of the
  #      following:
  #
  #      * description of a notebook object that cannot be naturally
  #        encoded using Markdown, such as Smart cell. In such case
  #        the JSON contains the `livebook_object` field
  #
  #      * notebook, section or cell metadata
  #
  #      * `{"force_markdown":true}` - an annotation forcing the next
  #        next Markdown block to be treated as part of Markdown cell
  #        (relevant for Elixir/Erlang code blocks, which otherwise
  #        are interpreted as Code cells)
  #
  #      * `{"break_markdown":true}` - an annotation splitting the
  #        markdown content into separate Markdown cells
  #
  #      * `{"output":true}` - an annotation marking a code snippet
  #        as cell output
  #
  #      * notebook stamp data with `"stamp"` and `"offset"` fields,
  #        placed at the very end of the file. For more details see
  #        `t:Livebook.Hubs.Provider.notebook_stamp/0`
  #
  #   6. An optional code block may appear between the notebook name
  #      heading and the first section heading. This block is parsed
  #      as the setup Code cell.
  #
  #   7. Any comments before the leading heading are kept.
  #
  # ## Example
  #
  # Here's an example LiveMarkdown file:
  #
  #     # My notebook
  #
  #     ## Section 1
  #
  #     Make sure to install:
  #
  #     * Erlang
  #     * Elixir
  #     * PostgreSQL
  #
  #     <!-- livebook:{"disable_formatting":true} -->
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

  @doc """
  The file extension used by Live Markdown files.
  """
  def extension(), do: ".livemd"

  @doc """
  Converts the given notebook into a Markdown document.

  ## Options

    * `:include_outputs` - whether to render cell outputs.
      Only textual outputs are included. Defaults to the
      value of `:persist_outputs` notebook attribute

  """
  @spec notebook_to_livemd(Notebook.t(), keyword()) :: {String.t(), list(String.t())}
  defdelegate notebook_to_livemd(notebook, opts \\ []), to: Livebook.LiveMarkdown.Export

  @doc """
  Converts the given Markdown document into a notebook data structure.

  Returns the notebook structure and a list of informative messages/warnings
  related to the imported input.
  """
  @spec notebook_from_livemd(String.t()) :: {Notebook.t(), list(String.t())}
  defdelegate notebook_from_livemd(markdown), to: Livebook.LiveMarkdown.Import
end
