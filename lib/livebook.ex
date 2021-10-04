defmodule Livebook do
  @moduledoc """
  Livebook is an interactive notebook system for Elixir.

  This module includes the public API.
  """

  @doc """
  Parses the given Live Markdown document and converts it to Elixir
  source code.

  ## Limitations

  Note that the resulting script may not compile in some cases, for
  example if you define a macro in one cell and import it in another
  cell, it works fine in Livebook, because each cell is compiled
  separately. However, when running the script it gets compiled as a
  whole and consequently doing so doesn't work.

  Additionally, branching sections are commented out.
  """
  @spec live_markdown_to_elixir(String.t()) :: String.t()
  def live_markdown_to_elixir(markdown) do
    {notebook, _messages} = Livebook.LiveMarkdown.Import.notebook_from_markdown(markdown)
    Livebook.Notebook.Export.Elixir.notebook_to_elixir(notebook)
  end
end
