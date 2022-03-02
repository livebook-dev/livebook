defmodule Livebook.Notebook.Export.Elixir do
  alias Livebook.Notebook
  alias Livebook.Notebook.Cell

  @doc """
  Converts the given notebook into a Elixir source code.
  """
  @spec notebook_to_elixir(Notebook.t()) :: String.t()
  def notebook_to_elixir(notebook) do
    iodata = render_notebook(notebook)
    # Add trailing newline
    IO.iodata_to_binary([iodata, "\n"])
  end

  defp render_notebook(notebook) do
    name = ["# Title: ", notebook.name]
    sections = Enum.map(notebook.sections, &render_section(&1, notebook))

    [name | sections]
    |> Enum.intersperse("\n\n")
  end

  defp render_section(section, notebook) do
    name = ["# ── ", section.name, " ──"]

    name =
      if section.parent_id do
        {:ok, parent} = Notebook.fetch_section(notebook, section.parent_id)
        [name, " (⎇ from ", parent.name, ")"]
      else
        name
      end

    cells =
      section.cells
      |> Enum.map(&render_cell(&1, section))
      |> Enum.reject(&(&1 == []))

    [name | cells]
    |> Enum.intersperse("\n\n")
  end

  defp render_cell(%Cell.Markdown{} = cell, _section) do
    cell.source
    |> Livebook.LiveMarkdown.MarkdownHelpers.reformat()
    |> String.split("\n")
    |> Enum.map_intersperse("\n", &comment_out/1)
  end

  defp render_cell(%Cell.Code{} = cell, section) do
    code = get_code_cell_code(cell)

    if section.parent_id do
      code
      |> IO.iodata_to_binary()
      |> String.split("\n")
      |> Enum.map_intersperse("\n", &comment_out/1)
    else
      code
    end
  end

  defp render_cell(%Cell.Smart{} = cell, ctx) do
    render_cell(%{Cell.Code.new() | source: cell.source}, ctx)
  end

  defp render_cell(_cell, _section), do: []

  defp comment_out(""), do: ""
  defp comment_out(line), do: ["# ", line]

  defp get_code_cell_code(%{source: source, disable_formatting: true}),
    do: source

  defp get_code_cell_code(%{source: source}), do: format_code(source)

  defp format_code(code) do
    try do
      Code.format_string!(code)
    rescue
      _ -> code
    end
  end
end
