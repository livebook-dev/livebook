defmodule LiveBook.LiveMarkdown.MarkdownHelpers do
  alias LiveBook.LiveMarkdown.MarkdownHelpers

  @doc """
  Reformats the given markdown document.
  """
  @spec reformat(String.t()) :: String.t()
  def reformat(markdown) do
    markdown
    |> EarmarkParser.as_ast()
    |> elem(1)
    |> MarkdownHelpers.Renderer.markdown_from_ast()
  end

  @doc """
  Extracts plain text from the given AST ignoring all the tags.
  """
  @spec text_from_ast(EarmarkParser.ast()) :: String.t()
  def text_from_ast(ast)

  def text_from_ast(ast) when is_list(ast) do
    ast
    |> Enum.map(&text_from_ast/1)
    |> Enum.join("")
  end

  def text_from_ast(ast) when is_binary(ast), do: ast
  def text_from_ast({_, _, ast, _}), do: text_from_ast(ast)
end
