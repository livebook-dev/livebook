defmodule Livebook.Runtime.Code do
  @moduledoc false

  @doc """
  Finds or adds a `Mix.install/2` call to `code` and modifies it to
  include the given Mix dependency.
  """
  @spec add_mix_dependency(String.t(), tuple()) :: {:ok, String.t()} | {:error, String.t()}
  def add_mix_dependency(code, dependency) do
    with {:ok, ast, comments} <- string_to_quoted_with_comments(code),
         {:ok, ast} <- insert_dependency(ast, dependency),
         do: {:ok, format(ast, comments)}
  end

  defp string_to_quoted_with_comments(code) do
    try do
      to_quoted_opts = [
        literal_encoder: &{:ok, {:__block__, &2, [&1]}},
        token_metadata: true,
        unescape: false
      ]

      {ast, comments} = Code.string_to_quoted_with_comments!(code, to_quoted_opts)
      {:ok, ast, comments}
    rescue
      error -> {:error, Exception.format(:error, error)}
    end
  end

  defp insert_dependency(ast, dependency) do
    dep_name = elem(dependency, 0)
    dep_node = {:__block__, [], [Macro.escape(dependency)]}

    with :error <- update_install(ast, dep_name, dep_node) do
      install_node =
        {{:., [], [{:__aliases__, [], [:Mix]}, :install]}, [],
         [{:__block__, [newlines: 1], [[dep_node]]}]}

      {:ok, prepend_node(ast, install_node)}
    end
  end

  defp format(ast, comments) do
    ast
    |> Code.quoted_to_algebra(comments: comments)
    |> Inspect.Algebra.format(90)
    |> IO.iodata_to_binary()
  end

  defp prepend_node({:__block__, meta, nodes}, node) do
    {:__block__, meta, [node | nodes]}
  end

  defp prepend_node(ast, node) do
    {:__block__, [], [node, ast]}
  end

  defp update_install(
         {{:., _, [{:__aliases__, _, [:Mix]}, :install]} = target, meta1,
          [{:__block__, meta2, [deps]} | args]},
         dep_name,
         dep_node
       ) do
    if has_dep?(deps, dep_name) do
      {:ok, {target, meta1, [{:__block__, meta2, [deps]} | args]}}
    else
      {:ok, {target, meta1, [{:__block__, meta2, [deps ++ [dep_node]]} | args]}}
    end
  end

  defp update_install({:__block__, meta, nodes}, dep_name, dep_node) do
    nodes
    |> Enum.map_reduce(:error, fn
      node, :error ->
        case update_install(node, dep_name, dep_node) do
          {:ok, node} -> {node, :ok}
          error -> {node, error}
        end

      node, result ->
        {node, result}
    end)
    |> case do
      {nodes, :ok} -> {:ok, {:__block__, meta, nodes}}
      {_, error} -> error
    end
  end

  defp update_install(_node, _dep_name, _dep_node), do: :error

  defp has_dep?(deps, name) do
    Enum.any?(deps, fn
      {:__block__, _, [{{:__block__, _, [^name]}, _}]} -> true
      {:{}, _, [{:__block__, _, [^name]} | _]} -> true
      _ -> false
    end)
  end
end
