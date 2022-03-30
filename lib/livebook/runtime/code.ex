defmodule Livebook.Runtime.Code do
  @moduledoc false

  @doc """
  Finds or adds a `Mix.install/2` call to `code` and modifies it to
  include the given Mix deps.
  """
  @spec add_mix_deps(String.t(), list(tuple())) :: {:ok, String.t()} | {:error, String.t()}
  def add_mix_deps(code, deps) do
    with {:ok, ast, comments} <- string_to_quoted_with_comments(code),
         {:ok, ast} <- insert_deps(ast, deps),
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

  defp insert_deps(ast, deps) do
    with :error <- update_install(ast, deps) do
      dep_nodes = Enum.map(deps, &dep_node/1)

      install_node =
        {{:., [], [{:__aliases__, [], [:Mix]}, :install]}, [],
         [{:__block__, [newlines: 1], [dep_nodes]}]}

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
          [{:__block__, meta2, [dep_nodes]} | args]},
         deps
       ) do
    new_dep_nodes = for dep <- deps, not has_dep?(dep_nodes, dep), do: dep_node(dep)
    {:ok, {target, meta1, [{:__block__, meta2, [dep_nodes ++ new_dep_nodes]} | args]}}
  end

  defp update_install({:__block__, meta, nodes}, deps) do
    {nodes, found} =
      Enum.map_reduce(nodes, _found = false, fn
        node, false ->
          case update_install(node, deps) do
            {:ok, node} -> {node, true}
            _ -> {node, false}
          end

        node, true ->
          {node, true}
      end)

    if found do
      {:ok, {:__block__, meta, nodes}}
    else
      :error
    end
  end

  defp update_install(_node, _deps), do: :error

  defp has_dep?(deps, dep) do
    name = elem(dep, 0)

    Enum.any?(deps, fn
      {:__block__, _, [{{:__block__, _, [^name]}, _}]} -> true
      {:{}, _, [{:__block__, _, [^name]} | _]} -> true
      _ -> false
    end)
  end

  defp dep_node(dep), do: {:__block__, [], [Macro.escape(dep)]}
end
