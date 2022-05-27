defmodule Livebook.Runtime.Dependencies do
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

  @doc """
  Parses a plain Elixir term from its string representation.

  ## Examples

      iex> Livebook.Runtime.Dependencies.parse_term(~s|{:jason, "~> 1.3.0"}|)
      {:ok, {:jason, "~> 1.3.0"}}

      iex> Livebook.Runtime.Dependencies.parse_term(~s|{:jason, "~> 1.3.0", runtime: false, meta: 'data'}|)
      {:ok, {:jason, "~> 1.3.0", runtime: false, meta: 'data'}}

      iex> Livebook.Runtime.Dependencies.parse_term(~s|%{name: "Jake", numbers: [1, 2, 3.4]}|)
      {:ok, %{name: "Jake", numbers: [1, 2, 3.4]}}

      iex> Livebook.Runtime.Dependencies.parse_term(~s|{:call, Enum.count([])}|)
      :error

      iex> Livebook.Runtime.Dependencies.parse_term(~s|Enum|)
      :error
  """
  @spec parse_term(String.t()) :: {:ok, term()} | :error
  def parse_term(string) do
    case Code.string_to_quoted(string) do
      {:ok, ast} -> unescape_term(ast)
      {:error, _} -> :error
    end
  end

  defp unescape_term(node)
       when is_atom(node)
       when is_integer(node)
       when is_float(node)
       when is_binary(node),
       do: {:ok, node}

  defp unescape_term([]), do: {:ok, []}

  defp unescape_term([head | tail]) do
    with {:ok, head} <- unescape_term(head),
         {:ok, tail} <- unescape_term(tail),
         do: {:ok, [head | tail]}
  end

  defp unescape_term({left, right}) do
    with {:ok, left} <- unescape_term(left),
         {:ok, right} <- unescape_term(right),
         do: {:ok, {left, right}}
  end

  defp unescape_term({:{}, _, nodes}) do
    with {:ok, terms} <- unescape_term(nodes), do: {:ok, List.to_tuple(terms)}
  end

  defp unescape_term({:%{}, _, nodes}) do
    with {:ok, terms} <- unescape_term(nodes), do: {:ok, Map.new(terms)}
  end

  defp unescape_term(_node), do: :error

  @doc """
  Implements `Livebook.Runtime.search_packages/3` on top of
  `search_hex/2`.
  """
  @spec search_packages_on_hex(pid(), String.t()) :: reference()
  def search_packages_on_hex(send_to, search) do
    ref = make_ref()

    Task.Supervisor.start_child(Livebook.TaskSupervisor, fn ->
      response = search_hex(search)
      send(send_to, {:runtime_search_packages_response, ref, response})
    end)

    ref
  end

  @doc """
  Implements `Livebook.Runtime.search_packages/3` by searching
  through the given list of packages.
  """
  @spec search_packages_in_list(
          list(Livebook.Runtime.package()),
          pid(),
          String.t()
        ) :: reference()
  def search_packages_in_list(packages, send_to, search) do
    ref = make_ref()
    packages = Enum.filter(packages, &String.starts_with?(&1.name, search))
    send(send_to, {:runtime_search_packages_response, ref, {:ok, packages}})
    ref
  end

  @doc """
  Searches for packages on Hex.

  ## Options

      * `:api_url` - the base URL for Hex API requests. Optional
  """
  @spec search_hex(String.t(), keyword()) :: Livebook.Runtime.search_packages_response()
  def search_hex(search, opts \\ [])

  def search_hex("", _opts), do: {:ok, []}

  def search_hex(search, opts) do
    api_url = opts[:api_url] || "https://hex.pm/api"

    params = %{"search" => "name:#{search}*", "sort" => "downloads"}
    url = api_url <> "/packages?" <> URI.encode_query(params)

    case Livebook.Utils.HTTP.request(:get, url) do
      {:ok, status, _headers, body} ->
        with 200 <- status, {:ok, packages} <- Jason.decode(body) do
          packages = Enum.map(packages, &parse_package/1)
          {:ok, packages}
        else
          _ -> {:error, "unexpected response"}
        end

      {:error, reason} ->
        {:error, "failed to make a request, reason: #{inspect(reason)}"}
    end
  end

  defp parse_package(package) do
    {:ok, dependency} = parse_term(package["configs"]["mix.exs"])

    %{
      name: package["name"],
      version: package["latest_stable_version"] || package["latest_version"],
      description: package["meta"]["description"],
      url: package["html_url"],
      dependency: dependency
    }
  end
end
