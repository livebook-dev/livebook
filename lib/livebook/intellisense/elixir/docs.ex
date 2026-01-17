defmodule Livebook.Intellisense.Elixir.Docs do
  # This module is responsible for extracting and normalizing
  # information like documentation, signatures and specs.
  #
  # Note that we only extract the docs information when requested for
  # for the current node. For remote nodes, making several requests
  # for docs (which may be necessary if there are multiple modules)
  # adds to the overall latency. Remote intellisense is primarily used
  # with remote release nodes, which have docs stripped out anyway.

  @type member_info :: %{
          kind: member_kind(),
          name: atom(),
          arity: non_neg_integer(),
          from_default: boolean(),
          documentation: documentation(),
          signatures: list(signature()),
          specs: list(spec()),
          type_spec: type_spec(),
          meta: meta()
        }

  @type member_kind :: :function | :macro | :type

  @type documentation :: {format :: String.t(), content :: String.t()} | :hidden | nil

  @type signature :: String.t()

  @type meta :: map()

  @typedoc """
  A single spec annotation in the Erlang Abstract Format.
  """
  @type spec :: term()

  @typedoc """
  A tuple containing a single type annotation in the Erlang Abstract Format,
  tagged by its kind.
  """
  @type type_spec() :: {type_kind(), term()}
  @type type_kind() :: :type | :opaque

  @type definition ::
          {:module, module()} | {:function | :type, name :: atom(), arity :: pos_integer()}

  @doc """
  Fetches documentation for the given module if available.
  """
  @spec get_module_documentation(module(), node()) :: documentation()
  def get_module_documentation(module, node)

  def get_module_documentation(_module, node) when node != node(), do: nil

  def get_module_documentation(module, _node) do
    case Code.fetch_docs(module) do
      {:docs_v1, _, _, format, %{"en" => docstring}, _, _} ->
        {format, docstring}

      {:docs_v1, _, _, _, :hidden, _, _} ->
        :hidden

      _ ->
        nil
    end
  end

  @doc """
  Fetches information about the given module members if available.

  The given `members` are used to limit the result to the relevant
  entries. Arity may be given as `:any`, in which case all entries
  matching the name are returned.

  Functions with default arguments are normalized, such that each
  arity is treated as a separate member, sourcing documentation from
  the original one.

  ## Options

    * `:kinds` - a list of member kinds to limit the lookup to. Valid
      kinds are `:function`, `:macro` and `:type`. Defaults to all
      kinds

  """
  @spec lookup_module_members(
          module(),
          list({name :: atom(), arity :: non_neg_integer() | :any}),
          node(),
          keyword()
        ) :: list(member_info())
  def lookup_module_members(module, members, node, opts \\ [])

  def lookup_module_members(_module, _members, node, _opts) when node != node(), do: []

  def lookup_module_members(module, members, _node, opts) do
    members = MapSet.new(members)
    kinds = opts[:kinds] || [:function, :macro, :type]

    specs =
      with true <- :function in kinds or :macro in kinds,
           {:ok, specs} <- Code.Typespec.fetch_specs(module) do
        Map.new(specs)
      else
        _ -> %{}
      end

    type_specs =
      with true <- :type in kinds,
           {:ok, types} <- Code.Typespec.fetch_types(module) do
        for {type_kind, {name, _defs, vars}} = type <- types,
            type_kind in [:type, :opaque],
            into: Map.new(),
            do: {{name, Enum.count(vars)}, type}
      else
        _ -> %{}
      end

    case Elixir.Code.fetch_docs(module) do
      {:docs_v1, _, _, format, _, _, docs} ->
        for {{kind, name, base_arity}, _line, signatures, doc, meta} <- docs,
            kind in kinds,
            defaults = Map.get(meta, :defaults, 0),
            arity <- (base_arity - defaults)..base_arity,
            MapSet.member?(members, {name, arity}) or MapSet.member?(members, {name, :any}),
            do: %{
              kind: kind,
              name: name,
              arity: arity,
              from_default: arity != base_arity,
              documentation: documentation(doc, format),
              signatures: signatures,
              specs: Map.get(specs, {name, base_arity}, []),
              type_spec: Map.get(type_specs, {name, base_arity}, nil),
              meta: meta
            }

      _ ->
        []
    end
  end

  defp documentation(%{"en" => docstr}, format), do: {format, docstr}
  defp documentation(:hidden, _format), do: :hidden
  defp documentation(_doc, _format), do: nil

  @doc """
  Determines a more specific module type if any.
  """
  @spec get_module_subtype(module) ::
          :protocol | :implementation | :exception | :struct | :behaviour | nil
  def get_module_subtype(module) do
    cond do
      not ensure_loaded?(module) ->
        nil

      function_exported?(module, :__protocol__, 1) ->
        :protocol

      function_exported?(module, :__impl__, 1) ->
        :implementation

      function_exported?(module, :__struct__, 0) ->
        if function_exported?(module, :exception, 1) do
          :exception
        else
          :struct
        end

      function_exported?(module, :behaviour_info, 1) ->
        :behaviour

      true ->
        nil
    end
  end

  # In case insensitive file systems, attempting to load Elixir will
  # log a warning in the terminal as it wrongly loads elixir.beam,
  # so we explicitly list it.
  defp ensure_loaded?(Elixir), do: false
  defp ensure_loaded?(module), do: Code.ensure_loaded?(module)

  @doc """
  Extracts the location about an identifier found.

  The function returns the line where the identifier is located.
  """
  @spec locate_definition(list() | binary(), definition()) :: {:ok, pos_integer()} | :error
  def locate_definition(path, identifier)

  def locate_definition(path, {:module, module}) do
    with {:ok, {:raw_abstract_v1, annotations}} <- beam_lib_chunks(path, :abstract_code) do
      {:attribute, anno, :module, ^module} =
        Enum.find(annotations, &match?({:attribute, _, :module, _}, &1))

      {:ok, :erl_anno.line(anno)}
    end
  end

#  def locate_definition(path, {:function, name, arity}) do
#    with {:ok, {:debug_info_v1, _, {:elixir_v1, meta, _}}} <- beam_lib_chunks(path, :debug_info),
#         {_pair, _kind, kw, _body} <- keyfind(meta.definitions, {name, arity}) do
#      Keyword.fetch(kw, :line)
#    end
#  end

  def locate_definition(path, {:function, name, arity}) do
    case beam_lib_chunks(path, :debug_info) do
      {:ok, {:debug_info_v1, _, {:elixir_v1, meta, _}}} ->
        with {_pair, _kind, kw, _body} <- keyfind(meta.definitions, {name, arity}) do
          Keyword.fetch(kw, :line)
        end
      _ ->
        locate_erlang_function(path, name, arity)
    end
  end

  defp locate_erlang_function(path, name, arity) do
    with {:ok, {:raw_abstract_v1, annotations}} <- beam_lib_chunks(path, :abstract_code) do
      result =
        Enum.find_value(annotations, fn
          {:function, anno, ^name, ^arity, _} -> :erl_anno.line(anno)
          _ -> nil
        end)

      if result, do: {:ok, result}, else: :error
    else
      _ -> :error
    end
  end

  def locate_definition(path, {:type, name, arity}) do
    with {:ok, {:raw_abstract_v1, annotations}} <- beam_lib_chunks(path, :abstract_code) do
      fetch_type_line(annotations, name, arity)
    end
  end

  defp fetch_type_line(annotations, name, arity) do
    for {:attribute, anno, :type, {^name, _, vars}} <- annotations, length(vars) == arity do
      :erl_anno.line(anno)
    end
    |> case do
      [] -> :error
      lines -> {:ok, Enum.min(lines)}
    end
  end

  defp beam_lib_chunks(path, key) do
    case :beam_lib.chunks(path, [key]) do
      {:ok, {_, [{^key, value}]}} -> {:ok, value}
      _ -> :error
    end
  end

  defp keyfind(list, key) do
    List.keyfind(list, key, 0) || :error
  end

  @doc """
  Formats the given documentation content as Markdown.

  The `variant` argument can be either `:all` to return the full content,
  or `:short` to return only the first paragraph.
  """
  @spec format_documentation(documentation(), :all | :short) :: String.t()
  def format_documentation(doc, variant)

  def format_documentation(nil, _variant) do
    "No documentation available"
  end

  def format_documentation(:hidden, _variant) do
    "This is a private API"
  end

  def format_documentation({"text/markdown", markdown}, :short) do
    # Extract just the first paragraph
    markdown
    |> String.split("\n\n")
    |> hd()
    |> String.trim()
  end

  def format_documentation({"application/erlang+html", erlang_html}, :short) do
    # Extract just the first paragraph
    erlang_html
    |> Enum.find(&match?({:p, _, _}, &1))
    |> case do
      nil -> nil
      paragraph -> erlang_html_to_md([paragraph])
    end
  end

  def format_documentation({"text/markdown", markdown}, :all) do
    markdown
  end

  def format_documentation({"application/erlang+html", erlang_html}, :all) do
    erlang_html_to_md(erlang_html)
  end

  def format_documentation({format, _content}, _variant) do
    raise "unknown documentation format #{inspect(format)}"
  end

  # Erlang HTML AST
  # See https://erlang.org/doc/apps/erl_docgen/doc_storage.html#erlang-documentation-format

  defp erlang_html_to_md(ast) do
    build_md([], ast)
    |> IO.iodata_to_binary()
    |> String.trim()
  end

  defp build_md(iodata, ast)

  defp build_md(iodata, []), do: iodata

  defp build_md(iodata, [string | ast]) when is_binary(string) do
    string |> append_inline(iodata) |> build_md(ast)
  end

  defp build_md(iodata, [{tag, _, content} | ast]) when tag in [:em, :i] do
    render_emphasis(content) |> append_inline(iodata) |> build_md(ast)
  end

  defp build_md(iodata, [{tag, _, content} | ast]) when tag in [:strong, :b] do
    render_strong(content) |> append_inline(iodata) |> build_md(ast)
  end

  defp build_md(iodata, [{:code, _, content} | ast]) do
    render_code_inline(content) |> append_inline(iodata) |> build_md(ast)
  end

  defp build_md(iodata, [{:a, attrs, content} | ast]) do
    render_link(content, attrs) |> append_inline(iodata) |> build_md(ast)
  end

  defp build_md(iodata, [{:br, _, []} | ast]) do
    render_line_break() |> append_inline(iodata) |> build_md(ast)
  end

  defp build_md(iodata, [{tag, _, content} | ast]) when tag in [:p, :div] do
    render_paragraph(content) |> append_block(iodata) |> build_md(ast)
  end

  @headings ~w(h1 h2 h3 h4 h5 h6)a

  defp build_md(iodata, [{tag, _, content} | ast]) when tag in @headings do
    n = 1 + Enum.find_index(@headings, &(&1 == tag))
    render_heading(n, content) |> append_block(iodata) |> build_md(ast)
  end

  defp build_md(iodata, [{:pre, _, [{:code, _, [content]}]} | ast]) do
    render_code_block(content, "erlang") |> append_block(iodata) |> build_md(ast)
  end

  defp build_md(iodata, [{:div, [{:class, class} | _], content} | ast]) do
    type = class |> to_string() |> String.upcase()

    render_blockquote([{:p, [], [{:strong, [], [type]}]} | content])
    |> append_block(iodata)
    |> build_md(ast)
  end

  defp build_md(iodata, [{:ul, [{:class, "types"} | _], content} | ast]) do
    render_types_list(content) |> append_block(iodata) |> build_md(ast)
  end

  defp build_md(iodata, [{:ul, _, content} | ast]) do
    render_unordered_list(content) |> append_block(iodata) |> build_md(ast)
  end

  defp build_md(iodata, [{:ol, _, content} | ast]) do
    render_ordered_list(content) |> append_block(iodata) |> build_md(ast)
  end

  defp build_md(iodata, [{:dl, _, content} | ast]) do
    render_description_list(content) |> append_block(iodata) |> build_md(ast)
  end

  defp append_inline(md, iodata), do: [iodata, md]
  defp append_block(md, iodata), do: [iodata, "\n", md, "\n"]

  # Renderers

  defp render_emphasis(content) do
    ["*", build_md([], content), "*"]
  end

  defp render_strong(content) do
    ["**", build_md([], content), "**"]
  end

  defp render_code_inline(content) do
    ["`", build_md([], content), "`"]
  end

  defp render_link(content, attrs) do
    caption = build_md([], content)

    if href = attrs[:href] do
      ["[", caption, "](", href, ")"]
    else
      caption
    end
  end

  defp render_line_break(), do: "\\\n"

  defp render_paragraph(content), do: erlang_html_to_md(content)

  defp render_heading(n, content) do
    title = build_md([], content)
    [String.duplicate("#", n), " ", title]
  end

  defp render_code_block(content, language) do
    ["```", language, "\n", content, "\n```"]
  end

  defp render_blockquote(content) do
    inner = erlang_html_to_md(content)

    inner
    |> String.split("\n")
    |> Enum.map_intersperse("\n", &["> ", &1])
  end

  defp render_unordered_list(content) do
    marker_fun = fn _index -> "* " end
    render_list(content, marker_fun, "  ")
  end

  defp render_ordered_list(content) do
    marker_fun = fn index -> "#{index + 1}. " end
    render_list(content, marker_fun, "   ")
  end

  defp render_list(items, marker_fun, indent) do
    spaced? = spaced_list_items?(items)
    item_separator = if(spaced?, do: "\n\n", else: "\n")

    items
    |> Enum.map(fn {:li, _, content} -> erlang_html_to_md(content) end)
    |> Enum.with_index()
    |> Enum.map(fn {inner, index} ->
      [first_line | lines] = String.split(inner, "\n")

      first_line = marker_fun.(index) <> first_line

      lines =
        Enum.map(lines, fn
          "" -> ""
          line -> indent <> line
        end)

      Enum.intersperse([first_line | lines], "\n")
    end)
    |> Enum.intersperse(item_separator)
  end

  defp spaced_list_items?([{:li, _, [{:p, _, _content} | _]} | _items]), do: true
  defp spaced_list_items?([_ | items]), do: spaced_list_items?(items)
  defp spaced_list_items?([]), do: false

  defp render_description_list(content) do
    # Rewrite description list as an unordered list with pseudo heading
    content
    |> Enum.chunk_every(2)
    |> Enum.map(fn [{:dt, _, dt}, {:dd, _, dd}] ->
      {:li, [], [{:p, [], [{:strong, [], dt}]}, {:p, [], dd}]}
    end)
    |> render_unordered_list()
  end

  defp render_types_list(content) do
    content
    |> group_type_list_items([])
    |> render_unordered_list()
  end

  defp group_type_list_items([], acc), do: Enum.reverse(acc)

  defp group_type_list_items([{:li, [{:name, _type_name}], []} | items], acc) do
    group_type_list_items(items, acc)
  end

  defp group_type_list_items([{:li, [{:class, "type"}], content} | items], acc) do
    group_type_list_items(items, [{:li, [], [{:code, [], content}]} | acc])
  end

  defp group_type_list_items(
         [{:li, [{:class, "description"}], content} | items],
         [{:li, [], prev_content} | acc]
       ) do
    group_type_list_items(items, [{:li, [], prev_content ++ [{:p, [], content}]} | acc])
  end
end
