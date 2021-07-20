defmodule Livebook.Intellisense do
  @moduledoc false

  # This module provides intellisense related operations
  # suitable for integration with a text editor.
  #
  # In a way, this provides the very basic features of a
  # language server that Livebook uses.

  alias Livebook.Intellisense.Completion

  # Configures width used for inspect and specs formatting.
  @line_length 30
  @extended_line_length 80

  @doc """
  Resolves an intellisense request as defined by `Livebook.Runtime`.

  In practice this function simply dispatches the request to one of
  the other public functions in this module.
  """
  @spec handle_request(
          Livebook.Runtime.intellisense_request(),
          Code.binding(),
          Macro.Env.t()
        ) :: Livebook.Runtime.intellisense_response()
  def handle_request(request, env, binding)

  def handle_request({:completion, hint}, binding, env) do
    items = get_completion_items(hint, binding, env)
    %{items: items}
  end

  def handle_request({:details, line, index}, binding, env) do
    get_details(line, index, binding, env)
  end

  def handle_request({:format, code}, _binding, _env) do
    case format_code(code) do
      {:ok, code} -> %{code: code}
      :error -> nil
    end
  end

  @doc """
  Formats Elixir code.
  """
  @spec format_code(String.t()) :: {:ok, String.t()} | :error
  def format_code(code) do
    try do
      formatted =
        code
        |> Code.format_string!()
        |> IO.iodata_to_binary()

      {:ok, formatted}
    rescue
      _ -> :error
    end
  end

  @doc """
  Returns a list of completion suggestions for the given `hint`.
  """
  @spec get_completion_items(String.t(), Code.binding(), Macro.Env.t()) ::
          list(Livebook.Runtime.completion_item())
  def get_completion_items(hint, binding, env) do
    Completion.get_completion_items(hint, binding, env)
    |> Enum.map(&format_completion_item/1)
    |> Enum.sort_by(&completion_item_priority/1)
  end

  defp format_completion_item({:variable, name, value}),
    do: %{
      label: name,
      kind: :variable,
      detail: "variable",
      documentation: value_snippet(value, @line_length),
      insert_text: name
    }

  defp format_completion_item({:map_field, name, value}),
    do: %{
      label: name,
      kind: :field,
      detail: "field",
      documentation: value_snippet(value, @line_length),
      insert_text: name
    }

  defp format_completion_item({:module, name, doc_content}),
    do: %{
      label: name,
      kind: :module,
      detail: "module",
      documentation: format_doc_content(doc_content, :short),
      insert_text: String.trim_leading(name, ":")
    }

  defp format_completion_item({:function, module, name, arity, doc_content, signatures, spec}),
    do: %{
      label: "#{name}/#{arity}",
      kind: :function,
      detail: format_signatures(signatures, module),
      documentation:
        join_with_newlines([
          format_doc_content(doc_content, :short),
          format_spec(spec, @line_length) |> code()
        ]),
      insert_text: name
    }

  defp format_completion_item({:type, _module, name, arity, doc_content}),
    do: %{
      label: "#{name}/#{arity}",
      kind: :type,
      detail: "typespec",
      documentation: format_doc_content(doc_content, :short),
      insert_text: name
    }

  defp format_completion_item({:module_attribute, name, doc_content}),
    do: %{
      label: name,
      kind: :variable,
      detail: "module attribute",
      documentation: format_doc_content(doc_content, :short),
      insert_text: name
    }

  defp completion_item_priority(completion_item) do
    {completion_item_kind_priority(completion_item.kind), completion_item.label}
  end

  @ordered_kinds [:field, :variable, :module, :function, :type]

  defp completion_item_kind_priority(kind) when kind in @ordered_kinds do
    Enum.find_index(@ordered_kinds, &(&1 == kind))
  end

  @doc """
  Returns detailed information about identifier being
  at `index` in `line`.
  """
  @spec get_details(String.t(), non_neg_integer(), Code.binding(), Macro.Env.t()) ::
          Livebook.Runtime.details() | nil
  def get_details(line, index, binding, env) do
    {from, to} = subject_range(line, index)

    if from < to do
      subject = binary_part(line, from, to - from)

      Completion.get_completion_items(subject, binding, env, exact: true)
      |> Enum.map(&format_details_item/1)
      |> Enum.uniq()
      |> case do
        [] -> nil
        contents -> %{range: %{from: from, to: to}, contents: contents}
      end
    else
      nil
    end
  end

  # Reference: https://github.com/elixir-lang/elixir/blob/d1223e11fda880d5646f6385b33684d1b2ec0b9c/lib/elixir/lib/code.ex#L341-L345
  @operators '\\<>+-*/:=|&~^@%'
  @non_closing_punctuation '.,([{;'
  @closing_punctuation ')]}'
  @space '\t\s'
  @closing_identifier '?!'
  @punctuation @non_closing_punctuation ++ @closing_punctuation

  defp subject_range(line, index) do
    {left, right} = String.split_at(line, index)

    left =
      left
      |> String.to_charlist()
      |> Enum.reverse()
      |> consume_until(@space ++ @operators ++ (@punctuation -- '.') ++ @closing_identifier, ':@')
      |> List.to_string()

    right =
      right
      |> String.to_charlist()
      |> consume_until(@space ++ @operators ++ @punctuation, @closing_identifier)
      |> List.to_string()

    {index - byte_size(left), index + byte_size(right)}
  end

  defp consume_until(acc \\ [], chars, stop, stop_include)

  defp consume_until(acc, [], _, _), do: Enum.reverse(acc)

  defp consume_until(acc, [char | chars], stop, stop_include) do
    cond do
      char in stop_include -> consume_until([char | acc], [], stop, stop_include)
      char in stop -> consume_until(acc, [], stop, stop_include)
      true -> consume_until([char | acc], chars, stop, stop_include)
    end
  end

  defp format_details_item({:variable, name, value}) do
    join_with_divider([
      code(name),
      value_snippet(value, @extended_line_length)
    ])
  end

  defp format_details_item({:map_field, _name, value}) do
    join_with_divider([
      value_snippet(value, @extended_line_length)
    ])
  end

  defp format_details_item({:module, name, doc_content}) do
    join_with_divider([
      code(name),
      format_doc_content(doc_content, :all)
    ])
  end

  defp format_details_item({:function, module, _name, _arity, doc_content, signatures, spec}) do
    join_with_divider([
      format_signatures(signatures, module) |> code(),
      format_spec(spec, @extended_line_length) |> code(),
      format_doc_content(doc_content, :all)
    ])
  end

  defp format_details_item({:type, _module, name, _arity, doc_content}) do
    join_with_divider([
      code(name),
      format_doc_content(doc_content, :all)
    ])
  end

  defp format_details_item({:module_attribute, name, doc_content}) do
    join_with_divider([
      code("@" <> name),
      format_doc_content(doc_content, :all)
    ])
  end

  # Formatting helpers

  defp join_with_divider(strings), do: join_with(strings, "\n\n---\n\n")

  defp join_with_newlines(strings), do: join_with(strings, "\n\n")

  defp join_with(strings, joiner) do
    case Enum.reject(strings, &is_nil/1) do
      [] -> nil
      parts -> Enum.join(parts, joiner)
    end
  end

  defp code(nil), do: nil

  defp code(code) do
    """
    ```
    #{code}
    ```\
    """
  end

  defp value_snippet(value, line_length) do
    """
    ```
    #{inspect(value, pretty: true, width: line_length)}
    ```\
    """
  end

  defp format_signatures([], _module), do: nil

  defp format_signatures(signatures, module) do
    module_to_prefix(module) <> Enum.join(signatures, "\n")
  end

  defp module_to_prefix(mod) do
    case Atom.to_string(mod) do
      "Elixir." <> name -> name <> "."
      name -> ":" <> name <> "."
    end
  end

  defp format_spec(nil, _line_length), do: nil

  defp format_spec({{name, _arity}, spec_ast_list}, line_length) do
    spec_lines =
      Enum.map(spec_ast_list, fn spec_ast ->
        spec =
          Code.Typespec.spec_to_quoted(name, spec_ast)
          |> Macro.to_string()
          |> Code.format_string!(line_length: line_length)

        ["@spec ", spec]
      end)

    spec_lines
    |> Enum.intersperse("\n")
    |> IO.iodata_to_binary()
  end

  defp format_doc_content(doc, variant)

  defp format_doc_content(nil, _variant) do
    "No documentation available"
  end

  defp format_doc_content({"text/markdown", markdown}, :short) do
    # Extract just the first paragraph
    markdown
    |> String.split("\n\n")
    |> hd()
    |> String.trim()
  end

  defp format_doc_content({"application/erlang+html", erlang_html}, :short) do
    # Extract just the first paragraph
    erlang_html
    |> Enum.find(&match?({:p, _, _}, &1))
    |> case do
      nil -> nil
      paragraph -> erlang_html_to_md([paragraph])
    end
  end

  defp format_doc_content({"text/markdown", markdown}, :all) do
    markdown
  end

  defp format_doc_content({"application/erlang+html", erlang_html}, :all) do
    erlang_html_to_md(erlang_html)
  end

  defp format_doc_content({format, _content}, _variant) do
    raise "unknown documentation format #{inspect(format)}"
  end

  # Erlang HTML AST
  # See https://erlang.org/doc/apps/erl_docgen/doc_storage.html#erlang-documentation-format

  def erlang_html_to_md(ast) do
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

  defp build_md(iodata, [{:p, _, content} | ast]) do
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
    lines = Enum.map(content, fn {:li, _, line} -> line end)
    render_code_block(lines, "erlang") |> append_block(iodata) |> build_md(ast)
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
    |> Enum.map(&("> " <> &1))
    |> Enum.join("\n")
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
end
