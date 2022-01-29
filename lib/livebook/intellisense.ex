defmodule Livebook.Intellisense do
  @moduledoc false

  # This module provides intellisense related operations
  # suitable for integration with a text editor.
  #
  # In a way, this provides the very basic features of a
  # language server that Livebook uses.

  alias Livebook.Intellisense.{IdentifierMatcher, SignatureMatcher, Docs}
  alias Livebook.Runtime

  # Configures width used for inspect and specs formatting.
  @line_length 45
  @extended_line_length 80

  @typedoc """
  Evaluation state to consider for intellisense.

  The `:map_binding` is only called when a value needs to
  be extracted from binding.
  """
  @type context :: %{
          env: Macro.Env.t(),
          map_binding: (Code.binding() -> any())
        }

  @doc """
  Resolves an intellisense request as defined by `Runtime`.

  In practice this function simply dispatches the request to one of
  the other public functions in this module.
  """
  @spec handle_request(
          Runtime.intellisense_request(),
          context()
        ) :: Runtime.intellisense_response()
  def handle_request(request, context)

  def handle_request({:completion, hint}, context) do
    items = get_completion_items(hint, context)
    %{items: items}
  end

  def handle_request({:details, line, column}, context) do
    get_details(line, column, context)
  end

  def handle_request({:signature, hint}, context) do
    get_signature_items(hint, context)
  end

  def handle_request({:format, code}, _context) do
    format_code(code)
  end

  @doc """
  Formats Elixir code.
  """
  @spec format_code(String.t()) :: Runtime.format_response()
  def format_code(code) do
    try do
      formatted =
        code
        |> Code.format_string!()
        |> IO.iodata_to_binary()

      %{code: formatted, code_error: nil}
    rescue
      error ->
        code_error = %{line: error.line, description: error.description}
        %{code: nil, code_error: code_error}
    end
  end

  @doc """
  Returns information about signatures matching the given `hint`.
  """
  @spec get_signature_items(String.t(), context()) :: Runtime.signature_response() | nil
  def get_signature_items(hint, context) do
    case SignatureMatcher.get_matching_signatures(hint, context) do
      {:ok, [], _active_argument} ->
        nil

      {:ok, signature_infos, active_argument} ->
        %{
          active_argument: active_argument,
          signature_items:
            signature_infos
            |> Enum.map(&format_signature_item/1)
            |> Enum.uniq()
        }

      :error ->
        nil
    end
  end

  defp format_signature_item({name, signature, documentation, specs}),
    do: %{
      signature: signature,
      arguments: arguments_from_signature(signature),
      documentation:
        join_with_divider([
          format_documentation(documentation, :short),
          format_specs(specs, name, @line_length) |> code()
        ])
    }

  defp arguments_from_signature(signature) do
    signature
    |> Code.string_to_quoted!()
    |> elem(2)
    |> Enum.map(&Macro.to_string/1)
  end

  @doc """
  Returns a list of completion suggestions for the given `hint`.
  """
  @spec get_completion_items(String.t(), context()) :: list(Runtime.completion_item())
  def get_completion_items(hint, context) do
    IdentifierMatcher.completion_identifiers(hint, context)
    |> Enum.filter(&include_in_completion?/1)
    |> Enum.map(&format_completion_item/1)
    |> Enum.concat(extra_completion_items(hint))
    |> Enum.sort_by(&completion_item_priority/1)
  end

  defp include_in_completion?({:module, _module, _display_name, :hidden}), do: false

  defp include_in_completion?(
         {:function, _module, _name, _arity, _type, _display_name, :hidden, _signatures, _specs,
          _meta}
       ),
       do: false

  defp include_in_completion?(_), do: true

  defp format_completion_item({:variable, name}),
    do: %{
      label: Atom.to_string(name),
      kind: :variable,
      detail: "variable",
      documentation: nil,
      insert_text: Atom.to_string(name)
    }

  defp format_completion_item({:map_field, name}),
    do: %{
      label: Atom.to_string(name),
      kind: :field,
      detail: "field",
      documentation: nil,
      insert_text: Atom.to_string(name)
    }

  defp format_completion_item({:in_struct_field, struct, name, default}),
    do: %{
      label: Atom.to_string(name),
      kind: :field,
      detail: "#{inspect(struct)} struct field",
      documentation:
        join_with_divider([
          code(name),
          """
          **Default**

          ```
          #{inspect(default, pretty: true, width: @line_length)}
          ```
          """
        ]),
      insert_text: "#{name}: "
    }

  defp format_completion_item({:module, module, display_name, documentation}) do
    subtype = Docs.get_module_subtype(module)

    kind =
      case subtype do
        :protocol -> :interface
        :exception -> :struct
        :struct -> :struct
        :behaviour -> :interface
        _ -> :module
      end

    detail = Atom.to_string(subtype || :module)

    %{
      label: display_name,
      kind: kind,
      detail: detail,
      documentation: format_documentation(documentation, :short),
      insert_text: String.trim_leading(display_name, ":")
    }
  end

  defp format_completion_item(
         {:function, module, name, arity, type, display_name, documentation, signatures, specs,
          _meta}
       ),
       do: %{
         label: "#{display_name}/#{arity}",
         kind: :function,
         detail: format_signatures(signatures, module),
         documentation:
           join_with_newlines([
             format_documentation(documentation, :short),
             format_specs(specs, name, @line_length) |> code()
           ]),
         insert_text:
           cond do
             type == :macro and keyword_macro?(name) ->
               "#{display_name} "

             type == :macro and env_macro?(name) ->
               display_name

             String.starts_with?(display_name, "~") ->
               display_name

             Macro.operator?(name, arity) ->
               display_name

             arity == 0 ->
               "#{display_name}()"

             true ->
               # A snippet with cursor in parentheses
               "#{display_name}($0)"
           end
       }

  defp format_completion_item({:type, _module, name, arity, documentation}),
    do: %{
      label: "#{name}/#{arity}",
      kind: :type,
      detail: "typespec",
      documentation: format_documentation(documentation, :short),
      insert_text: Atom.to_string(name)
    }

  defp format_completion_item({:module_attribute, name, documentation}),
    do: %{
      label: Atom.to_string(name),
      kind: :variable,
      detail: "module attribute",
      documentation: format_documentation(documentation, :short),
      insert_text: Atom.to_string(name)
    }

  defp keyword_macro?(name) do
    def? = name |> Atom.to_string() |> String.starts_with?("def")

    def? or
      name in [
        # Special forms
        :alias,
        :case,
        :cond,
        :for,
        :fn,
        :import,
        :quote,
        :receive,
        :require,
        :try,
        :with,

        # Kernel
        :destructure,
        :raise,
        :reraise,
        :if,
        :unless,
        :use
      ]
  end

  defp env_macro?(name) do
    name in [:__ENV__, :__MODULE__, :__DIR__, :__STACKTRACE__, :__CALLER__]
  end

  defp extra_completion_items(hint) do
    items = [
      %{
        label: "do",
        kind: :keyword,
        detail: "do-end block",
        documentation: nil,
        insert_text: "do\n  $0\nend"
      },
      %{
        label: "true",
        kind: :keyword,
        detail: "boolean",
        documentation: nil,
        insert_text: "true"
      },
      %{
        label: "false",
        kind: :keyword,
        detail: "boolean",
        documentation: nil,
        insert_text: "false"
      },
      %{
        label: "nil",
        kind: :keyword,
        detail: "special atom",
        documentation: nil,
        insert_text: "nil"
      },
      %{
        label: "when",
        kind: :keyword,
        detail: "guard operator",
        documentation: nil,
        insert_text: "when"
      }
    ]

    last_word = hint |> String.split(~r/\s/) |> List.last()

    if last_word == "" do
      []
    else
      Enum.filter(items, &String.starts_with?(&1.label, last_word))
    end
  end

  @ordered_kinds [:keyword, :field, :variable, :module, :struct, :interface, :function, :type]

  defp completion_item_priority(%{kind: :struct, detail: "exception"} = completion_item) do
    {length(@ordered_kinds), completion_item.label}
  end

  defp completion_item_priority(completion_item) do
    {completion_item_kind_priority(completion_item.kind), completion_item.label}
  end

  defp completion_item_kind_priority(kind) when kind in @ordered_kinds do
    Enum.find_index(@ordered_kinds, &(&1 == kind))
  end

  @doc """
  Returns detailed information about an identifier located
  in `column` in `line`.
  """
  @spec get_details(String.t(), pos_integer(), context()) :: Runtime.details_response() | nil
  def get_details(line, column, context) do
    case IdentifierMatcher.locate_identifier(line, column, context) do
      %{matches: []} ->
        nil

      %{matches: matches, range: range} ->
        contents =
          matches
          |> Enum.map(&format_details_item/1)
          |> Enum.uniq()

        %{range: range, contents: contents}
    end
  end

  defp format_details_item({:variable, name}), do: code(name)

  defp format_details_item({:map_field, name}), do: code(name)

  defp format_details_item({:in_struct_field, _struct, name, default}) do
    join_with_divider([
      code(name),
      """
      **Default**

      ```
      #{inspect(default, pretty: true, width: @line_length)}
      ```
      """
    ])
  end

  defp format_details_item({:module, module, _display_name, documentation}) do
    join_with_divider([
      code(inspect(module)),
      format_documentation(documentation, :all)
    ])
  end

  defp format_details_item(
         {:function, module, name, _arity, _type, _display_name, documentation, signatures, specs,
          meta}
       ) do
    join_with_divider([
      format_signatures(signatures, module) |> code(),
      format_meta(:since, meta),
      format_meta(:deprecated, meta),
      format_specs(specs, name, @extended_line_length) |> code(),
      format_documentation(documentation, :all)
    ])
  end

  defp format_details_item({:type, _module, name, _arity, documentation}) do
    join_with_divider([
      code(name),
      format_documentation(documentation, :all)
    ])
  end

  defp format_details_item({:module_attribute, name, documentation}) do
    join_with_divider([
      code("@#{name}"),
      format_documentation(documentation, :all)
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

  defp format_signatures([], _module), do: nil

  defp format_signatures(signatures, module) do
    signatures_string = Enum.join(signatures, "\n")

    # Don't add module prefix to operator signatures
    if :binary.match(signatures_string, ["(", "/"]) != :nomatch do
      inspect(module) <> "." <> signatures_string
    else
      signatures_string
    end
  end

  defp format_meta(:deprecated, %{deprecated: deprecated}) do
    "**Deprecated**. " <> deprecated
  end

  defp format_meta(:since, %{since: since}) do
    "Since " <> since
  end

  defp format_meta(_, _), do: nil

  defp format_specs([], _name, _line_length), do: nil

  defp format_specs(specs, name, line_length) do
    spec_lines =
      Enum.map(specs, fn spec ->
        code = Code.Typespec.spec_to_quoted(name, spec) |> Macro.to_string()
        ["@spec ", code]
      end)

    specs_code =
      spec_lines
      |> Enum.intersperse("\n")
      |> IO.iodata_to_binary()

    try do
      Code.format_string!(specs_code, line_length: line_length)
    rescue
      _ -> specs_code
    end
  end

  defp format_documentation(doc, variant)

  defp format_documentation(nil, _variant) do
    "No documentation available"
  end

  defp format_documentation(:hidden, _variant) do
    "This is a private API"
  end

  defp format_documentation({"text/markdown", markdown}, :short) do
    # Extract just the first paragraph
    markdown
    |> String.split("\n\n")
    |> hd()
    |> String.trim()
  end

  defp format_documentation({"application/erlang+html", erlang_html}, :short) do
    # Extract just the first paragraph
    erlang_html
    |> Enum.find(&match?({:p, _, _}, &1))
    |> case do
      nil -> nil
      paragraph -> erlang_html_to_md([paragraph])
    end
  end

  defp format_documentation({"text/markdown", markdown}, :all) do
    markdown
  end

  defp format_documentation({"application/erlang+html", erlang_html}, :all) do
    erlang_html_to_md(erlang_html)
  end

  defp format_documentation({format, _content}, _variant) do
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
end
