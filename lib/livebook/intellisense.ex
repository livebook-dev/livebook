defmodule Livebook.Intellisense do
  # This module provides intellisense related operations suitable for
  # integration with a text editor.
  #
  # In a way, this provides the very basic features of a language
  # server that Livebook uses.

  alias Livebook.Intellisense
  alias Livebook.Runtime

  # Configures width used for inspect and specs formatting.
  @line_length 45
  @extended_line_length 80

  @typedoc """
  Evaluation state to consider for intellisense.

  The `:map_binding` is only called when a value needs to be extracted
  from binding.
  """
  @type context :: %{
          env: Macro.Env.t(),
          ebin_path: String.t() | nil,
          map_binding: (Code.binding() -> any())
        }

  @doc """
  Adjusts the system for more accurate intellisense.
  """
  @spec load() :: :ok
  def load() do
    # Completion looks for modules in loaded applications, so we ensure
    # that the most relevant built-in applications are loaded
    apps = [:erts, :crypto, :inets, :public_key, :runtime_tools, :ex_unit, :iex]

    for app <- apps do
      Application.load(app)
    end

    :ok
  end

  @doc """
  Clears all cache stored by the intellisense modules.
  """
  @spec clear_cache() :: :ok
  def clear_cache() do
    for node <- Node.list() do
      clear_cache(node)
    end

    :ok
  end

  @doc """
  Clear any cache stored related to the given node.
  """
  @spec clear_cache(node()) :: :ok
  def clear_cache(node) do
    Intellisense.IdentifierMatcher.clear_all_loaded(node)
  end

  @doc """
  Resolves an intellisense request as defined by `Runtime`.

  In practice this function simply dispatches the request to one of
  the other public functions in this module.
  """
  @spec handle_request(
          Runtime.intellisense_request(),
          context(),
          node()
        ) :: Runtime.intellisense_response()
  def handle_request(request, context, node)

  def handle_request({:completion, hint}, context, node) do
    items = get_completion_items(hint, context, node)
    %{items: items}
  end

  def handle_request({:details, line, column}, context, node) do
    get_details(line, column, context, node)
  end

  def handle_request({:signature, hint}, context, node) do
    get_signature_items(hint, context, node)
  end

  def handle_request({:format, code}, _context, _node) do
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

      %{code: formatted, code_markers: []}
    rescue
      error in [SyntaxError, TokenMissingError, MismatchedDelimiterError] ->
        code_marker = %{line: error.line, description: error.description, severity: :error}
        %{code: nil, code_markers: [code_marker]}
    end
  end

  @doc """
  Returns information about signatures matching the given `hint`.
  """
  @spec get_signature_items(String.t(), context(), node()) :: Runtime.signature_response() | nil
  def get_signature_items(hint, context, node) do
    case Intellisense.SignatureMatcher.get_matching_signatures(hint, context, node) do
      {:ok, [], _active_argument} ->
        nil

      {:ok, signature_infos, active_argument} ->
        %{
          active_argument: active_argument,
          items:
            signature_infos
            |> Enum.map(&format_signature_item/1)
            |> Enum.uniq()
        }

      :error ->
        nil
    end
  end

  defp format_signature_item({_name, signature, _documentation, _specs}),
    do: %{
      signature: signature,
      arguments: arguments_from_signature(signature)
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
  @spec get_completion_items(String.t(), context(), node()) :: list(Runtime.completion_item())
  def get_completion_items(hint, context, node) do
    Intellisense.IdentifierMatcher.completion_identifiers(hint, context, node)
    |> Enum.filter(&include_in_completion?/1)
    |> Enum.map(&format_completion_item/1)
    |> Enum.concat(extra_completion_items(hint))
    |> Enum.sort_by(&completion_item_priority/1)
  end

  defp include_in_completion?(%{kind: :module, documentation: :hidden}), do: false
  defp include_in_completion?(%{kind: :function, documentation: :hidden}), do: false
  defp include_in_completion?(_), do: true

  defp format_completion_item(%{kind: :variable, name: name}),
    do: %{
      label: Atom.to_string(name),
      kind: :variable,
      documentation: "(variable)",
      insert_text: Atom.to_string(name)
    }

  defp format_completion_item(%{kind: :map_field, name: name}),
    do: %{
      label: Atom.to_string(name),
      kind: :field,
      documentation: "(field)",
      insert_text: Atom.to_string(name)
    }

  defp format_completion_item(%{kind: :in_map_field, name: name}),
    do: %{
      label: Atom.to_string(name),
      kind: :field,
      documentation: "(field)",
      insert_text: "#{name}: "
    }

  defp format_completion_item(%{
         kind: :in_struct_field,
         struct: struct,
         name: name,
         default: default
       }),
       do: %{
         label: Atom.to_string(name),
         kind: :field,
         documentation:
           join_with_divider([
             """
             `%#{inspect(struct)}{}` struct field.

             **Default**

             ```
             #{inspect(default, pretty: true, width: @line_length)}
             ```\
             """
           ]),
         insert_text: "#{name}: "
       }

  defp format_completion_item(%{
         kind: :module,
         module: module,
         display_name: display_name,
         documentation: documentation
       }) do
    subtype = Intellisense.Docs.get_module_subtype(module)

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
      documentation:
        join_with_newlines([
          format_documentation(documentation, :short),
          "(#{detail})"
        ]),
      insert_text: String.trim_leading(display_name, ":")
    }
  end

  defp format_completion_item(%{
         kind: :function,
         module: module,
         name: name,
         arity: arity,
         type: type,
         display_name: display_name,
         documentation: documentation,
         signatures: signatures
       }),
       do: %{
         label: "#{display_name}/#{arity}",
         kind: :function,
         documentation:
           join_with_newlines([
             format_documentation(documentation, :short),
             code(format_signatures(signatures, module, name, arity))
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
               "#{display_name}(${})"
           end
       }

  defp format_completion_item(%{
         kind: :type,
         name: name,
         arity: arity,
         documentation: documentation,
         type_spec: type_spec
       }),
       do: %{
         label: "#{name}/#{arity}",
         kind: :type,
         documentation:
           join_with_newlines([
             format_documentation(documentation, :short),
             format_type_spec(type_spec, @line_length) |> code()
           ]),
         insert_text:
           cond do
             arity == 0 -> "#{Atom.to_string(name)}()"
             true -> "#{Atom.to_string(name)}(${})"
           end
       }

  defp format_completion_item(%{
         kind: :module_attribute,
         name: name,
         documentation: documentation
       }),
       do: %{
         label: Atom.to_string(name),
         kind: :variable,
         documentation:
           join_with_newlines([
             format_documentation(documentation, :short),
             "(module attribute)"
           ]),
         insert_text: Atom.to_string(name)
       }

  defp format_completion_item(%{kind: :bitstring_modifier, name: name, arity: arity}) do
    insert_text =
      if arity == 0 do
        Atom.to_string(name)
      else
        "#{name}(${})"
      end

    %{
      label: Atom.to_string(name),
      kind: :type,
      documentation: "(bitstring option)",
      insert_text: insert_text
    }
  end

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
        label: "true",
        kind: :keyword,
        documentation: "(boolean)",
        insert_text: "true"
      },
      %{
        label: "false",
        kind: :keyword,
        documentation: "(boolean)",
        insert_text: "false"
      },
      %{
        label: "nil",
        kind: :keyword,
        documentation: "(special atom)",
        insert_text: "nil"
      },
      %{
        label: "when",
        kind: :keyword,
        documentation: "(guard operator)",
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

  @ordered_kinds [
    :keyword,
    :field,
    :variable,
    :module,
    :struct,
    :interface,
    :function,
    :type,
    :bitstring_option
  ]

  defp completion_item_priority(%{kind: :struct} = completion_item) do
    if completion_item.documentation =~ "(exception)" do
      {length(@ordered_kinds), completion_item.label}
    else
      {completion_item_kind_priority(completion_item.kind), completion_item.label}
    end
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
  @spec get_details(String.t(), pos_integer(), context(), node()) ::
          Runtime.details_response() | nil
  def get_details(line, column, context, node) do
    %{matches: matches, range: range} =
      Intellisense.IdentifierMatcher.locate_identifier(line, column, context, node)

    case Enum.filter(matches, &include_in_details?/1) do
      [] ->
        nil

      matches ->
        matches = Enum.sort_by(matches, & &1[:arity], :asc)
        contents = Enum.map(matches, &format_details_item/1)

        definition = get_definition_location(hd(matches), context)

        %{range: range, contents: contents, definition: definition}
    end
  end

  defp include_in_details?(%{kind: :function, from_default: true}), do: false
  defp include_in_details?(%{kind: :bitstring_modifier}), do: false
  defp include_in_details?(_), do: true

  defp format_details_item(%{kind: :variable, name: name}), do: code(name)

  defp format_details_item(%{kind: :map_field, name: name}), do: code(name)

  defp format_details_item(%{kind: :in_map_field, name: name}), do: code(name)

  defp format_details_item(%{kind: :in_struct_field, name: name, default: default}) do
    join_with_divider([
      code(name),
      """
      **Default**

      ```
      #{inspect(default, pretty: true, width: @line_length)}
      ```\
      """
    ])
  end

  defp format_details_item(%{kind: :module, module: module, documentation: documentation}) do
    join_with_divider([
      code(inspect(module)),
      format_docs_link(module),
      format_documentation(documentation, :all)
    ])
  end

  defp format_details_item(%{
         kind: :function,
         module: module,
         name: name,
         arity: arity,
         documentation: documentation,
         signatures: signatures,
         specs: specs,
         meta: meta
       }) do
    join_with_divider([
      format_signatures(signatures, module, name, arity) |> code(),
      join_with_middle_dot([
        format_docs_link(module, {:function, name, arity}),
        format_meta(:since, meta)
      ]),
      format_meta(:deprecated, meta),
      format_specs(specs, name, @extended_line_length) |> code(),
      format_documentation(documentation, :all)
    ])
  end

  defp format_details_item(%{
         kind: :type,
         module: module,
         name: name,
         arity: arity,
         documentation: documentation,
         type_spec: type_spec
       }) do
    join_with_divider([
      format_type_signature(type_spec, module, name, arity) |> code(),
      format_docs_link(module, {:type, name, arity}),
      format_type_spec(type_spec, @extended_line_length) |> code(),
      format_documentation(documentation, :all)
    ])
  end

  defp format_details_item(%{kind: :module_attribute, name: name, documentation: documentation}) do
    join_with_divider([
      code("@#{name}"),
      format_documentation(documentation, :all)
    ])
  end

  defp get_definition_location(%{kind: :module, module: module}, context) do
    get_definition_location(module, context, {:module, module})
  end

  defp get_definition_location(
         %{kind: :function, module: module, name: name, arity: arity},
         context
       ) do
    get_definition_location(module, context, {:function, name, arity})
  end

  defp get_definition_location(%{kind: :type, module: module, name: name, arity: arity}, context) do
    get_definition_location(module, context, {:type, name, arity})
  end

  defp get_definition_location(_idenfitier, _context), do: nil

  defp get_definition_location(module, context, identifier) do
    if context.ebin_path do
      path = Path.join(context.ebin_path, "#{module}.beam")

      with true <- File.exists?(path),
           {:ok, line} <-
             Intellisense.Docs.locate_definition(String.to_charlist(path), identifier) do
        file = module.module_info(:compile)[:source]
        %{file: to_string(file), line: line}
      else
        _otherwise -> nil
      end
    end
  end

  # Formatting helpers

  defp join_with_divider(strings), do: join_with(strings, "\n\n---\n\n")

  defp join_with_newlines(strings), do: join_with(strings, "\n\n")

  defp join_with_middle_dot(strings), do: join_with(strings, " · ")

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

  defp format_docs_link(module, function_or_type \\ nil) do
    app = Application.get_application(module)
    module_name = module_name(module)

    is_otp? =
      case :code.which(module) do
        :preloaded -> true
        [_ | _] = path -> List.starts_with?(path, :code.lib_dir())
        _ -> false
      end

    cond do
      is_otp? ->
        hash =
          case function_or_type do
            {:function, function, arity} -> "##{function}-#{arity}"
            {:type, type, _arity} -> "#type-#{type}"
            nil -> ""
          end

        url = "https://www.erlang.org/doc/man/#{module_name}.html#{hash}"
        "[View on Erlang Docs](#{url})"

      vsn = app && Application.spec(app, :vsn) ->
        hash =
          case function_or_type do
            {:function, function, arity} -> "##{function}/#{arity}"
            {:type, type, arity} -> "#t:#{type}/#{arity}"
            nil -> ""
          end

        url = "https://hexdocs.pm/#{app}/#{vsn}/#{module_name}.html#{hash}"
        "[View on Hexdocs](#{url})"

      true ->
        nil
    end
  end

  defp format_signatures([], module, name, arity) do
    signature_fallback(module, name, arity)
  end

  defp format_signatures(signatures, module, _name, _arity) do
    signatures_string = Enum.join(signatures, "\n")

    # Don't add module prefix to operator signatures
    if :binary.match(signatures_string, ["(", "/"]) != :nomatch do
      inspect(module) <> "." <> signatures_string
    else
      signatures_string
    end
  end

  defp format_type_signature(nil, module, name, arity) do
    signature_fallback(module, name, arity)
  end

  defp format_type_signature({_type_kind, type}, module, _name, _arity) do
    {:"::", _env, [lhs, _rhs]} = Code.Typespec.type_to_quoted(type)
    inspect(module) <> "." <> Macro.to_string(lhs)
  end

  defp signature_fallback(module, name, arity) do
    args = Enum.map_join(1..arity//1, ", ", fn n -> "arg#{n}" end)
    "#{inspect(module)}.#{name}(#{args})"
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

  defp format_type_spec({type_kind, type}, line_length) when type_kind in [:type, :opaque] do
    ast = {:"::", _env, [lhs, _rhs]} = Code.Typespec.type_to_quoted(type)

    type_string =
      case type_kind do
        :type -> ast
        :opaque -> lhs
      end
      |> Macro.to_string()

    type_spec_code = "@#{type_kind} #{type_string}"

    try do
      Code.format_string!(type_spec_code, line_length: line_length)
    rescue
      _ -> type_spec_code
    end
  end

  defp format_type_spec(_, _line_length), do: nil

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

  defp module_name(module) do
    case Atom.to_string(module) do
      "Elixir." <> name -> name
      name -> name
    end
  end
end
