defmodule Livebook.Intellisense.Elixir do
  alias Livebook.Intellisense

  @behaviour Intellisense

  # Configures width used for inspect and specs formatting.
  @line_length 45
  @extended_line_length 80

  @impl true
  def handle_request({:format, code}, _context, _node) do
    handle_format(code)
  end

  def handle_request({:completion, hint}, context, node) do
    handle_completion(hint, context, node)
  end

  def handle_request({:details, line, column}, context, node) do
    handle_details(line, column, context, node)
  end

  def handle_request({:signature, hint}, context, node) do
    handle_signature(hint, context, node)
  end

  defp handle_format(code) do
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

  defp handle_completion(hint, context, node) do
    Intellisense.Elixir.IdentifierMatcher.completion_identifiers(hint, context, node)
    |> format_completion_identifiers(extra_completion_items(hint))
  end

  def format_completion_identifiers(completions, extra \\ []) do
    items =
      completions
      |> Enum.filter(&include_in_completion?/1)
      |> Enum.map(&format_completion_item/1)
      |> Enum.concat(extra)
      |> Enum.sort_by(&completion_item_priority/1)

    %{items: items}
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
    subtype = Intellisense.Elixir.Docs.get_module_subtype(module)

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
          Intellisense.Elixir.Docs.format_documentation(documentation, :short),
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
             Intellisense.Elixir.Docs.format_documentation(documentation, :short),
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
             Intellisense.Elixir.Docs.format_documentation(documentation, :short),
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
             Intellisense.Elixir.Docs.format_documentation(documentation, :short),
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

  defp handle_details(line, column, context, node) do
    %{matches: matches, range: range} =
      Intellisense.Elixir.IdentifierMatcher.locate_identifier(line, column, context, node)

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
      Intellisense.Elixir.Docs.format_documentation(documentation, :all)
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
      Intellisense.Elixir.Docs.format_documentation(documentation, :all)
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
      Intellisense.Elixir.Docs.format_documentation(documentation, :all)
    ])
  end

  defp format_details_item(%{kind: :module_attribute, name: name, documentation: documentation}) do
    join_with_divider([
      code("@#{name}"),
      Intellisense.Elixir.Docs.format_documentation(documentation, :all)
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
             Intellisense.Elixir.Docs.locate_definition(String.to_charlist(path), identifier) do
        file = module.module_info(:compile)[:source]
        %{file: to_string(file), line: line}
      else
        _otherwise -> nil
      end
    end
  end

  defp handle_signature(hint, context, node) do
    case Intellisense.Elixir.SignatureMatcher.get_matching_signatures(hint, context, node) do
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

  defp module_name(module) do
    case Atom.to_string(module) do
      "Elixir." <> name -> name
      name -> name
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
end
