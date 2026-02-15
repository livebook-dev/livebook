defmodule Livebook.Intellisense.Erlang do

  alias Livebook.Intellisense

  @behaviour Intellisense

  @line_length 45

  @impl true
  def handle_request({:format, _code}, _context, _node) do
    # Not supported.
    nil
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

  defp handle_completion(hint, context, node) do
    Intellisense.Erlang.IdentifierMatcher.completion_identifiers(hint, context, node)
    |> format_completion_identifiers(extra_completion_items(hint))
  end

  def format_completion_identifiers(completions, extra \\ []) do
    items =
      completions
      |> Enum.filter(&Intellisense.Elixir.include_in_completion?/1)
      |> Enum.map(&format_completion_item/1)
      |> Enum.concat(extra)
      |> Enum.sort_by(&Intellisense.Elixir.completion_item_priority/1)

    %{items: items}
  end

  defp format_completion_item(%{kind: :variable, name: name}),
       do: %{
         label: Atom.to_string(name),
         kind: :variable,
         documentation: "(variable)",
         insert_text: Atom.to_string(name)
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
        Intellisense.Elixir.join_with_newlines([
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
           Intellisense.Elixir.join_with_newlines([
             Intellisense.Elixir.Docs.format_documentation(documentation, :short),
             Intellisense.Elixir.code(format_signatures(signatures, module, name, arity))
           ]),
         insert_text:
           cond do
             type == :macro and Intellisense.Elixir.keyword_macro?(name) ->
               "#{display_name} "

             type == :macro and Intellisense.Elixir.env_macro?(name) ->
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
           Intellisense.Elixir.join_with_newlines([
             Intellisense.Elixir.Docs.format_documentation(documentation, :short),
             format_type_spec(type_spec) |> Intellisense.Elixir.code()
           ]),
         insert_text:
           cond do
             arity == 0 -> "#{Atom.to_string(name)}()"
             #
             true -> "#{Atom.to_string(name)}(${})"
           end
       }


  # Note: array_needed is a boolean to know if '[]' should be put inside atrribute,
  # as in -export([]). It is also a way to differentiate erlang's atributes from elixir's.
  defp format_completion_item(%{
    kind: :module_attribute,
    name: name,
    documentation: documentation,
    array_needed: array_needed
  }),
       do: %{
         label: Atom.to_string(name),
         kind: :variable,
         documentation:
           Intellisense.Elixir.join_with_newlines([
             Intellisense.Elixir.Docs.format_documentation(documentation, :short),
             "(module attribute)"
           ]),
         # A snippet with cursor in parentheses
         insert_text:
           if array_needed do
             "#{name}([${}])."
           else
             "#{name}(${})."
           end
       }

  defp format_completion_item(%{
    kind: :module_attribute,
    name: name,
    documentation: documentation,
  }),
       do: %{
         label: Atom.to_string(name),
         kind: :variable,
         documentation:
           Intellisense.Elixir.join_with_newlines([
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

  defp handle_details(line, column, context, node) do
    %{matches: matches, range: range} =
      Intellisense.Erlang.IdentifierMatcher.locate_identifier(line, column, context, node)

    case Enum.filter(matches, &Intellisense.Elixir.include_in_details?/1) do
      [] ->
        nil

      matches ->
        matches = Enum.sort_by(matches, & &1[:arity], :asc)
        contents = Enum.map(matches, &format_details_item/1)

        definition = Intellisense.Elixir.get_definition_location(hd(matches), context)
        %{range: range, contents: contents, definition: definition}
    end
  end

  def format_details_item(%{kind: :module, module: module, documentation: documentation}) do
    Intellisense.Elixir.join_with_divider([
      Intellisense.Elixir.code(Atom.to_string(module)),
      Intellisense.Elixir.format_docs_link(module),
      Intellisense.Elixir.Docs.format_documentation(documentation, :all)
    ])
  end

  def format_details_item(%{
    kind: :function,
    module: module,
    name: name,
    arity: arity,
    documentation: documentation,
    signatures: signatures,
    specs: specs,
    meta: meta
  }) do
    Intellisense.Elixir.join_with_divider([
      format_signatures(signatures, module, name, arity) |> Intellisense.Elixir.code(),
      Intellisense.Elixir.join_with_middle_dot([
        Intellisense.Elixir.format_docs_link(module, {:function, name, arity}),
        Intellisense.Elixir.format_meta(:since, meta)
      ]),
      Intellisense.Elixir.format_meta(:deprecated, meta),
      format_specs(specs, name, arity) |> Intellisense.Elixir.code(),
      Intellisense.Elixir.Docs.format_documentation(documentation, :all)
    ])
  end

  def format_details_item(%{
    kind: :type,
    module: module,
    name: name,
    arity: arity,
    documentation: documentation,
    type_spec: type_spec
  }) do
    Intellisense.Elixir.join_with_divider([
      format_type_signature(type_spec, module, name, arity) |> Intellisense.Elixir.code(),
      Intellisense.Elixir.format_docs_link(module, {:type, name, arity}),
      format_type_spec(type_spec) |> Intellisense.Elixir.code(),
      Intellisense.Elixir.Docs.format_documentation(documentation, :all)
    ])
  end

  defp handle_signature(hint, context, node) do
    case Intellisense.Erlang.SignatureMatcher.get_matching_signatures(hint, context, node) do
      {:ok, [], _active_argument} ->
        nil
      {:ok, signature_infos, active_argument} ->
        %{
          active_argument: active_argument,
          items:
            signature_infos
            |> Enum.map(&Intellisense.Elixir.format_signature_item/1)
            |> Enum.uniq()
        }
      :error ->
        nil
    end
  end

  defp format_signatures([], module, name, arity) do
    signature_fallback(module, name, arity)
  end

  defp format_signatures(signatures, module, _name, _arity) do
    signatures_string = Enum.join(signatures, "\n")
    module_string = format_signature_module(module)

    module_string <> signatures_string
  end

  defp format_type_signature(nil, module, name, arity) do
    signature_fallback(module, name, arity)
  end

  defp format_type_signature({_type_kind, type}, module, _name, _arity) do
      {:"::", _env, [lhs, _rhs]} = Code.Typespec.type_to_quoted(type)

      {name, meta, args} = lhs

      capitalized_args = Enum.map(args, fn
        {var_name, var_meta, context} when is_atom(var_name) ->
          string_name = Atom.to_string(var_name)
          {first, rest} = String.split_at(string_name, 1)

          new_name =
            (String.upcase(first) <> rest)
            |> String.to_atom()

          {new_name, var_meta, context}

        other -> other
      end)

      new_lhs = {name, meta, capitalized_args}

      module_string = format_signature_module(module)

      module_string <> Macro.to_string(new_lhs)
  end

  defp signature_fallback(module, name, arity) do
    args = Enum.map_join(1..arity//1, ", ", fn n -> "Arg#{n}" end)
    "#{module}:#{name}(#{args})"
  end

  defp format_signature_module(module) do
    if module == :erlang do
      ""
    else
      "#{module}:"
    end
  end

  defp format_specs([], _name, _arity), do: nil

  defp format_specs(specs, name, arity) do
    erl_attribute = {:attribute, 1, :spec, {{name, arity}, specs}}
    format_spec(erl_attribute)

  rescue
    _ -> nil
  end

  defp format_type_spec({type_kind, type}) do
    erl_attribute = {:attribute, 1, type_kind, type}
    format_spec(erl_attribute)

  rescue
    _ -> nil
  end

  defp format_type_spec(_), do: nil

  defp format_spec(ast) do
    {:attribute, _, type, _} = ast

    offset = byte_size(Atom.to_string(type)) + 2

    options = [linewidth: 98 + offset]

    :erl_pp.attribute(ast)
    |> IO.chardata_to_string()
    |> String.trim()
  end

  @keywords [
      {"true", "(boolean)"},
      {"false", "(boolean)"},

      {"begin", "(block operator)"},
      {"case", "(case operator)"},
      {"fun", "(anonymous function operator)"},
      {"if", "(if operator)"},
      {"when", "(guard operator)"},

      {"after", "(after operator)"},
      {"catch", "(catch operator)"},
      {"receive", "(receive operator)"},
      {"try", "(try operator)"},

      {"and", "(logical AND operator)"},
      {"andalso", "(short-circuit logical AND operator)"},
      {"band", "(bitwise AND operator)"},

      {"not", "(logical NOT operator)"},
      {"bnot", "(bitwise NOT operator)"},

      {"or", "(logical OR operator)"},
      {"orelse", "(short-circuit logical OR operator)"},
      {"bor", "(bitwise OR operator)"},

      {"div", "(integer division operator)"},
      {"rem", "(integer remainder operator)"},
      {"bxor", "(bitwise XOR operator)"},
      {"bsl", "(bitshift left operator)"},
      {"bsr", "(bitshift right operator)"},
      {"xor", "(logical XOR operator)"},
    ]

  defp extra_completion_items(hint) do
    items = Enum.map(@keywords,
      fn {keyword, desc} -> %{
        label: keyword,
        kind: :keyword,
        documentation: desc,
        insert_text: keyword,
      } end
    )

    last_word = hint |> String.split(~r/\s/) |> List.last()

    if last_word == "" do
      []
    else
      Enum.filter(items, &String.starts_with?(&1.label, last_word))
    end
  end
end
