defmodule Livebook.Intellisense.Erlang.IdentifierMatcher do
  alias Livebook.Intellisense

  @type identifier_item ::
          %{
            kind: :variable,
            name: name()
          }
          | %{
              kind: :module,
              module: module(),
              display_name: display_name(),
            }
          | %{
              kind: :function,
              module: module(),
              name: name(),
              arity: arity(),
              display_name: display_name(),
            }
          | %{
              kind: :keyword,
              name: name(),
            }
          | %{
              kind: :bitstring_modifier,
              name: name(),
              arity: integer()
            }
          | %{
              kind: :module_attribute,
              name: name(),
              documentation: Docs.documentation(),
              array_needed: boolean()
            }

  @type name :: atom()
  @type display_name :: String.t()

  @prefix_matcher &String.starts_with?/2

  @reserved_attributes [
    {:module, %{doc: ""}, false},
    {:export, %{doc: ""}, true},
    {:import, %{doc: ""}, true},
    {:moduledoc, %{doc: ""}, false},
    {:compile, %{doc: ""}, true},
    {:vsn, %{doc: ""}, false},
    {:on_load, %{doc: ""}, false},
    {:nifs, %{doc: ""}, true},
    {:behaviour, %{doc: ""}, false},
    {:callback, %{doc: ""}, true},
    {:record, %{doc: ""}, false},
    {:include, %{doc: ""}, false},
    {:define, %{doc: ""}, false},
    {:file, %{doc: ""}, false},
    {:type, %{doc: ""}, false},
    {:spec, %{doc: ""}, false},
    {:doc, %{doc: ""}, false},
    {:feature, %{doc: ""}, false},
  ]

  def completion_identifiers(hint, intellisense_context, node) do
    context = cursor_context(hint)

    ctx = %{
      fragment: hint,
      intellisense_context: intellisense_context,
      matcher: @prefix_matcher,
      type: :completion,
      node: node,
    }

    context_to_matches(context, ctx)
  end

  defp context_to_matches(context, ctx) do
    case context do
      {:mod_func, mod, func} ->
        Intellisense.Elixir.IdentifierMatcher.match_module_function(mod, Atom.to_string(func), ctx)
      # TODO: all this:
      # {:macro, macro} ->
      #   []
      {:pre_directive, directive} ->
        match_module_attribute(directive, ctx)
      {:atom, atom} ->
        []
      {:var, var} ->
        var
        |> Livebook.Runtime.Evaluator.erlang_to_elixir_var
        |> to_string
        |> Intellisense.Elixir.IdentifierMatcher.match_variable(ctx)
        |> Enum.map(&%{&1 | name: Livebook.Runtime.Evaluator.elixir_to_erlang_var(&1[:name])})
      # TODO: bitstrings, need to be parsed!
      :expr ->
        []

      # :none
      _ ->
        []
    end
  end

  defp cursor_context(hint) do
    case :erl_scan.string(String.to_charlist(hint)) do
      {:error, _, _} ->
        :none
      {:ok, tokens, _} ->
        match_tokens_to_context(Enum.reverse(tokens))
    end
  end

  defp match_tokens_to_context(tokens) do
    case tokens do
      [{:atom, _, func}, {:":", _}, {:atom, _, mod} | _] -> {:mod_func, mod, func}
      [                  {:":", _}, {:atom, _, mod} | _] -> {:mod_func, mod, :""}

      [{:atom, _, macro}, {:"?", _} | _] -> {:macro, macro}
      [{:var,  _, macro}, {:"?", _} | _] -> {:macro, macro}

      [{:atom, _, directive}, {:"-", _} | _] -> {:pre_directive, directive}

      [{:atom, _, atom} | _] -> {:atom, atom}

      [{:var, _, var} | _] -> {:var, var}

      [] -> :none
      _  -> :expr
    end
  end

  defp match_module_attribute(directive, ctx) do
    for {attribute, info, array_needed}  <- @reserved_attributes,
        ctx.matcher.(Atom.to_string(attribute), Atom.to_string(directive)),
        do: %{
          kind: :module_attribute,
          name: attribute,
          documentation: {"text/markdown", info.doc},
          array_needed: array_needed,
        }
  end
end
