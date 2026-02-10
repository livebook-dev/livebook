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

  @exact_matcher &Kernel.==/2
  @prefix_matcher &String.starts_with?/2

  @bitstring_modifiers [
    :big,
    :binary,
    :bits,
    :bitstring,
    :bytes,
    :integer,
    :float,
    :little,
    :native,
    :signed,
    :unit,
    :unsigned,
    :utf8,
    :utf16,
    :utf32,
  ]

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

  @doc """
  Extracts information about an identifier found in `column` in
  `line`.

  The function returns range of columns where the identifier
  is located and a list of matching identifier items.
  """
  @spec locate_identifier(String.t(), pos_integer(), Intellisense.context(), node()) ::
          %{
            matches: list(identifier_item()),
            range: nil | %{from: pos_integer(), to: pos_integer()}
          }
  def locate_identifier(line, column, intellisense_context, node) do
    case surround_context(line, column) do
      %{context: context, begin: from, end: to} ->
        fragment = String.slice(line, 0, to - 1)

        ctx = %{
          fragment: fragment,
          intellisense_context: intellisense_context,
          matcher: @exact_matcher,
          type: :locate,
          node: node
        }

        matches = context_to_matches(context, ctx)
        %{matches: matches, range: %{from: from, to: to}}

      :none ->
        %{matches: [], range: nil}
    end
  end

  defp context_to_matches(context, ctx) do
    case context do
      {:mod_member, mod, member} ->
        Intellisense.Elixir.IdentifierMatcher.match_module_member(mod, Atom.to_string(member), ctx)
      {:pre_directive, directive} ->
        match_module_attribute(directive, ctx)
      {:atom, atom} ->
        match_atom(Atom.to_string(atom), ctx)
      {:var, var} ->
        match_var(var, ctx)
      {:bitstring_modifier, hint, existing} ->
        for modifier <- @bitstring_modifiers,
            @prefix_matcher.(Atom.to_string(modifier), Atom.to_string(hint)),
            modifier not in existing,
            do: %{kind: :bitstring_modifier, name: modifier, arity: 0}

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
      [{:atom, _, member}, {:":", _}, {:atom, _, mod} | _] -> {:mod_member, mod, member}
      [                  {:":", _}, {:atom, _, mod} | _] -> {:mod_member, mod, :""}

      [{:atom, _, _field}, {:".", _}, {:atom, _, _record}, {:"#", _} | _] -> :none
      [{:".", _}, {:atom, _, _record}, {:"#", _} | _] -> :none
      [{:atom, _, _record}, {:"#", _} | _] -> :none
      [{:"#", _} | _] -> :none

      [{:atom, _, macro}, {:"?", _} | _] -> {:macro, macro}
      [{:var,  _, macro}, {:"?", _} | _] -> {:macro, macro}

      [{:atom, _, directive}, {:"-", _}, {:".", _}] -> {:pre_directive, directive}
      [{:atom, _, directive}, {:"-", _}           ] -> {:pre_directive, directive}

      [{:atom, _, mod}, {:"-", _} | _] -> match_maybe_bitstring_mod(mod, tokens)
      [{:atom, _, mod}, {:"/", _} | _] -> match_maybe_bitstring_mod(mod, tokens)

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

  defp match_atom(hint, ctx) do
    (Intellisense.Elixir.IdentifierMatcher.match_erlang_module(hint, ctx) ++
       Intellisense.Elixir.IdentifierMatcher.match_module_member(:erlang, hint, ctx))
    |> Enum.map(fn
      %{display_name: name} = item when is_binary(name) ->
        %{item | display_name: String.trim_leading(name, ":")}
      item ->
        item
    end)
  end

  defp surround_context(line, column) do
    case :erl_scan.string(String.to_charlist(line)) do
      {:error, _, _} ->
        :none
      {:ok, tokens, _} ->
        before_cursor = split_tokens_at_column(tokens, column)
        match_tokens_to_context_with_columns(before_cursor)
    end
  end

  defp split_tokens_at_column(tokens, column) do
    {_, taken} =
      Enum.reduce_while(tokens, {0, []}, fn token, {pos, acc} ->
        text = token_to_text(token)
        start_col = pos + 1

        if start_col <= column do
          new_pos = pos + String.length(text)
          {:cont, {new_pos, [{token, start_col, new_pos + 1}| acc]}}
        else
          {:halt, {pos, acc}}
        end
      end)
      taken
  end

  defp token_to_text({_, _, val}), do: Atom.to_string(val)
  defp token_to_text({val, _}), do: Atom.to_string(val)

  defp match_tokens_to_context_with_columns(tokens) do
    case tokens do
      [{{:atom, _, member}, _, to}, {{:":", _}, _, _}, {{:atom, _, mod}, from, _} | _] ->
        %{context: {:mod_member, mod, member}, begin: from, end: to}

      [{{:atom,  _, atom}, from, to} | _] ->
        %{context: {:atom, atom}, begin: from, end: to}

      _ -> :none
    end
  end

  defp match_var(hint, ctx) do
    hint
    |> Livebook.Runtime.Evaluator.erlang_to_elixir_var
    |> to_string
    |> Intellisense.Elixir.IdentifierMatcher.match_variable(ctx)
    |> Enum.map(&%{&1 | name: Livebook.Runtime.Evaluator.elixir_to_erlang_var(&1[:name])})
  end

  defp match_maybe_bitstring_mod(hint, tokens) do
    if in_bitstring?(tokens) do
      existing = bitstring_mods(tokens) |> Enum.drop(1)

      case List.last(existing) do
        {:err} -> {:atom, hint}
        _ -> {:bitstring_modifier, hint, existing}
      end
    else
      {:atom, hint}
    end
  end

  defp bitstring_mods(tokens) do
    case tokens do
      # the unit modifier takes an argument, we skip it here
      [{:integer, _, _}, {:":", _}, {:atom, _, :unit} = head | tail] ->
        bitstring_mods([head | tail])

      [{:atom, _, mod}, {:"-", _} | tail] -> [mod | bitstring_mods(tail)]
      [{:atom, _, mod}, {:"/", _} | _] -> [mod]

      _ -> [{:err}]
    end
  end

  defp in_bitstring?(tokens, depth \\ 0) do
    case tokens do
      [] -> false
      [{:"<<", _} | _] when depth == 0 -> true
      [{:"<<", _} | tail] -> in_bitstring?(tail, depth - 1)
      [{:">>", _} | tail] -> in_bitstring?(tail, depth + 1)
      [_ | tail] -> in_bitstring?(tail, depth)
    end
  end
end
