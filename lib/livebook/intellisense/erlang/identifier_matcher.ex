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

  @type name :: atom()
  @type display_name :: String.t()

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
      {:macro, macro} ->
        []
      {:pre_directive, directive} ->
        []
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
      [{:atom, _, func}, {:":", _}, {:atom, _, mod} | _] -> {:mod_func, mod, func}
      [                  {:":", _}, {:atom, _, mod} | _] -> {:mod_func, mod, :""}

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

  defp match_atom(hint, ctx) do
    Intellisense.Elixir.IdentifierMatcher.match_erlang_module(hint, ctx)
    |> Enum.map(&%{&1 | display_name: String.slice(&1[:display_name], 1..-1//1)})
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
