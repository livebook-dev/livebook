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
end
