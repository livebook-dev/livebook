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

  defp match_tokens_to_context([{:atom, _, func}, {:":", _}, {:atom, _, mod} | _]),
    do: {:mod_func, mod, func}
  defp match_tokens_to_context([{:":", _}, {:atom, _, mod} | _]),
    do: {:mod_func, mod, :""}
  defp match_tokens_to_context([{:atom, _, macro}, {:"?", _} | _]),
    do: {:macro, macro}
  defp match_tokens_to_context([{:var, _, macro}, {:"?", _} | _]),
    do: {:macro, macro}
  defp match_tokens_to_context([{:atom, _, directive}, {:"-", _} | _]),
    do: {:pre_directive, directive}
  defp match_tokens_to_context([{:atom, _, atom} | _]),
    do: {:atom, atom}
  defp match_tokens_to_context([{:var, _, var} | _]),
    do: {:var, var}
  defp match_tokens_to_context([]),
    do: :none
  defp match_tokens_to_context(_),
    do: :expr
end
