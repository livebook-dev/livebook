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

  @exact_matcher &Kernel.==/2
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
      # TODO: all this:
      {:macro, macro} ->
        []
      {:pre_directive, directive} ->
        []
      {:atom, atom} ->
        match_atom(Atom.to_string(atom), ctx)
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
      [{:atom, _, member}, {:":", _}, {:atom, _, mod} | _] -> {:mod_member, mod, member}
      [                  {:":", _}, {:atom, _, mod} | _] -> {:mod_member, mod, :""}

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
    Intellisense.Elixir.IdentifierMatcher.match_erlang_module(hint, ctx) ++ Intellisense.Elixir.IdentifierMatcher.match_module_member(:erlang, hint, ctx)
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
      [{{:atom, _, member}, _, to}, {{:":", _}, _, _}, {{:atom, _, mod}, from, _} | _] -> %{context: {:mod_member, mod, member}, begin: from, end: to}
      [{{:atom,  _, atom}, from, to} | _] -> %{context: {:atom, atom}, begin: from, end: to}
      [] -> :none
      _ -> :expr
    end
  end
end
