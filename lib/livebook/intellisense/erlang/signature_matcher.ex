defmodule Livebook.Intellisense.Erlang.SignatureMatcher do
  @type signature_info :: {name :: atom(), Docs.signature(), Docs.documentation(), Docs.spec()}


  @spec get_matching_signatures(String.t(), Livebook.Intellisense.context(), node()) ::
          {:ok, list(signature_info()), active_argument :: non_neg_integer()} | :error
  def get_matching_signatures(hint, _intellisense_context, node) do
    case call_target_and_argument(hint) do
      {:ok, {:remote, mod, name}, active_argument} ->
        funs = [{name, :any}]
        signature_infos = Livebook.Intellisense.Elixir.SignatureMatcher.signature_infos_for_members(mod, funs, active_argument, node)
        {:ok, signature_infos, active_argument}
      {:ok, {:local, name}, active_argument} ->
        signature_infos = Livebook.Intellisense.Elixir.SignatureMatcher.signature_infos_for_members(:erlang,  [{name, :any}], active_argument, node)
        {:ok, signature_infos, active_argument}
      _ ->
        :error
    end
  end

  defp call_target_and_argument(hint) do
    with {:ok, ast} <- parse_last_call(hint) do
      [call_head | _] = ast
      case call_head do
        {:call, _,  {:remote, _, {:atom, _, mod}, {:atom, _, name}}, args} ->
          {:ok, {:remote, mod, name}, length(args) - 1}
        {:call, _, {:atom, _, name}, args} ->
          {:ok, {:local, name}, length(args) - 1}
        _ -> :error
      end
    else
      _ -> :error
    end
  end

  defp parse_last_call(hint) do
    case :erl_scan.string(String.to_charlist(hint)) do
      {:ok, tokens, _} ->
        tokens
        |> Enum.reverse
        |> filter_last_call
        |> Enum.concat([{:atom, 1, :__context__}, {:")", 1}, {:dot, 1}])
        |> :erl_parse.parse_exprs
      error ->
        error
    end
  end

  defp filter_last_call(tokens) do
    filter_last_call(tokens, :left_bracket)
  end
  defp filter_last_call([{:"(", 1} | tokens], :left_bracket) do
    filter_last_call(tokens, :function_name) ++ [{:"(", 1}]
  end
  defp filter_last_call([{:")", 1} | tokens], :left_bracket) do
    filter_last_call(tokens, :right_bracket) ++ [{:")", 1}]
  end
  defp filter_last_call([tok | tokens], :left_bracket) do
    filter_last_call(tokens, :left_bracket) ++ [tok]
  end
  defp filter_last_call([{:"(", 1} | tokens], :right_bracket) do
    filter_last_call(tokens, :left_bracket) ++ [{:"(", 1}]
  end
  defp filter_last_call([tok | tokens], :right_bracket) do
    filter_last_call(tokens, :right_bracket) ++ [tok]
  end
  defp filter_last_call([{:atom, 1, fun}, {:":", 1}, {:atom, 1, mod} | _], :function_name) do
    [{:atom, 1, mod}, {:":", 1}, {:atom, 1, fun}]
  end
  defp filter_last_call([{:atom, 1, fun} | _], :function_name) do
    [{:atom, 1, fun}]
  end
  defp filter_last_call(_, _) do
    []
  end

end
