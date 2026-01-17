defmodule Livebook.Intellisense.Erlang do

  alias Livebook.Intellisense
  alias Livebook.Intellisense.Elixir

  @behaviour Intellisense

  @impl true
  def handle_request({:format, _code}, _context, _node) do
    # Not supported.
    nil
  end

  def handle_request({:completion, hint}, context, node) do
    handle_completion(hint, context, node)
  end

  def handle_request({:details, line, column}, context, _node) do
    handle_details(line, column, context)
  end

  def handle_request({:signature, hint}, context, node) do
    handle_signature(hint, context, node)
  end

  defp handle_completion(hint, context, node) do
    # TODO: implement. See t:Livebook.Runtime.completion_response/0 for return type.
    IO.write("completion:")

    Intellisense.Erlang.IdentifierMatcher.completion_identifiers(hint, context, node)
    |> Intellisense.Elixir.format_completion_identifiers()
  end

  defp handle_details(line, _column, _context) do
    # TODO: implement. See t:Livebook.Runtime.details_response/0 for return type.
    IO.write("details:")
    IO.inspect(line)
    nil
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
end
