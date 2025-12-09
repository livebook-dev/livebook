defmodule Livebook.Intellisense.Erlang do

  alias Livebook.Intellisense

  @behaviour Intellisense

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

  def handle_request({:signature, hint}, context, _node) do
    handle_signature(hint, context)
  end

  defp handle_completion(hint, context, node) do
    # TODO: implement. See t:Livebook.Runtime.completion_response/0 for return type.
    IO.write("completion:")

    Intellisense.Erlang.IdentifierMatcher.completion_identifiers(hint, context, node)
    |> Intellisense.Elixir.format_completion_identifiers()
  end

  defp handle_details(line, column, context, node) do
    %{matches: matches, range: range} =
      Intellisense.Erlang.IdentifierMatcher.locate_identifier(line, column, context, node)

    case Enum.filter(matches, &Intellisense.Elixir.include_in_details?/1) do
      [] ->
        nil

      matches ->
        matches = Enum.sort_by(matches, & &1[:arity], :asc)
        contents = Enum.map(matches, &Intellisense.Elixir.format_details_item/1)

        definition = Intellisense.Elixir.get_definition_location(hd(matches), context)
        %{range: range, contents: contents, definition: definition}
    end
  end

  defp handle_signature(hint, _context) do
    # TODO: implement. See t:Livebook.Runtime.signature_response/0 for return type.
    IO.write("signature:")
    IO.inspect(hint)
    nil
  end
end
