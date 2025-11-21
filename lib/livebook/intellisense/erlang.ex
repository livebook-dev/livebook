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

  def handle_request({:details, line, column}, context, _node) do
    handle_details(line, column, context)
  end

  def handle_request({:signature, hint}, context, _node) do
    handle_signature(hint, context)
  end

  defp handle_completion(hint, context, node) do
    # TODO: implement. See t:Livebook.Runtime.completion_response/0 for return type.
    IO.write("completion:")

    items = Intellisense.Erlang.IdentifierMatcher.completion_identifiers(hint, context, node)
    |> Enum.filter(&Intellisense.Elixir.include_in_completion?/1)
    |> Enum.map(&Intellisense.Elixir.format_completion_item/1)
    |> Enum.concat(Intellisense.Elixir.extra_completion_items(hint))
    |> Enum.sort_by(&Intellisense.Elixir.completion_item_priority/1)

    IO.inspect(items)

    %{items: items}
  end

  defp handle_details(line, _column, _context) do
    # TODO: implement. See t:Livebook.Runtime.details_response/0 for return type.
    IO.write("details:")
    IO.inspect(line)
    nil
  end

  defp handle_signature(hint, _context) do
    # TODO: implement. See t:Livebook.Runtime.signature_response/0 for return type.
    IO.write("signature:")
    IO.inspect(hint)
    nil
  end
end
