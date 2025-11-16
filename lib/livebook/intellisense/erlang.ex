defmodule Livebook.Intellisense.Erlang do
  alias Livebook.Intellisense

  @behaviour Intellisense

  @impl true
  def handle_request({:format, _code}, _context, _node) do
    # Not supported.
    nil
  end

  def handle_request({:completion, hint}, context, _node) do
    handle_completion(hint, context)
  end

  def handle_request({:details, line, column}, context, _node) do
    handle_details(line, column, context)
  end

  def handle_request({:signature, hint}, context, _node) do
    handle_signature(hint, context)
  end

  defp handle_completion(_hint, _context) do
    # TODO: implement. See t:Livebook.Runtime.completion_response/0 for return type.
    nil
  end

  defp handle_details(_line, _column, _context) do
    # TODO: implement. See t:Livebook.Runtime.details_response/0 for return type.
    nil
  end

  defp handle_signature(_hint, _context) do
    # TODO: implement. See t:Livebook.Runtime.signature_response/0 for return type.
    nil
  end
end
