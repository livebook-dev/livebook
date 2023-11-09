defmodule Livebook.Copilot do
  # Arguments are passed through straight from the phoneix JS client event
  # for now for rapid prototyping.
  def handle_request(request) do
    result_ref = make_ref()
    pid = self()

    Task.start(fn ->
      response =
        Application.get_env(:livebook, Livebook.Copilot)[:model]
        |> apply(:get_completion_items, [request])
        |> Enum.map(
          &%{
            insertText: &1
          }
        )
        |> (&%{items: &1}).()

      send(pid, {:copilot_response, result_ref, request, response})
    end)

    result_ref
  end
end
