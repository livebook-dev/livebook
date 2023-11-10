defmodule Livebook.Copilot.LlamaCppHttpApi do
  @temperature 0.1
  @repeat_penalty 1.1
  @n_predict 20

  def get_completion_items(%{
        "context_before_cursor" => context_before_cursor,
        "context_after_cursor" => context_after_cursor
      }) do
    Livebook.Copilot.LlamaCppHttpApi.Client.infill(
      build_context_before_cursor(context_before_cursor),
      context_after_cursor,
      %{
        temperature: @temperature,
        repeat_penalty: @repeat_penalty,
        n_predict: @n_predict
      }
    )
    |> Map.get(:body)
    |> Map.get("content")
    |> String.trim_trailing("<EOT>")
    |> List.wrap()
    |> dbg()
  end

  defp build_context_before_cursor(context_before_cursor) do
    # The below doesn't seem to work well for codellama 7B
    # """
    # # cell.ex - A livebook code cell in the elixir programming language
    # """ <>
    context_before_cursor
  end
end
