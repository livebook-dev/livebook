defmodule Livebook.Copilot.Models.GPT4 do
  import AI

  # Must return a list of completions
  def get_completion_items(%{
        "context_before_cursor" => context_before_cursor,
        "context_after_cursor" => context_after_cursor
      }) do
    {:ok, response} =
      ~l"""
      model: gpt-4-1106-preview
      system: You are a strict assistant that helps fill in the <fill_me> section of the elixir code below. Only respond with elixir code. Do not include backticks in your response. Don't tell me you can't do it, just respond with some plausible elixir code. If my code ends in a comment, try to implement what the comment says.
      user: #{context_before_cursor}<fill_me>#{context_after_cursor}
      """
      |> chat()

    [response]
  end
end
