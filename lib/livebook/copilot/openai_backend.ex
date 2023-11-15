defmodule Livebook.Copilot.OpenaiBackend do
  alias OpenaiEx.ChatCompletion
  alias OpenaiEx.ChatMessage

  @max_tokens 100
  @temperature 0.1

  def completion(config, pre, suf) do
    api_key = config[:api_key] || System.get_env("OPENAI_API_KEY")
    model = config[:model] || "gpt-4"

    OpenaiEx.new(api_key)
    |> ChatCompletion.create(%{
      model: model,
      messages: build_prompt_messages(pre, suf),
      max_tokens: @max_tokens,
      temperature: @temperature
    })
    |> Map.get("choices")
    |> Enum.at(0)
    |> Map.get("message")
    |> Map.get("content")
  end

  def build_prompt_messages(pre, suf) do
    [
      ChatMessage.system("""
        You are a strict assistant that helps fill in the <fill_me> section of the elixir code below.
        Only respond with elixir code. Do not include backticks in your response.
        Don't tell me you can't do it, just respond with some plausible elixir code.
        If my code ends in a comment, try to implement what the comment says.
      """),
      ChatMessage.user("#{[pre]}<fill_me>#{suf}")
    ]
  end

  def model_loaded?(_), do: true
  def load_model!(_), do: true
end
