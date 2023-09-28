defmodule Livebook.AiHelper.Openai do
  @model "gpt-4"
  @temperature 0

  def stream_completion(
        prompt,
        code_before_selection,
        selected_code,
        code_after_selection
      ) do
    openai = OpenaiEx.new(Application.fetch_env!(:livebook, :openai_api_key))

    messages = [
      OpenaiEx.ChatMessage.system(
        Livebook.AiHelper.generate_system_prompt(
          code_before_selection,
          selected_code,
          code_after_selection
        )
      ),
      OpenaiEx.ChatMessage.user(prompt)
    ]

    openai
    |> OpenaiEx.ChatCompletion.create(
      %{
        model: @model,
        temperature: @temperature,
        messages: messages
      },
      stream: true
    )
    |> filter_response_tokens()
  end

  def filter_response_tokens(raw_stream) do
    raw_stream
    |> Stream.flat_map(& &1)
    |> Stream.map(fn %{data: d} -> d |> Map.get("choices") |> Enum.at(0) |> Map.get("delta") end)
    |> Stream.filter(fn map -> map |> Map.has_key?("content") end)
    |> Stream.map(fn map -> map |> Map.get("content") end)
  end
end
