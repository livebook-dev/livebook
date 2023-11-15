defmodule Livebook.Copilot.LlamaCppHttpBackend do
  @moduledoc """
  A client for interacting with the llama.cpp API described here:
  https://github.com/ggerganov/llama.cpp/blob/df9d1293defe783f42bc83af732d3c670552c541/examples/server/server.cpp

  You can run the llama.cpp server using a command like this

  ./server -m ~/Dev/llm/models/gguf/codellama-7b.Q5_K_M.gguf -c 4096
  """

  @base_url "http://127.0.0.1:8080"

  # :model determines generation options, prompt format and post processing
  def completion(%{model: model}, pre, suf) do
    payload = generation_options(model) |> Map.put(:prompt, build_prompt(model, pre, suf))

    completion =
      Req.post!("#{@base_url}/completion", json: payload)
      |> Map.get(:body)
      |> Map.get("content")
      |> dbg()

    post_process(model, completion)
  end

  def generation_options("codellama-7b"),
    do: %{
      temperature: 0.1,
      repeat_penalty: 1.1,
      n_predict: 20
    }

  def generation_options(_), do: %{}

  def build_prompt("codellama-7b", pre, suf),
    do: "<PRE>#{pre} <SUF>#{suf} <MID>"

  def build_prompt(_, pre, _suf), do: pre

  def post_process("codellama-7b", completion), do: completion |> String.trim_trailing("<EOT>")
  def post_process(_, completion), do: completion

  def model_loaded?(_), do: true
  def load_model!(_), do: true
end
