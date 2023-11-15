defmodule Livebook.Copilot.HuggingfaceBackend do
  require Logger

  def completion(config, pre, suf) do
    api_key = config[:api_key] || System.get_env("HF_TOKEN")
    model = config[:model] || "deepseek-coder-1.3"

    payload = %{
      inputs: build_prompt(model, pre, suf),
      parameters: generation_parameters(model),
      options: %{
        wait_for_model: false
      }
    }

    response =
      Req.post!(endpoint_url(model),
        json: payload,
        max_retries: 500,
        headers: %{
          "Authorization" => "Bearer #{api_key}"
        },
        retry: fn
          _req, %{status: 502} = res ->
            IO.puts("retrying")
            IO.inspect(res)
            {:delay, 1000}

          _, _ ->
            false
        end
      )

    IO.inspect(response)
    completion = response |> Map.get(:body) |> hd |> Map.get("generated_text")
    post_process(model, completion)
  end

  def endpoint_url("code-mistral-7b"),
    do: "https://yc8u7go82crywio5.eu-west-1.aws.endpoints.huggingface.cloud"

  def endpoint_url("deepseek-coder-6.7b"),
    do: "https://jnxgz6yis68mvccl.eu-west-1.aws.endpoints.huggingface.cloud"

  def endpoint_url("deepseek-coder-1.3b"),
    do: "https://dy1ssdpwxx76znaa.eu-west-1.aws.endpoints.huggingface.cloud"

  def endpoint_url("codellama-7b"),
    do: "https://li94ayvlh76wlbkk.eu-west-1.aws.endpoints.huggingface.cloud"

  # all the same for now
  def generation_parameters(_),
    do: %{
      temperature: 0.1,
      return_full_text: false,
      max_new_tokens: 250,
      repetition_penalty: 1.1
    }

  def build_prompt("codellama-7b", pre, suf),
    do: "<PRE>#{pre} <SUF>#{suf} <MID>"

  def build_prompt("deepseek-coder-6.7b", pre, suf),
    do: build_prompt("deepseek-coder-1.3b", pre, suf)

  def build_prompt("deepseek-coder-1.3b", pre, suf),
    do:
      "<｜fim▁begin｜># cell.ex: an elixir livebook cell written in elixir\n#{pre}<｜fim▁hole｜>#{suf}<｜fim▁end｜>"

  def build_prompt(_, pre, _suf), do: pre

  def post_process("codellama-7b", completion), do: completion |> String.trim_trailing("<EOT>")
  def post_process(_, completion), do: completion

  # chatgpt suggested I use an ETS table to store which huggingface endpoints have woken up
  # Not sure that's a good idea. Doesn't matter, though, as this backend isn't for production
  def model_loaded?(%{model: model}) do
    table_name = :huggingface_model_table

    if Enum.member?(:ets.all(), table_name) == false do
      :ets.new(table_name, [:public, :named_table])
    end

    response =
      case :ets.lookup(table_name, model) do
        [] -> false
        _ -> true
      end

    Logger.info("model_loaded? returned #{response}")
    response
  end

  # "loading" the model simply means hitting the inference API until you get a 200 response code
  def load_model!(%{model: model}) do
    Req.post!(endpoint_url(model),
      json: %{
        inputs: "ping",
        parameters: %{
          max_new_tokens: 3
        },
        options: %{
          wait_for_model: true
        }
      },
      max_retries: 500,
      # headers: %{
      #   "Authorization" => "Bearer #{api_key}"
      # },
      retry: fn
        _req, %{status: 502} = res ->
          IO.puts("retrying to load")
          IO.inspect(res)
          {:delay, 1000}

        _, _ ->
          false
      end
    )

    Logger.info("Received non-error response from endpoint - it's alive!")
    :ets.insert(:huggingface_model_table, {model, true})
  end
end
