defmodule Livebook.Copilot.BumblebeeBackend do
  require Logger

  # :model determines generation options, prompt format and post processing
  def completion(%{model: model} = config, pre, suf) do
    if !model_loaded?(config) do
      load_model!(config)
    end

    prompt = build_prompt(model, pre, suf)

    if String.trim(pre) == "" do
      ""
    else
      completion =
        Nx.Serving.batched_run(serving_name(model), prompt)
        |> Map.get(:results)
        |> hd
        |> Map.get(:text)

      post_process(model, completion)
    end
  end

  def build_prompt("deepseek-coder" <> _, pre, suf),
    do:
      "<｜fim▁begin｜># cell.ex: an elixir livebook cell written in elixir\n#{pre}<｜fim▁hole｜>#{suf}<｜fim▁end｜>"

  def build_prompt("codellama" <> _, pre, suf),
    do: "<PRE>#{pre} <SUF>#{suf} <MID>"

  def build_prompt(_, pre, _suf), do: pre

  # TODO for some reason this model returns the whole string, not just the completed parts
  def post_process("deepseek-coder" <> _, completion),
    do:
      String.split(completion, "<｜fim▁end｜>")
      |> Enum.at(1)

  def post_process("codellama" <> _, completion), do: completion |> String.trim_trailing("<EOT>")
  def post_process(_, completion), do: completion

  def nx_serving_spec(%{model: "deepseek-coder-1.3b"} = config) do
    repo = {:hf, "deepseek-ai/deepseek-coder-1.3b-base"}

    {:ok, model_info} =
      Bumblebee.load_model(repo,
        backend: {EXLA.Backend, client: config[:client] || :host}
      )

    {:ok, tokenizer} =
      Bumblebee.load_tokenizer(
        {:hf, "deepseek-ai/deepseek-coder-1.3b-base",
         revision: "e94f2b11bc28abbd67ecadfaad058c30b24a589f"}
      )

    {:ok, generation_config} = Bumblebee.load_generation_config(repo)

    generation_config =
      Bumblebee.configure(generation_config, max_new_tokens: 200, no_repeat_ngram_length: 7)

    serving =
      Bumblebee.Text.generation(model_info, tokenizer, generation_config,
        compile: [batch_size: 1, sequence_length: 512],
        stream: false,
        defn_options: [compiler: EXLA, lazy_transfers: :never],
        preallocate_params: true
      )

    {Nx.Serving, name: serving_name(config[:model]), serving: serving}
  end

  def nx_serving_spec(%{model: "codellama-7b"} = config) do
    repo = {:hf, "codellama/CodeLlama-7b-hf"}

    {:ok, model_info} =
      Bumblebee.load_model(repo,
        backend: {EXLA.Backend, client: config[:client] || :host}
      )

    {:ok, tokenizer} = Bumblebee.load_tokenizer(repo)
    {:ok, generation_config} = Bumblebee.load_generation_config(repo)

    generation_config =
      Bumblebee.configure(generation_config, max_new_tokens: 100, no_repeat_ngram_length: 7)

    serving =
      Bumblebee.Text.generation(model_info, tokenizer, generation_config,
        compile: [batch_size: 1, sequence_length: 512],
        stream: false,
        defn_options: [compiler: EXLA, lazy_transfers: :never]
      )

    {Nx.Serving, name: serving_name(config[:model]), serving: serving}
  end

  def nx_serving_spec(%{model: "gpt2"} = config) do
    repo = {:hf, "gpt2"}

    {:ok, model_info} =
      Bumblebee.load_model(repo,
        backend: {EXLA.Backend, client: config[:client] || :host}
      )

    {:ok, tokenizer} = Bumblebee.load_tokenizer(repo)
    {:ok, generation_config} = Bumblebee.load_generation_config(repo)

    generation_config =
      Bumblebee.configure(generation_config, max_new_tokens: 30, no_repeat_ngram_length: 5)

    serving =
      Bumblebee.Text.generation(model_info, tokenizer, generation_config,
        compile: [batch_size: 1, sequence_length: 512],
        stream: false,
        defn_options: [compiler: EXLA, lazy_transfers: :never]
      )

    {Nx.Serving, name: serving_name(config[:model]), serving: serving}
  end

  def load_model!(%{model: model} = config) do
    if !model_loaded?(config) do
      Logger.info("Starting copilot bumblebee serving for #{model}")

      {:ok, pid} =
        DynamicSupervisor.start_child(
          Livebook.Copilot.BumblebeeServingSupervisor,
          nx_serving_spec(%{model: model})
        )

      Logger.info("Started copilot bumblebee serving for #{model}: #{inspect(pid)}")
    end
  end

  @spec model_loaded?(%{:model => any(), optional(any()) => any()}) :: boolean()
  def model_loaded?(%{model: model}) do
    child =
      Supervisor.which_children(Livebook.Copilot.BumblebeeServingSupervisor)
      |> Enum.find(fn {_, pid, _, [Nx.Serving]} ->
        {_, process_name} = Process.info(pid, :registered_name)
        # process_name would be (whatever is returned from serving_name).Supervisor
        String.starts_with?(Atom.to_string(process_name), Atom.to_string(serving_name(model)))
      end)

    !!child
  end

  defp serving_name("deepseek-coder-1.3b"), do: Copilot.CompletionBackends.DeepseekSmall
  defp serving_name("gpt2"), do: Copilot.CompletionBackends.GPT2
  defp serving_name("codellama-7b"), do: Copilot.CompletionBackends.Codellama
end
