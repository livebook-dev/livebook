defmodule Livebook.Evaluator do
  @moduledoc false

  # A process responsible for evaluating notebook code.
  #
  # The process receives evaluation request and synchronously
  # evaluates the given code within itself (rather than spawning a separate process).
  # It stores the resulting binding and env as part of the state.
  #
  # It's important to store the binding in the same process
  # where the evaluation happens, as otherwise we would have to
  # send them between processes, effectively copying potentially large data.

  use GenServer, restart: :temporary

  alias Livebook.Evaluator

  @type t :: GenServer.server()

  @type state :: %{
          formatter: module(),
          io_proxy: pid(),
          contexts: %{ref() => context()},
          # We track the widgets rendered by every evaluation,
          # so that we can kill those no longer needed
          widget_pids: %{ref() => MapSet.t(pid())},
          widget_counts: %{pid() => non_neg_integer()}
        }

  @typedoc """
  An evaluation context.
  """
  @type context :: %{binding: Code.binding(), env: Macro.Env.t()}

  @typedoc """
  A term used to identify evaluation.
  """
  @type ref :: term()

  @typedoc """
  Either {:ok, result} for successful evaluation
  or {:error, kind, error, stacktrace} for a failed one.
  """
  @type evaluation_response ::
          {:ok, any()} | {:error, Exception.kind(), any(), Exception.stacktrace()}

  ## API

  @doc """
  Starts the evaluator.

  Options:

    * `formatter` - a module implementing the `Livebook.Evaluator.Formatter` behaviour,
      used for transforming evaluation response before it's sent to the client
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Asynchronously parses and evaluates the given code.

  Any exceptions are captured, in which case this method returns an error.

  The evaluator stores the resulting binding and environment under `ref`.
  Any subsequent calls may specify `prev_ref` pointing to a previous evaluation,
  in which case the corresponding binding and environment are used during evaluation.

  Evaluation response is sent to the process identified by `send_to` as `{:evaluation_response, ref, response}`.
  Note that response is transformed with the configured formatter (identity by default).

  ## Options

  * `:file` - file to which the evaluated code belongs. Most importantly,
    this has an impact on the value of `__DIR__`.
  """
  @spec evaluate_code(t(), pid(), String.t(), ref(), ref() | nil, keyword()) :: :ok
  def evaluate_code(evaluator, send_to, code, ref, prev_ref \\ nil, opts \\ []) when ref != nil do
    GenServer.cast(evaluator, {:evaluate_code, send_to, code, ref, prev_ref, opts})
  end

  @doc """
  Removes the evaluation identified by `ref` from history,
  so that further evaluations cannot use it.
  """
  @spec forget_evaluation(t(), ref()) :: :ok
  def forget_evaluation(evaluator, ref) do
    GenServer.cast(evaluator, {:forget_evaluation, ref})
  end

  @doc """
  Asynchronously finds completion items matching the given `hint` text.

  If `evaluation_ref` is given, its binding and environment are also
  used for the completion. Response is sent to the `send_to` process
  as `{:completion_response, ref, items}`.
  """
  @spec request_completion_items(t(), pid(), term(), String.t(), ref() | nil) :: :ok
  def request_completion_items(evaluator, send_to, ref, hint, evaluation_ref \\ nil) do
    GenServer.cast(evaluator, {:request_completion_items, send_to, ref, hint, evaluation_ref})
  end

  ## Callbacks

  @impl true
  def init(opts) do
    formatter = Keyword.get(opts, :formatter, Evaluator.IdentityFormatter)

    {:ok, io_proxy} = Evaluator.IOProxy.start_link()

    # Use the dedicated IO device as the group leader,
    # so that it handles all :stdio operations.
    Process.group_leader(self(), io_proxy)
    {:ok, initial_state(formatter, io_proxy)}
  end

  defp initial_state(formatter, io_proxy) do
    %{
      formatter: formatter,
      io_proxy: io_proxy,
      contexts: %{},
      widget_pids: %{},
      widget_counts: %{}
    }
  end

  defp initial_context() do
    env = :elixir.env_for_eval([])
    %{binding: [], env: env}
  end

  @impl true
  def handle_cast({:evaluate_code, send_to, code, ref, prev_ref, opts}, state) do
    Evaluator.IOProxy.configure(state.io_proxy, send_to, ref)

    context = Map.get_lazy(state.contexts, prev_ref, fn -> initial_context() end)
    file = Keyword.get(opts, :file, "nofile")
    context = put_in(context.env.file, file)

    {result_context, response} =
      case eval(code, context.binding, context.env) do
        {:ok, result, binding, env} ->
          result_context = %{binding: binding, env: env}
          response = {:ok, result}
          {result_context, response}

        {:error, kind, error, stacktrace} ->
          response = {:error, kind, error, stacktrace}
          {context, response}
      end

    state = put_in(state.contexts[ref], result_context)

    Evaluator.IOProxy.flush(state.io_proxy)
    Evaluator.IOProxy.clear_input_buffers(state.io_proxy)

    output = state.formatter.format_response(response)
    send(send_to, {:evaluation_response, ref, output})

    widget_pids = Evaluator.IOProxy.flush_widgets(state.io_proxy)
    state = track_evaluation_widgets(state, ref, widget_pids, output)

    {:noreply, state}
  end

  def handle_cast({:forget_evaluation, ref}, state) do
    state =
      state
      |> Map.update!(:contexts, &Map.delete(&1, ref))
      |> garbage_collect_widgets(ref, [])

    {:noreply, state}
  end

  def handle_cast({:request_completion_items, send_to, ref, hint, evaluation_ref}, state) do
    context = Map.get_lazy(state.contexts, evaluation_ref, fn -> initial_context() end)
    items = Livebook.Completion.get_completion_items(hint, context.binding, context.env)
    send(send_to, {:completion_response, ref, items})

    {:noreply, state}
  end

  defp eval(code, binding, env) do
    try do
      quoted = Code.string_to_quoted!(code)
      {result, binding, env} = :elixir.eval_quoted(quoted, binding, env)

      {:ok, result, binding, env}
    catch
      kind, error ->
        {kind, error, stacktrace} = prepare_error(kind, error, __STACKTRACE__)
        {:error, kind, error, stacktrace}
    end
  end

  defp prepare_error(kind, error, stacktrace) do
    {error, stacktrace} = Exception.blame(kind, error, stacktrace)
    stacktrace = prune_stacktrace(stacktrace)
    {kind, error, stacktrace}
  end

  # Adapted from https://github.com/elixir-lang/elixir/blob/1c1654c88adfdbef38ff07fc30f6fbd34a542c07/lib/iex/lib/iex/evaluator.ex#L355-L372

  @elixir_internals [:elixir, :elixir_expand, :elixir_compiler, :elixir_module] ++
                      [:elixir_clauses, :elixir_lexical, :elixir_def, :elixir_map] ++
                      [:elixir_erl, :elixir_erl_clauses, :elixir_erl_pass]

  defp prune_stacktrace(stacktrace) do
    # The order in which each drop_while is listed is important.
    # For example, the user may call Code.eval_string/2 in their code
    # and if there is an error we should not remove erl_eval
    # and eval_bits information from the user stacktrace.
    stacktrace
    |> Enum.reverse()
    |> Enum.drop_while(&(elem(&1, 0) == :proc_lib))
    |> Enum.drop_while(&(elem(&1, 0) == :gen_server))
    |> Enum.drop_while(&(elem(&1, 0) == __MODULE__))
    |> Enum.drop_while(&(elem(&1, 0) == :elixir))
    |> Enum.drop_while(&(elem(&1, 0) in [:erl_eval, :eval_bits]))
    |> Enum.reverse()
    |> Enum.reject(&(elem(&1, 0) in @elixir_internals))
  end

  # Widgets

  defp track_evaluation_widgets(state, ref, widget_pids, output) do
    widget_pids =
      case widget_pid_from_output(output) do
        {:ok, pid} -> MapSet.put(widget_pids, pid)
        :error -> widget_pids
      end

    garbage_collect_widgets(state, ref, widget_pids)
  end

  defp garbage_collect_widgets(state, ref, widget_pids) do
    prev_widget_pids = state.widget_pids[ref] || []

    state = put_in(state.widget_pids[ref], widget_pids)

    update_in(state.widget_counts, fn counts ->
      counts =
        Enum.reduce(prev_widget_pids, counts, fn pid, counts ->
          Map.update!(counts, pid, &(&1 - 1))
        end)

      counts =
        Enum.reduce(widget_pids, counts, fn pid, counts ->
          Map.update(counts, pid, 1, &(&1 + 1))
        end)

      {to_remove, to_keep} = Enum.split_with(counts, fn {_pid, count} -> count == 0 end)

      for {pid, 0} <- to_remove do
        Process.exit(pid, :shutdown)
      end

      Map.new(to_keep)
    end)
  end

  @doc """
  Checks the given output value for widget pid to track.
  """
  @spec widget_pid_from_output(term()) :: {:ok, pid()} | :error
  def widget_pid_from_output(output)

  def widget_pid_from_output({_type, pid}) when is_pid(pid) do
    {:ok, pid}
  end

  def widget_pid_from_output(_output), do: :error
end
