defmodule Livebook.Evaluator.IOProxy do
  @moduledoc false

  # An IO device process used by `Evaluator` as its `:stdio`.
  #
  # The process implements [The Erlang I/O Protocol](https://erlang.org/doc/apps/stdlib/io_protocol.html)
  # and can be thought of as a *virtual* IO device.
  #
  # Upon receiving an IO requests, the process sends a message
  # the `:send_to` process specified during initialization.
  # Currently only output requests are supported.
  #
  # The implementation is based on the built-in `StringIO`,
  # so check it out for more reference.

  use GenServer

  alias Livebook.Evaluator

  ## API

  @doc """
  Starts the IO device process.

  Make sure to use `configure/3` to correctly proxy the requests.
  """
  @spec start_link(pid(), pid(), pid(), pid()) :: GenServer.on_start()
  def start_link(evaluator, send_to, runtime_broadcast_to, object_tracker) do
    GenServer.start_link(__MODULE__, {evaluator, send_to, runtime_broadcast_to, object_tracker})
  end

  @doc """
  Sets IO proxy destination and the reference to be attached
  to all messages.

  For all supported requests a message is sent to `:send_to`,
  so this device serves as a proxy. The given evaluation
  reference (`ref`) is also sent in all messages.

  The possible messages are:

    * `{:evaluation_output, ref, output}`

    * `{:evaluation_input, ref, reply_to, input_id}`

  As described by the `Livebook.Runtime` protocol. The `ref`
  is always the given evaluation reference.
  """
  @spec configure(pid(), Evaluator.ref()) :: :ok
  def configure(pid, ref) do
    GenServer.cast(pid, {:configure, ref})
  end

  @doc """
  Synchronously sends all buffer contents to the configured
  `:send_to` process.
  """
  @spec flush(pid()) :: :ok
  def flush(pid) do
    GenServer.call(pid, :flush)
  end

  @doc """
  Asynchronously clears all buffered inputs, so next time they
  are requested again.
  """
  @spec clear_input_cache(pid()) :: :ok
  def clear_input_cache(pid) do
    GenServer.cast(pid, :clear_input_cache)
  end

  @doc """
  Returns the accumulated widget pids and clears the accumulator.
  """
  @spec flush_widgets(pid()) :: MapSet.t(pid())
  def flush_widgets(pid) do
    GenServer.call(pid, :flush_widgets)
  end

  ## Callbacks

  @impl true
  def init({evaluator, send_to, runtime_broadcast_to, object_tracker}) do
    {:ok,
     %{
       encoding: :unicode,
       ref: nil,
       buffer: [],
       input_cache: %{},
       token_count: 0,
       evaluator: evaluator,
       send_to: send_to,
       runtime_broadcast_to: runtime_broadcast_to,
       object_tracker: object_tracker
     }}
  end

  @impl true
  def handle_cast({:configure, ref}, state) do
    {:noreply, %{state | ref: ref, token_count: 0}}
  end

  def handle_cast(:clear_input_cache, state) do
    {:noreply, %{state | input_cache: %{}}}
  end

  @impl true
  def handle_call(:flush, _from, state) do
    {:reply, :ok, flush_buffer(state)}
  end

  @impl true
  def handle_info({:io_request, from, reply_as, req}, state) do
    {reply, state} = io_request(req, state)
    io_reply(from, reply_as, reply)
    {:noreply, state}
  end

  def handle_info(:flush, state) do
    {:noreply, flush_buffer(state)}
  end

  defp io_request({:put_chars, chars} = req, state) do
    put_chars(:latin1, chars, req, state)
  end

  defp io_request({:put_chars, mod, fun, args} = req, state) do
    put_chars(:latin1, apply(mod, fun, args), req, state)
  end

  defp io_request({:put_chars, encoding, chars} = req, state) do
    put_chars(encoding, chars, req, state)
  end

  defp io_request({:put_chars, encoding, mod, fun, args} = req, state) do
    put_chars(encoding, apply(mod, fun, args), req, state)
  end

  defp io_request({:get_chars, _prompt, count}, state) when count >= 0 do
    {{:error, :enotsup}, state}
  end

  defp io_request({:get_chars, _encoding, _prompt, count}, state) when count >= 0 do
    {{:error, :enotsup}, state}
  end

  defp io_request({:get_line, _prompt}, state) do
    {{:error, :enotsup}, state}
  end

  defp io_request({:get_line, _encoding, _prompt}, state) do
    {{:error, :enotsup}, state}
  end

  defp io_request({:get_until, _prompt, _mod, _fun, _args}, state) do
    {{:error, :enotsup}, state}
  end

  defp io_request({:get_until, _encoding, _prompt, _mod, _fun, _args}, state) do
    {{:error, :enotsup}, state}
  end

  defp io_request({:get_password, _encoding}, state) do
    {{:error, :enotsup}, state}
  end

  defp io_request({:setopts, [encoding: encoding]}, state) when encoding in [:latin1, :unicode] do
    {:ok, %{state | encoding: encoding}}
  end

  defp io_request({:setopts, _opts}, state) do
    {{:error, :enotsup}, state}
  end

  defp io_request(:getopts, state) do
    {[binary: true, encoding: state.encoding], state}
  end

  defp io_request({:get_geometry, :columns}, state) do
    {{:error, :enotsup}, state}
  end

  defp io_request({:get_geometry, :rows}, state) do
    {{:error, :enotsup}, state}
  end

  defp io_request({:requests, reqs}, state) do
    io_requests(reqs, {:ok, state})
  end

  # Livebook custom request types, handled in a special manner
  # by IOProxy and safely failing for any other IO device
  # (resulting in the {:error, :request} response).
  # Those requests are generally made by Kino

  defp io_request({:livebook_put_output, output}, state) do
    state = flush_buffer(state)
    send(state.send_to, {:evaluation_output, state.ref, output})
    {:ok, state}
  end

  defp io_request({:livebook_get_input_value, input_id}, state) do
    input_cache =
      Map.put_new_lazy(state.input_cache, input_id, fn ->
        request_input_value(input_id, state)
      end)

    {input_cache[input_id], %{state | input_cache: input_cache}}
  end

  # Token is a unique, reevaluation-safe opaque identifier
  defp io_request(:livebook_generate_token, state) do
    token = {state.ref, state.token_count}
    state = update_in(state.token_count, &(&1 + 1))
    {token, state}
  end

  defp io_request({:livebook_reference_object, object, pid}, state) do
    # When the request comes from evaluator we want the pointer
    # specific to the current evaluation. For any other process
    # we only care about monitoring.

    reference =
      if pid == state.evaluator do
        {pid, state.ref}
      else
        {pid, :process}
      end

    Evaluator.ObjectTracker.add_reference(state.object_tracker, object, reference)
    {:ok, state}
  end

  defp io_request({:livebook_monitor_object, object, destination, payload}, state) do
    reply = Evaluator.ObjectTracker.monitor(state.object_tracker, object, destination, payload)
    {reply, state}
  end

  defp io_request(:livebook_get_broadcast_target, state) do
    {{:ok, state.runtime_broadcast_to}, state}
  end

  defp io_request(_, state) do
    {{:error, :request}, state}
  end

  defp io_requests([req | rest], {:ok, state}) do
    io_requests(rest, io_request(req, state))
  end

  defp io_requests(_, result) do
    result
  end

  defp put_chars(encoding, chars, req, state) do
    case :unicode.characters_to_binary(chars, encoding, state.encoding) do
      string when is_binary(string) ->
        if state.buffer == [] do
          Process.send_after(self(), :flush, 50)
        end

        {:ok, update_in(state.buffer, &buffer_append(&1, string))}

      {_, _, _} ->
        {{:error, req}, state}
    end
  rescue
    ArgumentError -> {{:error, req}, state}
  end

  defp request_input_value(input_id, state) do
    send(state.send_to, {:evaluation_input, state.ref, self(), input_id})

    ref = Process.monitor(state.send_to)

    receive do
      {:evaluation_input_reply, {:ok, value}} ->
        Process.demonitor(ref, [:flush])
        {:ok, value}

      {:evaluation_input_reply, :error} ->
        Process.demonitor(ref, [:flush])
        {:error, :not_found}

      {:DOWN, ^ref, :process, _object, _reason} ->
        {:error, :terminated}
    end
  end

  defp io_reply(from, reply_as, reply) do
    send(from, {:io_reply, reply_as, reply})
  end

  defp flush_buffer(state) do
    string = state.buffer |> Enum.reverse() |> Enum.join()

    if state.send_to != nil and string != "" do
      send(state.send_to, {:evaluation_output, state.ref, {:stdout, string}})
    end

    %{state | buffer: []}
  end

  defp buffer_append(buffer, text) do
    # Sometimes there are intensive outputs that use \r
    # to dynamically refresh the printd text.
    # Since we buffer the messages anyway, it makes
    # sense to send only the latest of these outputs.
    # Note that \r works per-line, so if there are newlines
    # we keep the buffer, but for \r-intensive operations
    # there are usually no newlines involved, so this optimisation works fine.
    if has_rewind?(text) and not has_newline?(text) and not Enum.any?(buffer, &has_newline?/1) do
      [text]
    else
      [text | buffer]
    end
  end

  # Checks for [\r][not \r] sequence in the given string.
  defp has_rewind?(<<>>), do: false
  defp has_rewind?(<<?\r, next, _rest::binary>>) when next != ?\r, do: true
  defp has_rewind?(<<_head, rest::binary>>), do: has_rewind?(rest)

  defp has_newline?(text), do: String.contains?(text, "\n")
end
