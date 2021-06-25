defmodule Livebook.Evaluator.IOProxy do
  @moduledoc false

  # An IO device process used by `Evaluator` as its `:stdio`.
  #
  # The process implements [The Erlang I/O Protocol](https://erlang.org/doc/apps/stdlib/io_protocol.html)
  # and can be thought of as a *virtual* IO device.
  #
  # Upon receiving an IO requests, the process sends a message
  # the `target` process specified during initialization.
  # Currently only output requests are supported.
  #
  # The implementation is based on the built-in `StringIO`,
  # so check it out for more reference.

  use GenServer

  alias Livebook.Evaluator

  ## API

  @doc """
  Starts the IO device process.

  Make sure to use `configure/3` to actually proxy the requests.
  """
  @spec start_link() :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Sets IO proxy destination and the reference to be attached to all messages.

  For all supported requests a message is sent to `target`,
  so this device serves as a proxy. The given evaluation
  reference (`ref`) is also sent in all messages.

  The possible messages are:

  * `{:evaluation_output, ref, string}` - for output requests,
    where `ref` is the given evaluation reference and `string` is the output.
  """
  @spec configure(pid(), pid(), Evaluator.ref()) :: :ok
  def configure(pid, target, ref) do
    GenServer.cast(pid, {:configure, target, ref})
  end

  @doc """
  Synchronously sends all buffer contents to the configured target process.
  """
  @spec flush(pid()) :: :ok
  def flush(pid) do
    GenServer.call(pid, :flush)
  end

  @doc """
  Asynchronously clears all buffered inputs, so next time they
  are requested again.
  """
  @spec clear_input_buffers(pid()) :: :ok
  def clear_input_buffers(pid) do
    GenServer.cast(pid, :clear_input_buffers)
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
  def init(_opts) do
    {:ok,
     %{
       encoding: :unicode,
       target: nil,
       ref: nil,
       buffer: [],
       input_buffers: %{},
       widget_pids: MapSet.new()
     }}
  end

  @impl true
  def handle_cast({:configure, target, ref}, state) do
    {:noreply, %{state | target: target, ref: ref}}
  end

  def handle_cast(:clear_input_buffers, state) do
    {:noreply, %{state | input_buffers: %{}}}
  end

  @impl true
  def handle_call(:flush, _from, state) do
    {:reply, :ok, flush_buffer(state)}
  end

  def handle_call(:flush_widgets, _from, state) do
    {:reply, state.widget_pids, %{state | widget_pids: MapSet.new()}}
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

  defp io_request({:get_chars, prompt, count}, state) when count >= 0 do
    get_chars(:latin1, prompt, state, count)
  end

  defp io_request({:get_chars, encoding, prompt, count}, state) when count >= 0 do
    get_chars(encoding, prompt, state, count)
  end

  defp io_request({:get_line, prompt}, state) do
    get_line(:latin1, prompt, state)
  end

  defp io_request({:get_line, encoding, prompt}, state) do
    get_line(encoding, prompt, state)
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

  # Livebook custom request type, handled in a special manner
  # by IOProxy and safely failing for any other IO device
  # (resulting in the {:error, :request} response).
  defp io_request({:livebook_put_output, output}, state) do
    state = flush_buffer(state)
    send(state.target, {:evaluation_output, state.ref, output})

    state =
      case Evaluator.widget_pid_from_output(output) do
        {:ok, pid} -> update_in(state.widget_pids, &MapSet.put(&1, pid))
        :error -> state
      end

    {:ok, state}
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

  defp get_line(encoding, prompt, state) do
    prompt = :unicode.characters_to_binary(prompt, encoding, state.encoding)

    case get_input(prompt, state) do
      input when is_binary(input) ->
        {line, rest} = line_from_input(input)

        line =
          if is_binary(line) do
            :unicode.characters_to_binary(line, state.encoding, encoding)
          else
            line
          end

        state = put_in(state.input_buffers[prompt], rest)
        {line, state}

      error ->
        {error, state}
    end
  end

  defp get_chars(encoding, prompt, state, count) do
    prompt = :unicode.characters_to_binary(prompt, encoding, state.encoding)

    case get_input(prompt, state) do
      input when is_binary(input) ->
        {chars, rest} = chars_from_input(input, encoding, count)

        state = put_in(state.input_buffers[prompt], rest)
        {chars, state}

      error ->
        {error, state}
    end
  end

  defp get_input(prompt, state) do
    Map.get_lazy(state.input_buffers, prompt, fn ->
      request_input(prompt, state)
    end)
  end

  defp request_input(prompt, state) do
    send(state.target, {:evaluation_input, state.ref, self(), prompt})

    ref = Process.monitor(state.target)

    receive do
      {:evaluation_input_reply, {:ok, string}} ->
        Process.demonitor(ref, [:flush])
        string

      {:evaluation_input_reply, :error} ->
        Process.demonitor(ref, [:flush])
        {:error, "no matching Livebook input found"}

      {:DOWN, ^ref, :process, _object, _reason} ->
        {:error, :terminated}
    end
  end

  defp line_from_input(""), do: {:eof, ""}

  defp line_from_input(input) do
    case :binary.match(input, ["\r\n", "\n"]) do
      :nomatch ->
        {input, ""}

      {pos, len} ->
        size = byte_size(input)
        line = binary_part(input, 0, pos + len)
        rest = binary_part(input, pos + len, size - pos - len)
        {line, rest}
    end
  end

  defp chars_from_input("", _, _count), do: {:eof, ""}

  defp chars_from_input(input, :unicode, count) do
    if byte_size_utf8(input) >= count do
      chars_part(input, :unicode, count)
    else
      {input, ""}
    end
  end

  defp chars_from_input(input, :latin1, count) do
    if byte_size(input) >= count do
      chars_part(input, :latin1, count)
    else
      {input, ""}
    end
  end

  defp chars_part(chars, _, 0), do: {"", chars}

  defp chars_part(input, :unicode, count) do
    with {:ok, count} <- split_at(input, count, 0) do
      <<chars::binary-size(count), rest::binary>> = input
      {chars, rest}
    end
  end

  defp chars_part(input, :latin1, count) do
    <<chars::binary-size(count), rest::binary>> = input
    {chars, rest}
  end

  defp split_at(_, 0, acc), do: {:ok, acc}

  defp split_at(<<h::utf8, t::binary>>, count, acc),
    do: split_at(t, count - 1, acc + byte_size(<<h::utf8>>))

  defp split_at(<<_, _::binary>>, _count, _acc),
    do: {:error, :invalid_unicode}

  defp split_at(<<>>, _count, acc),
    do: {:ok, acc}

  defp byte_size_utf8(chars), do: byte_size_utf8(chars, 0)

  defp byte_size_utf8(<<>>, size), do: size

  defp byte_size_utf8(<<_h::utf8, t::binary>>, size), do: byte_size_utf8(t, size + 1)

  defp io_reply(from, reply_as, reply) do
    send(from, {:io_reply, reply_as, reply})
  end

  defp flush_buffer(state) do
    string = state.buffer |> Enum.reverse() |> Enum.join()

    if state.target != nil and string != "" do
      send(state.target, {:evaluation_output, state.ref, string})
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
