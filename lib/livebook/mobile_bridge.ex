defmodule Livebook.MobileBridge do
  @moduledoc """
  MobileBridge communicates with the native part of an iOS/Android application

  # Protocol
  Current protocol is json based.
  All :wx***.new(args...) calls generate keyword lists like:
    `[id: System.unique_integer([:positive]), type: module, args: args]`
  Most wx***.method(args...) calls are then forwarded via JSON to the native side with
  a 64-bit request id value:
    `<<request_ref :: unsigned-size(64), json :: binary>>`
  The responses correspndingly return the same ref and the json response:
    `<<response_ref :: unsigned-size(64), json :: binary>>`
  For receiving commands from the native side of the bridge there are three special ref
  values:
    * ref = 0 -> This indicates a published system event system, corresponding to `:wx.subscribe_events()`
      needed for publishing files that are shared to the app.
      `<<0 :: unsigned-size(64), event :: binary>>`
    * ref = 1 -> This indicates triggering a callback function call that was previously passed over.
      Internally an `funs` that are passed into `:wx.method()` calls are converted to 64-bit references,
      those can be used here to indicate which function to call.
    `<<1 :: unsigned-size(64), fun :: unsigned-size(64), event :: binary>>`
    * ref = 2 -> This indicates a call from the native side back into the app side. TBD
    `<<2 :: unsigned-size(64), ...>>`
    # JSON Encoding of Elixir Terms
  """
  alias Livebook.MobileBridge
  use GenServer
  require Logger

  defstruct socket: nil,
            requests: %{},
            funs: %{},
            events: [],
            subscribers: []

  def show_url(url) do
    cast(:webview, :loadURL, [url])
  end

  @impl true
  def init([]) do
    socket =
      with port when port > 0 <- String.to_integer(System.get_env("BRIDGE_PORT", "0")),
           {:ok, socket} <-
             :gen_tcp.connect({127, 0, 0, 1}, port, packet: 4, active: true, mode: :binary) do
        socket
      end

    {:ok, %MobileBridge{socket: socket}}
  end

  defp ensure_bridge() do
    case Process.whereis(__MODULE__) do
      nil ->
        {:ok, pid} = GenServer.start(__MODULE__, [], name: __MODULE__)
        pid

      pid ->
        pid
    end
  end

  defp cast(module, method, args) do
    Logger.info("bridge_cast: #{module}.#{method}(#{inspect(args)})")
    ref = System.unique_integer([:positive]) + 10
    json = encode!([module, method, args ++ [self()]])

    GenServer.cast(ensure_bridge(), {:bridge_call, ref, json})
  end

  defp call(module, method, args) do
    Logger.info("bridge_call: #{module}.#{method}(#{inspect(args)})")
    ref = System.unique_integer([:positive]) + 10
    json = encode!([module, method, args])

    ret =
      GenServer.call(ensure_bridge(), {:bridge_call, ref, json})
      |> decode!()

    Logger.info("bridge_call: #{module}.#{method}(#{inspect(args)}) => #{inspect(ret)}")

    ret
  end

  def encode!(var) do
    pre_encode!(var)
    |> Jason.encode!()
  end

  def pre_encode!(var) do
    case var do
      tuple when is_tuple(tuple) ->
        pre_encode!(%{_type: :tuple, value: Tuple.to_list(tuple)})

      pid when is_pid(pid) ->
        pre_encode!(%{_type: :pid, value: List.to_string(:erlang.pid_to_list(pid))})

      fun when is_function(fun) ->
        pre_encode!(%{_type: :fun, value: GenServer.call(__MODULE__, {:register_fun, fun})})

      list when is_list(list) ->
        Enum.map(list, &pre_encode!/1)

      map when is_map(map) ->
        Enum.reduce(map, %{}, fn {key, value}, map ->
          Map.put(map, pre_encode!(key), pre_encode!(value))
        end)

      atom when is_atom(atom) ->
        ":" <> Atom.to_string(atom)

      other ->
        other
    end
  end

  def decode!(json) do
    Jason.decode!(json) |> decode()
  end

  defp decode(list) when is_list(list) do
    Enum.map(list, &decode/1)
  end

  defp decode(":" <> name) do
    String.to_atom(name)
  end

  defp decode(map) when is_map(map) do
    Enum.reduce(map, %{}, fn {key, value}, ret ->
      Map.put(ret, decode(key), decode(value))
    end)
    |> decode_map()
  end

  defp decode(other), do: other

  defp decode_map(%{_type: :tuple, value: tuple}) do
    List.to_tuple(tuple)
  end

  defp decode_map(%{_type: :pid, value: pid}) do
    :erlang.list_to_pid(String.to_charlist(pid))
  end

  defp decode_map(other), do: other

  @impl true
  def handle_cast({:bridge_call, ref, json}, state) do
    case handle_call({:bridge_call, ref, json}, nil, state) do
      {:reply, _ret, state} -> {:noreply, state}
      {:noreply, state} -> {:noreply, state}
    end
  end

  @impl true
  def handle_call(
        {:subscribe_events, pid},
        _from,
        state = %MobileBridge{events: events, subscribers: subscribers}
      ) do
    for event <- events do
      send(pid, event)
    end

    {:reply, :ok, %MobileBridge{state | events: [], subscribers: [pid | subscribers]}}
  end

  def handle_call(
        {:bridge_call, ref, json},
        from,
        state = %MobileBridge{socket: socket, requests: reqs}
      ) do
    if socket do
      message = <<ref::unsigned-size(64), json::binary>>
      :gen_tcp.send(socket, message)
      {:noreply, %MobileBridge{state | requests: Map.put(reqs, ref, {from, message})}}
    else
      {:reply, ":ok", state}
    end
  end

  def handle_call({:register_fun, fun}, _from, state = %MobileBridge{funs: funs}) do
    ref = System.unique_integer([:positive])
    funs = Map.put(funs, ref, fun)
    {:reply, ref, %MobileBridge{state | funs: funs}}
  end

  @impl true
  def handle_info(
        {:tcp, _port, <<0::unsigned-size(64), json::binary>>},
        state = %MobileBridge{subscribers: subscribers, events: events}
      ) do
    event = decode!(json)

    if [] == subscribers do
      Logger.info("no subscriber for event #{inspect(event)}")
      {:noreply, %MobileBridge{state | events: events ++ [event]}}
    else
      Logger.info("sending event to subscribers #{inspect(event)}")

      for sub <- subscribers do
        send(sub, event)
      end

      {:noreply, state}
    end
  end

  def handle_info(
        {:tcp, _port, <<1::unsigned-size(64), fun_ref::unsigned-size(64), json::binary>>},
        state = %MobileBridge{funs: funs}
      ) do
    args = decode!(json)

    case Map.get(funs, fun_ref) do
      nil ->
        Logger.info("no fun defined for fun_ref #{fun_ref} (#{inspect(args)})")

      fun ->
        Logger.info("executing callback fun_ref #{fun_ref} (#{inspect(args)})")
        spawn(fn -> apply(fun, args) end)
    end

    {:noreply, state}
  end

  def handle_info(
        {:tcp, _port, <<2::unsigned-size(64), json::binary>>},
        state = %MobileBridge{}
      ) do
    json = decode!(json)
    payload = json[:payload]
    pid = json[:pid]
    Logger.info("sending event #{inspect(payload)} to #{inspect(pid)}")
    send(pid, payload)
    {:noreply, state}
  end

  def handle_info(
        {:tcp, _port, <<ref::unsigned-size(64), json::binary>>},
        state = %MobileBridge{requests: reqs}
      ) do
    {from, _message} = reqs[ref]
    if from, do: GenServer.reply(from, json)
    {:noreply, %MobileBridge{state | requests: Map.delete(reqs, ref)}}
  end
end
