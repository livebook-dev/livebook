defmodule LivebookWeb.JSViewChannel do
  use Phoenix.Channel

  @impl true
  def join("js_view", %{"session_token" => session_token}, socket) do
    case Phoenix.Token.verify(LivebookWeb.Endpoint, "session", session_token) do
      {:ok, data} ->
        {:ok,
         assign(socket,
           session_id: data.session_id,
           client_id: data.client_id,
           ref_with_info: %{}
         )}

      _error ->
        {:error, %{reason: "invalid token"}}
    end
  end

  @impl true
  def handle_in("connect", %{"connect_token" => connect_token, "ref" => ref, "id" => id}, socket) do
    {:ok, %{pid: pid}} =
      Phoenix.Token.verify(LivebookWeb.Endpoint, "js-view-connect", connect_token)

    send(pid, {:connect, self(), %{origin: socket.assigns.client_id, ref: ref}})

    socket =
      update_in(socket.assigns.ref_with_info[ref], fn
        nil -> %{pid: pid, count: 1, connect_queue: [id]}
        info -> %{info | count: info.count + 1, connect_queue: info.connect_queue ++ [id]}
      end)

    if socket.assigns.ref_with_info[ref].count == 1 do
      Livebook.Session.subscribe_to_runtime_events(
        socket.assigns.session_id,
        "js_live",
        ref,
        &fastlane_encoder/1,
        socket.transport_pid
      )
    end

    {:noreply, socket}
  end

  def handle_in("event", raw, socket) do
    {[event, ref], payload} = transport_decode!(raw)
    pid = socket.assigns.ref_with_info[ref].pid
    send(pid, {:event, event, payload, %{origin: socket.assigns.client_id, ref: ref}})
    {:noreply, socket}
  end

  def handle_in("ping", %{"ref" => ref}, socket) do
    pid = socket.assigns.ref_with_info[ref].pid
    send(pid, {:ping, self(), nil, %{ref: ref}})
    {:noreply, socket}
  end

  def handle_in("disconnect", %{"ref" => ref}, socket) do
    socket =
      if socket.assigns.ref_with_info[ref].count == 1 do
        Livebook.Session.unsubscribe_from_runtime_events(
          socket.assigns.session_id,
          "js_live",
          ref
        )

        {_, socket} = pop_in(socket.assigns.ref_with_info[ref])
        socket
      else
        update_in(socket.assigns.ref_with_info[ref], &%{&1 | count: &1.count - 1})
      end

    {:noreply, socket}
  end

  @impl true
  def handle_info({:connect_reply, payload, %{ref: ref}}, socket) do
    # Multiple connections for the same reference may be establish,
    # the replies come sequentially and we dispatch them according
    # to the clients queue

    {id, socket} =
      get_and_update_in(socket.assigns.ref_with_info[ref].connect_queue, fn [id | queue] ->
        {id, queue}
      end)

    with {:error, error} <- try_push(socket, "init:#{ref}:#{id}", nil, payload) do
      message = "Failed to serialize initial widget data, " <> error
      push(socket, "error:#{ref}", %{"message" => message, "init" => true})
    end

    {:noreply, socket}
  end

  def handle_info({:event, event, payload, %{ref: ref}}, socket) do
    with {:error, error} <- try_push(socket, "event:#{ref}", [event], payload) do
      message = "Failed to serialize widget data, " <> error
      push(socket, "error:#{ref}", %{"message" => message})
    end

    {:noreply, socket}
  end

  def handle_info({:pong, _, %{ref: ref}}, socket) do
    push(socket, "pong:#{ref}", %{})
    {:noreply, socket}
  end

  def handle_info({:encoding_error, error, {:event, _event, _payload, %{ref: ref}}}, socket) do
    message = "Failed to serialize widget data, " <> error
    push(socket, "error:#{ref}", %{"message" => message})
    {:noreply, socket}
  end

  defp try_push(socket, event, meta, payload) do
    with {:ok, _} <-
           run_safely(fn ->
             push(socket, event, transport_encode!(meta, payload))
           end),
         do: :ok
  end

  # In case the payload fails to encode we catch the error
  defp run_safely(fun) do
    try do
      {:ok, fun.()}
    rescue
      error ->
        case error do
          %Protocol.UndefinedError{protocol: Jason.Encoder, value: value} ->
            {:error, "value #{inspect(value)} is not JSON-serializable, use another data type"}

          error ->
            {:error, Exception.message(error)}
        end
    end
  end

  defp fastlane_encoder({:event, event, payload, %{ref: ref}}) do
    run_safely(fn ->
      Phoenix.Socket.V2.JSONSerializer.fastlane!(%Phoenix.Socket.Broadcast{
        topic: "js_view",
        event: "event:#{ref}",
        payload: transport_encode!([event], payload)
      })
    end)
  end

  # A user payload can be either a JSON-serializable term
  # or a {:binary, info, binary} tuple, where info is a
  # JSON-serializable term. The channel allows for sending
  # either maps or binaries, so we need to translare the
  # payload accordingly

  defp transport_encode!(meta, {:binary, info, binary}) do
    {:binary, encode!([meta, info], binary)}
  end

  defp transport_encode!(meta, payload) do
    %{"root" => [meta, payload]}
  end

  defp transport_decode!({:binary, raw}) do
    {[meta, info], binary} = decode!(raw)
    {meta, {:binary, info, binary}}
  end

  defp transport_decode!(raw) do
    %{"root" => [meta, payload]} = raw
    {meta, payload}
  end

  defp encode!(meta, binary) do
    meta = Jason.encode!(meta)
    meta_size = byte_size(meta)
    <<meta_size::size(32), meta::binary, binary::binary>>
  end

  defp decode!(raw) do
    <<meta_size::size(32), meta::binary-size(meta_size), binary::binary>> = raw
    meta = Jason.decode!(meta)
    {meta, binary}
  end
end
