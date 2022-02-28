defmodule LivebookWeb.JSViewChannel do
  use Phoenix.Channel

  @impl true
  def join("js_view", %{"session_id" => session_id}, socket) do
    {:ok, assign(socket, session_id: session_id, ref_with_pid: %{}, ref_with_count: %{})}
  end

  @impl true
  def handle_in("connect", %{"session_token" => session_token, "ref" => ref}, socket) do
    {:ok, data} = Phoenix.Token.verify(LivebookWeb.Endpoint, "js view", session_token)
    %{pid: pid} = data

    send(pid, {:connect, self(), %{origin: self(), ref: ref}})

    ref_with_pid = Map.put(socket.assigns.ref_with_pid, ref, pid)
    ref_with_count = Map.update(socket.assigns.ref_with_count, ref, 1, &(&1 + 1))
    socket = assign(socket, ref_with_pid: ref_with_pid, ref_with_count: ref_with_count)

    if socket.assigns.ref_with_count[ref] == 1 do
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
    pid = socket.assigns.ref_with_pid[ref]
    send(pid, {:event, event, payload, %{origin: self(), ref: ref}})
    {:noreply, socket}
  end

  def handle_in("disconnect", %{"ref" => ref}, socket) do
    socket =
      if socket.assigns.ref_with_count[ref] == 1 do
        Livebook.Session.unsubscribe_from_runtime_events(
          socket.assigns.session_id,
          "js_live",
          ref
        )

        {_, ref_with_count} = Map.pop!(socket.assigns.ref_with_count, ref)
        {_, ref_with_pid} = Map.pop!(socket.assigns.ref_with_pid, ref)
        assign(socket, ref_with_count: ref_with_count, ref_with_pid: ref_with_pid)
      else
        ref_with_count = Map.update!(socket.assigns.ref_with_count, ref, &(&1 - 1))
        assign(socket, ref_with_count: ref_with_count)
      end

    {:noreply, socket}
  end

  @impl true
  def handle_info({:connect_reply, payload, %{ref: ref}}, socket) do
    with {:error, error} <- try_push(socket, "init:#{ref}", nil, payload) do
      message = "Failed to serialize initial widget data, " <> error
      push(socket, "error:#{ref}", %{"message" => message})
    end

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
    catch
      :error, %Protocol.UndefinedError{protocol: Jason.Encoder, value: value} ->
        {:error, "value #{inspect(value)} is not JSON-serializable, use another data type"}

      :error, error ->
        {:error, Exception.message(error)}
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
