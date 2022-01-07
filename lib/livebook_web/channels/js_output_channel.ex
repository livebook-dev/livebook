defmodule LivebookWeb.JSOutputChannel do
  use Phoenix.Channel

  @impl true
  def join("js_output", %{"session_id" => session_id}, socket) do
    {:ok, assign(socket, session_id: session_id, ref_with_pid: %{}, ref_with_count: %{})}
  end

  @impl true
  def handle_in("connect", %{"session_token" => session_token, "ref" => ref}, socket) do
    {:ok, data} = Phoenix.Token.verify(LivebookWeb.Endpoint, "js output", session_token)
    %{pid: pid} = data

    send(pid, {:connect, self(), %{origin: self(), ref: ref}})

    ref_with_pid = Map.put(socket.assigns.ref_with_pid, ref, pid)
    ref_with_count = Map.update(socket.assigns.ref_with_count, ref, 1, &(&1 + 1))
    socket = assign(socket, ref_with_pid: ref_with_pid, ref_with_count: ref_with_count)

    if socket.assigns.ref_with_count[ref] == 1 do
      Livebook.Session.subscribe_to_runtime_events(socket.assigns.session_id, "js_live", ref)
    end

    {:noreply, socket}
  end

  def handle_in("event", %{"event" => event, "payload" => payload, "ref" => ref}, socket) do
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
  def handle_info({:connect_reply, data, %{ref: ref}}, socket) do
    with {:error, error} <- try_push(socket, "init:#{ref}", %{"data" => data}) do
      message = "Failed to serialize initial widget data, " <> error
      push(socket, "error:#{ref}", %{"message" => message})
    end

    {:noreply, socket}
  end

  def handle_info({:event, event, payload, %{ref: ref}}, socket) do
    with {:error, error} <-
           try_push(socket, "event:#{ref}", %{"event" => event, "payload" => payload}) do
      message = "Failed to serialize event payload, " <> error
      push(socket, "error:#{ref}", %{"message" => message})
    end

    {:noreply, socket}
  end

  # In case the payload fails to encode we catch the error
  defp try_push(socket, event, payload) do
    try do
      push(socket, event, payload)
    catch
      :error, %Protocol.UndefinedError{protocol: Jason.Encoder, value: value} ->
        {:error, "value #{inspect(value)} is not JSON-serializable, use another data type"}

      :error, error ->
        {:error, Exception.message(error)}
    end
  end
end
