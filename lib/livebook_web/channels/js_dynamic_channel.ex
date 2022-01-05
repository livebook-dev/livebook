defmodule LivebookWeb.JSDynamicChannel do
  use Phoenix.Channel

  @impl true
  def join("js_dynamic", %{}, socket) do
    {:ok, assign(socket, ref_with_pid: %{}, ref_with_count: %{})}
  end

  @impl true
  def handle_in("connect", %{"session_token" => session_token, "ref" => ref}, socket) do
    {:ok, data} = Phoenix.Token.verify(LivebookWeb.Endpoint, "js dynamic", session_token)
    %{pid: pid} = data

    send(pid, {:connect, self(), %{origin: self()}})

    ref_with_pid = Map.put(socket.assigns.ref_with_pid, ref, pid)
    ref_with_count = Map.update(socket.assigns.ref_with_count, ref, 1, &(&1 + 1))
    socket = assign(socket, ref_with_pid: ref_with_pid, ref_with_count: ref_with_count)
    {:noreply, socket}
  end

  def handle_in("event", %{"event" => event, "payload" => payload, "ref" => ref}, socket) do
    pid = socket.assigns.ref_with_pid[ref]
    send(pid, {:event, event, payload, %{origin: self()}})
    {:noreply, socket}
  end

  def handle_in("diconnect", %{"ref" => ref}, socket) do
    socket =
      if socket.assigns.ref_with_count[ref] == 1 do
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
    # TODO validate serialization
    push(socket, "init:#{ref}", %{"data" => data})
    {:noreply, socket}
  end

  def handle_info({:event, event, payload, %{ref: ref}}, socket) do
    push(socket, "event:#{ref}", %{"event" => event, "payload" => payload})
    {:noreply, socket}
  end
end
