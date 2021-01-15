defmodule LiveBookWeb.SessionLive do
  use LiveBookWeb, :live_view

  alias LiveBook.{SessionSupervisor, Session}

  @impl true
  def mount(%{"id" => session_id}, _session, socket) do
    if SessionSupervisor.session_exists?(session_id) do
      if connected?(socket) do
        Session.register_client(session_id, self())
        Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")
      end

      data = Session.get_data(session_id)

      {:ok, assign(socket, session_id: session_id, data: data)}
    else
      {:ok, redirect(socket, to: Routes.live_path(socket, LiveBookWeb.SessionsLive))}
    end
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="container max-w-screen-md p-4 mx-auto">
      Session <%= @session_id %>
    </div>
    """
  end

  @impl true
  def handle_info({:operation, operation}, socket) do
    case Session.Data.apply_operation(socket.assigns.data, operation) do
      {:ok, data, _actions} ->
        {:noreply, assign(socket, data: data)}

      :error ->
        {:noreply, socket}
    end
  end
end
