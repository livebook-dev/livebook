defmodule LiveBookWeb.SessionLive do
  use LiveBookWeb, :live_view

  @impl true
  def mount(%{"id" => session_id}, _session, socket) do
    {:ok, assign(socket, session_id: session_id)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="container max-w-screen-md p-4 mx-auto">
      Session <%= @session_id %>
    </div>
    """
  end
end
