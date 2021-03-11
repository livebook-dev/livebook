defmodule LivebookWeb.SessionLive.ElixirStandaloneLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Runtime}

  @impl true
  def mount(_params, %{"session_id" => session_id}, socket) do
    {:ok, assign(socket, session_id: session_id, output: nil)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex-col space-y-3">
      <p class="text-gray-500">
        Start a new local node to handle code evaluation.
        This is the default runtime and is started automatically
        as soon as you evaluate the first cell.
      </p>
      <button class="button-base" phx-click="init">
        Connect
      </button>
      <%= if @output do %>
        <div class="markdown max-h-20 overflow-y-auto tiny-scrollbar">
          <pre><code><%= @output %></code></pre>
        </div>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("init", _params, socket) do
    {:ok, runtime} = Runtime.ElixirStandalone.init()
    Session.connect_runtime(socket.assigns.session_id, runtime)
    {:noreply, socket}
  end
end
