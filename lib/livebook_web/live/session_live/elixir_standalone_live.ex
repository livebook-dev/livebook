defmodule LivebookWeb.SessionLive.ElixirStandaloneLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Runtime}

  @impl true
  def mount(_params, %{"session_id" => session_id, "current_runtime" => current_runtime}, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
    end

    {:ok, assign(socket, session_id: session_id, output: nil, current_runtime: current_runtime)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex-col space-y-3">
      <p class="text-gray-700">
        Start a new local node to handle code evaluation.
        This is the default runtime and is started automatically
        as soon as you evaluate the first cell.
      </p>
      <button class="button button-primary" phx-click="init">
        <%= if(matching_runtime?(@current_runtime), do: "Reconnect", else: "Connect") %>
      </button>
      <%= if @output do %>
        <div class="markdown max-h-20 overflow-y-auto tiny-scrollbar">
          <pre><code><%= @output %></code></pre>
        </div>
      <% end %>
    </div>
    """
  end

  defp matching_runtime?(%Runtime.ElixirStandalone{}), do: true

  defp matching_runtime?(_runtime), do: false

  @impl true
  def handle_event("init", _params, socket) do
    {:ok, runtime} = Runtime.ElixirStandalone.init()
    Session.connect_runtime(socket.assigns.session_id, runtime)
    {:noreply, socket}
  end

  @impl true
  def handle_info({:operation, {:set_runtime, _pid, runtime}}, socket) do
    {:noreply, assign(socket, current_runtime: runtime)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}
end
