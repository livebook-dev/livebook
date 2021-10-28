defmodule LivebookWeb.SessionLive.ElixirStandaloneLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Runtime}
  alias LivebookWeb.SessionLive.RuntimeHelpers

  @impl true
  def mount(_params, %{"session" => session, "current_runtime" => current_runtime}, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
    end

    {:ok, assign(socket, session: session, current_runtime: current_runtime, error_message: nil)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex-col space-y-5">
      <%= if @error_message do %>
        <div class="error-box">
          <%= @error_message %>
        </div>
      <% end %>
      <RuntimeHelpers.default_runtime_note module={Runtime.ElixirStandalone} />
      <p class="text-gray-700">
        Start a new local node to handle code evaluation.
      </p>
      <button class="button button-blue" phx-click="init">
        <%= if(matching_runtime?(@current_runtime), do: "Reconnect", else: "Connect") %>
      </button>
    </div>
    """
  end

  defp matching_runtime?(%Runtime.ElixirStandalone{}), do: true
  defp matching_runtime?(_runtime), do: false

  @impl true
  def handle_event("init", _params, socket) do
    case Runtime.ElixirStandalone.init() do
      {:ok, runtime} ->
        Session.connect_runtime(socket.assigns.session.pid, runtime)
        {:noreply, assign(socket, error_message: nil)}

      {:error, message} ->
        {:noreply, assign(socket, error_message: message)}
    end
  end

  @impl true
  def handle_info({:operation, {:set_runtime, _pid, runtime}}, socket) do
    {:noreply, assign(socket, current_runtime: runtime)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}
end
