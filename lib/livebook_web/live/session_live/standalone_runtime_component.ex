defmodule LivebookWeb.SessionLive.StandaloneRuntimeComponent do
  use LivebookWeb, :live_component

  alias Livebook.{Session, Runtime}

  @impl true
  def mount(socket) do
    unless Livebook.Config.runtime_enabled?(Livebook.Runtime.Standalone) do
      raise "runtime module not allowed"
    end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex-col space-y-5">
      <p class="text-gray-700">
        Start a new local Elixir node to evaluate code. Whenever you reconnect this runtime,
        a fresh node is started.
      </p>
      <.button phx-click="init" phx-target={@myself} disabled={@runtime_status == :connecting}>
        <%= label(@runtime, @runtime_status) %>
      </.button>
    </div>
    """
  end

  defp label(%Runtime.Standalone{}, :connecting), do: "Connecting..."
  defp label(%Runtime.Standalone{}, :connected), do: "Reconnect"
  defp label(_runtime, _runtime_status), do: "Connect"

  @impl true
  def handle_event("init", _params, socket) do
    runtime = Runtime.Standalone.new()
    Session.set_runtime(socket.assigns.session.pid, runtime)
    Session.connect_runtime(socket.assigns.session.pid)
    {:noreply, socket}
  end
end
