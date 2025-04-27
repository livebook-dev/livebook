defmodule LivebookWeb.SessionLive.EmbeddedRuntimeComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    unless Livebook.Config.runtime_enabled?(Livebook.Runtime.Embedded) do
      raise "runtime module not allowed"
    end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex-col space-y-5">
      <p class="text-gray-700">
        Run the notebook code within the Livebook node itself.
        This is reserved for specific cases where there is no option
        of starting a separate Elixir runtime (for example, on embedded
        devices or cases where the amount of memory available is
        limited). Prefer the "Standalone" runtime whenever possible.
      </p>
      <p class="text-gray-700">
        <span class="font-semibold">Warning:</span>
        any module that you define will be defined globally until
        you restart Livebook. Furthermore, code in one notebook
        may interfere with code from another notebook.
      </p>
      <.button phx-click="init" phx-target={@myself} disabled={@runtime_status == :connecting}>
        {label(@runtime, @runtime_status)}
      </.button>
    </div>
    """
  end

  defp label(%Livebook.Runtime.Embedded{}, :connecting), do: "Connecting..."
  defp label(%Livebook.Runtime.Embedded{}, :connected), do: "Reconnect"
  defp label(_runtime, _runtime_status), do: "Connect"

  @impl true
  def handle_event("init", _params, socket) do
    runtime = Livebook.Runtime.Embedded.new()
    Livebook.Session.set_runtime(socket.assigns.session.pid, runtime)
    Livebook.Session.connect_runtime(socket.assigns.session.pid)
    {:noreply, socket}
  end
end
