defmodule LivebookWeb.SessionLive.EmbeddedLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Runtime}
  alias LivebookWeb.SessionLive.RuntimeHelpers

  @impl true
  def mount(_params, %{"session" => session, "current_runtime" => current_runtime}, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
    end

    {:ok, assign(socket, session: session, current_runtime: current_runtime)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex-col space-y-5">
      <RuntimeHelpers.default_runtime_note module={Runtime.Embedded} />
      <p class="text-gray-700">
        Run the notebook code within the Livebook node itself.
        This is reserved for specific cases where there is no option
        of starting a separate Elixir runtime (for example, on embedded
        devices or cases where the amount of memory available is
        limited). Prefer the "Elixir standalone" runtime whenever possible.
      </p>
      <p class="text-gray-700">
        <span class="font-semibold">Warning:</span>
        any module that you define will be defined globally until
        you restart Livebook. Furthermore, code in one notebook
        may interfere with code from another notebook.
      </p>
      <button class="button button-blue" phx-click="init">
        <%= if(matching_runtime?(@current_runtime), do: "Reconnect", else: "Connect") %>
      </button>
    </div>
    """
  end

  defp matching_runtime?(%Runtime.Embedded{}), do: true
  defp matching_runtime?(_runtime), do: false

  @impl true
  def handle_event("init", _params, socket) do
    {:ok, runtime} = Runtime.Embedded.init()
    Session.connect_runtime(socket.assigns.session.pid, runtime)
    {:noreply, socket}
  end

  @impl true
  def handle_info({:operation, {:set_runtime, _pid, runtime}}, socket) do
    {:noreply, assign(socket, current_runtime: runtime)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}
end
