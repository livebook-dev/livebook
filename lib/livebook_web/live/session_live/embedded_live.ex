defmodule LivebookWeb.SessionLive.EmbeddedLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Runtime}

  @impl true
  def mount(_params, %{"session" => session}, socket) do
    {:ok, assign(socket, session: session)}
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
        limited). Prefer the "Elixir standalone" runtime whenever possible.
      </p>
      <p class="text-gray-700">
        <span class="font-semibold">Warning:</span>
        any module that you define will be defined globally until
        you restart Livebook. Furthermore, code in one notebook
        may interfere with code from another notebook.
      </p>
      <button class="button button-blue" phx-click="init">
        Connect
      </button>
    </div>
    """
  end

  @impl true
  def handle_event("init", _params, socket) do
    {:ok, runtime} = Runtime.Embedded.init()
    Session.connect_runtime(socket.assigns.session.pid, runtime)
    {:noreply, socket}
  end
end
