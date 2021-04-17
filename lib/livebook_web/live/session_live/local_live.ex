defmodule LivebookWeb.SessionLive.LocalLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Runtime}

  @impl true
  def mount(_params, %{"session_id" => session_id, "current_runtime" => current_runtime}, socket) do
    {:ok, assign(socket, session_id: session_id, output: nil, current_runtime: current_runtime)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex-col space-y-5">
      <p class="text-gray-700">
        Connect to the node running the Livebook application.
      </p>
      <p class="text-gray-700">
        IMPORTANT: Any module that you define will be defined forever and
        code in one notebook can conflict with code from another notebook.
      </p>
      <%= form_for :node, "#", phx_submit: "init" %>
        <%= submit "Connect", class: "mt-5 button button-blue" %>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("init", _params, socket) do
    {:ok, runtime} = Runtime.Local.init()
    Session.connect_runtime(socket.assigns.session_id, runtime)
    {:noreply, socket}
  end

  @impl true
  def handle_info({:operation, {:set_runtime, _pid, runtime}}, socket) do
    {:noreply, assign(socket, current_runtime: runtime)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}
end
