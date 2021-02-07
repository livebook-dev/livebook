defmodule LiveBookWeb.ModalComponent do
  use LiveBookWeb, :live_component

  @impl true
  def render(assigns) do
    ~L"""
    <div class="fixed z-10 inset-0"
      id="<%= @id %>">

      <!-- Modal container -->
      <div class="h-screen flex items-center justify-center p-4">
        <!-- Overlay -->
        <div class="absolute inset-0 bg-gray-500 opacity-75 z-0"
          aria-hidden="true"
          phx-capture-click="close"
          phx-window-keydown="close"
          phx-key="escape"
          phx-target="#<%= @id %>"
          phx-page-loading></div>

        <!-- Modal box -->
        <div class="relative max-h-full overflow-y-auto bg-white rounded-md shadow-xl sm:max-w-2xl sm:w-full"
          role="dialog"
          aria-modal="true">

          <%= live_component @socket, @component, @opts %>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("close", _params, socket) do
    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end
end
