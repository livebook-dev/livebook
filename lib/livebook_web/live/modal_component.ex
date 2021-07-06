defmodule LivebookWeb.ModalComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="fixed z-[10000] inset-0">

      <!-- Modal container -->
      <div class="h-screen flex items-center justify-center p-4">
        <!-- Overlay -->
        <div class="absolute inset-0 bg-gray-500 opacity-75 z-0"
          aria-hidden="true"
          phx-capture-click="close"
          phx-window-keydown="close"
          phx-key="escape"
          phx-target={@myself}
          phx-page-loading></div>

        <!-- Modal box -->
        <div class={"relative max-h-full overflow-y-auto bg-white rounded-lg shadow-xl #{@modal_class}"}
          role="dialog"
          aria-modal="true">

          <%= live_patch to: @return_to, class: "absolute top-6 right-6 text-gray-400 flex space-x-1 items-center" do %>
            <span class="text-sm">(esc)</span>
            <.remix_icon icon="close-line" class="text-2xl" />
          <% end %>

          <%= live_component @component, @opts %>
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
