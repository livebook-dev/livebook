defmodule LivebookWeb.HomeLive.CloseSessionComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Close session
      </h3>
      <p class="text-gray-700">
        Are you sure you want to close this section -
        <span class="font-semibold">“<%= @session.notebook_name %>”</span>?
        This won't delete any persisted files.
      </p>
      <div class="mt-8 flex justify-end space-x-2">
        <button class="button button-red" phx-click="close" phx-target={@myself}>
          <.remix_icon icon="close-circle-line" class="align-middle mr-1" />
          Close session
        </button>
        <%= live_patch "Cancel", to: @return_to, class: "button button-outlined-gray" %>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("close", %{}, socket) do
    Livebook.Session.close(socket.assigns.session.pid)
    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end
end
