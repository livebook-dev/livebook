defmodule LivebookWeb.HomeLive.CloseSessionComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.HomeLive.SessionListComponent, only: [toggle_edit: 1]

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
        <br/>
        <%= if @session.file,
              do: "This won't delete any persisted files.",
              else: "The notebook is not persisted and content may be lost." %>
      </p>
      <div class="mt-8 flex justify-end space-x-2">
        <button class="button-base button-red" role="button"
          phx-click={toggle_edit(:off) |> JS.push("close", target: @myself)}>
          <.remix_icon icon="close-circle-line" class="align-middle mr-1" />
          Close session
        </button>
        <%= live_patch "Cancel", to: @return_to, class: "button-base button-outlined-gray" %>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("close", %{}, socket) do
    Livebook.Session.close(socket.assigns.session.pid)
    {:noreply, push_patch(socket, to: socket.assigns.return_to, replace: true)}
  end
end
