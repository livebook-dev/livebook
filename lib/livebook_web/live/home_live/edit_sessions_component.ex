defmodule LivebookWeb.HomeLive.EditSessionsComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        <%= title(@action) %>
      </h3>
      <.message action={@action} selected_sessions={@selected_sessions} sessions={@sessions}/>
      <div class="mt-8 flex justify-end space-x-2">
        <button class="button-base button-red" phx-click={@action} phx-target={@myself}>
          <.remix_icon icon="close-circle-line" class="align-middle mr-1" />
          <%= button_label(@action) %>
        </button>
        <%= live_patch "Cancel", to: @return_to, class: "button-base button-outlined-gray" %>
      </div>
    </div>
    """
  end

  defp message(%{action: "close_all"} = assigns) do
    ~H"""
    <p class="text-gray-700">
      Are you sure you want to close <%= length(assigns.selected_sessions) %> sections?
      <%= if not_persisted(assigns) > 0 do %>
      <br/>
        <span class="font-medium">Important:</span>
        <%= not_persisted(assigns) %> notebooks are not persisted and their content will be lost.
      <% end %>
    </p>
    """
  end

  defp message(%{action: "disconnect"} = assigns) do
    ~H"""
    <p class="text-gray-700">
      Are you sure you want to disconnect <%= length(@selected_sessions) %> sections?
    </p>
    """
  end

  @impl true
  def handle_event("close_all", %{}, socket) do
    socket.assigns
    |> selected_sessions()
    |> Enum.map(&Livebook.Session.close(&1.pid))

    {:noreply, push_patch(socket, to: socket.assigns.return_to, replace: true)}
  end

  def handle_event("disconnect", %{}, socket) do
    socket.assigns
    |> selected_sessions()
    |> Enum.reject(&(&1.memory_usage.runtime == nil))
    |> Enum.map(&Livebook.Session.disconnect_runtime(&1.pid))

    {:noreply, push_patch(socket, to: socket.assigns.return_to, replace: true)}
  end

  defp selected_sessions(assigns) do
    assigns.sessions
    |> Enum.filter(&(&1.id in assigns.selected_sessions))
  end

  defp button_label("close_all"), do: "Close sessions"
  defp button_label("disconnect"), do: "Disconnect runtime"

  defp title("close_all"), do: "Close sessions"
  defp title("disconnect"), do: "Disconnect runtime"

  defp not_persisted(assigns) do
    assigns
    |> selected_sessions()
    |> Enum.count(&(!&1.file))
  end
end
