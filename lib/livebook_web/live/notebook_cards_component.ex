defmodule LivebookWeb.NotebookCardsComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    assigns = assign_new(assigns, :card_icon, fn -> nil end)

    ~H"""
    <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-5" role="group">
      <%= for {info, idx} <- Enum.with_index(@notebook_infos) do %>
        <div
          class="flex flex-col p-4 border bg-gray-50 border-gray-300 rounded-lg"
          data-test-idx={idx}
        >
          <div class="flex items-center justify-between">
            <span class="tooltip top" data-tooltip={info.file.path}>
              <span class="text-gray-800 font-medium">
                <%= info.name %>
              </span>
            </span>
            <%= @card_icon && render_slot(@card_icon, {info, idx}) %>
          </div>
          <div class="mt-1 flex-grow text-gray-600 text-sm">
            <%= @added_at_label %> <%= format_datetime_relatively(info.added_at) %> ago
          </div>
          <div class="mt-2 flex space-x-6">
            <%= if session = session_by_file(info.file, @sessions) do %>
              <.link navigate={~p"/sessions/#{session.id}"} class="text-blue-600 font-medium">
                Join session
              </.link>
            <% else %>
              <button
                class="text-blue-600 font-medium"
                phx-click={JS.push("open", value: %{idx: idx}, target: @myself)}
              >
                Open
              </button>
            <% end %>
            <button
              class="text-blue-600 font-medium"
              phx-click={JS.push("fork", value: %{idx: idx}, target: @myself)}
            >
              Fork
            </button>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("open", %{"idx" => idx}, socket) do
    %{file: file} = Enum.at(socket.assigns.notebook_infos, idx)

    if file_running?(file, socket.assigns.sessions) do
      session = session_by_file(file, socket.assigns.sessions)
      {:noreply, push_navigate(socket, to: ~p"/sessions/#{session.id}")}
    else
      send(self(), {:open, file})
      {:noreply, socket}
    end
  end

  def handle_event("fork", %{"idx" => idx}, socket) do
    %{file: file} = Enum.at(socket.assigns.notebook_infos, idx)

    send(self(), {:fork, file})

    {:noreply, socket}
  end

  defp file_running?(file, sessions) do
    Enum.any?(sessions, &(&1.file == file))
  end

  defp session_by_file(file, sessions) do
    Enum.find(sessions, &(&1.file == file))
  end
end
