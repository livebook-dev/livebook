defmodule LivebookWeb.SessionLive.SharingComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, shared_mode_url: "")}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Share to session
      </h3>
      <div class="w-full flex-col space-y-8">
        <div class="flex">
          <form phx-change="set_options" onsubmit="return false;" class="flex flex-col space-y-4 items-start max-w-full">
            <div class="flex space-x-2 items-center max-w-full">
              <button class="button-base button-gray button-small"
                phx-click="enable_share_mode"
                phx-target={@myself}>
                share mode
              </button>
            </div>
            <div class="flex flex-col">
              <div class="mt-6 text-gray-500 text-sm">
                <div class="flex space-x-2 items-center max-w-full">
                  <%= if @shared_mode_url && @shared_mode_url != "" do %>
                    <span class="text-gray-700 whitespace-nowrap">URL: <%= @shared_mode_url %></span>
                    <span class="tooltip top" data-tooltip="Link">
                      <a href={@shared_mode_url} class="icon-button"
                        role="button"
                        aria-label="link to cell">
                        <.remix_icon icon="link" class="text-xl" />
                      </a>
                    </span>
                  <% end %>
                </div>
              </div>
            </div>
          </form>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("enable_share_mode", _, socket) do
    shared_mode_url =
      LivebookWeb.Endpoint.access_struct_url()
      |> URI.parse()
      |> Map.put(:path, "/public/sessions/#{socket.assigns.session.id}")
      |> URI.to_string()

    {:noreply, assign(socket, shared_mode_url: shared_mode_url)}
  end
end
