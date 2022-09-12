defmodule LivebookWeb.Hub.EditLive do
  use LivebookWeb, :live_view

  alias LivebookWeb.{PageHelpers, LayoutHelpers}
  alias Livebook.Hubs
  alias Livebook.Hubs.Provider

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(params, _session, socket) do
    {:ok, assign(socket, hub: nil, type: nil, page_title: "Livebook - Hub", params: params)}
  end

  @impl true
  def handle_params(params, _url, socket) do
    hub = Hubs.fetch_hub!(params["id"])
    type = Provider.type(hub)

    if type == "local" do
      {:ok,
       socket |> redirect(to: "/") |> put_flash(:warning, "You can't edit the localhost Hub")}
    else
      {:ok, assign(socket, hub: hub, type: type, page_title: "Livebook - Hub", params: params)}
    end

    {:noreply, assign(socket, hub: hub, type: type, params: params)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <LayoutHelpers.layout
      socket={@socket}
      current_page={Routes.hub_path(@socket, :edit, @hub.id)}
      current_user={@current_user}
      saved_hubs={@saved_hubs}
    >
      <div class="p-4 sm:px-8 md:px-16 sm:py-7 max-w-screen-md mx-auto space-y-8">
        <div>
          <PageHelpers.title text="Edit Hub" socket={@socket} />
        </div>

        <%= if @type == "fly" do %>
          <.live_component
            module={LivebookWeb.Hub.Edit.FlyComponent}
            hub={@hub}
            id="fly-form"
            live_action={@live_action}
            params={@params}
          />
        <% end %>
      </div>
    </LayoutHelpers.layout>
    """
  end
end
