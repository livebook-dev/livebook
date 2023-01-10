defmodule LivebookWeb.Hub.EditLive do
  use LivebookWeb, :live_view

  alias LivebookWeb.{PageHelpers, LayoutHelpers}
  alias Livebook.Hubs
  alias Livebook.Hubs.Provider

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, hub: nil, type: nil, page_title: "Livebook - Hub", env_var_id: nil)}
  end

  @impl true
  def handle_params(params, _url, socket) do
    hub = Hubs.get_hub!(params["id"])
    type = Provider.type(hub)

    if type == "local" do
      {:noreply,
       socket
       |> redirect(to: "/")
       |> put_flash(:warning, "You can't edit the localhost Hub")}
    else
      {:noreply,
       assign(socket,
         hub: hub,
         type: type,
         page_title: "Livebook - Hub",
         params: params,
         env_var_id: params["env_var_id"]
       )}
    end
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
      <div class="p-4 md:px-12 md:py-7 max-w-screen-md mx-auto space-y-8">
        <div>
          <PageHelpers.title text="Edit Hub" socket={@socket} />
        </div>

        <%= if @type == "fly" do %>
          <.live_component
            module={LivebookWeb.Hub.Edit.FlyComponent}
            hub={@hub}
            id="fly-form"
            live_action={@live_action}
            env_var_id={@env_var_id}
          />
        <% end %>

        <%= if @type == "enterprise" do %>
          <.live_component
            module={LivebookWeb.Hub.Edit.EnterpriseComponent}
            hub={@hub}
            id="enterprise-form"
          />
        <% end %>
      </div>
    </LayoutHelpers.layout>
    """
  end
end
