defmodule LivebookWeb.Hub.EditLive do
  use LivebookWeb, :live_view

  alias LivebookWeb.LayoutHelpers
  alias Livebook.Hubs
  alias Livebook.Hubs.Provider

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, hub: nil, type: nil, page_title: "Livebook - Hub", env_var_id: nil)}
  end

  @impl true
  def handle_params(params, _url, socket) do
    hub = Hubs.fetch_hub!(params["id"])
    type = Provider.type(hub)

    {:noreply,
     assign(socket,
       hub: hub,
       type: type,
       page_title: "Livebook - Hub",
       params: params,
       env_var_id: params["env_var_id"]
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <LayoutHelpers.layout
      current_page={~p"/hub/#{@hub.id}"}
      current_user={@current_user}
      saved_hubs={@saved_hubs}
    >
      <div class="p-4 md:px-12 md:py-7 max-w-screen-md mx-auto">
        <%= case @type do %>
          <% "fly" -> %>
            <.live_component
              module={LivebookWeb.Hub.Edit.FlyComponent}
              hub={@hub}
              id="fly-form"
              live_action={@live_action}
              env_var_id={@env_var_id}
            />
          <% "personal" -> %>
            <.live_component
              module={LivebookWeb.Hub.Edit.PersonalComponent}
              hub={@hub}
              id="personal-form"
            />
          <% "enterprise" -> %>
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

  @impl true
  def handle_event("delete_hub", %{"id" => id}, socket) do
    Hubs.delete_hub(id)

    {:noreply,
     socket
     |> put_flash(:success, "Hub deleted successfully")
     |> push_navigate(to: "/")}
  end
end
