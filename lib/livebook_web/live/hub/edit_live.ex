defmodule LivebookWeb.Hub.EditLive do
  use LivebookWeb, :live_view

  alias LivebookWeb.LayoutHelpers
  alias Livebook.Hubs
  alias Livebook.Hubs.Provider

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       hub: nil,
       secrets: [],
       type: nil,
       page_title: "Livebook - Hub",
       env_var_id: nil
     )}
  end

  @impl true
  def handle_params(params, _url, socket) do
    Livebook.Hubs.subscribe([:secrets])
    hub = Hubs.fetch_hub!(params["id"])
    type = Provider.type(hub)

    {:noreply,
     assign(socket,
       hub: hub,
       type: type,
       secrets: Hubs.get_secrets(hub),
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
              secrets={@secrets}
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

  @impl true
  def handle_info({:secret_created, _secret}, socket) do
    {:noreply, refresh_secrets(socket)}
  end

  def handle_info({:secret_updated, _secret}, socket) do
    {:noreply, refresh_secrets(socket)}
  end

  def handle_info({:secret_deleted, _secret}, socket) do
    {:noreply, refresh_secrets(socket)}
  end

  def handle_info(_message, socket) do
    {:noreply, socket}
  end

  defp refresh_secrets(socket) do
    assign(socket, secrets: Livebook.Hubs.get_secrets(socket.assigns.hub))
  end
end
