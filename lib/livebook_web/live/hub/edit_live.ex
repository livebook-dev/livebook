defmodule LivebookWeb.Hub.EditLive do
  use LivebookWeb, :live_view

  alias LivebookWeb.LayoutComponents
  alias Livebook.Hubs
  alias Livebook.Hubs.Provider

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Hubs.Broadcasts.subscribe([:connection])

      Livebook.Teams.Broadcasts.subscribe([:deployment_groups, :app_deployments, :agents])
    end

    {:ok,
     assign(socket,
       hub: nil,
       counter: 0,
       type: nil,
       page_title: "Workspace - Livebook",
       params: %{}
     )}
  end

  @impl true
  def handle_params(params, _url, socket) do
    {id, params} = Map.pop(params, "id")

    {:noreply,
     socket
     |> load_hub(id)
     |> assign(:params, params)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <LayoutComponents.layout
      current_page={~p"/hub/#{@hub.id}"}
      current_user={@current_user}
      saved_hubs={@saved_hubs}
    >
      <.hub_component
        type={@type}
        hub={@hub}
        counter={@counter}
        live_action={@live_action}
        params={@params}
      />
    </LayoutComponents.layout>
    """
  end

  defp hub_component(%{type: "personal"} = assigns) do
    ~H"""
    <.live_component
      module={LivebookWeb.Hub.Edit.PersonalComponent}
      hub={@hub}
      params={@params}
      counter={@counter}
      live_action={@live_action}
      id="personal-form"
    />
    """
  end

  defp hub_component(%{type: "team"} = assigns) do
    ~H"""
    <.live_component
      module={LivebookWeb.Hub.Edit.TeamComponent}
      hub={@hub}
      live_action={@live_action}
      params={@params}
      counter={@counter}
      id="team-form"
    />
    """
  end

  @impl true
  def handle_event("delete_hub", %{"id" => id}, socket) do
    on_confirm = fn socket ->
      Hubs.delete_hub(id)

      socket
      |> put_flash(:success, "Workspace deleted successfully")
      |> push_navigate(to: ~p"/")
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Delete workspace",
       description: "Are you sure you want to delete this workspace?",
       confirm_text: "Delete",
       confirm_icon: "close-circle-line"
     )}
  end

  @impl true
  def handle_info({:hub_connected, id}, %{assigns: %{hub: %{id: id}}} = socket) do
    {:noreply, load_hub(socket, id)}
  end

  def handle_info({_event, id, _reason}, %{assigns: %{hub: %{id: id}}} = socket) do
    {:noreply, load_hub(socket, id)}
  end

  def handle_info({_event, %{hub_id: id}}, %{assigns: %{hub: %{id: id}}} = socket) do
    {:noreply, load_hub(socket, id)}
  end

  def handle_info({:hub_changed, id}, %{assigns: %{hub: %{id: id}}} = socket) do
    {:noreply, load_hub(socket, id)}
  end

  def handle_info(_message, socket) do
    {:noreply, socket}
  end

  defp load_hub(socket, id) do
    hub = Hubs.fetch_hub!(id)
    type = Provider.type(hub)

    socket
    |> assign(hub: hub, type: type)
    # Hub-specific components load data, such as secrets and we use
    # a counter to force re-render on every patch.
    |> update(:counter, &(&1 + 1))
  end
end
