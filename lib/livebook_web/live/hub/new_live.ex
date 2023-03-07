defmodule LivebookWeb.Hub.NewLive do
  use LivebookWeb, :live_view

  alias LivebookWeb.LayoutHelpers
  alias Phoenix.LiveView.JS

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(_params, _session, socket) do
    enabled? = Livebook.Config.feature_flag_enabled?(:create_hub)
    {:ok, assign(socket, selected_type: nil, page_title: "Livebook - Hub", enabled?: enabled?)}
  end

  @impl true
  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def render(%{enabled?: false} = assigns) do
    ~H"""
    TODO
    """
  end

  def render(assigns) do
    ~H"""
    <LayoutHelpers.layout current_page="/hub" current_user={@current_user} saved_hubs={@saved_hubs}>
      <div class="p-4 md:px-12 md:py-7 max-w-screen-md mx-auto space-y-8">
        <div>
          <LayoutHelpers.title text="Add Hub" />
          <p class="mt-4 text-gray-700">
            Manage your Livebooks in the cloud with Hubs.
          </p>
        </div>

        <div class="flex flex-col space-y-4">
          <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
            1. Select your Hub service
          </h2>

          <div class="grid grid-cols-1 sm:grid-cols-2 gap-4">
            <.card_item id="fly" selected={@selected_type} title="Fly">
              <:logo>
                <%= Phoenix.HTML.raw(File.read!("static/images/fly.svg")) %>
              </:logo>
              <:headline>
                Deploy notebooks to your Fly account.
              </:headline>
            </.card_item>

            <.card_item id="enterprise" selected={@selected_type} title="Livebook Enterprise">
              <:logo>
                <img
                  src="/images/enterprise.png"
                  class="max-h-full max-w-[75%]"
                  alt="Livebook Enterprise logo"
                />
              </:logo>
              <:headline>
                Control access, manage secrets, and deploy notebooks within your team.
              </:headline>
            </.card_item>
          </div>
        </div>

        <div :if={@selected_type} class="flex flex-col space-y-4">
          <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
            2. Configure your Hub
          </h2>

          <.live_component
            :if={@selected_type == "fly"}
            module={LivebookWeb.Hub.New.FlyComponent}
            id="fly-form"
          />

          <.live_component
            :if={@selected_type == "enterprise"}
            module={LivebookWeb.Hub.New.EnterpriseComponent}
            id="enterprise-form"
          />
        </div>
      </div>
    </LayoutHelpers.layout>
    """
  end

  defp card_item(assigns) do
    ~H"""
    <div
      id={@id}
      class="flex flex-col cursor-pointer"
      phx-click={JS.push("select_type", value: %{value: @id})}
    >
      <div class={[
        "flex items-center justify-center p-6 border-2 rounded-t-2xl h-[150px]",
        if(@id == @selected, do: "border-gray-200", else: "border-gray-100")
      ]}>
        <%= render_slot(@logo) %>
      </div>
      <div class={[
        "px-6 py-4 rounded-b-2xl grow",
        if(@id == @selected, do: "bg-gray-200", else: "bg-gray-100")
      ]}>
        <p class="text-gray-800 font-semibold cursor-pointer">
          <%= @title %>
        </p>

        <p class="mt-2 text-sm text-gray-600">
          <%= render_slot(@headline) %>
        </p>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("select_type", %{"value" => service}, socket) do
    {:noreply, assign(socket, selected_type: service)}
  end
end
