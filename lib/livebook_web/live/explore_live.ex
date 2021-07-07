defmodule LivebookWeb.ExploreLive do
  use LivebookWeb, :live_view

  import LivebookWeb.UserHelpers
  import LivebookWeb.SessionHelpers

  alias LivebookWeb.{SidebarHelpers, ExploreHelpers}
  alias Livebook.Notebook.Explore

  @impl true
  def mount(_params, %{"current_user_id" => current_user_id} = session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "users:#{current_user_id}")
    end

    current_user = build_current_user(session, socket)

    [lead_notebook_info | notebook_infos] = Explore.notebook_infos()

    {:ok,
     assign(socket,
       current_user: current_user,
       lead_notebook_info: lead_notebook_info,
       notebook_infos: notebook_infos
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-grow h-full">
      <SidebarHelpers.sidebar>
        <SidebarHelpers.logo_item socket={@socket} />
        <SidebarHelpers.break_item />
        <SidebarHelpers.user_item current_user={@current_user} path={Routes.explore_path(@socket, :user)} />
      </SidebarHelpers.sidebar>
      <div class="flex-grow px-6 py-8 overflow-y-auto">
        <div class="max-w-screen-md w-full mx-auto px-4 pb-8 space-y-8">
          <div>
            <div class="relative">
              <%= live_patch to: Routes.home_path(@socket, :page),
                    class: "hidden md:block absolute top-[50%] left-[-12px] transform -translate-y-1/2 -translate-x-full" do %>
                <.remix_icon icon="arrow-left-line" class="text-2xl align-middle" />
              <% end %>
              <h1 class="text-3xl text-gray-800 font-semibold">
                Explore
              </h1>
            </div>
            <p class="mt-4 text-gray-700">
              Check out a number of examples showcasing various parts of the Elixir ecosystem.
              Click on any notebook you like and start playing around with it!
            </p>
          </div>
          <div class="p-8 bg-gray-900 rounded-2xl flex space-x-4 shadow-xl">
            <div class="self-end max-w-sm">
              <h3 class="text-xl text-gray-50 font-semibold">
                <%= @lead_notebook_info.title %>
              </h3>
              <p class="mt-2 text-sm text-gray-300">
                <%= @lead_notebook_info.description %>
              </p>
              <div class="mt-4">
                <%= live_patch "Let's go",
                      to: Routes.explore_path(@socket, :notebook, @lead_notebook_info.slug),
                      class: "button button-blue" %>
              </div>
            </div>
            <div class="flex-grow hidden md:flex flex items-center justify-center">
              <img src={@lead_notebook_info.image_url} height="120" width="120" alt="livebook" />
            </div>
          </div>
          <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
            <%# Note: it's fine to use stateless components in this comprehension,
                because @notebook_infos never change %>
            <%= for info <- @notebook_infos do %>
              <ExploreHelpers.notebook_card notebook_info={info} socket={@socket} />
            <% end %>
          </div>
        </div>
      </div>
    </div>

    <%= if @live_action == :user do %>
      <%= live_modal LivebookWeb.UserComponent,
            id: "user",
            modal_class: "w-full max-w-sm",
            user: @current_user,
            return_to: Routes.explore_path(@socket, :page) %>
    <% end %>
    """
  end

  @impl true
  def handle_params(%{"slug" => "new"}, _url, socket) do
    {:noreply, create_session(socket)}
  end

  def handle_params(%{"slug" => slug}, _url, socket) do
    {notebook, images} = Explore.notebook_by_slug!(slug)
    {:noreply, create_session(socket, notebook: notebook, images: images)}
  end

  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_info(
        {:user_change, %{id: id} = user},
        %{assigns: %{current_user: %{id: id}}} = socket
      ) do
    {:noreply, assign(socket, :current_user, user)}
  end
end
