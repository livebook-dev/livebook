defmodule LivebookWeb.ExploreLive do
  use LivebookWeb, :live_view

  import LivebookWeb.UserHelpers
  import LivebookWeb.SessionHelpers

  @impl true
  def mount(_params, %{"current_user_id" => current_user_id}, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "users:#{current_user_id}")
    end

    current_user = build_current_user(current_user_id, socket)

    [lead_notebook_info | notebook_infos] = Livebook.Notebook.Explore.notebook_infos()

    {:ok,
     assign(socket,
       current_user: current_user,
       lead_notebook_info: lead_notebook_info,
       notebook_infos: notebook_infos
     )}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex flex-grow h-full">
      <div class="w-16 flex flex-col items-center space-y-5 px-3 py-7 bg-gray-900">
        <%= live_patch to: Routes.home_path(@socket, :page) do %>
          <img src="/logo.png" height="40" width="40" alt="livebook" />
        <% end %>
        <div class="flex-grow"></div>
        <span class="tooltip right distant" aria-label="User profile">
          <%= live_patch to: Routes.explore_path(@socket, :user),
                class: "text-gray-400 rounded-xl h-8 w-8 flex items-center justify-center" do %>
            <%= render_user_avatar(@current_user, class: "h-full w-full", text_class: "text-xs") %>
          <% end %>
        </span>
      </div>
      <div class="flex-grow px-6 py-8 overflow-y-auto">
        <div class="max-w-screen-md w-full mx-auto px-4 pb-8 space-y-8">
          <div>
            <div class="relative">
              <%= live_patch to: Routes.home_path(@socket, :page),
                    class: "hidden md:block absolute top-[50%] left-[-12px] transform -translate-y-1/2 -translate-x-full" do %>
                <%= remix_icon("arrow-left-line", class: "text-2xl align-middle") %>
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
          <div class="p-8 bg-gray-900 rounded-2xl flex space-x-4 h-64 shadow-xl">
            <div class="self-end max-w-sm">
              <h3 class="text-xl text-gray-50 font-semibold">
                <%= @lead_notebook_info.notebook.name %>
              </h3>
              <p class="mt-2 text-sm text-gray-300">
                <%= @lead_notebook_info.description %>
              </p>
              <div class="mt-4">
                <button class="button button-blue" phx-click="fork_lead_notebook">
                  Let's go
                </button>
              </div>
            </div>
            <div class="flex-grow hidden md:flex flex items-center justify-center">
              <img src="/logo.png" height="120" width="120" alt="livebook" />
            </div>
          </div>
          <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
            <%= for {info, idx} <- Enum.with_index(@notebook_infos) do %>
              <%= live_component @socket, LivebookWeb.NotebookCardComponent,
                    id: idx,
                    notebook: info.notebook,
                    description: info.description %>
            <% end %>
          </div>
        </div>
      </div>
    </div>

    <%= if @live_action == :user do %>
      <%= live_modal @socket, LivebookWeb.UserComponent,
            id: :user_modal,
            modal_class: "w-full max-w-sm",
            user: @current_user,
            return_to: Routes.explore_path(@socket, :page) %>
    <% end %>
    """
  end

  @impl true
  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_event("fork_lead_notebook", %{}, socket) do
    notebook = socket.assigns.lead_notebook_info.notebook
    {:noreply, create_session(socket, notebook: notebook)}
  end

  @impl true
  def handle_info(
        {:user_change, %{id: id} = user},
        %{assigns: %{current_user: %{id: id}}} = socket
      ) do
    {:noreply, assign(socket, :current_user, user)}
  end
end
