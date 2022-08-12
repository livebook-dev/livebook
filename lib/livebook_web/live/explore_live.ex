defmodule LivebookWeb.ExploreLive do
  use LivebookWeb, :live_view

  import LivebookWeb.SessionHelpers
  import LivebookWeb.UserHelpers

  alias LivebookWeb.{SidebarHelpers, ExploreHelpers, PageHelpers}
  alias Livebook.Notebook.Explore

  @impl true
  def mount(_params, _session, socket) do
    [lead_notebook_info | notebook_infos] = Explore.visible_notebook_infos()

    {:ok,
     socket
     |> SidebarHelpers.sidebar_handlers()
     |> assign(
       lead_notebook_info: lead_notebook_info,
       notebook_infos: notebook_infos,
       page_title: "Livebook - Explore"
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex grow h-full">
      <SidebarHelpers.sidebar
        socket={@socket}
        current_page={Routes.explore_path(@socket, :page)}
        current_user={@current_user}
      />
      <div class="grow overflow-y-auto">
        <div class="flex items-center justify-between sticky sm:pt-1 px-2 top-0 left-0 right-0 z-[500] sm:border-b-0 bg-white border-b border-gray-200">
          <div class="sm:hidden my-2 text-2xl text-gray-400 hover:text-gray-600 focus:text-gray-600 rounded-xl h-10 w-10 flex items-center justify-center">
            <button
              aria-label="hide sidebar"
              data-el-toggle-sidebar
              phx-click={
                JS.add_class("hidden sm:flex", to: "[data-el-sidebar]")
                |> JS.toggle(to: "[data-el-toggle-sidebar]", display: "flex")
              }
            >
              <.remix_icon icon="menu-fold-line" />
            </button>
            <button
              class="hidden"
              aria-label="show sidebar"
              data-el-toggle-sidebar
              phx-click={
                JS.remove_class("hidden sm:flex", to: "[data-el-sidebar]")
                |> JS.toggle(to: "[data-el-toggle-sidebar]", display: "flex")
              }
            >
              <.remix_icon icon="menu-unfold-line" />
            </button>
          </div>
          <div class="hidden items-center justify-end p-2 sm:p-0" data-el-toggle-sidebar>
            <button arial-label="new-notebook" class="sm:hidden ">
              <%= live_redirect to: Routes.home_path(@socket, :page), class: "flex items-center text-gray-400" do %>
                <.remix_icon icon="home-6-line" />
                <span class=" text-gray-400 p-2 right-[1.5rem]">
                  Home
                </span>
              <% end %>
            </button>
          </div>
        </div>
        <div class="grow px-4 sm:px-8 md:px-16 pt-4 sm:py-7 overflow-y-auto">
          <div class="max-w-screen-md w-full mx-auto space-y-8">
            <div>
              <PageHelpers.title text="Explore" />
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
                  <%= @lead_notebook_info.details.description %>
                </p>
                <div class="mt-4">
                  <%= live_patch("Let's go",
                    to: Routes.explore_path(@socket, :notebook, @lead_notebook_info.slug),
                    class: "button-base button-blue"
                  ) %>
                </div>
              </div>
              <div class="grow hidden md:flex flex items-center justify-center">
                <img
                  src={@lead_notebook_info.details.cover_url}
                  height="120"
                  width="120"
                  alt="livebook"
                />
              </div>
            </div>
            <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
              <% # Note: it's fine to use stateless components in this comprehension,
              # because @notebook_infos never change %>
              <%= for info <- @notebook_infos do %>
                <ExploreHelpers.notebook_card notebook_info={info} socket={@socket} />
              <% end %>
            </div>
            <%= for group_info <- Explore.group_infos() do %>
              <.notebook_group group_info={group_info} socket={@socket} />
            <% end %>
          </div>
        </div>
      </div>
    </div>

    <.current_user_modal current_user={@current_user} />
    """
  end

  defp notebook_group(assigns) do
    ~H"""
    <div>
      <div class="p-8 rounded-2xl border border-gray-300 flex flex-col sm:flex-row space-y-8 sm:space-y-0 space-x-0 sm:space-x-8 items-center">
        <img src={@group_info.cover_url} width="100" />
        <div>
          <div class="inline-flex px-2 py-0.5 bg-gray-200 rounded-3xl text-gray-700 text-xs font-medium">
            <%= length(@group_info.notebook_infos) %> notebooks
          </div>
          <h3 class="mt-1 text-xl text-gray-800 font-semibold">
            <%= @group_info.title %>
          </h3>
          <p class="mt-2 text-gray-700">
            <%= @group_info.description %>
          </p>
        </div>
      </div>
      <div class="mt-4">
        <ul>
          <%= for {notebook_info, number} <- Enum.with_index(@group_info.notebook_infos, 1) do %>
            <li class="py-4 flex flex-col sm:flex-row items-start sm:items-center sm:space-x-5 border-b border-gray-200 last:border-b-0">
              <div class="text-lg text-gray-400 font-semibold">
                <%= number |> Integer.to_string() |> String.pad_leading(2, "0") %>
              </div>
              <div class="grow text-gray-800 font-semibold">
                <%= notebook_info.title %>
              </div>
              <%= live_redirect to: Routes.explore_path(@socket, :notebook, notebook_info.slug),
                    class: "button-base button-outlined-gray mt-3 sm:mt-0" do %>
                <.remix_icon icon="play-circle-line" class="align-middle mr-1" /> Open notebook
              <% end %>
            </li>
          <% end %>
        </ul>
      </div>
    </div>
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
end
