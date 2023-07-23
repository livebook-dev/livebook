defmodule LivebookWeb.LearnLive do
  use LivebookWeb, :live_view

  import LivebookWeb.SessionHelpers

  alias LivebookWeb.{LayoutHelpers, LearnHelpers}
  alias Livebook.Notebook.Learn

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(_params, _session, socket) do
    [lead_notebook_info | notebook_infos] = Learn.visible_notebook_infos()

    {:ok,
     assign(socket,
       lead_notebook_info: lead_notebook_info,
       notebook_infos: notebook_infos,
       page_title: "Learn - Livebook"
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <LayoutHelpers.layout
      current_page={~p"/learn"}
      current_user={@current_user}
      saved_hubs={@saved_hubs}
    >
      <div class="p-4 md:px-12 md:py-7 max-w-screen-lg mx-auto space-y-4">
        <div>
          <LayoutHelpers.title text="Learn" />
          <p class="mt-4 mb-8 text-gray-700">
            Check out a number of examples showcasing various parts of the Elixir ecosystem.<br />
            Click on any notebook you like and start playing around with it!
          </p>
        </div>
        <div
          id="welcome-to-livebook"
          class="p-8 bg-gray-900 rounded-2xl flex flex-col sm:flex-row space-y-8 sm:space-y-0 space-x-0 sm:space-x-8 items-center"
        >
          <img
            src={LearnHelpers.learn_img_src(@lead_notebook_info.details.cover)}
            width="100"
            alt="livebook"
          />
          <div>
            <h3 class="text-xl text-gray-50 font-semibold">
              <%= @lead_notebook_info.title %>
            </h3>
            <p class="mt-2 text-sm text-gray-300">
              <%= @lead_notebook_info.details.description %>
            </p>
            <div class="mt-4">
              <.link
                patch={~p"/learn/notebooks/#{@lead_notebook_info.slug}"}
                class="button-base button-blue"
              >
                Open notebook
              </.link>
            </div>
          </div>
        </div>
        <div class="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-1 lg:grid-cols-2 xl:grid-cols-3 gap-4">
          <% # Note: it's fine to use stateless components in this comprehension,
          # because @notebook_infos never change %>
          <LearnHelpers.notebook_card :for={info <- @notebook_infos} notebook_info={info} />
        </div>
        <.notebook_group :for={group_info <- Learn.group_infos()} group_info={group_info} />
      </div>
    </LayoutHelpers.layout>
    """
  end

  defp notebook_group(assigns) do
    ~H"""
    <div>
      <div class="p-8 mt-16 rounded-2xl border border-gray-300 flex flex-col sm:flex-row space-y-8 sm:space-y-0 space-x-0 sm:space-x-8 items-center">
        <img src={LearnHelpers.learn_img_src(@group_info.cover)} width="100" />
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
      <div class="mt-3 mb-20">
        <ul>
          <li
            :for={{notebook_info, number} <- Enum.with_index(@group_info.notebook_infos, 1)}
            class="py-3 flex flex-row items-center space-x-5 border-b border-gray-200 last:border-b-0"
          >
            <div class="text-lg text-gray-400 font-semibold">
              <%= number |> Integer.to_string() |> String.pad_leading(2, "0") %>
            </div>
            <div class="grow text-gray-800 font-semibold">
              <%= notebook_info.title %>
            </div>
            <.link
              navigate={~p"/learn/notebooks/#{notebook_info.slug}"}
              class="button-base button-outlined-gray"
            >
              <.remix_icon icon="play-circle-line" class="align-middle mr-1" /> Open
            </.link>
          </li>
        </ul>
      </div>
    </div>
    """
  end

  @impl true
  def handle_params(%{"slug" => slug}, _url, socket) do
    {notebook, files} = Learn.notebook_by_slug!(slug)
    {:noreply, create_session(socket, notebook: notebook, files_source: {:inline, files})}
  end

  def handle_params(_params, _url, socket), do: {:noreply, socket}
end
