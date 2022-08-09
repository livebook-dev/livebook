defmodule LivebookWeb.SidebarHelpers do
  use Phoenix.Component

  import LivebookWeb.LiveHelpers
  import LivebookWeb.UserHelpers

  alias Phoenix.LiveView.JS
  alias Livebook.Hub.Settings
  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  Renders the mobile toggle for the sidebar.
  """
  def toggle(assigns) do
    assigns = assign_new(assigns, :inner_block, fn -> [] end)

    ~H"""
    <div class="flex sm:hidden items-center justify-between sticky sm:pt-1 px-2 top-0 left-0 right-0 z-[500] bg-white border-b border-gray-200">
      <div class="my-2 text-2xl text-gray-400 hover:text-gray-600 focus:text-gray-600 rounded-xl h-10 w-10 flex items-center justify-center">
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
      <div
        class="hidden items-center justify-end p-2 text-gray-400 hover:text-gray-600 focus:text-gray-600"
        data-el-toggle-sidebar
      >
        <% # TODO: Use render_slot(@inner_block) || default() on LiveView 0.18 %>
        <%= if @inner_block == [] do %>
          <%= live_redirect to: Routes.home_path(@socket, :page), class: "flex items-center", aria: [label: "go to home"] do %>
            <.remix_icon icon="home-6-line" />
            <span class="pl-2">Home</span>
          <% end %>
        <% else %>
          <%= render_slot(@inner_block) %>
        <% end %>
      </div>
    </div>
    """
  end

  @doc """
  Renders sidebar container.
  """
  def sidebar(assigns) do
    ~H"""
    <nav
      class="w-[18.75rem] min-w-[14rem] flex flex-col justify-between py-1 sm:py-7 bg-gray-900"
      aria-label="sidebar"
      data-el-sidebar
    >
      <div class="flex flex-col space-y-3">
        <div class="group flex items-center mb-5">
          <%= live_redirect to: Routes.home_path(@socket, :page), class: "flex items-center border-l-4 border-gray-900" do %>
            <img src="/images/logo.png" class="group mx-2" height="40" width="40" alt="logo livebook" />
            <span class="text-gray-300 text-2xl font-logo ml-[-1px] group-hover:text-white pt-1">
              Livebook
            </span>
          <% end %>
          <span class="text-gray-300 text-xs font-normal font-sans mx-2.5 pt-3 cursor-default">
            v<%= Application.spec(:livebook, :vsn) %>
          </span>
        </div>
        <.sidebar_link
          title="Home"
          icon="home-6-line"
          to={Routes.home_path(@socket, :page)}
          current={@current_page}
        />
        <.sidebar_link
          title="Explore"
          icon="compass-3-line"
          to={Routes.explore_path(@socket, :page)}
          current={@current_page}
        />
        <.sidebar_link
          title="Settings"
          icon="settings-3-line"
          to={Routes.settings_path(@socket, :page)}
          current={@current_page}
        />
        <.hub_section socket={@socket} hubs={@saved_hubs} />
      </div>
      <div class="flex flex-col">
        <%= if Livebook.Config.shutdown_enabled?() do %>
          <button
            class="h-7 flex items-center text-gray-400 hover:text-white border-l-4 border-transparent hover:border-white"
            aria-label="shutdown"
            phx-click={
              with_confirm(
                JS.push("shutdown"),
                title: "Shut Down",
                description: "Are you sure you want to shut down Livebook now?",
                confirm_text: "Shut Down",
                confirm_icon: "shut-down-line"
              )
            }
          >
            <.remix_icon icon="shut-down-line" class="text-lg leading-6 w-[56px] flex justify-center" />
            <span class="text-sm font-medium">
              Shut Down
            </span>
          </button>
        <% end %>
        <button
          class="mt-8 flex items-center group border-l-4 border-transparent"
          aria_label="user profile"
          phx-click={show_current_user_modal()}
        >
          <div class="w-[56px] flex justify-center">
            <.user_avatar
              user={@current_user}
              class="w-8 h-8 group-hover:ring-white group-hover:ring-2"
              text_class="text-xs"
            />
          </div>
          <span class="text-sm text-gray-400 font-medium group-hover:text-white">
            <%= @current_user.name %>
          </span>
        </button>
      </div>
    </nav>
    """
  end

  defp sidebar_link(assigns) do
    ~H"""
    <%= live_redirect to: @to, class: "h-7 flex items-center hover:text-white #{sidebar_link_text_color(@to, @current)} border-l-4 #{sidebar_link_border_color(@to, @current)} hover:border-white" do %>
      <.remix_icon icon={@icon} class="text-lg leading-6 w-[56px] flex justify-center" />
      <span class="text-sm font-medium">
        <%= @title %>
      </span>
    <% end %>
    """
  end

  defp hub_section(assigns) do
    ~H"""
    <%= if Application.get_env(:livebook, :feature_flags)[:hub] do %>
      <div class="sidebar--hub">
        <div class="grid grid-cols-1 md:grid-cols-2 relative leading-6 mb-2">
          <div class="flex flex-col">
            <small class="ml-5 font-medium text-white">HUBS</small>
          </div>
          <div class="flex flex-col">
            <%= live_redirect to: hub_path(@socket),
                              class: "flex absolute right-5 items-center justify-center
                                      text-gray-400 hover:text-white hover:border-white" do %>
              <.remix_icon icon="add-line" />
            <% end %>
          </div>
        </div>

        <%= for machine <- @hubs do %>
          <div class="h-7 flex items-center cursor-pointer text-gray-400 hover:text-white">
            <.remix_icon
              class="text-lg ml-1 leading-6 w-[56px] flex justify-center"
              icon="checkbox-blank-circle-fill"
              style={"color: #{machine.color}"}
            />

            <span class="ml-1 text-sm font-medium">
              <%= machine.name %>
            </span>
          </div>
        <% end %>

        <div class="h-7 flex items-center cursor-pointer text-gray-400 hover:text-white mt-2 ml-5">
          <%= live_redirect to: hub_path(@socket), class: "flex text-lg leading-6 flex items-center" do %>
            <.remix_icon icon="add-line" class="mr-6" />
            <span class="text-sm font-medium">Add Hub</span>
          <% end %>
        </div>
      </div>
    <% end %>
    """
  end

  defp hub_path(socket) do
    if Application.get_env(:livebook, :feature_flags)[:hub] do
      apply(Routes, :hub_path, [socket, :page])
    end
  end

  defp sidebar_link_text_color(to, current) when to == current, do: "text-white"
  defp sidebar_link_text_color(_to, _current), do: "text-gray-400"

  defp sidebar_link_border_color(to, current) when to == current, do: "border-white"
  defp sidebar_link_border_color(_to, _current), do: "border-transparent"

  def sidebar_handlers(socket) do
    socket |> attach_shutdown_event() |> attach_hub_event()
  end

  defp attach_shutdown_event(socket) do
    if Livebook.Config.shutdown_enabled?() do
      attach_hook(socket, :shutdown, :handle_event, fn
        "shutdown", _params, socket ->
          System.stop()
          {:halt, put_flash(socket, :info, "Livebook is shutting down. You can close this page.")}

        _event, _params, socket ->
          {:cont, socket}
      end)
    else
      socket
    end
  end

  defp attach_hub_event(socket) do
    if Application.get_env(:livebook, :feature_flags)[:hub] do
      socket
      |> assign(saved_hubs: Settings.fetch_machines())
      |> attach_hook(:hub, :handle_info, fn
        :update_hub, socket ->
          {:cont, assign(socket, saved_hubs: Settings.fetch_machines())}

        _event, socket ->
          {:cont, socket}
      end)
    else
      assign(socket, saved_hubs: [])
    end
  end
end
