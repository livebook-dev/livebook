defmodule LivebookWeb.LayoutHelpers do
  use Phoenix.Component

  import LivebookWeb.LiveHelpers
  import LivebookWeb.UserHelpers

  alias Phoenix.LiveView.JS
  alias Livebook.Hubs.Provider
  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  The layout used in the non-session pages.
  """
  def layout(assigns) do
    assigns = assign_new(assigns, :topbar_action, fn -> [] end)

    ~H"""
    <div class="flex grow h-full">
      <div class="absolute md:static h-full z-[600]">
        <.live_region role="alert" />
        <.sidebar
          socket={@socket}
          current_page={@current_page}
          current_user={@current_user}
          saved_hubs={@saved_hubs}
        />
      </div>
      <div class="grow overflow-y-auto">
        <div class="md:hidden sticky flex items-center justify-between h-14 px-4 top-0 left-0 z-[500] bg-white border-b border-gray-200">
          <div class="pt-1 text-xl text-gray-400 hover:text-gray-600 focus:text-gray-600">
            <button
              data-el-toggle-sidebar
              aria-label="show sidebar"
              phx-click={
                JS.remove_class("hidden", to: "[data-el-sidebar]")
                |> JS.toggle(to: "[data-el-toggle-sidebar]")
              }
            >
              <.remix_icon icon="menu-unfold-line" />
            </button>
          </div>

          <div>
            <%= if @topbar_action == [] do %>
              <div class="text-gray-400 hover:text-gray-600 focus:text-gray-600">
                <%= live_redirect to: Routes.home_path(@socket, :page), class: "flex items-center", aria: [label: "go to home"] do %>
                  <.remix_icon icon="home-6-line" />
                  <span class="pl-2">Home</span>
                <% end %>
              </div>
            <% else %>
              <%= render_slot(@topbar_action) %>
            <% end %>
          </div>
        </div>
        <%= render_slot(@inner_block) %>
      </div>
    </div>

    <.current_user_modal current_user={@current_user} />
    """
  end

  defp sidebar(assigns) do
    ~H"""
    <nav
      class="hidden md:flex w-[17rem] h-full py-2 md:py-5 bg-gray-900"
      aria-label="sidebar"
      data-el-sidebar
    >
      <button
        class="hidden text-xl text-gray-300 hover:text-white focus:text-white absolute top-4 right-3"
        aria-label="hide sidebar"
        data-el-toggle-sidebar
        phx-click={
          JS.add_class("hidden", to: "[data-el-sidebar]")
          |> JS.toggle(to: "[data-el-toggle-sidebar]")
        }
      >
        <.remix_icon icon="menu-fold-line" />
      </button>

      <div class="flex flex-col justify-between h-full">
        <div class="flex flex-col">
          <div class="space-y-3">
            <div class="flex items-center mb-5">
              <%= live_redirect to: Routes.home_path(@socket, :page), class: "flex items-center border-l-4 border-gray-900 group" do %>
                <img
                  src={Routes.static_path(@socket, "/images/logo.png")}
                  class="mx-2"
                  height="40"
                  width="40"
                  alt="logo livebook"
                />
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
              title="Learn"
              icon="article-line"
              to={Routes.learn_path(@socket, :page)}
              current={@current_page}
            />
            <.sidebar_link
              title="Settings"
              icon="settings-3-line"
              to={Routes.settings_path(@socket, :page)}
              current={@current_page}
            />
          </div>
          <.hub_section socket={@socket} hubs={@saved_hubs} current_page={@current_page} />
        </div>
        <div class="flex flex-col">
          <%= if Livebook.Config.shutdown_callback() do %>
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
              <.remix_icon
                icon="shut-down-line"
                class="text-lg leading-6 w-[56px] flex justify-center"
              />
              <span class="text-sm font-medium">
                Shut Down
              </span>
            </button>
          <% end %>
          <button
            class="mt-6 flex items-center group border-l-4 border-transparent"
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

  defp sidebar_hub_link(assigns) do
    ~H"""
    <%= live_redirect to: @to, class: "h-7 flex items-center hover:text-white #{sidebar_link_text_color(@to, @current)} border-l-4 #{sidebar_link_border_color(@to, @current)} hover:border-white" do %>
      <div class="text-lg leading-6 w-[56px] flex justify-center">
        <span class="relative">
          <%= @hub.emoji %>

          <%= if Provider.connect(@hub.provider) do %>
            <div class={[
              "absolute w-[10px] h-[10px] border-gray-900 border-2 rounded-full right-0 bottom-0",
              if(@hub.connected?, do: "bg-green-400", else: "bg-red-400")
            ]} />
          <% end %>
        </span>
      </div>
      <span class="text-sm font-medium">
        <%= @hub.name %>
      </span>
    <% end %>
    """
  end

  defp hub_section(assigns) do
    ~H"""
    <%= if Livebook.Config.feature_flag_enabled?(:hub) do %>
      <div id="hubs" class="flex flex-col mt-12">
        <div class="space-y-3">
          <div class="grid grid-cols-1 md:grid-cols-2 relative leading-6 mb-2">
            <small class="ml-5 font-medium text-gray-300 cursor-default">HUBS</small>
          </div>

          <%= for hub <- @hubs do %>
            <.sidebar_hub_link
              hub={hub}
              to={Routes.hub_path(@socket, :edit, hub.id)}
              current={@current_page}
            />
          <% end %>

          <.sidebar_link
            title="Add Hub"
            icon="add-line"
            to={Routes.hub_path(@socket, :new)}
            current={@current_page}
          />
        </div>
      </div>
    <% end %>
    """
  end

  defp sidebar_link_text_color(to, current) when to == current, do: "text-white"
  defp sidebar_link_text_color(_to, _current), do: "text-gray-400"

  defp sidebar_link_border_color(to, current) when to == current, do: "border-white"
  defp sidebar_link_border_color(_to, _current), do: "border-transparent"
end
