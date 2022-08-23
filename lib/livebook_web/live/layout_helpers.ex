defmodule LivebookWeb.LayoutHelpers do
  use Phoenix.Component

  import LivebookWeb.LiveHelpers
  import LivebookWeb.UserHelpers

  alias Phoenix.LiveView.JS
  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  The layout used in the non-session pages.
  """
  def layout(assigns) do
    assigns = assign_new(assigns, :topbar_action, fn -> [] end)

    ~H"""
    <div class="flex grow h-full">
      <.live_region role="alert" />
      <.sidebar
        socket={@socket}
        current_page={@current_page}
        current_user={@current_user}
        saved_hubs={@saved_hubs}
      />
      <div class="grow overflow-y-auto">
        <div class="flex sm:hidden items-center justify-between sticky sm:pt-1 px-2 top-0 left-0 right-0 z-[500] bg-white border-b border-gray-200">
          <.topbar_sidebar_toggle />
          <div
            class="hidden items-center justify-end p-2 text-gray-400 hover:text-gray-600 focus:text-gray-600"
            data-el-toggle-sidebar
          >
            <% # TODO: Use render_slot(@topbar_action) || default() on LiveView 0.18 %>
            <%= if @topbar_action == [] do %>
              <%= live_redirect to: Routes.home_path(@socket, :page), class: "flex items-center", aria: [label: "go to home"] do %>
                <.remix_icon icon="home-6-line" />
                <span class="pl-2">Home</span>
              <% end %>
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

  defp topbar_sidebar_toggle(assigns) do
    ~H"""
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
    """
  end

  defp sidebar(assigns) do
    ~H"""
    <nav
      class="w-[18.75rem] min-w-[14rem] flex flex-col justify-between py-1 sm:py-7 bg-gray-900"
      aria-label="sidebar"
      data-el-sidebar
    >
      <div class="flex flex-col">
        <div class="space-y-3">
          <div class="group flex items-center mb-5">
            <%= live_redirect to: Routes.home_path(@socket, :page), class: "flex items-center border-l-4 border-gray-900" do %>
              <img
                src="/images/logo.png"
                class="group mx-2"
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
        </div>
        <.hub_section socket={@socket} hubs={@saved_hubs} current_page={@current_page} />
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
    assigns = assign_new(assigns, :icon_style, fn -> nil end)

    ~H"""
    <%= live_redirect to: @to, class: "h-7 flex items-center hover:text-white #{sidebar_link_text_color(@to, @current)} border-l-4 #{sidebar_link_border_color(@to, @current)} hover:border-white" do %>
      <.remix_icon
        icon={@icon}
        class="text-lg leading-6 w-[56px] flex justify-center"
        style={@icon_style}
      />
      <span class="text-sm font-medium">
        <%= @title %>
      </span>
    <% end %>
    """
  end

  defp hub_section(assigns) do
    ~H"""
    <%= if Application.get_env(:livebook, :feature_flags)[:hub] do %>
      <div id="hubs" class="flex flex-col mt-12">
        <div class="space-y-1">
          <div class="grid grid-cols-1 md:grid-cols-2 relative leading-6 mb-2">
            <small class="ml-5 font-medium text-gray-300 cursor-default">HUBS</small>
          </div>

          <%= for hub <- @hubs do %>
            <.sidebar_link
              title={hub.name}
              icon="checkbox-blank-circle-fill"
              icon_style={"color: #{hub.color}"}
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
