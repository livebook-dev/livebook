defmodule LivebookWeb.SidebarHelpers do
  use Phoenix.Component

  import LivebookWeb.LiveHelpers
  import LivebookWeb.UserHelpers

  alias Phoenix.LiveView.JS
  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  Renders sidebar container.
  """
  def sidebar(assigns) do
    ~H"""
    <!--Nav Desktop-->
    <nav
      class="w-[18.75rem] min-w-[14rem] flex flex-col justify-between py-7 bg-gray-900"
      aria-label="sidebar"
      data-el-sidebar
    >
      <div class="flex flex-col space-y-2">
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
      </div>
      <div class="flex flex-col">
        <!--Shut down button-->
        <%= if Livebook.Config.shutdown_enabled?() do %>
          <a
            class="h-8 flex items-center group"
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
              class="ri-sb flex justify-center w-[60px] group border-l-4 border-gray-900 group-hover:border-white text-gray-400 group-hover:text-white"
            />
            <span class="text-sm text-gray-400 font-normal group-hover:text-white group-hover:font-medium">
              Shut Down
            </span>
          </a>
        <% end %>
        <!--User Profile-->
        <button
          class="mt-8 flex items-center group"
          aria_label="user profile"
          phx-click={show_current_user_modal()}
        >
          <div class="w-[60px] pl-1 h-8 flex justify-center group">
            <.user_avatar
              user={@current_user}
              class="w-8 h-8 group-hover:ring-white group-hover:ring-2"
              text_class="text-xs"
            />
          </div>
          <span class="text-sm text-gray-400 font-normal group-hover:text-white group-hover:font-medium">
            <%= @current_user.name %>
          </span>
        </button>
      </div>
    </nav>
    <!--Nav Mobile-->
    """
  end

  defp sidebar_link(assigns) do
    ~H"""
    <%= live_redirect to: @to, class: "h-8 flex items-center group #{sidebar_link_text_color(@to, @current)}" do %>
      <.remix_icon
        icon={@icon}
        class={
          "ri-sb flex justify-center w-[60px] group border-l-4 #{sidebar_link_border_color(@to, @current)} group-hover:border-white group-hover:text-white"
        }
      />
      <span class="text-sm font-normal group-hover:text-white hover:font-medium">
        <%= @title %>
      </span>
    <% end %>
    """
  end

  defp sidebar_link_text_color(to, current) when to == current, do: "text-white"
  defp sidebar_link_text_color(_to, _current), do: "text-gray-400"

  defp sidebar_link_border_color(to, current) when to == current, do: "border-white"
  defp sidebar_link_border_color(_to, _current), do: "border-gray-900"

  def sidebar_handlers(socket) do
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
end
