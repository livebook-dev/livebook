defmodule LivebookWeb.SidebarHelpers do
  use Phoenix.Component

  import LivebookWeb.LiveHelpers
  import LivebookWeb.UserHelpers

  alias Phoenix.LiveView.JS
  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  Renders sidebar container.

  Other functions in this module render sidebar
  items of various type.
  """
  def sidebar(assigns) do
    ~H"""
    <nav
      class="w-16 flex flex-col items-center px-3 py-1 space-y-2 sm:space-y-4 sm:py-7 bg-gray-900"
      aria-label="sidebar"
      data-el-sidebar
    >
      <%= render_slot(@inner_block) %>
    </nav>
    """
  end

  def new_sidebar(assigns) do
    ~H"""
    <!--Nav Desktop-->
    <nav
      class="w-[18.75rem] min-w-[14rem] flex flex-col justify-between py-7 bg-gray-900"
      aria-label="sidebar"
      data-el-sidebar
    >
      <div class="flex flex-col space-y-2">
        <div class="flex items-center mb-6">
          <%= live_patch to: Routes.home_path(@socket, :page), class: "flex items-center mb-6" do %>
            <img src="/images/logo.png" class="mx-3" height="40" width="40" alt="logo livebook" />
            <span class="text-gray-300 text-2xl font-logo">Livebook</span>
          <% end %>
          <span class=" text-gray-300 text-sm font-normal font-sans mx-2.5 pt-2">
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
            class="h-8 flex items-center group hover:border-l-4 text-gray-400"
            aria-label="shutdown"
            phx-click={
              with_confirm(
                JS.push("shutdown"),
                title: "Shut down",
                description: "Are you sure you want to shut down Livebook now?",
                confirm_text: "Shut down",
                confirm_icon: "shut-down-line"
              )
            }
          >
            <.remix_icon
              icon="shut-down-line"
              class="ri-sb flex justify-center w-16 text-gray-400 group-hover:text-white"
            />
            <span class="text-sm text-gray-400 font-normal group-hover:text-white hover:font-medium">
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
          <div class="w-16 h-8 flex justify-center group">
            <.user_avatar user={@current_user} class="p-2 group-hover:ring-white group-hover:ring-2" text_class="text-xs" />
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
    <%= live_patch to: @to, class: "h-8 flex items-center group hover:border-l-4 #{sidebar_link_color(@to, @current)}" do %>
      <.remix_icon
        icon={@icon}
        class="ri-sb flex justify-center w-16 text-gray-400 group-hover:text-white"
      />
      <span class="text-sm text-gray-400 font-normal group-hover:text-white hover:font-medium">
        <%= @title %>
      </span>
    <% end %>
    """
  end

  defp sidebar_link_color(to, current) when to == current, do: "text-white"
  defp sidebar_link_color(_to, _current), do: "text-gray-400"

  def logo_item(assigns) do
    ~H"""
    <span>
      <%= live_redirect to: Routes.home_path(@socket, :page), aria_label: "go to homepage" do %>
        <img src="/images/logo.png" height="40" width="40" alt="" />
      <% end %>
    </span>
    """
  end

  def button_item(assigns) do
    ~H"""
    <span class="tooltip right distant" data-tooltip={@label}>
      <button
        class="text-2xl text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center"
        aria-label={@label}
        {@button_attrs}
      >
        <.remix_icon icon={@icon} />
      </button>
    </span>
    """
  end

  def link_item(assigns) do
    assigns = assign_new(assigns, :link_attrs, fn -> [] end)

    ~H"""
    <span class="tooltip right distant" data-tooltip={@label}>
      <%= live_patch [to: @path,
            class: "text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center #{if(@active, do: "text-gray-50 bg-gray-700")}",
            aria_label: @label] ++ @link_attrs do %>
        <.remix_icon icon={@icon} class="text-2xl" />
      <% end %>
    </span>
    """
  end

  def shutdown_item(assigns) do
    if Livebook.Config.shutdown_enabled?() do
      ~H"""
      <span class="tooltip right distant" data-tooltip="Shutdown">
        <button
          class="text-2xl text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center"
          aria-label="shutdown"
          phx-click={
            with_confirm(
              JS.push("shutdown"),
              title: "Shutdown",
              description: "Are you sure you want to shutdown Livebook?",
              confirm_text: "Shutdown",
              confirm_icon: "shut-down-line"
            )
          }
        >
          <.remix_icon icon="shut-down-line" />
        </button>
      </span>
      """
    else
      ~H"""

      """
    end
  end

  def break_item(assigns) do
    ~H"""
    <div class="grow"></div>
    """
  end

  def user_item(assigns) do
    ~H"""
    <span class="tooltip right distant" data-tooltip="User profile">
      <button
        class="text-gray-400 rounded-xl h-8 w-8 flex items-center justify-center mt-2"
        aria_label="user profile"
        phx-click={show_current_user_modal()}
      >
        <.user_avatar user={@current_user} text_class="text-xs" />
      </button>
    </span>
    """
  end

  ## Shared home functionality

  @doc """
  A footer shared across home, settings, explore, etc.

  Note you must call `shared_home_handlers` on mount/3.
  """
  def shared_home_footer(assigns) do
    ~H"""
    <.break_item />
    <.shutdown_item />
    <.link_item
      icon="settings-3-fill"
      label="Settings"
      path={Routes.settings_path(@socket, :page)}
      active={false}
    />
    <.user_item current_user={@current_user} />
    """
  end

  def shared_home_handlers(socket) do
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
