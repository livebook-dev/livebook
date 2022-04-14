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
    <nav class="w-16 flex flex-col items-center space-y-4 px-3 py-7 bg-gray-900" aria-label="sidebar">
      <%= render_slot(@inner_block) %>
    </nav>
    """
  end

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
      <button class="text-2xl text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center"
        aria-label={@label}
        {@button_attrs}>
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
        <button class="text-2xl text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center"
          aria-label="shutdown"
          phx-click={
            with_confirm(
              JS.push("shutdown"),
              title: "Shutdown",
              description: "Are you sure you want to shutdown Livebook?",
              confirm_text: "Shutdown",
              confirm_icon: "shut-down-line"
            )
          }>
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
      <button class="text-gray-400 rounded-xl h-8 w-8 flex items-center justify-center mt-2"
        aria_label="user profile"
        phx-click={show_current_user_modal()}>
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
      active={false} />
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
