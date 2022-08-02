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
    <nav class="w-[18.75rem] min-w-[14rem] flex flex-col justify-between py-7 bg-gray-900" aria-label="sidebar" data-el-sidebar>
      <div class="flex flex-col space-y-2">
        <div class="flex items-center mb-6">
          <div class="w-16 flex justify-center "><img src="/images/logo.png" height="40" width="40" alt="logo livebook" /></div>
          <span class="text-gray-300 text-2xl font-medium font-sans ">Livebook</span>
          <span class=" text-gray-300 text-sm font-normal font-sans mx-2.5 pt-2">v 1.3</span>
        </div>
        <!--Home-->
        <div class="h-8 flex items-center group hover:border-l-4">
          <svg class="w-16 fill-gray-400 group-hover:fill-white" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="18" height="18"><path fill="none" d="M0 0h24v24H0z"/><path d="M21 20a1 1 0 0 1-1 1H4a1 1 0 0 1-1-1V9.49a1 1 0 0 1 .386-.79l8-6.222a1 1 0 0 1 1.228 0l8 6.222a1 1 0 0 1 .386.79V20zm-2-1V9.978l-7-5.444-7 5.444V19h14zM7 15h10v2H7v-2z"/></svg>
          <span class="text-sm text-gray-400 font-normal group-hover:text-white hover:font-medium">Home</span>
        </div>
        <!--Explore-->
        <div class="h-8 flex items-center group hover:border-l-4">
          <svg class="w-16 fill-gray-400 group-hover:fill-white" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="18" height="18"><path fill="none" d="M0 0h24v24H0z"/><path d="M12 22C6.477 22 2 17.523 2 12S6.477 2 12 2s10 4.477 10 10-4.477 10-10 10zm0-2a8 8 0 1 0 0-16 8 8 0 0 0 0 16zm4.5-12.5L14 14l-6.5 2.5L10 10l6.5-2.5zM12 13a1 1 0 1 0 0-2 1 1 0 0 0 0 2z"/></svg>
          <span class="text-sm text-gray-400 font-normal group-hover:text-white hover:font-medium">Explore</span>
        </div>
        <!--Settings-->
        <div class="h-8 flex items-center group hover:border-l-4">
          <svg class="w-16 fill-gray-400 group-hover:fill-white" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="18" height="18"><path fill="none" d="M0 0h24v24H0z"/><path d="M3.34 17a10.018 10.018 0 0 1-.978-2.326 3 3 0 0 0 .002-5.347A9.99 9.99 0 0 1 4.865 4.99a3 3 0 0 0 4.631-2.674 9.99 9.99 0 0 1 5.007.002 3 3 0 0 0 4.632 2.672c.579.59 1.093 1.261 1.525 2.01.433.749.757 1.53.978 2.326a3 3 0 0 0-.002 5.347 9.99 9.99 0 0 1-2.501 4.337 3 3 0 0 0-4.631 2.674 9.99 9.99 0 0 1-5.007-.002 3 3 0 0 0-4.632-2.672A10.018 10.018 0 0 1 3.34 17zm5.66.196a4.993 4.993 0 0 1 2.25 2.77c.499.047 1 .048 1.499.001A4.993 4.993 0 0 1 15 17.197a4.993 4.993 0 0 1 3.525-.565c.29-.408.54-.843.748-1.298A4.993 4.993 0 0 1 18 12c0-1.26.47-2.437 1.273-3.334a8.126 8.126 0 0 0-.75-1.298A4.993 4.993 0 0 1 15 6.804a4.993 4.993 0 0 1-2.25-2.77c-.499-.047-1-.048-1.499-.001A4.993 4.993 0 0 1 9 6.803a4.993 4.993 0 0 1-3.525.565 7.99 7.99 0 0 0-.748 1.298A4.993 4.993 0 0 1 6 12c0 1.26-.47 2.437-1.273 3.334a8.126 8.126 0 0 0 .75 1.298A4.993 4.993 0 0 1 9 17.196zM12 15a3 3 0 1 1 0-6 3 3 0 0 1 0 6zm0-2a1 1 0 1 0 0-2 1 1 0 0 0 0 2z"/></svg>
          <span class="text-sm text-gray-400 font-normal group-hover:text-white hover:font-medium">Settings</span>
        </div>
      </div>
      <div class="flex flex-col">
        <!--Shut Down-->
        <div class="h-8 flex items-center group hover:border-l-4">
          <svg class="w-16 fill-gray-400 group-hover:fill-white" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="18" height="18"><path fill="none" d="M0 0h24v24H0z"/><path d="M6.265 3.807l1.147 1.639a8 8 0 1 0 9.176 0l1.147-1.639A9.988 9.988 0 0 1 22 12c0 5.523-4.477 10-10 10S2 17.523 2 12a9.988 9.988 0 0 1 4.265-8.193zM11 12V2h2v10h-2z"/></svg>
          <span class="text-sm text-gray-400 font-normal group-hover:text-white hover:font-medium">Shut down</span>
        </div>
        <!--User Profile-->
        <div class=" mt-8 flex items-center group">
          <div class="w-16 h-8 flex justify-center group">
            <span class=" bg-green-600 p-2 rounded-full text-white text-xs  group-hover:ring-white group-hover:ring-2">JV</span>
          </div>
          <span class="text-sm text-gray-400 font-normal group-hover:text-white hover:font-medium">Jonatan K. (You)</span>
        </div>
      </div>
    </nav>

    <!--Nav Mobile-->
    <%# <nav class="w-16 flex flex-col justify-between px-3 py-1 space-y-2 sm:space-y-2 sm:py-7 bg-gray-900" aria-label="sidebar" data-el-sidebar>
      <div class="flex flex-col items-center">
        <img class="mb-8" src="/images/logo.png" height="40" width="40" alt="logo livebook" />
        <!--Home-->
        <div class="w-16 h-8 flex justify-center items-center group hover:border-l-4">
          <svg class=" fill-gray-400 group-hover:fill-white" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="18" height="18"><path fill="none" d="M0 0h24v24H0z"/><path d="M21 20a1 1 0 0 1-1 1H4a1 1 0 0 1-1-1V9.49a1 1 0 0 1 .386-.79l8-6.222a1 1 0 0 1 1.228 0l8 6.222a1 1 0 0 1 .386.79V20zm-2-1V9.978l-7-5.444-7 5.444V19h14zM7 15h10v2H7v-2z"/></svg>
        </div>
        <!--Explore-->
        <div class="w-16 h-8 flex justify-center items-center group hover:border-l-4">
          <svg class="fill-gray-400 group-hover:fill-white" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="18" height="18"><path fill="none" d="M0 0h24v24H0z"/><path d="M12 22C6.477 22 2 17.523 2 12S6.477 2 12 2s10 4.477 10 10-4.477 10-10 10zm0-2a8 8 0 1 0 0-16 8 8 0 0 0 0 16zm4.5-12.5L14 14l-6.5 2.5L10 10l6.5-2.5zM12 13a1 1 0 1 0 0-2 1 1 0 0 0 0 2z"/></svg>
        </div>
        <!--Settings-->
        <div class="w-16 h-8 flex justify-center items-center group hover:border-l-4">
          <svg class="fill-gray-400 group-hover:fill-white" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="18" height="18"><path fill="none" d="M0 0h24v24H0z"/><path d="M3.34 17a10.018 10.018 0 0 1-.978-2.326 3 3 0 0 0 .002-5.347A9.99 9.99 0 0 1 4.865 4.99a3 3 0 0 0 4.631-2.674 9.99 9.99 0 0 1 5.007.002 3 3 0 0 0 4.632 2.672c.579.59 1.093 1.261 1.525 2.01.433.749.757 1.53.978 2.326a3 3 0 0 0-.002 5.347 9.99 9.99 0 0 1-2.501 4.337 3 3 0 0 0-4.631 2.674 9.99 9.99 0 0 1-5.007-.002 3 3 0 0 0-4.632-2.672A10.018 10.018 0 0 1 3.34 17zm5.66.196a4.993 4.993 0 0 1 2.25 2.77c.499.047 1 .048 1.499.001A4.993 4.993 0 0 1 15 17.197a4.993 4.993 0 0 1 3.525-.565c.29-.408.54-.843.748-1.298A4.993 4.993 0 0 1 18 12c0-1.26.47-2.437 1.273-3.334a8.126 8.126 0 0 0-.75-1.298A4.993 4.993 0 0 1 15 6.804a4.993 4.993 0 0 1-2.25-2.77c-.499-.047-1-.048-1.499-.001A4.993 4.993 0 0 1 9 6.803a4.993 4.993 0 0 1-3.525.565 7.99 7.99 0 0 0-.748 1.298A4.993 4.993 0 0 1 6 12c0 1.26-.47 2.437-1.273 3.334a8.126 8.126 0 0 0 .75 1.298A4.993 4.993 0 0 1 9 17.196zM12 15a3 3 0 1 1 0-6 3 3 0 0 1 0 6zm0-2a1 1 0 1 0 0-2 1 1 0 0 0 0 2z"/></svg>
        </div>
      </div>
      <div class="flex flex-col items-center">
        <!--Shut Down-->
        <div class="w-16 h-8 flex justify-center items-center group hover:border-l-4">
          <svg class="fill-gray-400 group-hover:fill-white" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="18" height="18"><path fill="none" d="M0 0h24v24H0z"/><path d="M6.265 3.807l1.147 1.639a8 8 0 1 0 9.176 0l1.147-1.639A9.988 9.988 0 0 1 22 12c0 5.523-4.477 10-10 10S2 17.523 2 12a9.988 9.988 0 0 1 4.265-8.193zM11 12V2h2v10h-2z"/></svg>
        </div>
        <!--User Profile-->
        <div class="w-16 mt-8 text-center">
          <span class="bg-green-600 p-2 rounded-full text-white text-xs  hover:ring-white hover:ring-2">JV</span>
        </div>
      </div>
    </nav> %>

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
