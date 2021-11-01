defmodule LivebookWeb.SidebarHelpers do
  use Phoenix.Component

  import LivebookWeb.Helpers
  import LivebookWeb.UserHelpers

  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  Renders sidebar container.

  Other functions in this module render sidebar
  items of various type.
  """
  def sidebar(assigns) do
    ~H"""
    <div class="w-16 flex flex-col items-center space-y-5 px-3 py-7 bg-gray-900">
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  def logo_item(assigns) do
    ~H"""
    <span>
      <%= live_patch to: Routes.home_path(@socket, :page), aria_label: "go to homepage" do %>
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
        data-element={@data_element}>
        <.remix_icon icon={@icon} />
      </button>
    </span>
    """
  end

  def link_item(assigns) do
    ~H"""
    <span class="tooltip right distant" data-tooltip={@label}>
      <%= live_patch to: @path,
            class: "text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center #{if(@active, do: "text-gray-50 bg-gray-700")}",
            aria_label: @label do %>
        <.remix_icon icon={@icon} class="text-2xl" />
      <% end %>
    </span>
    """
  end

  def break_item(assigns) do
    ~H"""
    <div class="flex-grow"></div>
    """
  end

  def user_item(assigns) do
    ~H"""
    <span class="tooltip right distant" data-tooltip="User profile">
      <%= live_patch to: @path,
            class: "text-gray-400 rounded-xl h-8 w-8 flex items-center justify-center",
            aria_label: "user profile" do %>
        <.user_avatar user={@current_user} text_class="text-xs" />
      <% end %>
    </span>
    """
  end
end
