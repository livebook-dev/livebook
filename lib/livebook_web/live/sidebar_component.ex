defmodule LivebookWeb.SidebarComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.UserHelpers

  # ## Attributes
  #
  #   * `:items` - a list of sidebar items

  @impl true
  def render(assigns) do
    ~H"""
    <div class="w-16 flex flex-col items-center space-y-5 px-3 py-7 bg-gray-900">
      <%= for item <- @items do %>
        <.sidebar_item item={item} socket={@socket} />
      <% end %>
    </div>
    """
  end

  defp sidebar_item(%{item: %{type: :logo}} = assigns) do
    ~H"""
    <span>
      <%= live_patch to: Routes.home_path(@socket, :page) do %>
        <img src="/images/logo.png" height="40" width="40" alt="livebook" />
      <% end %>
    </span>
    """
  end

  defp sidebar_item(%{item: %{type: :button}} = assigns) do
    ~H"""
    <span class="tooltip right distant" aria-label={@item.label}>
      <button class="text-2xl text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center"
        data-element={@item.data_element}>
        <%= remix_icon(@item.icon) %>
      </button>
    </span>
    """
  end

  defp sidebar_item(%{item: %{type: :link}} = assigns) do
    ~H"""
    <span class="tooltip right distant" aria-label={@item.label}>
      <%= live_patch to: @item.path,
            class: "text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center #{if(@item.active, do: "text-gray-50 bg-gray-700")}" do %>
        <%= remix_icon(@item.icon, class: "text-2xl") %>
      <% end %>
    </span>
    """
  end

  defp sidebar_item(%{item: %{type: :break}} = assigns) do
    ~H"""
    <div class="flex-grow"></div>
    """
  end

  defp sidebar_item(%{item: %{type: :user}} = assigns) do
    ~H"""
    <span class="tooltip right distant" aria-label="User profile">
      <%= live_patch to: @item.path,
            class: "text-gray-400 rounded-xl h-8 w-8 flex items-center justify-center" do %>
        <%= render_user_avatar(@item.current_user, class: "h-full w-full", text_class: "text-xs") %>
      <% end %>
    </span>
    """
  end
end
