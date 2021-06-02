defmodule LivebookWeb.SidebarComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.UserHelpers

  # ## Attributes
  #
  #   * `:items` - a list of sidebar items

  @impl true
  def render(assigns) do
    ~L"""
    <div class="w-16 flex flex-col items-center space-y-5 px-3 py-7 bg-gray-900">
      <%= for item <- @items do %>
        <%= render_item(@socket, item) %>
      <% end %>
    </div>
    """
  end

  defp render_item(socket, %{type: :logo} = item) do
    assigns = %{item: item}

    ~L"""
    <%= live_patch to: Routes.home_path(socket, :page) do %>
      <img src="/images/logo.png" height="40" width="40" alt="livebook" />
    <% end %>
    """
  end

  defp render_item(_socket, %{type: :button} = item) do
    assigns = %{item: item}

    ~L"""
    <span class="tooltip right distant" aria-label="<%= item.label %>">
      <button class="text-2xl text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center"
        data-element="<%= item.data_element %>">
        <%= remix_icon(item.icon) %>
      </button>
    </span>
    """
  end

  defp render_item(_socket, %{type: :link} = item) do
    assigns = %{item: item}

    ~L"""
    <span class="tooltip right distant" aria-label="<%= item.label %>">
      <%= live_patch to: item.path,
            class: "text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center #{if(item.active, do: "text-gray-50 bg-gray-700")}" do %>
        <%= remix_icon(item.icon, class: "text-2xl") %>
      <% end %>
    </span>
    """
  end

  defp render_item(_socket, %{type: :break}) do
    assigns = %{}

    ~L"""
    <div class="flex-grow"></div>
    """
  end

  defp render_item(_socket, %{type: :user} = item) do
    assigns = %{item: item}

    ~L"""
    <span class="tooltip right distant" aria-label="User profile">
      <%= live_patch to: item.path,
            class: "text-gray-400 rounded-xl h-8 w-8 flex items-center justify-center" do %>
        <%= render_user_avatar(item.current_user, class: "h-full w-full", text_class: "text-xs") %>
      <% end %>
    </span>
    """
  end
end
