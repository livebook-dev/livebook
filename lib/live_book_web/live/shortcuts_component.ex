defmodule LiveBookWeb.ShortcutsComponent do
  use LiveBookWeb, :live_component

  @shortcuts [
    %{combination: "cmd + enter", description: "Evaluate Elixir cell / Toggle Markdown cell"},
    %{combination: "shift + enter", description: "Evaluate cell and focus next"},
    %{combination: "cmd + j", description: "Focus next cell"},
    %{combination: "cmd + k", description: "Focus previous cell"},
    %{combination: "cmd + opt + n", description: "Insert Elixir cell below"},
    %{combination: "cmd + opt + m", description: "Insert Markdown cell below"},
    %{combination: "cmd + opt + w", description: "Delete cell"}
  ]

  @impl true
  def mount(socket) do
    {:ok, assign(socket, shortcuts: @shortcuts)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6">
      <h3 class="text-lg font-medium text-gray-900">
        Keyboard shortcuts
      </h3>
      <div class="mt-4">
        <ul>
          <%= for shortcut <- @shortcuts do %>
            <li class="flex items-center my-3">
              <span class="w-1/3">
                <span class="bg-editor text-editor py-1 px-2 rounded-md">
                  <%= shortcut.combination %>
                </span>
              </span>
              <span class="w-2/3">
                <%= shortcut.description %>
              </span>
            </li>
          <% end %>
        </ul>
      </div>
    </div>
    """
  end
end
