defmodule LiveBookWeb.ShortcutsComponent do
  use LiveBookWeb, :live_component

  @shortcuts [
    %{
      combo: "?",
      mac_combo: "?",
      description: "Open this help modal"
    },
    %{
      combo: "ctrl + enter",
      mac_combo: "cmd + enter",
      description: "Evaluate Elixir cell / Toggle Markdown cell"
    },
    %{
      combo: "shift + enter",
      mac_combo: "shift + enter",
      description: "Evaluate cell and focus next"
    },
    %{
      combo: "ctrl + shift + enter",
      mac_combo: "cmd + shift + enter",
      description: "Evaluate section"
    },
    %{
      combo: "ctrl + alt + enter",
      mac_combo: "cmd + opt + enter",
      description: "Evaluate cells below"
    },
    %{
      combo: "ctrl + j",
      mac_combo: "cmd + j",
      description: "Focus next cell"
    },
    %{
      combo: "ctrl + k",
      mac_combo: "cmd + k",
      description: "Focus previous cell"
    },
    %{
      combo: "ctrl + alt + n",
      mac_combo: "cmd + opt + n",
      description: "Insert Elixir cell below"
    },
    %{
      combo: "ctrl + alt + m",
      mac_combo: "cmd + opt + m",
      description: "Insert Markdown cell below"
    },
    %{
      combo: "ctrl + alt + w",
      mac_combo: "cmd + opt + w",
      description: "Delete cell"
    }
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
                <span class="non-mac-only bg-editor text-editor py-1 px-2 rounded-md">
                  <%= shortcut.combo %>
                </span>
                <span class="mac-only bg-editor text-editor py-1 px-2 rounded-md">
                  <%= shortcut.mac_combo %>
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
