defmodule LiveBookWeb.SessionLive.ShortcutsComponent do
  use LiveBookWeb, :live_component

  @shortcuts %{
    insert_mode: [
      %{seq: "esc", desc: "Switch back to navigation mode"},
      %{seq: "ctrl + enter", desc: "Evaluate cell and stay in insert mode"}
    ],
    navigation_mode: [
      %{seq: "?", desc: "Open this help modal"},
      %{seq: "j", desc: "Focus next cell"},
      %{seq: "k", desc: "Focus previous cell"},
      %{seq: "J", desc: "Move cell down"},
      %{seq: "K", desc: "Move cell up"},
      %{seq: "i", desc: "Switch to insert mode"},
      %{seq: "n", desc: "Insert Elixir cell below"},
      %{seq: "m", desc: "Insert Markdown cell below"},
      %{seq: "N", desc: "Insert Elixir cell above"},
      %{seq: "M", desc: "Insert Markdown cell above"},
      %{seq: "dd", desc: "Delete cell"},
      %{seq: "ee", desc: "Evaluate cell"},
      %{seq: "es", desc: "Evaluate section"},
      %{seq: "ej", desc: "Evaluate cells below"},
      %{seq: "ex", desc: "Cancel cell evaluation"}
    ]
  }

  @impl true
  def mount(socket) do
    {:ok, assign(socket, shortcuts: @shortcuts)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 sm:max-w-4xl sm:w-full flex flex-col space-y-3">
      <h3 class="text-lg font-medium text-gray-900">
        Keyboard shortcuts
      </h3>
      <p class="text-gray-500">
        LiveBook highly embraces keyboard navigation to improve your productivity.
        It operates in one of two modes similarly to the Vim text editor.
        In <span class="font-semibold">navigation mode</span> you move around
        the notebook and execute commands, whereas in the <span class="font-semibold">insert mode</span>
        you have editor focus and directly modify the given cell content.
      </p>
      <%= render_shortcuts_section("Navigation mode", @shortcuts.navigation_mode, @platform) %>
      <%= render_shortcuts_section("Insert mode", @shortcuts.insert_mode, @platform) %>
    </div>
    """
  end

  defp render_shortcuts_section(title, shortcuts, platform) do
    {left, right} = split_in_half(shortcuts)
    assigns = %{title: title, left: left, right: right, platform: platform}

    ~L"""
    <h3 class="text-lg font-medium text-gray-900">
      <%= @title %>
    </h3>
    <div class="mt-2 flex">
      <div class="w-1/2">
        <%= render_shortcuts_section_table(@left, @platform) %>
      </div>
      <div class="w-1/2">
        <%= render_shortcuts_section_table(@right, @platform) %>
      </div>
    </div>
    """
  end

  defp render_shortcuts_section_table(shortcuts, platform) do
    assigns = %{shortcuts: shortcuts, platform: platform}

    ~L"""
    <table>
      <tbody>
        <%= for shortcut <- @shortcuts do %>
          <tr>
            <td class="py-1 pr-4">
              <span class="bg-editor text-editor py-0.5 px-2 rounded-md inline-flex items-center">
                <%= if(@platform == :mac, do: seq_for_mac(shortcut.seq), else: shortcut.seq) %>
              </span>
            </td>
            <td>
              <%= shortcut.desc %>
            </td>
          </tr>
        <% end %>
      </tbody>
    </table>
    """
  end

  defp seq_for_mac(seq) do
    seq
    |> String.replace("ctrl", "cmd")
    |> String.replace("alt", "option")
  end

  defp split_in_half(list) do
    half_idx = list |> length() |> Kernel.+(1) |> div(2)
    Enum.split(list, half_idx)
  end
end
