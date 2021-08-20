defmodule LivebookWeb.SessionLive.ShortcutsComponent do
  use LivebookWeb, :live_component

  @shortcuts %{
    insert_mode: [
      %{seq: ["esc"], desc: "Switch back to navigation mode", basic: true},
      %{seq: ["tab"], desc: "Autocomplete expression when applicable", basic: true},
      %{
        seq: ["ctrl", "␣"],
        press_all: true,
        desc: "Show completion list, use twice for details",
        basic: true
      },
      %{
        seq: ["ctrl", "i"],
        seq_mac: ["⌘", "i"],
        press_all: true,
        desc: "Show identifier documentation",
        basic: true
      },
      %{
        seq: ["ctrl", "shift", "i"],
        seq_mac: ["⇧", "⌥", "f"],
        seq_windows: ["shift", "alt", "f"],
        press_all: true,
        desc: "Format Elixir code",
        basic: true
      },
      %{
        seq: ["ctrl", "/"],
        seq_mac: ["⌘", "/"],
        press_all: true,
        desc: "Toggle lines comment"
      },
      %{
        seq: ["ctrl", "shift", "k"],
        seq_mac: ["⌘", "⇧", "k"],
        press_all: true,
        desc: "Delete lines"
      },
      %{
        seq: ["ctrl", "]"],
        seq_mac: ["⌘", "]"],
        press_all: true,
        desc: "Indent lines"
      },
      %{
        seq: ["ctrl", "]"],
        seq_mac: ["⌘", "]"],
        press_all: true,
        desc: "Outdent lines"
      },
      %{
        seq: ["ctrl", "h"],
        seq_mac: ["⌘", "⌥", "f"],
        press_all: true,
        desc: "Replace"
      },
      %{
        seq: ["alt", "↑"],
        seq_mac: ["⌥", "↑"],
        press_all: true,
        desc: "Move lines up"
      },
      %{
        seq: ["alt", "↓"],
        seq_mac: ["⌥", "↓"],
        press_all: true,
        desc: "Move lines down"
      },
      %{
        seq: ["ctrl", "←"],
        seq_mac: ["⌥", "←"],
        press_all: true,
        desc: "Cursor skip word left"
      },
      %{
        seq: ["ctrl", "→"],
        seq_mac: ["⌥", "→"],
        press_all: true,
        desc: "Cursor skip word right"
      }
    ],
    navigation_mode: [
      %{seq: ["?"], desc: "Open this help modal", basic: true},
      %{seq: ["j"], desc: "Focus next cell", basic: true},
      %{seq: ["k"], desc: "Focus previous cell", basic: true},
      %{seq: ["J"], desc: "Move cell down"},
      %{seq: ["K"], desc: "Move cell up"},
      %{seq: ["i"], desc: "Switch to insert mode", basic: true},
      %{seq: ["n"], desc: "Insert Elixir cell below", basic: true},
      %{seq: ["m"], desc: "Insert Markdown cell below", basic: true},
      %{seq: ["N"], desc: "Insert Elixir cell above"},
      %{seq: ["M"], desc: "Insert Markdown cell above"},
      %{seq: ["d", "d"], desc: "Delete cell", basic: true},
      %{seq: ["e", "e"], desc: "Evaluate cell"},
      %{seq: ["e", "s"], desc: "Evaluate section"},
      %{seq: ["e", "a"], desc: "Evaluate all stale/new cells", basic: true},
      %{seq: ["e", "j"], desc: "Evaluate cells below"},
      %{seq: ["e", "x"], desc: "Cancel cell evaluation"},
      %{seq: ["s", "s"], desc: "Toggle sections panel"},
      %{seq: ["s", "u"], desc: "Toggle users panel"},
      %{seq: ["s", "r"], desc: "Show runtime settings"},
      %{seq: ["s", "b"], desc: "Show bin"},
      %{seq: ["0", "0"], desc: "Restart current runtime"}
    ],
    universal: [
      %{
        seq: ["ctrl", "↵"],
        seq_mac: ["⌘", "↵"],
        press_all: true,
        desc: "Evaluate cell in either mode",
        basic: true
      },
      %{
        seq: ["ctrl", "s"],
        seq_mac: ["⌘", "s"],
        press_all: true,
        desc: "Save notebook",
        basic: true
      }
    ]
  }

  @impl true
  def mount(socket) do
    {:ok, assign(socket, shortcuts: @shortcuts, basic: false)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-3">
      <h3 class="text-2xl font-semibold text-gray-800">
        Keyboard shortcuts
      </h3>
      <p class="text-gray-700">
        Livebook highly embraces keyboard navigation to improve your productivity.
        It operates in one of two modes similarly to the Vim text editor.
        In <span class="font-semibold">navigation mode</span> you move around
        the notebook and execute commands, whereas in the <span class="font-semibold">insert mode</span>
        you have editor focus and directly modify the given cell content.
      </p>
      <.shortcuts_section title="Navigation mode" shortcuts={@shortcuts.navigation_mode} basic={@basic} platform={@platform} />
      <.shortcuts_section title="Insert mode" shortcuts={@shortcuts.insert_mode} basic={@basic} platform={@platform} />
      <.shortcuts_section title="Universal" shortcuts={@shortcuts.universal} basic={@basic} platform={@platform} />
      <div class="mt-8 flex justify-end">
        <form phx-change="settings" onsubmit="return false;" phx-target={@myself}>
          <.switch_checkbox
            name="basic"
            label="Basic view (essential shortcuts only)"
            checked={@basic} />
        </form>
      </div>
    </div>
    """
  end

  defp shortcuts_section(assigns) do
    shortcuts =
      if assigns.basic do
        Enum.filter(assigns.shortcuts, & &1[:basic])
      else
        assigns.shortcuts
      end

    {left, right} = split_in_half(shortcuts)
    assigns = assign(assigns, left: left, right: right)

    ~H"""
    <h3 class="text-lg font-medium text-gray-900 pt-4">
      <%= @title %>
    </h3>
    <div class="mt-2 flex flex-col lg:flex-row lg:space-x-4">
      <div class="lg:flex-grow">
        <.shortcuts_section_table shortcuts={@left} platform={@platform} />
      </div>
      <div class="lg:w-1/2">
        <.shortcuts_section_table shortcuts={@right} platform={@platform} />
      </div>
    </div>
    """
  end

  defp shortcuts_section_table(assigns) do
    ~H"""
    <table>
      <tbody>
        <%= for shortcut <- @shortcuts do %>
          <tr>
            <td class="py-2 pr-3">
              <.shortcut shortcut={shortcut} platform={@platform} />
            </td>
            <td class="lg:whitespace-nowrap">
              <%= shortcut.desc %>
            </td>
          </tr>
        <% end %>
      </tbody>
    </table>
    """
  end

  defp shortcut(%{shortcut: shortcut, platform: platform}) do
    seq = shortcut[:"seq_#{platform}"] || shortcut.seq
    press_all = Map.get(shortcut, :press_all, false)

    joiner =
      if press_all do
        assigns = %{}

        ~H"""
        <.remix_icon icon="add-line" class="text-lg text-gray-600" />
        """
      end

    elements = Enum.map_intersperse(seq, joiner, &content_tag("kbd", &1))

    assigns = %{elements: elements}

    ~H"""
    <div class="flex space-x-1 items-center markdown">
      <%= for element <- @elements do %>
        <%= element %>
      <% end %>
    </div>
    """
  end

  defp split_in_half(list) do
    half_idx = list |> length() |> Kernel.+(1) |> div(2)
    Enum.split(list, half_idx)
  end

  @impl true
  def handle_event("settings", %{"basic" => basic}, socket) do
    basic? = basic == "true"
    {:noreply, assign(socket, :basic, basic?)}
  end
end
