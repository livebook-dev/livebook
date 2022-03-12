defmodule WxDemo.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      WxDemo.Window
    ]

    opts = [strategy: :one_for_one, name: WxDemo.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

defmodule WxDemo.Window do
  @moduledoc false

  @behaviour :wx_object

  # https://github.com/erlang/otp/blob/OTP-24.1.2/lib/wx/include/wx.hrl#L1314
  @wx_id_exit 5006

  def start_link(_) do
    {:wx_ref, _, _, pid} = :wx_object.start_link(__MODULE__, [], [])
    {:ok, pid}
  end

  def child_spec(init_arg) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [init_arg]},
      restart: :transient
    }
  end

  def windows_connected(url) do
    url
    |> String.trim()
    |> String.trim_leading("\"")
    |> String.trim_trailing("\"")
    |> windows_to_wx()
  end

  @impl true
  def init(_) do
    title = "WxDemo"

    true = Process.register(self(), __MODULE__)
    os = os()
    wx = :wx.new()
    frame = :wxFrame.new(wx, -1, title, size: {100, 100})

    if os == :macos do
      fixup_macos_menubar(frame, title)
    end

    :wxFrame.show(frame)
    :wxFrame.connect(frame, :command_menu_selected)
    :wxFrame.connect(frame, :close_window, skip: true)

    case os do
      :macos ->
        :wx.subscribe_events()

      :windows ->
        windows_to_wx(System.get_env("WXDEMO_URL") || "")
    end

    state = %{frame: frame}
    {frame, state}
  end

  @impl true
  def handle_event({:wx, @wx_id_exit, _, _, _}, state) do
    :init.stop()
    {:stop, :shutdown, state}
  end

  @impl true
  def handle_event({:wx, _, _, _, {:wxClose, :close_window}}, state) do
    :init.stop()
    {:stop, :shutdown, state}
  end

  @impl true
  def handle_info(event, state) do
    show_dialog(state, inspect(event))
    {:noreply, state}
  end

  # Helpers

  defp show_dialog(state, data) do
    :wxMessageDialog.new(state.frame, data)
    |> :wxDialog.showModal()
  end

  defp fixup_macos_menubar(frame, title) do
    menubar = :wxMenuBar.new()
    # :wxMenuBar.setAutoWindowMenu(false)
    :wxFrame.setMenuBar(frame, menubar)

    # App Menu
    menu = :wxMenuBar.oSXGetAppleMenu(menubar)

    # Remove all items except for quit
    for item <- :wxMenu.getMenuItems(menu) do
      if :wxMenuItem.getId(item) == @wx_id_exit do
        :wxMenuItem.setText(item, "Quit #{title}\tCtrl+Q")
      else
        :wxMenu.delete(menu, item)
      end
    end
  end

  defp os() do
    case :os.type() do
      {:unix, :darwin} -> :macos
      {:win32, _} -> :windows
    end
  end

  defp windows_to_wx("") do
    send(__MODULE__, {:new_file, ''})
  end

  defp windows_to_wx("wxdemo://" <> _ = url) do
    send(__MODULE__, {:open_url, String.to_charlist(url)})
  end

  defp windows_to_wx(path) do
    send(__MODULE__, {:open_file, String.to_charlist(path)})
  end
end
