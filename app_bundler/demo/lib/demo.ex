defmodule Demo.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      Demo.Window
    ]

    opts = [strategy: :one_for_one, name: Demo.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

defmodule Demo.Window do
  @moduledoc false
  use GenServer, restart: :transient

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  # https://github.com/erlang/otp/blob/OTP-24.1.2/lib/wx/include/wx.hrl#L1314
  @wx_id_exit 5006
  @wx_id_osx_hide 5250

  @impl true
  def init(_) do
    AppBundler.init()
    app_name = "Demo"
    os = AppBundler.os()
    wx = :wx.new()
    frame = :wxFrame.new(wx, -1, app_name, size: {400, 400})

    if os == :macos do
      fixup_macos_menubar(frame, app_name)
    end

    :wxFrame.show(frame)
    :wxFrame.connect(frame, :command_menu_selected, skip: true)
    :wxFrame.connect(frame, :close_window, skip: true)

    {:ok, %{frame: frame}}
  end

  @impl true
  def handle_info({:wx, @wx_id_exit, _, _, _}, state) do
    :init.stop()
    {:stop, :shutdown, state}
  end

  @impl true
  def handle_info({:wx, _, _, _, {:wxClose, :close_window}}, state) do
    :init.stop()
    {:stop, :shutdown, state}
  end

  # ignore other menu events
  @impl true
  def handle_info({:wx, _, _, _, {:wxCommand, :command_menu_selected, _, _, _}}, state) do
    {:noreply, state}
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

  # 1. WxeApp attaches event handler to "Quit" menu item that does nothing (to not accidentally bring
  #    down the VM). Let's create a fresh menu bar without that caveat.
  # 2. Fix app name
  defp fixup_macos_menubar(frame, app_name) do
    menubar = :wxMenuBar.new()
    :wxFrame.setMenuBar(frame, menubar)
    menu = :wxMenuBar.oSXGetAppleMenu(menubar)

    menu
    |> :wxMenu.findItem(@wx_id_osx_hide)
    |> :wxMenuItem.setItemLabel("Hide #{app_name}\tCtrl+H")

    menu
    |> :wxMenu.findItem(@wx_id_exit)
    |> :wxMenuItem.setItemLabel("Quit #{app_name}\tCtrl+Q")
  end
end
