if Mix.target() == :app do
  defmodule LivebookApp do
    @moduledoc false

    @behaviour :wx_object

    # https://github.com/erlang/otp/blob/OTP-24.1.2/lib/wx/include/wx.hrl#L1314
    @wx_id_exit 5006
    @wx_id_osx_hide 5250

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
      app_name = "Livebook"

      true = Process.register(self(), __MODULE__)
      os = os()

      # TODO: on all platforms either add a basic window with some buttons OR a wxwebview
      size = if os == :macos, do: {0, 0}, else: {100, 100}

      wx = :wx.new()
      frame = :wxFrame.new(wx, -1, app_name, size: size)
      :wxFrame.show(frame)

      if os == :macos do
        fixup_macos_menubar(frame, app_name)
      end

      :wxFrame.connect(frame, :command_menu_selected, skip: true)
      :wxFrame.connect(frame, :close_window, skip: true)

      case os do
        :macos ->
          :wx.subscribe_events()

        :windows ->
          windows_to_wx(System.get_env("LIVEBOOK_URL") || "")
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

    # This event is triggered when the application is opened for the first time
    @impl true
    def handle_info({:new_file, ''}, state) do
      Livebook.Utils.browser_open(LivebookWeb.Endpoint.access_url())
      {:noreply, state}
    end

    # TODO: investigate "Universal Links" [1], that is, instead of livebook://foo, we have
    # https://livebook.dev/foo, which means the link works with and without Livebook.app.
    #
    # [1] https://developer.apple.com/documentation/xcode/allowing-apps-and-websites-to-link-to-your-content
    @impl true
    def handle_info({:open_url, 'livebook://' ++ rest}, state) do
      "https://#{rest}"
      |> Livebook.Utils.notebook_import_url()
      |> Livebook.Utils.browser_open()

      {:noreply, state}
    end

    @impl true
    def handle_info({:open_file, path}, state) do
      path
      |> List.to_string()
      |> Livebook.Utils.notebook_open_url()
      |> Livebook.Utils.browser_open()

      {:noreply, state}
    end

    @impl true
    def handle_info({:reopen_app, _}, state) do
      Livebook.Utils.browser_open(LivebookWeb.Endpoint.access_url())
      {:noreply, state}
    end

    # ignore other events
    @impl true
    def handle_info(_event, state) do
      {:noreply, state}
    end

    # 1. WxeApp attaches event handler to "Quit" menu item that does nothing (to not accidentally bring
    #    down the VM). Let's create a fresh menu bar without that caveat.
    # 2. Fix app name
    defp fixup_macos_menubar(frame, app_name) do
      menubar = :wxMenuBar.new()
      :wxFrame.setMenuBar(frame, menubar)

      menu = :wxMenuBar.oSXGetAppleMenu(menubar)

      # without this, for some reason setting the title later will make it non-bold
      :wxMenu.getTitle(menu)

      # this is useful in dev, not needed when bundled in .app
      :wxMenu.setTitle(menu, app_name)

      menu
      |> :wxMenu.findItem(@wx_id_osx_hide)
      |> :wxMenuItem.setItemLabel("Hide #{app_name}\tCtrl+H")

      menu
      |> :wxMenu.findItem(@wx_id_exit)
      |> :wxMenuItem.setItemLabel("Quit #{app_name}\tCtrl+Q")
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

    defp windows_to_wx("livebook://" <> _ = url) do
      send(__MODULE__, {:open_url, String.to_charlist(url)})
    end

    defp windows_to_wx(path) do
      path =
        path
        |> String.replace("\\", "/")
        |> String.to_charlist()

      send(__MODULE__, {:open_file, path})
    end
  end
end
