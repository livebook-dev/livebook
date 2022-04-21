if Mix.target() == :app do
  defmodule WxUtils do
    @moduledoc false

    defmacro wxID_ANY, do: -1
    defmacro wxID_OPEN, do: 5000
    defmacro wxID_EXIT, do: 5006
    defmacro wxID_OSX_HIDE, do: 5250
    defmacro wxBITMAP_TYPE_PNG, do: 15

    def os do
      case :os.type() do
        {:unix, :darwin} -> :macos
        {:win32, _} -> :windows
      end
    end

    def taskbar(title, icon, menu_items) do
      pid = self()
      options = if os() == :macos, do: [iconType: 1], else: []

      # skip keyboard shortcuts
      menu_items =
        for item <- menu_items do
          {title, options} = item
          options = Keyword.delete(options, :key)
          {title, options}
        end

      taskbar =
        :wxTaskBarIcon.new(
          [
            createPopupMenu: fn ->
              menu = menu(menu_items)

              # For some reason, on macOS the menu event must be handled in another process
              # but on Windows it must be either the same process OR we use the callback.
              case os() do
                :macos ->
                  env = :wx.get_env()

                  Task.start_link(fn ->
                    :wx.set_env(env)
                    :wxMenu.connect(menu, :command_menu_selected)

                    receive do
                      message ->
                        send(pid, message)
                    end
                  end)

                :windows ->
                  :ok =
                    :wxMenu.connect(menu, :command_menu_selected,
                      callback: fn wx, _ ->
                        send(pid, wx)
                      end
                    )
              end

              menu
            end
          ] ++ options
        )

      :wxTaskBarIcon.setIcon(taskbar, icon, tooltip: title)
      taskbar
    end

    def menu(items) do
      menu = :wxMenu.new()

      Enum.each(items, fn
        {title, options} ->
          id = Keyword.get(options, :id, wxID_ANY())

          title =
            case Keyword.fetch(options, :key) do
              {:ok, key} ->
                title <> "\t" <> key

              :error ->
                title
            end

          :wxMenu.append(menu, id, title)
      end)

      menu
    end

    def menubar(app_name, menus) do
      menubar = :wxMenuBar.new()

      if os() == :macos, do: fixup_macos_menubar(menubar, app_name)

      for {title, menu_items} <- menus do
        true = :wxMenuBar.append(menubar, menu(menu_items), title)
      end

      menubar
    end

    defp fixup_macos_menubar(menubar, app_name) do
      menu = :wxMenuBar.oSXGetAppleMenu(menubar)

      menu
      |> :wxMenu.findItem(wxID_OSX_HIDE())
      |> :wxMenuItem.setItemLabel("Hide #{app_name}\tCtrl+H")

      menu
      |> :wxMenu.findItem(wxID_EXIT())
      |> :wxMenuItem.setItemLabel("Quit #{app_name}\tCtrl+Q")
    end
  end

  defmodule LivebookApp do
    @moduledoc false
    @name __MODULE__

    use GenServer
    import WxUtils

    def start_link(arg) do
      GenServer.start_link(__MODULE__, arg, name: @name)
    end

    taskbar_icon_path = "rel/app/taskbar_icon.png"
    @external_resource taskbar_icon_path
    @taskbar_icon File.read!(taskbar_icon_path)

    @impl true
    def init(_) do
      os = os()
      wx = :wx.new()
      AppBuilder.Wx.subscribe_to_app_events(@name)

      menu_items = [
        {"Open Browser", key: "ctrl+o", id: wxID_OPEN()},
        {"Quit", key: "ctrl+q", id: wxID_EXIT()}
      ]

      if os == :macos do
        :wxMenuBar.setAutoWindowMenu(false)

        menubar =
          menubar("Livebook", [
            {"File", menu_items}
          ])

        :ok = :wxMenuBar.connect(menubar, :command_menu_selected, skip: true)

        # TODO: use :wxMenuBar.macSetCommonMenuBar/1 when OTP 25 is out
        frame = :wxFrame.new(wx, -1, "", size: {0, 0})
        :wxFrame.show(frame)
        :wxFrame.setMenuBar(frame, menubar)
      end

      # TODO: instead of embedding the icon and copying to tmp, copy the file known location.
      # It's a bit tricky because it needs to support all the ways of running the app:
      # 1. MIX_TARGET=app mix phx.server
      # 2. mix app
      # 3. mix release app
      taskbar_icon_path = Path.join(System.tmp_dir!(), "taskbar_icon.png")
      File.write!(taskbar_icon_path, @taskbar_icon)
      icon = :wxIcon.new(taskbar_icon_path, type: wxBITMAP_TYPE_PNG())

      taskbar = taskbar("Livebook", icon, menu_items)

      if os == :windows do
        :wxTaskBarIcon.connect(taskbar, :taskbar_left_down,
          callback: fn _, _ ->
            open_browser()
          end
        )
      end

      {:ok, nil}
    end

    @impl true
    def handle_info({:wx, wxID_EXIT(), _, _, _}, _state) do
      System.stop(0)
    end

    @impl true
    def handle_info({:wx, wxID_OPEN(), _, _, _}, state) do
      open_browser()
      {:noreply, state}
    end

    # This event is triggered when the application is opened for the first time
    @impl true
    def handle_info({:new_file, ''}, state) do
      open_browser()
      {:noreply, state}
    end

    @impl true
    def handle_info({:reopen_app, _}, state) do
      open_browser()
      {:noreply, state}
    end

    @impl true
    def handle_info({:open_file, path}, state) do
      path
      |> List.to_string()
      |> Livebook.Utils.notebook_open_url()
      |> open_browser()

      {:noreply, state}
    end

    @impl true
    def handle_info({:open_url, 'livebook://' ++ rest}, state) do
      "https://#{rest}"
      |> Livebook.Utils.notebook_import_url()
      |> open_browser()

      {:noreply, state}
    end

    if Mix.env() == :dev do
      @impl true
      def handle_info(event, state) do
        IO.inspect(event)
        {:noreply, state}
      end
    end

    defp open_browser(url \\ LivebookWeb.Endpoint.access_url()) do
      Livebook.Utils.browser_open(url)
    end
  end
end
