if Mix.target() == :app do
  defmodule LivebookApp do
    @moduledoc false
    @name __MODULE__
    @wxID_OPEN 5000
    @wxID_EXIT 5006
    @wxBITMAP_TYPE_PNG 15

    use GenServer

    def start_link(arg) do
      GenServer.start_link(__MODULE__, arg, name: @name)
    end

    taskbar_icon_path = "rel/app/icon.png"
    @external_resource taskbar_icon_path
    @taskbar_icon File.read!(taskbar_icon_path)

    @impl true
    def init(_) do
      AppBundler.init()
      os = AppBundler.os()
      :wx.new()

      # TODO: instead of embedding the icon and copying to tmp, copy the file known location.
      # It's a bit tricky because it needs to support all the ways of running the app:
      # 1. MIX_TARGET=app mix phx.server
      # 2. mix app
      # 3. mix release app
      taskbar_icon_path = Path.join(System.tmp_dir!(), "icon.png")
      File.write!(taskbar_icon_path, @taskbar_icon)
      icon = :wxIcon.new(taskbar_icon_path, type: @wxBITMAP_TYPE_PNG)

      menu_items = [
        {"Open Browser", key: "ctrl+o", id: @wxID_OPEN},
        {"Quit", key: "ctrl+q", id: @wxID_EXIT}
      ]

      taskbar = WxUtils.taskbar("Livebook", icon, menu_items)

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
    def handle_info(:open_app, state) do
      open_browser()
      {:noreply, state}
    end

    @impl true
    def handle_info({:open_file, path}, state) do
      path
      |> Livebook.Utils.notebook_open_url()
      |> open_browser()

      {:noreply, state}
    end

    @impl true
    def handle_info({:open_url, "livebook://" <> rest}, state) do
      "https://#{rest}"
      |> Livebook.Utils.notebook_import_url()
      |> open_browser()

      {:noreply, state}
    end

    @impl true
    def handle_info({:wx, @wxID_EXIT, _, _, _}, _state) do
      System.stop(0)
    end

    @impl true
    def handle_info({:wx, @wxID_OPEN, _, _, _}, state) do
      open_browser()
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

  defmodule WxUtils do
    @moduledoc false
    @wxID_ANY -1

    def taskbar(title, icon, menu_items) do
      pid = self()
      os = AppBundler.os()
      options = if os == :macos, do: [iconType: 1], else: []

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
              case os do
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
          id = Keyword.get(options, :id, @wxID_ANY)

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
  end
end
