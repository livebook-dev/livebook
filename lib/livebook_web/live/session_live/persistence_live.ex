defmodule LivebookWeb.SessionLive.PersistenceLive do
  # TODO: rewrite this live view as a component, once live_view
  # has a unified way of sending events programmatically from a child
  # component to parent live view or component. Currently we send an
  # event to self() from FileSelectComponent and use handle_info in
  # the parent live view.
  use LivebookWeb, :live_view

  import LivebookWeb.FileSystemHelpers

  alias Livebook.{Sessions, Session, LiveMarkdown, FileSystem}

  @impl true
  def mount(
        _params,
        %{
          "session" => session,
          "file" => file,
          "persist_outputs" => persist_outputs,
          "autosave_interval_s" => autosave_interval_s
        },
        socket
      ) do
    sessions = Sessions.list_sessions()
    running_files = Enum.map(sessions, & &1.file)

    attrs = %{
      file: file,
      persist_outputs: persist_outputs,
      autosave_interval_s: autosave_interval_s
    }

    {:ok,
     assign(socket,
       session: session,
       running_files: running_files,
       attrs: attrs,
       new_attrs: attrs,
       draft_file: nil,
       saved_file: nil,
       suggested_file: safe_suggested_file(session, attrs.file)
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Save to file
      </h3>
      <div class="w-full flex-col space-y-8">
        <div class="flex">
          <form
            phx-change="set_options"
            onsubmit="return false;"
            class="flex flex-col space-y-4 items-start max-w-full"
          >
            <div class="flex flex-col space-y-4">
              <.switch_checkbox
                name="persist_outputs"
                label="Persist outputs"
                checked={@new_attrs.persist_outputs}
              />
              <div class="flex space-x-2 items-center">
                <span class="text-gray-700 whitespace-nowrap">Autosave</span>
                <.select
                  name="autosave_interval_s"
                  selected={@new_attrs.autosave_interval_s}
                  options={
                    [
                      {5, "every 5 seconds"},
                      {30, "every 30 seconds"},
                      {60, "every minute"},
                      {600, "every 10 minutes"},
                      {nil, "never"}
                    ]
                  }
                />
              </div>
            </div>
            <.file_location_display label="File:" file={@new_attrs.file}>
              <%= if @new_attrs.file do %>
                <button
                  class="button-base button-gray button-small"
                  phx-click="open_file_select"
                  disabled={@draft_file != nil}
                >
                  Change file
                </button>
                <button class="button-base button-gray button-small" phx-click="clear_file">
                  Stop saving
                </button>
              <% else %>
                <%= unless @draft_file do %>
                  <button class="button-base button-gray button-small" phx-click="open_file_select">
                    Choose a file
                  </button>
                <% end %>
              <% end %>
            </.file_location_display>
            <%= if (!@new_attrs.file or !@session.file) and @suggested_file do %>
              <.file_location_display file={@suggested_file}>
                <:before_location>
                  <button
                    class="button-base button-gray button-small"
                    phx-click="open_file_select_suggested"
                    disabled={
                      select_suggested_disabled?(@suggested_file, @draft_file, @new_attrs.file)
                    }
                  >
                    Select Suggested
                  </button>
                </:before_location>
              </.file_location_display>
            <% end %>
          </form>
        </div>
        <%= if @draft_file do %>
          <div class="flex flex-col">
            <div class="h-full h-52">
              <.live_component
                module={LivebookWeb.FileSelectComponent}
                id="persistence_file_select"
                file={@draft_file}
                extnames={[extension()]}
                running_files={@running_files}
                submit_event={:confirm_file}
              >
                <div class="flex justify-end space-x-2">
                  <button class="button-base button-gray" phx-click="close_file_select" tabindex="-1">
                    Cancel
                  </button>
                  <button
                    class="button-base button-blue"
                    phx-click="confirm_file"
                    disabled={FileSystem.File.dir?(@draft_file)}
                    tabindex="-1"
                  >
                    Choose
                  </button>
                </div>
              </.live_component>
            </div>
            <div class="mt-6 text-gray-500 text-sm">
              File: <%= normalize_file(@draft_file).path %>
            </div>
          </div>
        <% end %>
        <div class="flex">
          <%= if @new_attrs.file do %>
            <button
              class="button-base button-blue"
              phx-click="save"
              disabled={
                not savable?(@new_attrs, @attrs, @running_files, @draft_file) or
                  (same_attrs?(@new_attrs, @attrs) and @saved_file == @new_attrs.file)
              }
            >
              Save now
            </button>
          <% else %>
            <button
              class="button-base button-blue"
              phx-click="save"
              disabled={
                not savable?(@new_attrs, @attrs, @running_files, @draft_file) or
                  same_attrs?(@new_attrs, @attrs)
              }
            >
              Apply
            </button>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("open_file_select", %{}, socket) do
    file = socket.assigns.new_attrs.file || Livebook.Config.local_filesystem_home()

    socket =
      case socket.assigns.new_attrs.file do
        nil -> socket
        file -> input_select_range(socket, file)
      end

    {:noreply, assign(socket, draft_file: file)}
  end

  def handle_event("close_file_select", %{}, socket) do
    suggested_file = safe_suggested_file(socket.assigns.session, socket.assigns.new_attrs.file)
    {:noreply, assign(socket, draft_file: nil, suggested_file: suggested_file)}
  end

  def handle_event("confirm_file", %{}, socket) do
    handle_confirm_file(socket)
  end

  def handle_event("clear_file", %{}, socket) do
    socket =
      socket
      |> put_new_file(nil)
      |> assign(draft_file: nil)
      |> assign(suggested_file: safe_suggested_file(socket.assigns.session))

    {:noreply, socket}
  end

  def handle_event("open_file_select_suggested", %{}, socket) do
    %{suggested_file: suggested_file} = socket.assigns
    %{path: dirname} = FileSystem.File.containing_dir(suggested_file)

    select_params = [
      String.length(dirname),
      String.length(suggested_file.path)
    ]

    socket =
      socket
      |> assign(draft_file: suggested_file)
      |> push_event("focus", %{to: "#input-path"})
      |> push_event("select_range", %{to: "#input-path", params: select_params})

    {:noreply, socket}
  end

  def handle_event(
        "set_options",
        %{"persist_outputs" => persist_outputs, "autosave_interval_s" => autosave_interval_s},
        socket
      ) do
    persist_outputs = persist_outputs == "true"
    autosave_interval_s = parse_optional_integer(autosave_interval_s)

    {:noreply,
     socket
     |> put_new_attr(:persist_outputs, persist_outputs)
     |> put_new_attr(:autosave_interval_s, autosave_interval_s)}
  end

  def handle_event("save", %{}, %{assigns: assigns} = socket) do
    %{new_attrs: new_attrs, attrs: attrs} = assigns
    new_attrs = Map.update!(new_attrs, :file, &normalize_file/1)
    diff = map_diff(new_attrs, attrs)

    if Map.has_key?(diff, :file) do
      Session.set_file(assigns.session.pid, diff.file)
    end

    notebook_attrs_diff = Map.take(diff, [:autosave_interval_s, :persist_outputs])

    if notebook_attrs_diff != %{} do
      Session.set_notebook_attributes(assigns.session.pid, notebook_attrs_diff)
    end

    if new_attrs.file do
      Session.save_sync(assigns.session.pid)
    end

    {:noreply, push_patch(socket, to: Routes.session_path(socket, :page, assigns.session.id))}
  end

  @impl true
  def handle_info({:set_file, file, _file_info}, socket) do
    suggested_file = safe_suggested_file(socket.assigns.session, file)
    {:noreply, assign(socket, draft_file: file, suggested_file: suggested_file)}
  end

  def handle_info(:confirm_file, socket) do
    handle_confirm_file(socket)
  end

  defp handle_confirm_file(socket) do
    file = normalize_file(socket.assigns.draft_file)

    socket =
      socket
      |> put_new_file(file)
      |> assign(draft_file: nil)
      |> assign(suggested_file: safe_suggested_file(socket.assigns.session, file))

    {:noreply, socket}
  end

  defp parse_optional_integer(string) do
    case Integer.parse(string) do
      {number, _} -> number
      :error -> nil
    end
  end

  defp put_new_file(socket, file) do
    new_attrs = socket.assigns.new_attrs
    current_file_system = new_attrs.file && new_attrs.file.file_system
    new_file_system = file && file.file_system

    autosave_interval_s =
      case new_file_system do
        ^current_file_system ->
          new_attrs.autosave_interval_s

        nil ->
          Livebook.Notebook.default_autosave_interval_s()

        %FileSystem.Local{} ->
          Livebook.Notebook.default_autosave_interval_s()

        _other ->
          nil
      end

    socket
    |> put_new_attr(:file, file)
    |> put_new_attr(:autosave_interval_s, autosave_interval_s)
  end

  defp put_new_attr(socket, key, value) do
    new_attrs = socket.assigns.new_attrs

    if new_attrs[key] == value do
      socket
    else
      new_attrs = put_in(new_attrs[key], value)
      assign(socket, :new_attrs, new_attrs)
    end
  end

  def extension(), do: LiveMarkdown.extension()

  defp normalize_file(nil), do: nil

  defp normalize_file(file) do
    FileSystem.File.force_extension(file, extension())
  end

  def safe_suggested_file(session, selected_file \\ nil, ext \\ extension()) do
    root_path =
      case selected_file do
        nil -> Livebook.Config.local_filesystem_home()
        file -> FileSystem.File.containing_dir(normalize_file(file))
      end

    with suggested_filename when is_binary(suggested_filename) <-
           Session.suggested_filename(session, ext),
         false <- suggested_filename in ["untitled_notebook.livemd", "untitled.livemd"] do
      FileSystem.File.resolve(
        root_path,
        suggested_filename
      )
      |> FileSystem.File.to_savable_without_conflict(ext)
    else
      _ -> nil
    end
  end

  defp savable?(new_attrs, attrs, running_files, draft_file) do
    new_attrs = Map.update!(new_attrs, :file, &normalize_file/1)
    valid_file? = new_attrs.file == attrs.file or file_savable?(new_attrs.file, running_files)
    valid_file? and draft_file == nil
  end

  defp same_attrs?(new_attrs, attrs) do
    new_attrs = Map.update!(new_attrs, :file, &normalize_file/1)
    new_attrs == attrs
  end

  defp file_savable?(nil, _running_files), do: true

  defp file_savable?(file, running_files) do
    not FileSystem.File.dir?(file) and file not in running_files
  end

  defp map_diff(left, right) do
    Map.new(Map.to_list(left) -- Map.to_list(right))
  end

  def select_suggested_disabled?(suggested, draft, new) do
    cond do
      draft != nil -> suggested == draft
      new != nil -> suggested == new
      true -> false
    end
  end

  def input_select_range(socket, file, to \\ "#input-path") do
    if FileSystem.File.dir?(file) do
      socket
    else
      dir = FileSystem.File.containing_dir(file)

      params = [
        String.length(dir.path),
        String.length(file.path)
      ]

      push_event(
        socket,
        "select_range",
        %{to: to, params: params}
      )
    end
  end
end
