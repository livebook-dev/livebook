defmodule LivebookWeb.SessionLive.PersistenceLive do
  # TODO: rewrite this live view as a component, once live_view
  # has a unified way of sending events programatically from a child
  # component to parent live view or component. Currently we send an
  # event to self() from FileSelectComponent and use handle_info in
  # the parent live view.
  use LivebookWeb, :live_view

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
       new_attrs: attrs
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        File
      </h3>
      <div class="w-full flex-col space-y-8">
        <div class="flex">
          <form phx-change="set_options" onsubmit="return false;" class="flex flex-col space-y-4">
            <.switch_checkbox
              name="persist_outputs"
              label="Persist outputs"
              checked={@new_attrs.persist_outputs} />
            <div class="flex space-x-2 items-center">
              <span class="text-gray-700 whitespace-nowrap">Autosave</span>
               <.select
                 name="autosave_interval_s"
                 selected={@new_attrs.autosave_interval_s}
                 options={[
                   {5, "every 5 seconds"},
                   {30, "every 30 seconds"},
                   {60, "every minute"},
                   {600, "every 10 minutes"},
                   {nil, "never"}
                 ]} />
            </div>
          </form>
        </div>
        <div class="flex space-x-4">
          <.choice_button
            active={@new_attrs.file != nil}
            phx-click="set_persistence_type"
            phx-value-type="file">
            Save to file
          </.choice_button>
          <.choice_button
            active={@new_attrs.file == nil}
            phx-click="set_persistence_type"
            phx-value-type="memory">
            Memory only
          </.choice_button>
        </div>
        <%= if @new_attrs.file do %>
          <div class="h-full h-52">
            <%= live_component LivebookWeb.FileSelectComponent,
                  id: "persistence_file_select",
                  file: @new_attrs.file,
                  extnames: [LiveMarkdown.extension()],
                  running_files: @running_files,
                  submit_event: if(disabled?(@new_attrs, @attrs, @running_files), do: nil, else: :save) %>
          </div>
        <% end %>
        <div class="flex flex-col space-y-8">
          <%= if @new_attrs.file do %>
            <div class="text-gray-500 text-sm">
              File: <%= normalize_file(@new_attrs.file).path %>
            </div>
          <% end %>
          <div>
            <button class="button button-blue"
              phx-click="save"
              disabled={disabled?(@new_attrs, @attrs, @running_files)}>
              Save
            </button>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("set_persistence_type", %{"type" => type}, socket) do
    file =
      case type do
        "file" -> socket.assigns.attrs.file || Livebook.Config.default_dir()
        "memory" -> nil
      end

    {:noreply, put_new_file(socket, file)}
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

  def handle_event("save", %{}, socket) do
    handle_save(socket)
  end

  @impl true
  def handle_info({:set_file, file, _file_info}, socket) do
    {:noreply, put_new_file(socket, file)}
  end

  def handle_info(:save, socket) do
    handle_save(socket)
  end

  defp handle_save(%{assigns: assigns} = socket) do
    file = normalize_file(assigns.new_attrs.file)
    autosave_interval_s = assigns.new_attrs.autosave_interval_s

    if file != assigns.attrs.file do
      Session.set_file(assigns.session.pid, file)
    end

    if autosave_interval_s != assigns.attrs.autosave_interval_s do
      Session.set_notebook_attributes(assigns.session.pid, %{
        autosave_interval_s: autosave_interval_s
      })
    end

    Session.set_notebook_attributes(assigns.session.pid, %{
      persist_outputs: assigns.new_attrs.persist_outputs
    })

    Session.save_sync(assigns.session.pid)

    running_files =
      if file do
        [file | assigns.running_files]
      else
        List.delete(assigns.running_files, file)
      end

    if file do
      # After saving the file reload the directory contents,
      # so that the new file gets shown.
      send_update(LivebookWeb.FileSelectComponent,
        id: "persistence_file_select",
        running_files: running_files,
        force_reload: true
      )
    end

    {:noreply, assign(socket, running_files: running_files, attrs: assigns.new_attrs)}
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

  defp normalize_file(nil), do: nil

  defp normalize_file(file) do
    Map.update!(file, :path, fn path ->
      if String.ends_with?(path, LiveMarkdown.extension()) do
        path
      else
        path <> LiveMarkdown.extension()
      end
    end)
  end

  defp disabled?(new_attrs, attrs, running_files) do
    if normalize_file(new_attrs.file) == attrs.file do
      new_attrs.persist_outputs == attrs.persist_outputs and
        new_attrs.autosave_interval_s == attrs.autosave_interval_s
    else
      not file_savable?(normalize_file(new_attrs.file), running_files)
    end
  end

  defp file_savable?(nil, _running_files), do: true

  defp file_savable?(file, running_files) do
    not FileSystem.File.dir?(file) and file not in running_files
  end
end
