defmodule LivebookWeb.SessionLive.PersistenceLive do
  # TODO: rewrite this live view as a component, once live_view
  # has a unified way of sending events programmatically from a child
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
      persist_outputs: persist_outputs,
      autosave_interval_s: autosave_interval_s
    }

    {:ok,
     assign(socket,
       session: session,
       running_files: running_files,
       attrs: attrs,
       new_attrs: attrs,
       draft_file: file || Livebook.Config.local_filesystem_home(),
       saved_file: file
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Save to file
      </h3>

      <div class="w-full flex-col space-y-6">
        <div class="h-full h-52">
          <.live_component
            module={LivebookWeb.FileSelectComponent}
            id="persistence_file_select"
            file={@draft_file}
            extnames={[LiveMarkdown.extension()]}
            running_files={@running_files}
            submit_event={:confirm_file}
          />
        </div>

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
                options={[
                  {5, "every 5 seconds"},
                  {30, "every 30 seconds"},
                  {60, "every minute"},
                  {600, "every 10 minutes"},
                  {nil, "never"}
                ]}
              />
            </div>
          </div>

          <span class="text-gray-700 whitespace-nowrap pt-2">
            File: <%= normalize_file(@draft_file).path %>
          </span>
        </form>
      </div>
      <div class="flex justify-between">
        <div class="flex space-x-3">
          <button
            class="button-base button-blue"
            phx-click="save"
            disabled={not savable?(@draft_file, @saved_file, @running_files)}
          >
            Save
          </button>

          <%= live_patch("Cancel",
            to: Routes.session_path(@socket, :page, @session.id),
            class: "button-base button-outlined-gray"
          ) %>
        </div>

        <%= if @saved_file do %>
          <button class="button-base button-outlined-red" phx-click="stop_saving">
            Stop saving
          </button>
        <% end %>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("clear_file", %{}, socket) do
    {:noreply, socket |> put_new_file(nil) |> assign(draft_file: nil)}
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
    save(socket)
  end

  def handle_event("stop_saving", %{}, socket) do
    Session.set_file(socket.assigns.session.pid, nil)

    {:noreply,
     push_patch(socket, to: Routes.session_path(socket, :page, socket.assigns.session.id))}
  end

  @impl true
  def handle_info({:set_file, file, _file_info}, socket) do
    {:noreply, assign(socket, draft_file: file)}
  end

  def handle_info(:confirm_file, socket) do
    save(socket)
  end

  defp save(%{assigns: assigns} = socket) do
    %{new_attrs: new_attrs, attrs: attrs, draft_file: draft_file, saved_file: saved_file} =
      assigns

    draft_file = normalize_file(draft_file)

    if draft_file != saved_file do
      Session.set_file(assigns.session.pid, draft_file)
    end

    diff = map_diff(new_attrs, attrs)

    if diff != %{} do
      Session.set_notebook_attributes(assigns.session.pid, diff)
    end

    if draft_file do
      Session.save_sync(assigns.session.pid)
    end

    {:noreply, push_patch(socket, to: Routes.session_path(socket, :page, assigns.session.id))}
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

  defp normalize_file(file) do
    Map.update!(file, :path, fn path ->
      if String.ends_with?(path, LiveMarkdown.extension()) do
        path
      else
        path <> LiveMarkdown.extension()
      end
    end)
  end

  defp savable?(draft_file, saved_file, running_files) do
    file = normalize_file(draft_file)
    not FileSystem.File.dir?(draft_file) and (file not in running_files or file == saved_file)
  end

  defp map_diff(left, right) do
    Map.new(Map.to_list(left) -- Map.to_list(right))
  end
end
