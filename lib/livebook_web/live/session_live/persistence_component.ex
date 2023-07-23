defmodule LivebookWeb.SessionLive.PersistenceComponent do
  use LivebookWeb, :live_component

  alias Livebook.{Sessions, Session, LiveMarkdown, FileSystem}

  @impl true
  def mount(socket) do
    sessions = Sessions.list_sessions()
    running_files = Enum.map(sessions, & &1.file)
    {:ok, assign(socket, running_files: running_files)}
  end

  @impl true
  def update(%{event: {:set_file, file, _info}}, socket) do
    current_file_system = socket.assigns.draft_file.file_system

    autosave_interval_s =
      case file.file_system do
        ^current_file_system ->
          socket.assigns.new_attrs.autosave_interval_s

        %FileSystem.Local{} ->
          Livebook.Notebook.default_autosave_interval_s()

        _other ->
          nil
      end

    {:ok,
     socket
     |> assign(draft_file: file)
     |> put_new_attr(:autosave_interval_s, autosave_interval_s)}
  end

  def update(%{event: :confirm_file}, socket) do
    {:ok, save(socket)}
  end

  def update(assigns, socket) do
    {file, assigns} = Map.pop!(assigns, :file)
    {persist_outputs, assigns} = Map.pop!(assigns, :persist_outputs)
    {autosave_interval_s, assigns} = Map.pop!(assigns, :autosave_interval_s)

    attrs = %{
      persist_outputs: persist_outputs,
      autosave_interval_s: autosave_interval_s
    }

    socket =
      socket
      |> assign(assigns)
      |> assign_new(:attrs, fn -> attrs end)
      |> assign_new(:new_attrs, fn -> attrs end)
      |> assign_new(:draft_file, fn ->
        file ||
          case assigns.session.origin do
            # If it's a forked notebook, default to the same folder
            {:file, file} -> FileSystem.File.containing_dir(file)
            _ -> Livebook.Config.local_file_system_home()
          end
      end)
      |> assign_new(:saved_file, fn -> file end)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Save to file
      </h3>
      <div class="w-full flex-col space-y-6">
        <div class="h-80">
          <.live_component
            module={LivebookWeb.FileSelectComponent}
            id="persistence_file_select"
            file={@draft_file}
            extnames={[LiveMarkdown.extension()]}
            running_files={@running_files}
            submit_event={:confirm_file}
            target={{__MODULE__, @id}}
          />
        </div>
        <div>
          <.label>File</.label>
          <div class="whitespace-nowrap text-gray-700 leading-4">
            <%= normalize_file(@draft_file).path %>
          </div>
        </div>
        <form
          phx-change="set_options"
          phx-target={@myself}
          phx-nosubmit
          class="flex flex-col items-start max-w-full"
        >
          <div class="flex flex-col space-y-6">
            <.select_field
              name="autosave_interval_s"
              label="Autosave"
              value={@new_attrs.autosave_interval_s || ""}
              options={[
                {"every 5 seconds", "5"},
                {"every 30 seconds", "30"},
                {"every minute", "60"},
                {"every 10 minutes", "600"},
                {"never", ""}
              ]}
            />
            <.checkbox_field
              name="persist_outputs"
              label="Persist outputs"
              value={@new_attrs.persist_outputs}
            />
          </div>
        </form>
      </div>
      <div class="flex justify-between">
        <div class="flex space-x-3">
          <button
            class="button-base button-blue"
            phx-click="save"
            phx-target={@myself}
            disabled={not savable?(@draft_file, @saved_file, @running_files)}
          >
            Save
          </button>
          <.link patch={~p"/sessions/#{@session.id}"} class="button-base button-outlined-gray">
            Cancel
          </.link>
        </div>
        <button
          :if={@saved_file}
          class="button-base button-outlined-red"
          phx-click="stop_saving"
          phx-target={@myself}
        >
          Stop saving to file
        </button>
      </div>
    </div>
    """
  end

  @impl true
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
    {:noreply, save(socket)}
  end

  def handle_event("stop_saving", %{}, socket) do
    Session.set_file(socket.assigns.session.pid, nil)

    {:noreply, push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}")}
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

    Session.save_sync(assigns.session.pid)

    # We can't do push_patch from update/2, so we ask the LV to do so
    send(self(), {:push_patch, ~p"/sessions/#{assigns.session.id}"})

    socket
  end

  defp parse_optional_integer(string) do
    case Integer.parse(string) do
      {number, _} -> number
      :error -> nil
    end
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
    FileSystem.File.ensure_extension(file, LiveMarkdown.extension())
  end

  defp savable?(draft_file, saved_file, running_files) do
    file = normalize_file(draft_file)
    not FileSystem.File.dir?(draft_file) and (file not in running_files or file == saved_file)
  end

  defp map_diff(left, right) do
    Map.new(Map.to_list(left) -- Map.to_list(right))
  end
end
