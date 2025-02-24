defmodule LivebookWeb.SessionLive.PersistenceComponent do
  use LivebookWeb, :live_component

  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    sessions = Livebook.Sessions.list_sessions()
    running_files = for session <- sessions, session.file, do: session.file
    {:ok, assign(socket, running_files: running_files)}
  end

  @impl true
  def update(%{event: {:set_file, file, _info}}, socket) do
    current_file = socket.assigns.draft_file

    autosave_interval_s =
      cond do
        FileSystem.File.same_file_system?(file, current_file) ->
          socket.assigns.new_attrs.autosave_interval_s

        FileSystem.File.local?(file) ->
          Livebook.Notebook.default_autosave_interval_s()

        true ->
          nil
      end

    {:ok,
     socket
     |> assign(draft_file: file)
     |> put_new_attr(:autosave_interval_s, autosave_interval_s)}
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
            _ -> Livebook.Settings.default_dir(assigns.hub)
          end
      end)
      |> assign_new(:saved_file, fn -> file end)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Save to file
      </h3>
      <div class="w-full flex-col space-y-6">
        <div class="h-80">
          <.live_component
            module={LivebookWeb.FileSelectComponent}
            id="persistence_file_select"
            file={@draft_file}
            hub={@hub}
            extnames={[Livebook.LiveMarkdown.extension()]}
            running_files={@running_files}
            on_submit={JS.push("save", target: @myself)}
            target={{__MODULE__, @id}}
          />
        </div>
        <div>
          <.label>File</.label>
          <div class="whitespace-nowrap text-gray-700 leading-4">
            {normalize_file(@draft_file).path}
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
          <.button
            phx-click="save"
            phx-target={@myself}
            disabled={not savable?(@draft_file, @saved_file, @running_files)}
          >
            Save
          </.button>
          <.button color="gray" outlined patch={~p"/sessions/#{@session.id}"}>
            Cancel
          </.button>
        </div>
        <.button :if={@saved_file} color="red" outlined phx-click="stop_saving" phx-target={@myself}>
          Stop saving to file
        </.button>
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
    Livebook.Session.set_file(socket.assigns.session.pid, nil)

    {:noreply, push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}")}
  end

  defp save(%{assigns: assigns} = socket) do
    %{new_attrs: new_attrs, attrs: attrs, draft_file: draft_file, saved_file: saved_file} =
      assigns

    draft_file = normalize_file(draft_file)

    if draft_file != saved_file do
      Livebook.Session.set_file(assigns.session.pid, draft_file)
    end

    diff = map_diff(new_attrs, attrs)

    if diff != %{} do
      Livebook.Session.set_notebook_attributes(assigns.session.pid, diff)
    end

    Livebook.Session.save_sync(assigns.session.pid)

    push_patch(socket, to: return_to(assigns))
  end

  defp return_to(assigns) do
    if context = assigns.context do
      ~p"/sessions/#{assigns.session.id}/#{context}"
    else
      ~p"/sessions/#{assigns.session.id}"
    end
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
    FileSystem.File.ensure_extension(file, Livebook.LiveMarkdown.extension())
  end

  defp savable?(draft_file, saved_file, running_files) do
    file = normalize_file(draft_file)
    running? = Enum.any?(running_files, &FileSystem.File.equal?(&1, file))
    changed? = saved_file == nil or not FileSystem.File.equal?(file, saved_file)
    not FileSystem.File.dir?(draft_file) and (running? or changed?)
  end

  defp map_diff(left, right) do
    Map.new(Map.to_list(left) -- Map.to_list(right))
  end
end
