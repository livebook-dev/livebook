defmodule LivebookWeb.SessionLive.PersistenceComponent do
  use LivebookWeb, :live_component

  alias Livebook.{Session, SessionSupervisor, LiveMarkdown}

  @impl true
  def mount(socket) do
    session_summaries = SessionSupervisor.get_session_summaries()
    running_paths = Enum.map(session_summaries, & &1.path)
    {:ok, assign(socket, running_paths: running_paths)}
  end

  @impl true
  def update(assigns, socket) do
    {path, assigns} = Map.pop!(assigns, :path)
    {persist_outputs, assigns} = Map.pop!(assigns, :persist_outputs)

    attrs = %{path: path, persist_outputs: persist_outputs}

    socket =
      socket
      |> assign(assigns)
      |> assign(attrs: attrs, new_attrs: attrs)

    {:ok, socket}
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
          <form phx-change="set_options" onsubmit="return false;" phx-target={@myself}>
            <.switch_checkbox
              name="persist_outputs"
              label="Persist outputs"
              checked={@new_attrs.persist_outputs} />
          </form>
        </div>
        <div class="flex space-x-4">
          <.choice_button
            active={@new_attrs.path != nil}
            phx-click="set_persistence_type"
            phx-value-type="file"
            phx-target={@myself}>
            Save to file
          </.choice_button>
          <.choice_button
            active={@new_attrs.path == nil}
            phx-click="set_persistence_type"
            phx-value-type="memory"
            phx-target={@myself}>
            Memory only
          </.choice_button>
        </div>
        <%= if @new_attrs.path != nil do %>
          <div class="h-full h-52">
            <%= live_component LivebookWeb.PathSelectComponent,
                  id: "path_select",
                  path: @new_attrs.path,
                  extnames: [LiveMarkdown.extension()],
                  running_paths: @running_paths,
                  phx_target: @myself,
                  phx_submit: if(disabled?(@new_attrs, @attrs, @running_paths), do: nil, else: "save") %>
          </div>
        <% end %>
        <div class="flex flex-col space-y-8">
          <%= if @new_attrs.path != nil do %>
            <div class="text-gray-500 text-sm">
              File: <%= normalize_path(@new_attrs.path) %>
            </div>
          <% end %>
          <div>
            <button class="button button-blue"
              phx-click="save"
              phx-target={@myself}
              disabled={disabled?(@new_attrs, @attrs, @running_paths)}>
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
    path =
      case type do
        "file" -> socket.assigns.attrs.path || default_path()
        "memory" -> nil
      end

    {:noreply, put_new_attr(socket, :path, path)}
  end

  def handle_event("set_path", %{"path" => path}, socket) do
    {:noreply, put_new_attr(socket, :path, path)}
  end

  def handle_event("set_options", %{"persist_outputs" => persist_outputs}, socket) do
    persist_outputs = persist_outputs == "true"
    {:noreply, put_new_attr(socket, :persist_outputs, persist_outputs)}
  end

  def handle_event("save", %{}, %{assigns: assigns} = socket) do
    path = normalize_path(assigns.new_attrs.path)

    if path != assigns.attrs.path do
      Session.set_path(assigns.session_id, path)
    end

    Session.set_notebook_attributes(assigns.session_id, %{
      persist_outputs: assigns.new_attrs.persist_outputs
    })

    Session.save_sync(assigns.session_id)

    running_paths =
      if path do
        [path | assigns.running_paths]
      else
        List.delete(assigns.running_paths, path)
      end

    # After saving the file reload the directory contents,
    # so that the new file gets shown.
    send_update(LivebookWeb.PathSelectComponent,
      id: "path_select",
      running_paths: running_paths,
      force_reload: true
    )

    {:noreply, assign(socket, running_paths: running_paths)}
  end

  defp put_new_attr(socket, key, value) do
    new_attrs = socket.assigns.new_attrs
    new_attrs = put_in(new_attrs[key], value)
    assign(socket, :new_attrs, new_attrs)
  end

  defp default_path() do
    Livebook.Config.root_path() |> Path.join("notebook")
  end

  defp path_savable?(nil, _running_paths), do: true

  defp path_savable?(path, running_paths) do
    if File.exists?(path) do
      File.regular?(path) and path not in running_paths
    else
      true
    end
  end

  defp normalize_path(nil), do: nil

  defp normalize_path(path) do
    if String.ends_with?(path, LiveMarkdown.extension()) do
      path
    else
      path <> LiveMarkdown.extension()
    end
  end

  defp disabled?(new_attrs, attrs, running_paths) do
    if normalize_path(new_attrs.path) == attrs.path do
      new_attrs.persist_outputs == attrs.persist_outputs
    else
      not path_savable?(normalize_path(new_attrs.path), running_paths)
    end
  end
end
