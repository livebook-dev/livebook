defmodule LivebookWeb.SessionLive.AddFileEntryUrlComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  @impl true
  def mount(socket) do
    {:ok, assign(socket, changeset: changeset(), error_message: nil, fetching: false)}
  end

  @impl true
  def update(%{file_entry_result: file_entry_result}, socket) do
    socket = assign(socket, fetching: false)

    case file_entry_result do
      {:ok, file_entry} ->
        {:ok, add_file_entry(socket, file_entry)}

      {:error, message} ->
        {:ok, assign(socket, error_message: Livebook.Utils.upcase_first(message))}
    end
  end

  def update(assigns, socket) do
    {:ok, assign(socket, assigns)}
  end

  defp changeset(attrs \\ %{}) do
    data = %{url: nil, name: nil, copy: false}
    types = %{url: :string, name: :string, copy: :boolean}

    cast({data, types}, attrs, [:url, :name, :copy])
    |> validate_required([:url, :name, :copy])
    |> Livebook.Notebook.validate_file_entry_name(:name)
    |> Livebook.Utils.validate_url(:url)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <div :if={@error_message} class="mb-6 error-box">
        <%= @error_message %>
      </div>
      <.form
        :let={f}
        for={@changeset}
        as={:data}
        id="add-file-entry-form"
        phx-change="validate"
        phx-submit="add"
        phx-target={@myself}
      >
        <div class="flex flex-col space-y-4">
          <.text_field
            field={f[:url]}
            label="URL"
            autocomplete="off"
            phx-debounce="200"
            phx-blur="url_blur"
            phx-target={@myself}
          />
          <.text_field
            field={f[:name]}
            label="Name"
            id="add-file-entry-form-name"
            autocomplete="off"
            phx-debounce="200"
          />
          <.radio_field
            field={f[:copy]}
            options={[
              {"false", "Store only URL location"},
              {"true", "Download URL contents to the notebook files directory"}
            ]}
          />
        </div>
        <div class="mt-6 flex space-x-3">
          <button
            class="button-base button-blue"
            type="submit"
            disabled={not @changeset.valid? or @fetching}
          >
            <.spinner :if={@fetching} class="mr-2" />
            <span>Add</span>
          </button>
          <.link patch={~p"/sessions/#{@session.id}"} class="button-base button-outlined-gray">
            Cancel
          </.link>
        </div>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => data}, socket) do
    changeset = data |> changeset() |> Map.replace!(:action, :validate)
    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("url_blur", %{"value" => url}, socket) do
    name = Livebook.Utils.url_basename(url)

    socket =
      if socket.assigns.changeset.params["name"] == "" and name != "" do
        # Emulate input event to make sure validation errors are shown
        exec_js(
          socket,
          JS.dispatch("lb:set_value",
            to: "#add-file-entry-form-name",
            detail: %{value: name}
          )
          |> JS.dispatch("input", to: "#add-file-entry-form-name")
          |> JS.dispatch("blur", to: "#add-file-entry-form-name")
        )
      else
        socket
      end

    {:noreply, socket}
  end

  def handle_event("add", %{"data" => data}, socket) do
    data
    |> changeset()
    |> apply_action(:insert)
    |> case do
      {:ok, data} ->
        file_entry = %{name: data.name, type: :url, url: data.url}

        if data.copy do
          async_create_attachment_file_entry(socket, file_entry)
          {:noreply, assign(socket, fetching: true, error_message: nil)}
        else
          {:noreply, add_file_entry(socket, file_entry)}
        end

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  defp async_create_attachment_file_entry(socket, file_entry) do
    pid = self()
    id = socket.assigns.id
    session = socket.assigns.session

    Task.Supervisor.async_nolink(Livebook.TaskSupervisor, fn ->
      file_entry_result = Livebook.Session.to_attachment_file_entry(session, file_entry)
      send_update(pid, __MODULE__, id: id, file_entry_result: file_entry_result)
    end)
  end

  defp add_file_entry(socket, file_entry) do
    Livebook.Session.add_file_entries(socket.assigns.session.pid, [file_entry])
    # We can't do push_patch from update/2, so we ask the LV to do so
    send(self(), {:push_patch, ~p"/sessions/#{socket.assigns.session.id}"})
    socket
  end
end
