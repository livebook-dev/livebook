defmodule LivebookWeb.Hub.FileSystemFormComponent do
  use LivebookWeb, :live_component

  alias Livebook.FileSystem
  alias Livebook.FileSystems

  @impl true
  def update(assigns, socket) do
    {file_system, assigns} = Map.pop!(assigns, :file_system)

    mode = mode(file_system)
    button = button(file_system)
    title = title(file_system)

    file_system = file_system || %FileSystem.S3{hub_id: assigns.hub.id}
    changeset = FileSystems.change_file_system(file_system)
    socket = assign(socket, assigns)

    {:ok,
     assign(socket,
       file_system: file_system,
       changeset: changeset,
       mode: mode,
       title: title,
       button: button,
       error_message: nil
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        <%= @title %>
      </h3>
      <p class="text-gray-700">
        Configure an AWS S3 bucket as a Livebook file storage.
        Many storage services offer an S3-compatible API and
        those work as well.
      </p>
      <div :if={@error_message} class="error-box">
        <%= @error_message %>
      </div>
      <.form
        :let={f}
        id="file-systems-form"
        for={to_form(@changeset, as: :file_system)}
        phx-target={@myself}
        phx-submit="save"
        phx-change="validate"
        autocomplete="off"
        spellcheck="false"
      >
        <div class="flex flex-col space-y-4">
          <.text_field
            field={f[:bucket_url]}
            label="Bucket URL"
            placeholder="https://s3.[region].amazonaws.com/[bucket]"
          />
          <.text_field field={f[:region]} label="Region (optional)" />
          <%= if Livebook.Config.aws_credentials?() do %>
            <.password_field field={f[:access_key_id]} label="Access Key ID (optional)" />
            <.password_field field={f[:secret_access_key]} label="Secret Access Key (optional)" />
            <p class="text-xs text-gray-700">
              You may leave Access Key fields empty. In such cases,
              they will be automatically read from your environment variables,
              AWS credentials, or Amazon EC2/ECS metadata.
            </p>
          <% else %>
            <.password_field field={f[:access_key_id]} label="Access Key ID" />
            <.password_field field={f[:secret_access_key]} label="Secret Access Key" />
            <p class="text-xs text-gray-700">
              Start Livebook with <code>LIVEBOOK_AWS_CREDENTIALS</code> environment
              variable set if you want to automatically read credentials from
              environment variables, AWS credentials, or Amazon EC2/ECS metadata.
            </p>
          <% end %>
          <div class="flex space-x-2">
            <button class="button-base button-blue" type="submit" disabled={not @changeset.valid?}>
              <.remix_icon icon={@button.icon} class="align-middle mr-1" />
              <span class="font-normal"><%= @button.label %></span>
            </button>
            <.link patch={@return_to} class="button-base button-outlined-gray">
              Cancel
            </.link>
          </div>
        </div>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"file_system" => attrs}, socket) do
    changeset =
      socket.assigns.file_system
      |> FileSystems.change_file_system(attrs)
      |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("save", %{"file_system" => attrs}, socket) do
    changeset = FileSystems.change_file_system(socket.assigns.file_system, attrs)

    with {:ok, file_system} <- Ecto.Changeset.apply_action(changeset, :update),
         :ok <- check_file_system_connectivity(file_system),
         :ok <- save_file_system(file_system, socket) do
      message =
        case socket.assigns.mode do
          :new -> "File storage added successfully"
          :edit -> "File storage updated successfully"
        end

      {:noreply,
       socket
       |> put_flash(:success, message)
       |> push_redirect(to: socket.assigns.return_to)}
    else
      {:error, %Ecto.Changeset{} = changeset} -> {:noreply, assign(socket, changeset: changeset)}
      {:transport_error, message} -> {:noreply, assign(socket, error_message: message)}
      {:error, message} -> {:noreply, assign(socket, error_message: message)}
    end
  end

  defp check_file_system_connectivity(file_system) do
    default_path = FileSystem.default_path(file_system)

    case FileSystem.list(file_system, default_path, false) do
      {:ok, _} -> :ok
      {:error, message} -> {:error, "Connection test failed: " <> message}
    end
  end

  defp save_file_system(file_system, socket) do
    case socket.assigns.mode do
      :new -> Livebook.Hubs.create_file_system(socket.assigns.hub, file_system)
      :edit -> Livebook.Hubs.update_file_system(socket.assigns.hub, file_system)
    end
  end

  defp mode(nil), do: :new
  defp mode(_), do: :edit

  defp title(nil), do: "Add file storage"
  defp title(_), do: "Edit file storage"

  defp button(nil), do: %{icon: "add-line", label: "Add"}
  defp button(_), do: %{icon: "save-line", label: "Save"}
end
