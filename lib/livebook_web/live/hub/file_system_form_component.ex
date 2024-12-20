defmodule LivebookWeb.Hub.FileSystemFormComponent do
  use LivebookWeb, :live_component

  alias Livebook.FileSystem
  alias Livebook.FileSystems

  @impl true
  def mount(socket) do
    {:ok, assign(socket, changeset: nil, error_message: nil)}
  end

  @impl true
  def update(assigns, socket) do
    {file_system, assigns} = Map.pop!(assigns, :file_system)

    mode = mode(file_system)
    button = button_attrs(file_system)
    title = title(file_system)

    file_system = file_system || %FileSystem.S3{hub_id: assigns.hub.id}
    changeset = socket.assigns.changeset || FileSystems.change_file_system(file_system)

    {:ok,
     socket
     |> assign(assigns)
     |> assign(
       file_system: file_system,
       changeset: changeset,
       mode: mode,
       title: title,
       button: button
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        {@title}
      </h3>
      <p class="text-gray-700">
        Configure an AWS S3 bucket as a Livebook file storage.
        Many storage services offer an S3-compatible API and
        those work as well.
      </p>
      <div :if={@error_message} class="error-box">
        {@error_message}
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
          <.text_field
            field={f[:region]}
            label="Region (optional)"
            placeholder={
              FileSystem.S3.region_from_url(Ecto.Changeset.get_field(@changeset, :bucket_url) || "")
            }
          />
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
            <.button type="submit" disabled={@disabled or not @changeset.valid?}>
              <.remix_icon icon={@button.icon} />
              <span class="font-normal">{@button.label}</span>
            </.button>
            <.button color="gray" outlined patch={@return_to}>
              Cancel
            </.button>
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
    changeset =
      socket.assigns.file_system
      |> FileSystems.change_file_system(attrs)
      |> FileSystem.S3.maybe_put_region_from_url()

    with {:ok, file_system} <- Ecto.Changeset.apply_action(changeset, :update),
         :ok <- check_file_system_connectivity(file_system),
         :ok <- save_file_system(file_system, changeset, socket) do
      message =
        case socket.assigns.mode do
          :new -> "File storage added successfully"
          :edit -> "File storage updated successfully"
        end

      {:noreply,
       socket
       |> put_flash(:success, message)
       |> push_patch(to: socket.assigns.return_to)}
    else
      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, changeset: Map.replace!(changeset, :action, :validate))}

      {:transport_error, message} ->
        {:noreply, assign(socket, error_message: message)}

      {:error, message} ->
        {:noreply, assign(socket, error_message: message)}
    end
  end

  defp check_file_system_connectivity(file_system) do
    default_path = FileSystem.default_path(file_system)

    case FileSystem.list(file_system, default_path, false) do
      {:ok, _} -> :ok
      {:error, message} -> {:error, "Connection test failed: " <> message}
    end
  end

  defp save_file_system(file_system, changeset, socket) do
    result =
      case socket.assigns.mode do
        :new -> Livebook.Hubs.create_file_system(socket.assigns.hub, file_system)
        :edit -> Livebook.Hubs.update_file_system(socket.assigns.hub, file_system)
      end

    with {:error, errors} <- result do
      {:error,
       changeset
       |> Livebook.Utils.put_changeset_errors(errors)
       |> Map.replace!(:action, :validate)}
    end
  end

  defp mode(nil), do: :new
  defp mode(_), do: :edit

  defp title(nil), do: "Add file storage"
  defp title(_), do: "Edit file storage"

  defp button_attrs(nil), do: %{icon: "add-line", label: "Add"}
  defp button_attrs(_), do: %{icon: "save-line", label: "Save"}
end
