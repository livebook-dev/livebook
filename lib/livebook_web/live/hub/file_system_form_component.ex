defmodule LivebookWeb.Hub.FileSystemFormComponent do
  use LivebookWeb, :live_component

  alias Livebook.FileSystem
  alias Livebook.FileSystems

  @impl true
  def mount(socket) do
    {:ok, assign(socket, error_message: nil)}
  end

  @impl true
  def update(assigns, socket) do
    {file_system, assigns} = Map.pop!(assigns, :file_system)

    mode = mode(file_system)
    title = title(file_system)
    button = button_attrs(file_system)
    file_system = file_system || %FileSystem.S3{hub_id: assigns.hub}
    changeset = FileSystems.change_file_system(file_system)

    {:ok,
     socket
     |> assign_new(:hub, fn -> assigns.hub end)
     |> assign_new(:file_system, fn -> file_system end)
     |> assign_new(:type, fn -> FileSystems.type(file_system) end)
     |> assign_new(:mode, fn -> mode end)
     |> assign_new(:title, fn -> title end)
     |> assign_new(:button, fn -> button end)
     |> assign_new(:return_to, fn -> assigns.return_to end)
     |> assign_new(:disabled, fn -> assigns.disabled end)
     |> assign_form(changeset)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        {@title}
      </h3>

      <div :if={@error_message} class="error-box">
        {@error_message}
      </div>

      <div>
        <label class="mb-2 flex items-center gap-1 text-sm text-gray-800 font-medium">
          Type
        </label>

        <div class="flex gap-y-6 sm:gap-x-4">
          <.radio_card_input
            id="file_system_type-s3"
            name="file_system[type]"
            title="S3"
            phx-click={JS.push("select_type", value: %{value: :s3})}
            phx-target={@myself}
            value={@type}
            checked_value={:s3}
            disabled={@mode == :edit}
          >
            Configure an AWS S3 bucket as a Livebook file storage. Many storage services offer an S3-compatible API and those work as well.
          </.radio_card_input>

          <.radio_card_input
            id="file_system_type-git"
            name="file_system[type]"
            title="Git"
            phx-click={JS.push("select_type", value: %{value: :git})}
            phx-target={@myself}
            value={@type}
            checked_value={:git}
            disabled={@mode == :edit}
          >
            Configure a read-only Git repository as a Livebook file storage. You will need a valid SSH key to access your Git repository.
          </.radio_card_input>
        </div>
      </div>

      <.form
        id="file-systems-form"
        for={@form}
        phx-target={@myself}
        phx-submit="save"
        phx-change="validate"
        autocomplete="off"
        spellcheck="false"
      >
        <div class="flex flex-col space-y-4">
          <.file_system_form_fields {assigns} />

          <div class="flex space-x-2">
            <.button type="submit" disabled={@disabled or not @form.source.valid?}>
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

  defp file_system_form_fields(%{file_system: %FileSystem.S3{}} = assigns) do
    ~H"""
    <div class="flex flex-col space-y-4">
      <.text_field
        field={@form[:bucket_url]}
        label="Bucket URL"
        placeholder="https://s3.[region].amazonaws.com/[bucket]"
      />
      <.text_field
        field={@form[:region]}
        label="Region (optional)"
        placeholder={FileSystem.S3.region_from_url(@form[:bucket_url].value || "")}
      />
      <%= if Livebook.Config.aws_credentials?() do %>
        <.password_field field={@form[:access_key_id]} label="Access Key ID (optional)" />
        <.password_field field={@form[:secret_access_key]} label="Secret Access Key (optional)" />
        <p class="text-xs text-gray-700">
          You may leave Access Key fields empty. In such cases,
          they will be automatically read from your environment variables,
          AWS credentials, or Amazon EC2/ECS metadata.
        </p>
      <% else %>
        <.password_field field={@form[:access_key_id]} label="Access Key ID" />
        <.password_field field={@form[:secret_access_key]} label="Secret Access Key" />
        <p class="text-xs text-gray-700">
          Start Livebook with <code>LIVEBOOK_AWS_CREDENTIALS</code> environment
          variable set if you want to automatically read credentials from
          environment variables, AWS credentials, or Amazon EC2/ECS metadata.
        </p>
      <% end %>
    </div>
    """
  end

  defp file_system_form_fields(%{file_system: %FileSystem.Git{}} = assigns) do
    ~H"""
    <div class="flex flex-col space-y-4">
      <.text_field
        field={@form[:repo_url]}
        label="Repository URL"
        placeholder="git@[provider]:[username]/[repo_name].git"
      />
      <.text_field
        field={@form[:branch]}
        label="Branch"
        placeholder="main"
      />
      <.password_field field={@form[:key]} label="SSH Private Key" />
      <p class="text-xs text-gray-700">
        You can use your own SSH Private Key or you can set up the Deploy Key
        only for this repository. Check the documentation from your
        version control system to create your Deploy Key.
      </p>
    </div>
    """
  end

  @impl true
  def handle_event("select_type", %{"value" => type}, socket) do
    if to_string(socket.assigns.type) == to_string(type) do
      {:noreply, socket}
    else
      file_system = FileSystems.load(type, %{"hub_id" => socket.assigns.hub.id})
      changeset = FileSystems.change_file_system(file_system)

      {:noreply,
       socket
       |> assign(type: type, file_system: file_system)
       |> assign_form(changeset)}
    end
  end

  def handle_event("validate", %{"file_system" => attrs}, socket) do
    changeset = FileSystems.change_file_system(socket.assigns.file_system, attrs)
    {:noreply, assign_form(socket, changeset)}
  end

  def handle_event("save", %{"file_system" => attrs}, socket) do
    changeset = prepare_to_save(socket.assigns.file_system, attrs)

    with {:ok, file_system} <- Ecto.Changeset.apply_action(changeset, :update),
         :ok <- check_file_system_connectivity(file_system, socket),
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
      {:error, %Ecto.Changeset{} = changeset} -> {:noreply, assign_form(socket, changeset)}
      {:transport_error, message} -> {:noreply, assign(socket, error_message: message)}
      {:error, message} -> {:noreply, assign(socket, error_message: message)}
    end
  end

  defp check_file_system_connectivity(file_system, socket) do
    default_path = FileSystem.default_path(file_system)

    with :ok <- FileSystem.mount(file_system),
         {:ok, _} <- FileSystem.list(file_system, default_path, false) do
      if socket.assigns.mode == :new do
        FileSystem.unmount(file_system)
      end

      :ok
    else
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

  defp prepare_to_save(file_system, attrs) do
    changeset = FileSystems.change_file_system(file_system, attrs)

    if FileSystems.type(file_system) == "s3" do
      FileSystem.S3.maybe_put_region_from_url(changeset)
    else
      changeset
    end
  end

  defp assign_form(socket, changeset) do
    changeset = Map.replace!(changeset, :action, :validate)
    assign(socket, :form, to_form(changeset, as: :file_system))
  end

  defp mode(nil), do: :new
  defp mode(_), do: :edit

  defp title(nil), do: "Add file storage"
  defp title(_), do: "Edit file storage"

  defp button_attrs(nil), do: %{icon: "add-line", label: "Add"}
  defp button_attrs(_), do: %{icon: "save-line", label: "Save"}
end
