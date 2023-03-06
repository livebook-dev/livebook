defmodule LivebookWeb.SettingsLive.AddFileSystemComponent do
  use LivebookWeb, :live_component

  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    {:ok, assign(socket, data: empty_data(), error_message: nil)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        Add file system
      </h3>
      <p class="text-gray-700">
        Configure an AWS S3 bucket as a Livebook file system.
        Many storage services offer an S3-compatible API and
        those work as well.
      </p>
      <div :if={@error_message} class="error-box">
        <%= @error_message %>
      </div>
      <.form
        :let={f}
        for={@data}
        as={:data}
        phx-target={@myself}
        phx-submit="add"
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
          <.password_field field={f[:access_key_id]} label="Access Key ID" />
          <.password_field field={f[:secret_access_key]} label="Access Key ID" />
          <div class="flex space-x-2">
            <button class="button-base button-blue" type="submit" disabled={not data_valid?(@data)}>
              Add
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
  def handle_event("validate", %{"data" => data}, socket) do
    {:noreply, assign(socket, :data, data)}
  end

  def handle_event("add", %{"data" => data}, socket) do
    file_system = data_to_file_system(data)
    default_dir = FileSystem.File.new(file_system)

    case FileSystem.File.list(default_dir) do
      {:ok, _} ->
        Livebook.Settings.save_file_system(file_system)
        send(self(), {:file_systems_updated, Livebook.Settings.file_systems()})
        {:noreply, push_patch(socket, to: socket.assigns.return_to)}

      {:error, message} ->
        {:noreply, assign(socket, error_message: "Connection test failed: " <> message)}
    end
  end

  defp empty_data() do
    %{"bucket_url" => "", "region" => "", "access_key_id" => "", "secret_access_key" => ""}
  end

  defp data_valid?(data) do
    Livebook.Utils.valid_url?(data["bucket_url"]) and data["access_key_id"] != "" and
      data["secret_access_key"] != ""
  end

  defp data_to_file_system(data) do
    region = if(data["region"] != "", do: data["region"])

    FileSystem.S3.new(data["bucket_url"], data["access_key_id"], data["secret_access_key"],
      region: region
    )
  end
end
