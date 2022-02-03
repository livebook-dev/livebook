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
    <div class="p-6 pb-4 max-w-4xl flex flex-col space-y-3">
      <h3 class="text-2xl font-semibold text-gray-800">
        Add file system
      </h3>
      <div class="w-full flex-col space-y-5">
        <p class="text-gray-700">
          Configure an AWS S3 bucket as a drop in replacement of the regular
          file system. Many storage services offer an S3-compatible API, so
          you can use those as well. One such service is
          <a class="link" href="https://www.storj.io" rel="noreferrer noopener">Storj</a>
          and they offer free storage.
        </p>
        <p class="text-gray-700">
          Note that any other user of your Livebook could theoretically extract
          the credentials, so make sure to properly limit their access to this
          specific bucket.
        </p>
        <%= if @error_message do %>
          <div class="error-box">
            <%= @error_message %>
          </div>
        <% end %>
        <.form let={f} for={:data}
          phx-target={@myself}
          phx-submit="add"
          phx-change="validate"
          autocomplete="off"
          spellcheck="false">
          <div class="flex flex-col space-y-4">
            <div>
              <div class="input-label">Bucket URL</div>
              <%= text_input f, :bucket_url, value: @data["bucket_url"], class: "input", placeholder: "https://s3.[region].amazonaws.com/[bucket]" %>
            </div>
            <div>
              <div class="input-label">Access Key ID</div>
              <.with_password_toggle id="access-key">
                <%= text_input f, :access_key_id, value: @data["access_key_id"], class: "input", type: "password" %>
              </.with_password_toggle>
            </div>
            <div>
              <div class="input-label">Secret Access Key</div>
              <.with_password_toggle id="secret-access-key">
                <%= text_input f, :secret_access_key, value: @data["secret_access_key"], class: "input", type: "password" %>
              </.with_password_toggle>
            </div>
          </div>
          <div class="mt-5 flex justify-end space-x-2">
            <%= live_patch "Cancel", to: @return_to, class: "button-base button-outlined-gray" %>
            <button class="button-base button-blue"
              type="submit"
              disabled={not data_valid?(@data)}>
              Add
            </button>
          </div>
        </.form>
      </div>
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
        Livebook.Settings.append_file_system(file_system)
        send(self(), {:file_systems_updated, Livebook.Settings.file_systems()})
        {:noreply, push_patch(socket, to: socket.assigns.return_to)}

      {:error, message} ->
        {:noreply, assign(socket, error_message: "Connection test failed: " <> message)}
    end
  end

  defp empty_data() do
    %{"bucket_url" => "", "access_key_id" => "", "secret_access_key" => ""}
  end

  defp data_valid?(data) do
    Livebook.Utils.valid_url?(data["bucket_url"]) and data["access_key_id"] != "" and
      data["secret_access_key"] != ""
  end

  defp data_to_file_system(data) do
    FileSystem.S3.new(data["bucket_url"], data["access_key_id"], data["secret_access_key"])
  end
end
