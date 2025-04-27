defmodule Livebook.Apps.TeamsAppSpec do
  # App spec for organization apps fetched from Livebook Teams.

  @enforce_keys [:slug, :version, :hub_id, :app_deployment_id]

  defstruct [:slug, :version, :hub_id, :app_deployment_id]
end

defimpl Livebook.Apps.AppSpec, for: Livebook.Apps.TeamsAppSpec do
  alias Livebook.Hubs

  def load(app_spec, files_tmp_path) do
    with {:ok, hub} <- fetch_hub(app_spec.hub_id),
         {:ok, app_deployment, derived_key} <-
           fetch_download_info(hub.id, app_spec.app_deployment_id),
         {:ok, encrypted_binary} <- download(hub, app_deployment),
         {:ok, archive_binary} <- decrypt(encrypted_binary, derived_key),
         {:ok, notebook_source} <- unzip(archive_binary, files_tmp_path) do
      {notebook, %{warnings: warnings}} =
        Livebook.LiveMarkdown.notebook_from_livemd(notebook_source)

      {:ok, %{notebook: notebook, warnings: warnings}}
    end
  end

  defp fetch_hub(hub_id) do
    with :error <- Hubs.fetch_hub(hub_id) do
      {:error, "the hub no longer exists"}
    end
  end

  defp fetch_download_info(hub_id, app_deployment_id) do
    with :error <- Hubs.TeamClient.get_app_deployment_download_info(hub_id, app_deployment_id) do
      {:error, "the app deployment no longer exists"}
    end
  end

  defp download(hub, app_deployment) do
    case Livebook.Teams.Requests.download_revision(hub, app_deployment) do
      {:ok, body} ->
        {:ok, body}

      {:error, %{} = error} ->
        {:error, "downloading app archive failed, reason: #{inspect(error)}"}

      {:error, message} ->
        {:error, "downloading app archive failed, reason: #{message}"}

      {:transport_error, message} ->
        {:error, "downloading app archive failed, reason: #{message}"}
    end
  end

  defp decrypt(binary, derived_key) do
    with :error <- Livebook.Teams.decrypt(binary, derived_key) do
      {:error, "failed to decrypt app archive"}
    end
  end

  defp unzip(archive_binary, files_tmp_path) do
    File.mkdir_p!(files_tmp_path)

    case :zip.extract(archive_binary, [:memory]) do
      {:ok, entries} ->
        Enum.reduce_while(entries, {:error, "notebook file missing in the app archive"}, fn
          {~c"notebook.livemd", binary}, _acc ->
            {:cont, {:ok, binary}}

          {~c"files/" ++ name, binary}, acc ->
            name = List.to_string(name)
            destination = Path.join(files_tmp_path, name)

            case File.write(destination, binary) do
              :ok ->
                {:cont, acc}

              {:error, reason} ->
                message =
                  "failed to write a notebook file #{destination}, reason: #{:file.format_error(reason)}"

                {:halt, {:error, message}}
            end
        end)

      {:error, {name, reason}} ->
        {:error,
         "failed to unzip app archive, entry #{name} failed with reason: #{inspect(reason)}"}

      {:error, reason} ->
        {:error, "failed to unzip app archive, reason: #{inspect(reason)}"}
    end
  end

  def should_warmup?(_app_spec) do
    true
  end
end
