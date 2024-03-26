defmodule Livebook.Apps.TeamsAppSpec do
  # App spec for organization apps fetched from Livebook Teams.

  @enforce_keys [:slug, :version, :archive_url, :derived_key]

  # (apps) TODO: adjust archive download with new teams changes
  defstruct [:slug, :version, :archive_url, :derived_key]
end

defimpl Livebook.Apps.AppSpec, for: Livebook.Apps.TeamsAppSpec do
  def load(app_spec, files_tmp_path) do
    with {:ok, encrypted_binary} <- download(app_spec.archive_url),
         {:ok, archive_binary} <- decrypt(encrypted_binary, app_spec.derived_key),
         {:ok, notebook_source} <- unzip(archive_binary, files_tmp_path) do
      {notebook, %{warnigns: warnings}} =
        Livebook.LiveMarkdown.notebook_from_livemd(notebook_source)

      {:ok, %{notebook: notebook, warnings: warnings}}
    end
  end

  defp download(archive_url) do
    case Req.get(archive_url) do
      {:ok, %{status: 200} = response} ->
        {:ok, response.body}

      {:ok, %{status: status}} ->
        {:error, "downloading app archive failed with HTTP status #{status}"}

      {:error, exception} ->
        {:error, "downloading app archive failed, #{Exception.message(exception)}"}
    end
  end

  defp decrypt(binary, derived_key) do
    with :error <- Livebook.Teams.decrypt(binary, derived_key) do
      {:error, "failed to decrypt app archive"}
    end
  end

  defp unzip(archive_binary, files_tmp_path) do
    case :zip.extract(archive_binary, [:memory]) do
      {:ok, entries} ->
        Enum.reduce_while(entries, {:error, "notebook file missing in the app archive"}, fn
          {"notebook.livemd", binary}, _acc ->
            {:cont, {:ok, binary}}

          {"files/" <> name, binary}, acc ->
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
