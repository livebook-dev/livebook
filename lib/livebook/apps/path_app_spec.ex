defmodule Livebook.Apps.PathAppSpec do
  # App spec pointing to an app notebook in the local file system.
  #
  # Note that the struct holds the path, rather than the notebook and
  # files source. The app spec may be sent across nodes and processes,
  # so this way we do less copying.

  @enforce_keys [:slug, :path]

  defstruct [:slug, :path, :password, should_warmup: true, version: "1"]
end

defimpl Livebook.Apps.AppSpec, for: Livebook.Apps.PathAppSpec do
  def load(app_spec, files_tmp_path) do
    case File.read(app_spec.path) do
      {:ok, source} ->
        {notebook, %{warnings: warnings}} = Livebook.LiveMarkdown.notebook_from_livemd(source)
        notebook = put_in(notebook.app_settings.password, app_spec.password)

        notebook_file = Livebook.FileSystem.File.local(app_spec.path)
        files_dir = Livebook.Session.files_dir_for_notebook(notebook_file)

        warnings = Enum.map(warnings, &("Import: " <> &1))

        files_tmp_dir =
          files_tmp_path
          |> Livebook.FileSystem.Utils.ensure_dir_path()
          |> Livebook.FileSystem.File.local()

        with :ok <- Livebook.Notebook.copy_files(notebook, files_dir, files_tmp_dir) do
          {:ok, %{notebook: notebook, warnings: warnings}}
        end

      {:error, reason} ->
        {:error,
         "failed to read app notebook file at #{app_spec.path}, reason: #{:file.format_error(reason)}"}
    end
  end

  def should_warmup?(app_spec) do
    app_spec.should_warmup
  end
end
