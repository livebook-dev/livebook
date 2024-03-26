defmodule Livebook.Apps.PreviewAppSpec do
  # App spec for deploying notebook directly from a session.
  #
  # This spec is used when deploying apps directly from the session,
  # which we call previews.
  #
  # The notebook files may be stored on the local file system, so this
  # app spec should always be deployed on the same node where it is
  # built.

  @enforce_keys [:slug, :session_id]

  defstruct [:slug, :session_id, version: "1"]
end

defimpl Livebook.Apps.AppSpec, for: Livebook.Apps.PreviewAppSpec do
  def load(app_spec, files_tmp_path) do
    case Livebook.Sessions.fetch_session(app_spec.session_id) do
      {:ok, %{pid: pid, files_dir: files_dir}} ->
        notebook = Livebook.Session.get_notebook(pid)

        files_tmp_dir =
          files_tmp_path
          |> Livebook.FileSystem.Utils.ensure_dir_path()
          |> Livebook.FileSystem.File.local()

        with :ok <- Livebook.Notebook.copy_files(notebook, files_dir, files_tmp_dir) do
          {:ok, %{notebook: notebook, warnings: []}}
        end

      {:error, _reason} ->
        {:error, "the session has already been closed"}
    end
  end

  def should_warmup?(_app_spec) do
    false
  end
end
