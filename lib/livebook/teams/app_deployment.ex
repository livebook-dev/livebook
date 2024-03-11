defmodule Livebook.Teams.AppDeployment do
  use Ecto.Schema
  alias Livebook.FileSystem

  @file_extension ".zip"

  @type t :: %__MODULE__{
          id: String.t() | nil,
          filename: String.t() | nil,
          slug: String.t() | nil,
          sha: String.t() | nil,
          title: String.t() | nil,
          deployment_group_id: String.t() | nil,
          file: FileSystem.File.t() | String.t() | nil,
          deployed_by: String.t() | nil,
          deployed_at: NaiveDateTime.t() | nil
        }

  @primary_key {:id, :string, autogenerate: false}
  embedded_schema do
    field :filename, :string
    field :slug, :string
    field :sha, :string
    field :title, :string
    field :deployment_group_id, :string
    field :file, :map
    field :deployed_by, :string

    timestamps(updated_at: nil, inserted_at: :deployed_at)
  end

  @doc """
  Creates a new app deployment from session with compressed file.
  """
  @spec new(Livebook.Session.Data.t()) :: {:ok, t()} | {:error, FileSystem.error()}
  def new(%Livebook.Session.Data{} = data) do
    attachments =
      data.notebook.file_entries
      |> Enum.filter(&(&1.type == :file))
      |> Enum.map(& &1)

    with {:ok, file_system} <- FileSystem.File.fetch_file_system(data.file),
         {:ok, file} <- zip_files(file_system, data.file, attachments),
         {:ok, content} <- FileSystem.File.read(file) do
      md5_hash = :crypto.hash(:md5, content)
      shasum = Base.encode16(md5_hash, case: :lower)

      {:ok,
       %__MODULE__{
         filename: shasum <> @file_extension,
         slug: data.notebook.app_settings.slug,
         sha: shasum,
         title: data.notebook.name,
         deployment_group_id: data.notebook.deployment_group_id,
         file: file
       }}
    end
  end

  @doc """
  Decompresses the given app into given destination path.
  """
  @spec unzip_app(t(), String.t()) :: :ok | {:error, FileSystem.error()}
  def unzip_app(%__MODULE__{} = app_deployment, destination_path) do
    case :zip.extract(to_charlist(app_deployment.file.path), cwd: to_charlist(destination_path)) do
      {:ok, _extracted_paths} -> :ok
      {:error, error} -> FileSystem.Utils.posix_error(error)
    end
  end

  defp zip_files(file_system, notebook_file, attachments) do
    filename = FileSystem.File.name(notebook_file)
    cwd = Path.dirname(notebook_file.path)

    files =
      Enum.reduce([notebook_file | attachments], [], fn file, acc ->
        relative_path = Path.relative_to(file.path, cwd)

        case FileSystem.File.exists?(file) do
          {:ok, true} -> [to_charlist(relative_path) | acc]
          _ -> acc
        end
      end)

    zipfile_basename =
      Path.basename(filename, Livebook.LiveMarkdown.extension()) <> @file_extension

    zipfile_path = Path.join(System.tmp_dir!(), zipfile_basename)
    zipfile = FileSystem.File.new(file_system, zipfile_path)

    with {:ok, true} <- FileSystem.File.exists?(zipfile) do
      :ok = FileSystem.File.remove(zipfile)
    end

    case :zip.create(to_charlist(zipfile_path), files, cwd: to_charlist(cwd)) do
      {:ok, _} -> {:ok, FileSystem.File.new(file_system, zipfile_path)}
      {:error, error} -> FileSystem.Utils.posix_error(error)
    end
  end
end
