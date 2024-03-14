defmodule Livebook.Teams.AppDeployment do
  use Ecto.Schema
  alias Livebook.FileSystem

  @file_extension ".zip"

  @type zip_file :: {filename :: String.t(), content :: String.t()}
  @type zip_files :: list(zip_file())

  @type t :: %__MODULE__{
          id: String.t() | nil,
          filename: String.t() | nil,
          slug: String.t() | nil,
          sha: String.t() | nil,
          title: String.t() | nil,
          deployment_group_id: String.t() | nil,
          file: binary() | nil,
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
    field :file, :string
    field :deployed_by, :string

    timestamps(updated_at: nil, inserted_at: :deployed_at)
  end

  @doc """
  Creates a new app deployment from session with compressed file.
  """
  @spec new(String.t(), String.t(), String.t(), zip_files()) ::
          {:ok, t()} | {:error, FileSystem.error()}
  def new(title, slug, deployment_group_id, files) do
    files =
      Enum.map(files, fn {filename, content} ->
        {to_charlist(filename), content}
      end)

    with {:ok, content} <- zip_files(files),
         :ok <- validate_size(content) do
      md5_hash = :crypto.hash(:md5, content)
      shasum = Base.encode16(md5_hash, case: :lower)

      {:ok,
       %__MODULE__{
         filename: shasum <> @file_extension,
         slug: slug,
         sha: shasum,
         title: title,
         deployment_group_id: deployment_group_id,
         file: content
       }}
    end
  end

  defp zip_files(files) do
    case :zip.create(~c"app_deployment.zip", files, [:memory]) do
      {:ok, {_filename, content}} -> {:ok, content}
      {:error, error} -> FileSystem.Utils.posix_error(error)
    end
  end

  defp validate_size(data) do
    if byte_size(data) <= 20 * 1024 * 1024 do
      :ok
    else
      {:error, "file size too large"}
    end
  end
end
