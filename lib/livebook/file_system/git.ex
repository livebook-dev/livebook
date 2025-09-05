defmodule Livebook.FileSystem.Git do
  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.FileSystem

  # File system backed by an Git repository.

  @type t :: %__MODULE__{
          id: String.t(),
          repo_url: String.t(),
          branch: String.t(),
          key: String.t(),
          external_id: String.t() | nil,
          hub_id: String.t()
        }

  embedded_schema do
    field :repo_url, :string
    field :branch, :string
    field :key, :string
    field :external_id, :string
    field :hub_id, :string
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking file system changes.
  """
  @spec change_file_system(t(), map()) :: Ecto.Changeset.t()
  def change_file_system(git, attrs \\ %{}) do
    changeset(git, attrs)
  end

  defp changeset(git, attrs) do
    git
    |> cast(attrs, [:repo_url, :branch, :key, :external_id, :hub_id])
    |> validate_format(:repo_url, ~r/^git@[\w\.\-]+:(?:v\d+\/)?[\w\.\-\/]+\.git$/,
      message: "must be a valid repo URL"
    )
    |> validate_required([:repo_url, :branch, :key, :hub_id])
    |> put_id()
  end

  defp put_id(changeset) do
    hub_id = get_field(changeset, :hub_id)
    repo_url = get_field(changeset, :repo_url)

    if get_field(changeset, :id) do
      changeset
    else
      put_change(changeset, :id, FileSystem.Utils.id("git", hub_id, repo_url))
    end
  end

  @doc false
  def git_dir(%__MODULE__{id: id}), do: Path.join(Livebook.Config.tmp_path(), id)

  @doc false
  def ssh_path, do: Path.join(Livebook.Config.home(), ".ssh")

  @doc false
  def key_path(%__MODULE__{id: id}), do: Path.join(ssh_path(), "#{id}_key")
end

defimpl Livebook.FileSystem, for: Livebook.FileSystem.Git do
  alias Livebook.FileSystem
  alias Livebook.FileSystem.Git

  def type(_file_system) do
    :global
  end

  def default_path(_file_system) do
    "/"
  end

  def list(file_system, path, _recursive) do
    with {:ok, []} <- Git.Client.list_files(file_system, path) do
      FileSystem.Utils.posix_error(:enoent)
    end
  end

  def read(file_system, path) do
    Git.Client.read_file(file_system, path)
  end

  def write(_file_system, _path, _content), do: raise("not implemented")

  def access(_file_system, _path) do
    {:ok, :read}
  end

  def create_dir(_file_system, _path), do: raise("not implemented")

  def remove(_file_system, _path), do: raise("not implemented")

  def copy(_file_system, _source_path, _destination_path), do: raise("not implemented")

  def rename(_file_system, _source_path, _destination_path), do: raise("not implemented")

  def etag_for(file_system, path) do
    FileSystem.Utils.assert_regular_path!(path)
    Git.Client.etag(file_system, path)
  end

  def exists?(file_system, path) do
    Git.Client.exists?(file_system, path)
  end

  def resolve_path(_file_system, dir_path, subject) do
    FileSystem.Utils.resolve_unix_like_path(dir_path, subject)
  end

  def write_stream_init(_file_system, _path, _opts), do: raise("not implemented")

  def write_stream_chunk(_file_system, _state, _chunk), do: raise("not implemented")

  def write_stream_finish(_file_system, _state), do: raise("not implemented")

  def write_stream_halt(_file_system, _state), do: raise("not implemented")

  def read_stream_into(_file_system, _path, _collectable), do: raise("not implemented")

  def load(file_system, %{"hub_id" => _} = fields) do
    load(file_system, %{
      id: fields["id"],
      repo_url: fields["repo_url"],
      branch: fields["branch"],
      key: fields["key"],
      external_id: fields["external_id"],
      hub_id: fields["hub_id"]
    })
  end

  def load(file_system, fields) do
    %{
      file_system
      | id: fields.id,
        repo_url: fields.repo_url,
        branch: fields.branch,
        key: fields.key,
        external_id: fields.external_id,
        hub_id: fields.hub_id
    }
  end

  def dump(file_system) do
    file_system
    |> Map.from_struct()
    |> Map.take([:id, :repo_url, :branch, :key, :external_id, :hub_id])
  end

  def external_metadata(file_system) do
    %{name: file_system.repo_url, error_field: "repo_url"}
  end
end
