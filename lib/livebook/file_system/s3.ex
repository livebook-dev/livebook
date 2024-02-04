defmodule Livebook.FileSystem.S3 do
  use Ecto.Schema
  import Ecto.Changeset

  # File system backed by an S3 bucket.

  @type t :: %__MODULE__{
          id: String.t(),
          bucket_url: String.t(),
          external_id: String.t() | nil,
          region: String.t(),
          access_key_id: String.t() | nil,
          secret_access_key: String.t() | nil,
          hub_id: String.t()
        }

  @type credentials :: %{
          access_key_id: String.t() | nil,
          secret_access_key: String.t() | nil,
          token: String.t() | nil
        }

  embedded_schema do
    field :bucket_url, :string
    field :external_id, :string
    field :region, :string
    field :access_key_id, :string
    field :secret_access_key, :string
    field :hub_id, :string
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking file system changes.
  """
  @spec change_file_system(t(), map()) :: Ecto.Changeset.t()
  def change_file_system(s3, attrs \\ %{}) do
    changeset(s3, attrs)
  end

  defp changeset(s3, attrs) do
    s3
    |> cast(attrs, [
      :bucket_url,
      :external_id,
      :region,
      :access_key_id,
      :secret_access_key,
      :hub_id
    ])
    |> validate_required([:bucket_url, :hub_id])
    |> Livebook.Utils.validate_url(:bucket_url)
    |> validate_credentials()
    |> put_id()
  end

  defp validate_credentials(changeset) do
    case {get_field(changeset, :access_key_id), get_field(changeset, :secret_access_key)} do
      {nil, nil} -> try_environment_credentials(changeset)
      _ -> validate_required(changeset, [:access_key_id, :secret_access_key])
    end
  end

  defp try_environment_credentials(changeset) do
    case get_credentials() do
      :undefined ->
        add_error(
          changeset,
          :access_key_id,
          "credentials missing and cannot be obtained from the environment"
        )

      _ ->
        changeset
    end
  end

  defp put_id(changeset) do
    hub_id = get_field(changeset, :hub_id)
    bucket_url = get_field(changeset, :bucket_url)

    if get_field(changeset, :id) do
      changeset
    else
      put_change(changeset, :id, id(hub_id, bucket_url))
    end
  end

  @personal_id Livebook.Hubs.Personal.id()

  def id(_, nil), do: nil
  def id(nil, bucket_url), do: hashed_id(bucket_url)
  def id(@personal_id, bucket_url), do: hashed_id(bucket_url)
  def id(hub_id, bucket_url), do: "#{hub_id}-#{hashed_id(bucket_url)}"

  defp hashed_id(bucket_url) do
    hash = :crypto.hash(:sha256, bucket_url)
    encrypted_hash = Base.url_encode64(hash, padding: false)

    "s3-#{encrypted_hash}"
  end

  @doc """
  Retrieves credentials for the given file system.

  If the credentials are not specified by the file system, they are
  fetched from environment variables or AWS instance if applicable.
  """
  @spec credentials(S3.t()) :: S3.credentials()
  def credentials(%__MODULE__{} = file_system) do
    case {file_system.access_key_id, file_system.secret_access_key} do
      {nil, nil} ->
        defaults = %{token: nil, access_key_id: nil, secret_access_key: nil}

        case get_credentials() do
          :undefined ->
            defaults

          credentials ->
            credentials = Map.take(credentials, [:access_key_id, :secret_access_key, :token])
            Map.merge(defaults, credentials)
        end

      _ ->
        %{
          access_key_id: file_system.access_key_id,
          secret_access_key: file_system.secret_access_key,
          token: nil
        }
    end
  end

  defp get_credentials do
    if Livebook.Config.aws_credentials?() do
      :aws_credentials.get_credentials()
    else
      :undefined
    end
  end

  @doc """
  Infers region from the given bucket URL.
  """
  @spec region_from_url(String.t()) :: String.t()
  def region_from_url(url) do
    # For many services the API host is of the form *.[region].[rootdomain].com
    host = URI.parse(url).host || ""

    splitted_host = host |> String.split(".") |> Enum.reverse()

    case Enum.at(splitted_host, 2, "auto") do
      "s3" -> "us-east-1"
      "r2" -> "auto"
      region -> region
    end
  end

  @doc """
  Sets region field on `changeset` based on bucket URL, unless already
  specified.
  """
  @spec maybe_put_region_from_url(Ecto.Changeset.t()) :: Ecto.Changeset.t()
  def maybe_put_region_from_url(changeset) do
    case {get_field(changeset, :region), get_field(changeset, :bucket_url)} do
      {nil, bucket_url} when bucket_url != nil ->
        put_change(changeset, :region, region_from_url(bucket_url))

      _ ->
        changeset
    end
  end
end

defimpl Livebook.FileSystem, for: Livebook.FileSystem.S3 do
  alias Livebook.FileSystem
  alias Livebook.FileSystem.S3

  def type(_file_system) do
    :global
  end

  def default_path(_file_system) do
    "/"
  end

  def list(file_system, path, recursive) do
    FileSystem.Utils.assert_dir_path!(path)
    "/" <> dir_key = path
    delimiter = if recursive, do: nil, else: "/"
    opts = [prefix: dir_key, delimiter: delimiter]

    with {:ok, %{keys: keys}} <- S3.Client.list_objects(file_system, opts) do
      if keys == [] and dir_key != "" do
        FileSystem.Utils.posix_error(:enoent)
      else
        paths = keys |> List.delete(dir_key) |> Enum.map(&("/" <> &1))
        {:ok, paths}
      end
    end
  end

  def read(file_system, path) do
    FileSystem.Utils.assert_regular_path!(path)
    "/" <> key = path

    S3.Client.get_object(file_system, key)
  end

  def write(file_system, path, content) do
    FileSystem.Utils.assert_regular_path!(path)
    "/" <> key = path

    S3.Client.put_object(file_system, key, content)
  end

  def access(_file_system, _path) do
    {:ok, :read_write}
  end

  def create_dir(file_system, path) do
    FileSystem.Utils.assert_dir_path!(path)
    "/" <> key = path

    # S3 has no concept of directories, but keys with trailing
    # slash are interpreted as such, so we create an empty
    # object for the given key
    S3.Client.put_object(file_system, key, nil)
  end

  def remove(file_system, path) do
    "/" <> key = path

    if FileSystem.Utils.dir_path?(path) do
      with {:ok, %{keys: keys}} <- S3.Client.list_objects(file_system, prefix: key) do
        if keys == [] do
          FileSystem.Utils.posix_error(:enoent)
        else
          S3.Client.delete_objects(file_system, keys)
        end
      end
    else
      S3.Client.delete_object(file_system, key)
    end
  end

  def copy(file_system, source_path, destination_path) do
    FileSystem.Utils.assert_same_type!(source_path, destination_path)
    "/" <> source_key = source_path
    "/" <> destination_key = destination_path

    if FileSystem.Utils.dir_path?(source_path) do
      with {:ok, %{bucket: bucket, keys: keys}} <-
             S3.Client.list_objects(file_system, prefix: source_key) do
        if keys == [] do
          FileSystem.Utils.posix_error(:enoent)
        else
          keys
          |> Enum.map(fn key ->
            renamed_key = String.replace_prefix(key, source_key, destination_key)

            Task.async(fn ->
              S3.Client.copy_object(file_system, bucket, key, renamed_key)
            end)
          end)
          |> Task.await_many(:infinity)
          |> Enum.filter(&match?({:error, _}, &1))
          |> case do
            [] -> :ok
            [error | _] -> error
          end
        end
      end
    else
      with {:ok, bucket} <- S3.Client.get_bucket_name(file_system) do
        S3.Client.copy_object(file_system, bucket, source_key, destination_key)
      end
    end
  end

  def rename(file_system, source_path, destination_path) do
    FileSystem.Utils.assert_same_type!(source_path, destination_path)
    "/" <> destination_key = destination_path

    with {:ok, destination_exists?} <- S3.Client.object_exists(file_system, destination_key) do
      if destination_exists? do
        FileSystem.Utils.posix_error(:eexist)
      else
        # S3 doesn't support an atomic rename/move operation,
        # so we implement it as copy followed by remove
        with :ok <- copy(file_system, source_path, destination_path) do
          remove(file_system, source_path)
        end
      end
    end
  end

  def etag_for(file_system, path) do
    FileSystem.Utils.assert_regular_path!(path)
    "/" <> key = path

    with {:ok, %{etag: etag}} <- S3.Client.head_object(file_system, key) do
      {:ok, etag}
    end
  end

  def exists?(file_system, path) do
    "/" <> key = path

    S3.Client.object_exists(file_system, key)
  end

  def resolve_path(_file_system, dir_path, subject) do
    FileSystem.Utils.resolve_unix_like_path(dir_path, subject)
  end

  def write_stream_init(_file_system, path, opts) do
    opts = Keyword.validate!(opts, part_size: 50_000_000)

    FileSystem.Utils.assert_regular_path!(path)
    "/" <> key = path

    {:ok,
     %{
       key: key,
       parts: 0,
       etags: [],
       current_chunks: [],
       current_size: 0,
       part_size: opts[:part_size],
       upload_id: nil
     }}
  end

  def write_stream_chunk(file_system, state, chunk) when is_binary(chunk) do
    chunk_size = byte_size(chunk)

    state = update_in(state.current_size, &(&1 + chunk_size))
    state = update_in(state.current_chunks, &[chunk | &1])

    if state.current_size >= state.part_size do
      maybe_state =
        if state.upload_id do
          {:ok, state}
        else
          with {:ok, upload_id} <- S3.Client.create_multipart_upload(file_system, state.key) do
            {:ok, %{state | upload_id: upload_id}}
          end
        end

      with {:ok, state} <- maybe_state do
        upload_part_from_state(file_system, state, state.part_size)
      end
    else
      {:ok, state}
    end
  end

  defp upload_part_from_state(file_system, state, part_size) do
    <<part::binary-size(part_size), rest::binary>> =
      state.current_chunks
      |> Enum.reverse()
      |> IO.iodata_to_binary()

    parts = state.parts + 1

    with {:ok, %{etag: etag}} <-
           S3.Client.upload_part(file_system, state.key, state.upload_id, parts, part) do
      {:ok,
       %{
         state
         | current_chunks: [rest],
           current_size: byte_size(rest),
           etags: [etag | state.etags],
           parts: parts
       }}
    end
  end

  def write_stream_finish(file_system, state) do
    if state.upload_id do
      maybe_state =
        if state.current_size > 0 do
          upload_part_from_state(file_system, state, state.current_size)
        else
          {:ok, state}
        end

      with {:ok, state} <- maybe_state,
           :ok <-
             S3.Client.complete_multipart_upload(
               file_system,
               state.key,
               state.upload_id,
               Enum.reverse(state.etags)
             ) do
        :ok
      else
        {:error, error} ->
          S3.Client.abort_multipart_upload(file_system, state.key, state.upload_id)
          {:error, error}
      end
    else
      content = state.current_chunks |> Enum.reverse() |> IO.iodata_to_binary()
      S3.Client.put_object(file_system, state.key, content)
    end
  end

  def write_stream_halt(file_system, state) do
    if state.upload_id do
      S3.Client.abort_multipart_upload(file_system, state.key, state.upload_id)
    else
      :ok
    end
  end

  def read_stream_into(file_system, path, collectable) do
    FileSystem.Utils.assert_regular_path!(path)
    "/" <> key = path

    S3.Client.multipart_get_object(file_system, key, collectable)
  end

  def load(file_system, %{"bucket_url" => _} = fields) do
    load(file_system, %{
      bucket_url: fields["bucket_url"],
      external_id: fields["external_id"],
      region: fields["region"],
      access_key_id: fields["access_key_id"],
      secret_access_key: fields["secret_access_key"],
      id: fields["id"],
      hub_id: fields["hub_id"]
    })
  end

  def load(file_system, fields) do
    %{
      file_system
      | id: fields.id,
        bucket_url: fields.bucket_url,
        external_id: fields.external_id,
        region: fields.region,
        access_key_id: fields.access_key_id,
        secret_access_key: fields.secret_access_key,
        hub_id: fields.hub_id
    }
  end

  def dump(file_system) do
    file_system
    |> Map.from_struct()
    |> Map.take([
      :id,
      :bucket_url,
      :region,
      :access_key_id,
      :secret_access_key,
      :hub_id,
      :external_id
    ])
  end

  def external_metadata(file_system) do
    %{name: file_system.bucket_url, error_field: "bucket_url"}
  end
end
