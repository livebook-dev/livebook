defprotocol Livebook.FileSystem do
  @moduledoc false

  # This protocol defines an interface for a virtual file system that
  # can be plugged into Livebook.

  @typedoc """
  An identifier uniquely identifying the given file system.

  Every file system struct is expected have an `:id` field.
  """
  @type id :: String.t()

  @typedoc """
  A path uniquely identifies file in the file system.

  Path has most of the semantics of regular file paths, with the
  following exceptions:

    * path must be be absolute for consistency

    * directory path must have a trailing slash, whereas regular file
      path must not have a trailing slash. Rationale: certain file
      systems allow a directory and a file with the same name to
      co-exist, while path needs to distinguish between them

  """
  @type path :: String.t()

  @typedoc """
  A human-readable error message clarifying the operation failure
  reason.
  """
  @type error :: String.t()

  @type access :: :read | :write | :read_write | :none

  @doc """
  Returns a term uniquely identifying the resource used as a file
  system.
  """
  @spec resource_identifier(t()) :: term()
  def resource_identifier(file_system)

  @doc """
  Returns the file system type.

  Based on the underlying resource, the type can be either:

    * `:local` - if the resource is local to its node

    * `:global` - if the resource is external and accessible from any
      node

  """
  @spec type(t()) :: :local | :global
  def type(file_system)

  @doc """
  Returns the default directory path.

  To some extent this is similar to current working directory in a
  regular file system. For most file systems this will just be the
  root path.
  """
  @spec default_path(t()) :: path()
  def default_path(file_system)

  @doc """
  Returns a list of files located in the given directory.

  When `recursive` is set to `true`, nested directories are traversed
  and the final list includes all the paths.
  """
  @spec list(t(), path(), boolean()) :: {:ok, list(path())} | {:error, error()}
  def list(file_system, path, recursive)

  @doc """
  Returns binary content of the given file.
  """
  @spec read(t(), path()) :: {:ok, binary()} | {:error, error()}
  def read(file_system, path)

  @doc """
  Writes the given binary content to the given file.

  If the file exists, it gets overridden.

  If the file doesn't exist, it gets created along with all the
  necessary directories.
  """
  @spec write(t(), path(), binary()) :: :ok | {:error, error()}
  def write(file_system, path, content)

  @doc """
  Returns the current access level to the given file.

  If determining the access is costly, then this function may always
  return the most liberal access, since all access functions return
  error on an invalid attempt.
  """
  @spec access(t(), path()) :: {:ok, access()} | {:error, error()}
  def access(file_system, path)

  @doc """
  Creates the given directory unless it already exists.

  All necessary parent directories are created as well.
  """
  @spec create_dir(t(), path()) :: :ok | {:error, error()}
  def create_dir(file_system, path)

  @doc """
  Removes the given file.

  If a directory is given, all of its contents are removed recursively.

  If the file doesn't exist, no error is returned.
  """
  @spec remove(t(), path()) :: :ok | {:error, error()}
  def remove(file_system, path)

  @doc """
  Copies the given file.

  The given files must be of the same type.

  If regular files are given, the contents are copied, potentially
  overriding the destination if it already exists.

  If directories are given, the directory contents are copied
  recursively.
  """
  @spec copy(t(), path(), path()) :: :ok | {:error, error()}
  def copy(file_system, source_path, destination_path)

  @doc """
  Renames the given file.

  If a directory is given, it gets renamed as expected and consequently
  all of the child paths change.

  If the destination exists, an error is returned.
  """
  @spec rename(t(), path(), path()) :: :ok | {:error, error()}
  def rename(file_system, source_path, destination_path)

  @doc """
  Returns a version identifier for the given file.

  The resulting value must be a string of ASCII characters placed
  between double quotes, suitable for use as the value of the ETag
  HTTP header.
  """
  @spec etag_for(t(), path()) :: {:ok, String.t()} | {:error, error()}
  def etag_for(file_system, path)

  @doc """
  Checks if the given path exists in the file system.
  """
  @spec exists?(t(), path()) :: {:ok, boolean()} | {:error, error()}
  def exists?(file_system, path)

  @doc """
  Resolves `subject` against a valid directory path.

  The `subject` may be either relative or absolute, contain special
  sequences such as ".." and ".", but the interpretation is left up
  to the file system.

  In other words, this has the semantics of path join followed by
  expand.
  """
  @spec resolve_path(t(), path(), String.t()) :: path()
  def resolve_path(file_system, dir_path, subject)

  @doc """
  Initializes chunked write to the given file.

  Should return the initial state, which is then reduced over in
  `write_stream_chunk/3`
  """
  @spec write_stream_init(t(), path(), keyword()) :: {:ok, state} | {:error, error()}
        when state: term()
  def write_stream_init(file_system, path, opts)

  @doc """
  Writes a file chunk.

  There is no assumption on the chunk size, you can accumulate chunks
  in `state` and perform the write operation once the desired chunk
  size is achieved.
  """
  @spec write_stream_chunk(t(), state, binary()) :: {:ok, state} | {:error, error()}
        when state: term()
  def write_stream_chunk(file_system, state, chunk)

  @doc """
  Finalizes chunked write operation.

  This function is called when all chunks have been successfully
  written.

  Note that if the finish operation fails, `write_stream_halt/2`
  is **not** expected to be called, so you should do the necessary
  cleanup here in case of failure as well.
  """
  @spec write_stream_finish(t(), state) :: :ok | {:error, error()} when state: term()
  def write_stream_finish(file_system, state)

  @doc """
  Halts chunked write operation.

  This function is called when writing any of the chunks fails or the
  writing is aborted by the caller.
  """
  @spec write_stream_halt(t(), state) :: :ok | {:error, error()} when state: term()
  def write_stream_halt(file_system, state)

  @doc """
  Similar to `read/2`, but streams file contents into `collectable`
  chunk by chunk.

  The `Collectable` protocol does not make room for gracefully
  signalling an error, so implementations generally raise an
  exception. `read_stream_into/3` is not expected to raise, so make
  sure to convert collectable exceptions into an error tuple.
  """
  @spec read_stream_into(t(), path(), Collectable.t()) ::
          {:ok, Collectable.t()} | {:error, error()}
  def read_stream_into(file_system, path, collectable)
end
