defprotocol Livebook.FileSystem do
  @moduledoc false

  # This protocol defines an interface for file systems
  # that can be plugged into Livebook.

  @typedoc """
  A path uniquely idenfies file in the file system.

  Path has most of the semantics of regular file paths,
  with the following exceptions:

    * path must be be absolute for consistency

    * directory path must have a trailing slash, whereas
      regular file path must not have a trailing slash.
      Rationale: some file systems allow a directory and
      a file with the same name to co-exist, while path
      needs to distinguish between them
  """
  @type path :: String.t()

  @typedoc """
  A human-readable error message clarifying the operation
  failure reason.
  """
  @type error :: String.t()

  @type access :: :read | :write | :read_write | :none

  @doc """
  Returns the default directory path.

  To some extent this is similar to current working directory
  in a regular file system. For most file systems this
  will just be the root path.
  """
  @spec default_path(t()) :: path()
  def default_path(file_system)

  @doc """
  Returns a list of files located in the given directory.

  When `recursive` is set to `true`, nested directories
  are traversed and the final list includes all the paths.
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

  If the file doesn't exist, it gets created along with
  all the necessary directories.
  """
  @spec write(t(), path(), binary()) :: :ok | {:error, error()}
  def write(file_system, path, content)

  @doc """
  Returns the current access level to the given file.

  If determining the access is costly, then this function may
  always return the most liberal access, since all access
  functions return error on an invalid attempt.
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

  If a directory is given, all of its contents are removed
  recursively.

  If the file doesn't exist, no error is returned.
  """
  @spec remove(t(), path()) :: :ok | {:error, error()}
  def remove(file_system, path)

  @doc """
  Copies the given file.

  The given files must be of the same type.

  If regular files are given, the contents are copied,
  potentially overriding the destination if it already exists.

  If directories are given, the directory contents are copied
  recursively.
  """
  @spec copy(t(), path(), path()) :: :ok | {:error, error()}
  def copy(file_system, source_path, destination_path)

  @doc """
  Renames the given file.

  If a directory is given, it gets renamed as expected and
  consequently all of the child paths change.

  If the destination exists, an error is returned.
  """
  @spec rename(t(), path(), path()) :: :ok | {:error, error()}
  def rename(file_system, source_path, destination_path)

  @doc """
  Returns a version identifier for the given file.

  The resulting value must be a string of ASCII characters
  placed between double quotes, suitable for use as the
  value of the ETag HTTP header.
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

  The `subject` may be either relative or absolute,
  contain special sequences such as ".." and ".",
  but the interpretation is left up to the file system.

  In other words, this has the semantics of path join
  followed by expand.
  """
  @spec resolve_path(t(), path(), String.t()) :: path()
  def resolve_path(file_system, dir_path, subject)
end
