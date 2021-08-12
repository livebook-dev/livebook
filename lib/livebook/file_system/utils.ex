defmodule Livebook.FileSystem.Utils do
  @moduledoc false

  alias Livebook.FileSystem

  @doc """
  Asserts that the given path is absolute and expands it.

  ## Options

    * `:type` - if set to `:directory` or `:regular`, the
      path type is also asserted against
  """
  @spec normalize_path!(String.t(), keyword()) :: FileSystem.path()
  def normalize_path!(path, opts \\ []) do
    unless absolute_path?(path) do
      raise ArgumentError, "expected an absolute path, got: #{inspect(path)}"
    end

    path = expand_path(path)

    case opts[:type] do
      nil ->
        :ok

      :directory ->
        unless dir_path?(path) do
          raise ArgumentError, "expected a directory path, got: #{inspect(path)}"
        end

      :regular ->
        unless regular_path?(path) do
          raise ArgumentError, "expected a regular file path, got: #{inspect(path)}"
        end

      other ->
        raise ArgumentError,
              "expected :type option to be either :direcotry, :regular or nil, got: #{inspect(other)}"
    end

    path
  end

  defp absolute_path?("/" <> _), do: true
  defp absolute_path?(_), do: false

  @doc """
  Checks if the given path describes a directory.
  """
  @spec dir_path?(FileSystem.path()) :: boolean()
  def dir_path?(path) do
    String.ends_with?(path, "/")
  end

  @doc """
  Checks if the given path describes a regular file.
  """
  @spec regular_path?(FileSystem.path()) :: boolean()
  def regular_path?(path) do
    not String.ends_with?(path, "/")
  end

  @doc """
  Asserts that the given paths are of the same type.
  """
  @spec assert_same_type!(FileSystem.path(), FileSystem.path()) :: :ok
  def assert_same_type!(path1, path2) do
    if dir_path?(path1) != dir_path?(path2) do
      raise ArgumentError,
            "expected paths of the same type, got: #{inspect(path1)} and #{inspect(path2)}"
    end

    :ok
  end

  @doc """
  Expands the given path, optionally against the given
  absolute path.

  This works similarly to `Path.expand/2`, except it
  takes trailing slashes into account.
  """
  @spec expand_path(String.t(), FileSystem.path() | nil) :: FileSystem.path()
  def expand_path(path, relative_to \\ nil) do
    if not absolute_path?(path) and relative_to == nil do
      raise ArgumentError,
            "expected an absolute path to expand the relative path against, but none was given"
    end

    relative_to = relative_to && normalize_path!(relative_to, type: :directory)

    if path == "" do
      relative_to
    else
      dir? = dir_path?(path) or Path.basename(path) in [".", ".."]

      case {dir?, Path.expand(path, relative_to || "")} do
        {_, "/"} -> "/"
        {true, path} -> path <> "/"
        {false, path} -> path
      end
    end
  end

  @doc """
  Converts the given path into dir path by appending a trailing
  slash if necessary.
  """
  @spec ensure_dir_path(String.t()) :: FileSystem.path()
  def ensure_dir_path(path) do
    if String.ends_with?(path, "/") do
      path
    else
      path <> "/"
    end
  end

  @doc """
  Converts the given posix error atom into readable error tuple.
  """
  @spec posix_error(atom()) :: {:error, FileSystem.error()}
  def posix_error(error) do
    message = error |> :file.format_error() |> List.to_string()
    {:error, message}
  end
end
