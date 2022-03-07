defmodule Livebook.FileSystem.Utils do
  @moduledoc false

  alias Livebook.FileSystem

  @doc """
  Asserts that the given path is a directory.
  """
  @spec assert_dir_path!(FileSystem.path()) :: :ok
  def assert_dir_path!(path) do
    unless dir_path?(path) do
      raise ArgumentError, "expected a directory path, got: #{inspect(path)}"
    end

    :ok
  end

  @doc """
  Asserts that the given path is a regular file path.
  """
  @spec assert_regular_path!(FileSystem.path()) :: :ok
  def assert_regular_path!(path) do
    unless regular_path?(path) do
      raise ArgumentError, "expected a regular file path, got: #{inspect(path)}"
    end

    :ok
  end

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

  @doc """
  Implements `Livebook.FileSystem.resolve_path` assuming Unix-like
  path conventions.

  This function assumes absolute paths to have a leading "/"
  and handles sequences such as "." and "..".
  """
  @spec resolve_unix_like_path(FileSystem.path(), String.t()) :: FileSystem.t()
  def resolve_unix_like_path(relative_to, subject) do
    dir_path = relative_to |> Path.dirname() |> ensure_dir_path()

    subject =
      if Path.basename(subject) in [".", ".."] do
        ensure_dir_path(subject)
      else
        subject
      end

    absolute_path? = String.starts_with?(subject, "/")
    path = if absolute_path?, do: subject, else: dir_path <> subject

    path
    |> String.split("/")
    |> remove_in_middle("")
    |> expand_parts([])
    |> Enum.join("/")
  end

  defp remove_in_middle([], _elem), do: []
  defp remove_in_middle([head], _elem), do: [head]
  defp remove_in_middle([head | tail], elem), do: remove_in_middle(tail, elem, [head])

  defp remove_in_middle([head], _elem, acc), do: Enum.reverse([head | acc])
  defp remove_in_middle([elem | tail], elem, acc), do: remove_in_middle(tail, elem, acc)
  defp remove_in_middle([head | tail], elem, acc), do: remove_in_middle(tail, elem, [head | acc])

  defp expand_parts([], acc), do: Enum.reverse(acc)
  defp expand_parts(["." | parts], acc), do: expand_parts(parts, acc)
  defp expand_parts([".." | parts], [_parent] = acc), do: expand_parts(parts, acc)
  defp expand_parts([".." | parts], [_parent | acc]), do: expand_parts(parts, acc)
  defp expand_parts([part | parts], acc), do: expand_parts(parts, [part | acc])
end
