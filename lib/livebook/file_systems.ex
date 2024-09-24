defmodule Livebook.FileSystems do
  alias Livebook.FileSystem

  @doc """
  Returns the type identifier for the given file system.
  """
  @spec type(FileSystem.t()) :: String.t()
  def type(%module{}), do: module_to_type(module)

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking file system changes.
  """
  @spec change_file_system(FileSystem.t(), map()) :: Ecto.Changeset.t()
  def change_file_system(file_system, attrs \\ %{})

  def change_file_system(%FileSystem.S3{} = file_system, attrs) do
    FileSystem.S3.change_file_system(file_system, attrs)
  end

  @doc """
  Loads the file system from given type and dumped data.
  """
  @spec load(String.t(), map()) :: FileSystem.t()
  def load(type, dumped_data) do
    type
    |> type_to_module()
    |> struct!()
    |> FileSystem.load(dumped_data)
  end

  @doc """
  Returns file system module corresponding to the given type.
  """
  @spec type_to_module(String.t()) :: module()
  def type_to_module(type)
  def type_to_module("local"), do: FileSystem.Local
  def type_to_module("s3"), do: FileSystem.S3

  @doc """
  Returns a serializable type for corresponding to the given file
  system module.
  """
  @spec module_to_type(module()) :: String.t()
  def module_to_type(module)
  def module_to_type(FileSystem.Local), do: "local"
  def module_to_type(FileSystem.S3), do: "s3"
end
