defmodule Livebook.FileSystems do
  alias Livebook.FileSystem

  @doc """
  Returns the type identifier for the given file system.
  """
  @spec type(FileSystem.t()) :: String.t()
  def type(%FileSystem.S3{}), do: "s3"

  @doc """
  Updates file system with the given changes.
  """
  @spec update_file_system(FileSystem.t(), map()) ::
          {:ok, FileSystem.t()} | {:error, Ecto.Changeset.t()}
  def update_file_system(file_system, attrs) do
    file_system
    |> change_file_system(attrs)
    |> Ecto.Changeset.apply_action(:update)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking file system changes.
  """
  @spec change_file_system(FileSystem.t()) :: Ecto.Changeset.t()
  def change_file_system(file_system) do
    change_file_system(file_system, %{})
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking file system changes.
  """
  @spec change_file_system(FileSystem.t(), map()) :: Ecto.Changeset.t()
  def change_file_system(%FileSystem.S3{} = file_system, attrs) do
    FileSystem.S3.change_file_system(file_system, attrs)
  end

  @doc """
  Loads the file system from given type and dumped data.
  """
  @spec load(String.t(), map()) :: FileSystem.t()
  def load("s3", dumped_data) do
    FileSystem.load(%FileSystem.S3{}, dumped_data)
  end
end
