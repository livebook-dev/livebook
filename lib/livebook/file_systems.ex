defmodule Livebook.FileSystems do
  @moduledoc false

  alias Livebook.FileSystem

  @doc """
  Returns the atom type from given file system.
  """
  @spec type(FileSystem.t()) :: String.t()
  def type(%FileSystem.S3{}), do: "s3"

  @doc """
  Loads the file system from given type and dumped data.
  """
  @spec load(String.t(), map()) :: FileSystem.t()
  def load("s3", dumped_data) do
    FileSystem.load(%FileSystem.S3{}, dumped_data)
  end
end
