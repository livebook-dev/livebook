defmodule LivebookCLI.Task do
  @moduledoc false

  @doc """
  Returns a description of the task usage.
  """
  @callback usage() :: String.t()

  @doc """
  Runs the task with the given list of command line arguments.
  """
  @callback call(args :: list(String.t())) :: :ok
end
