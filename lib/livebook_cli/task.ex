defmodule LivebookCLI.Task do
  import LivebookCLI.Utils

  @doc """
  Returns a description of the task usage.
  """
  @callback usage() :: IO.chardata()

  @doc """
  Runs the task with the given list of command line arguments.
  """
  @callback call(args :: list(String.t())) :: :ok

  @doc """
  Runs the task with the given list of command line arguments.
  """
  @spec call(String.t(), list(String.t())) :: :ok
  def call(name, args) do
    task = fetch_task!(name)
    task.call(args)

    :ok
  rescue
    exception -> log_exception(exception, name, __STACKTRACE__)
  end

  @doc """
  Shows the description of the task usage.
  """
  @spec usage(String.t()) :: :ok
  def usage(name) do
    task = fetch_task!(name)
    print_text(task.usage())

    :ok
  rescue
    exception -> log_exception(exception, name, __STACKTRACE__)
  end

  @spec fetch_task!(String.t()) :: module() | no_return()
  defp fetch_task!("server"), do: LivebookCLI.Server
  defp fetch_task!("deploy"), do: LivebookCLI.Deploy
  defp fetch_task!(name), do: raise("Unknown command #{name}")
end
