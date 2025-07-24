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
  rescue
    exception -> log_exception(exception, name, __STACKTRACE__)
  end

  @doc """
  Shows the description of the task usage.
  """
  @spec usage(String.t()) :: :ok
  def usage(name) do
    task = fetch_task!(name)
    log_info(task.usage())
  rescue
    exception -> log_exception(exception, name, __STACKTRACE__)
  end

  defp fetch_task!("server"), do: LivebookCLI.Server
  defp fetch_task!("deploy"), do: LivebookCLI.Deploy

  defp fetch_task!(name) do
    log_error("Unknown command #{name}")
    log_info(LivebookCLI.usage())

    System.halt(1)
  end

  @spec log_exception(Exception.t(), String.t(), Exception.stacktrace()) :: no_return()
  defp log_exception(exception, command_name, stacktrace) when is_exception(exception) do
    [:red, format_exception(exception, command_name, stacktrace)]
    |> IO.ANSI.format()
    |> IO.puts()

    System.halt(1)
  end

  defp format_exception(%OptionParser.ParseError{} = exception, command_name, _) do
    """
    #{Exception.message(exception)}

    For more information try:

       livebook #{command_name} --help
    """
  end

  defp format_exception(%LivebookCLI.Error{} = exception, command_name, _) do
    """
    #{Exception.message(exception)}

    For more information try:

       livebook #{command_name} --help
    """
  end

  defp format_exception(exception, _, stacktrace) do
    Exception.format(:error, exception, stacktrace)
  end
end
