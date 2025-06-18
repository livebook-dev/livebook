defmodule LivebookCLI.Command do
  @moduledoc false

  @doc """
  Returns a description of the command usage.
  """
  @callback usage() :: String.t()

  @doc """
  Runs the command with the given list of command line arguments.
  """
  @callback call(args :: list(String.t())) :: :ok

  @doc false
  defmacro __using__(_) do
    quote do
      import LivebookCLI, only: [error: 1, warning: 1, debug: 1, info: 1]

      @behaviour LivebookCLI.Command
    end
  end

  @doc """
  Fetches the command by given name.
  """
  @spec fetch_command(String.t()) :: {:ok, module()} | {:error, String.t()}
  def fetch_command("server"), do: {:ok, LivebookCLI.Server}
  def fetch_command("deploy"), do: {:ok, LivebookCLI.Deploy}

  def fetch_command(name), do: {:error, "Unknown command #{name}"}

  @doc """
  Shows the description of the command usage.
  """
  @spec call(String.t(), list(String.t())) :: :ok
  def call(name, args) do
    with {:ok, command} <- fetch_command(name),
         :ok <- command.call(args) do
      :ok
    else
      {:error, reason} -> LivebookCLI.error(reason)
    end
  rescue
    exception -> LivebookCLI.raise(exception, name, __STACKTRACE__)
  end

  @doc """
  Shows the description of the command usage.
  """
  @spec usage(String.t()) :: :ok
  def usage(name) do
    case fetch_command(name) do
      {:ok, command} -> LivebookCLI.info(command.usage())
      {:error, reason} -> LivebookCLI.error(reason)
    end
  rescue
    exception -> LivebookCLI.raise(exception, name, __STACKTRACE__)
  end
end
