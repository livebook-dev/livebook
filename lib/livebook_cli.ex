defmodule LivebookCLI do
  alias LivebookCLI.{Task, Utils}

  @switches [
    help: :boolean,
    version: :boolean
  ]

  @aliases [
    h: :help,
    v: :version
  ]

  def main(args) do
    Utils.setup()

    case Utils.option_parse(args, strict: @switches, aliases: @aliases) do
      {parsed, [], _} when parsed.help -> display_help()
      {parsed, [name], _} when parsed.help -> Task.usage(name)
      {parsed, _, _} when parsed.version -> display_version()
      # We want to keep the switches for the task
      {_, [name | _], _} -> Task.call(name, List.delete(args, name))
    end
  end

  defp display_help() do
    Utils.print_text("""
    Livebook is an interactive notebook system for Elixir

    Usage: livebook [command] [options]

    Available commands:

      livebook server    Starts the Livebook web application

    The --help and --version options can be given instead of a command for usage and versioning information.\
    """)
  end

  defp display_version() do
    Utils.print_text("""
    #{:erlang.system_info(:system_version)}
    Elixir #{System.build_info()[:build]}
    Livebook #{Livebook.Config.app_version()}\
    """)
  end
end
