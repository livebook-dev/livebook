defmodule LivebookCLI.Utils do
  def option_parse(argv, opts \\ []) do
    {parsed, argv, errors} = OptionParser.parse(argv, opts)
    {Enum.into(parsed, %{}), argv, errors}
  end

  def log_info(message) do
    IO.puts(message)
  end

  if Mix.env() == :dev do
    def log_debug(message) do
      [:cyan, message]
      |> IO.ANSI.format()
      |> IO.puts()
    end
  else
    def log_debug(_message) do
      :ok
    end
  end

  def log_warning(message) do
    [:yellow, message]
    |> IO.ANSI.format()
    |> IO.puts()
  end

  def print_text(message) do
    message
    |> IO.ANSI.format()
    |> IO.puts()
  end
end
