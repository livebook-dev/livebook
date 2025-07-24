defmodule LivebookCLI.Utils do
  def log_info(message) do
    message
    |> IO.ANSI.format()
    |> IO.puts()
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

  def log_error(message) do
    [:red, message]
    |> IO.ANSI.format()
    |> IO.puts()
  end
end
