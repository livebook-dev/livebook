defmodule LivebookCLI do
  def main(args) do
    case args do
      ["server" | _opts] ->
        start_server()
        Process.sleep(:infinity)
      _ ->
        IO.puts("usage")
    end
  end

  defp start_server() do
    Application.put_env(:phoenix, :serve_endpoints, true, persistent: true)
    {:ok, _} = Application.ensure_all_started(:livebook)
  end
end
