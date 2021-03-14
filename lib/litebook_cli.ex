defmodule LivebookCLI do
  def main(args) do
    case args do
      ["server" | _opts] ->
        Application.put_env(:phoenix, :serve_endpoints, true, persistent: true)
        Supervisor.terminate_child(Livebook.Supervisor, LivebookWeb.Endpoint)
        Supervisor.restart_child(Livebook.Supervisor, LivebookWeb.Endpoint)
        Process.sleep(:infinity)
      _ ->
        IO.puts("usage")
    end
  end
end
