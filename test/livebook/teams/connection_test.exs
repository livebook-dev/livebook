defmodule Livebook.Teams.ConnectionTest do
  use ExUnit.Case, async: false

  alias Livebook.Teams.Connection

  @moduletag :capture_log

  setup do
    original_url = Application.get_env(:livebook, :teams_url)

    on_exit(fn ->
      Application.put_env(:livebook, :teams_url, original_url)
    end)

    :ok
  end

  describe "server error handling" do
    test "handles invalid protobuf without crashing" do
      html_error = """
      <!DOCTYPE html>
      <html><body><h1>500 Internal Server Error</h1></body></html>
      """

      start_error_server(500, html_error, "text/html")

      {:ok, conn_pid} = Connection.start_link(self(), [{"x-test", "true"}])

      assert_receive {:server_error, "Server error (unexpected response format)"}, 5000
      assert Process.alive?(conn_pid)
    end

    test "sends :service_unavailable on 503 and retries with backoff" do
      error = %LivebookProto.Error{details: "Service temporarily unavailable"}
      encoded = LivebookProto.Error.encode(error)

      start_error_server(503, encoded, "application/octet-stream")

      {:ok, conn_pid} = Connection.start_link(self(), [{"x-test", "true"}])

      # First attempt
      assert_receive {:service_unavailable, "Service temporarily unavailable"}, 2000
      assert Process.alive?(conn_pid)

      # Retry after backoff (3-10 seconds)
      assert_receive {:service_unavailable, _}, 12_000
      assert Process.alive?(conn_pid)
    end
  end

  defp start_error_server(status_code, body, content_type) do
    port = get_free_port()
    Application.put_env(:livebook, :teams_url, "http://localhost:#{port}")

    plug =
      {__MODULE__.ErrorPlug, status_code: status_code, body: body, content_type: content_type}

    start_supervised!({Bandit, plug: plug, port: port, startup_log: false})
  end

  defp get_free_port do
    {:ok, socket} = :gen_tcp.listen(0, [])
    {:ok, port} = :inet.port(socket)
    :gen_tcp.close(socket)
    port
  end

  defmodule ErrorPlug do
    @behaviour Plug

    @impl true
    def init(opts), do: opts

    @impl true
    def call(conn, opts) do
      status_code = Keyword.fetch!(opts, :status_code)
      body = Keyword.fetch!(opts, :body)
      content_type = Keyword.fetch!(opts, :content_type)

      conn
      |> Plug.Conn.put_resp_content_type(content_type)
      |> Plug.Conn.send_resp(status_code, body)
    end
  end
end
