defmodule Livebook.Proxy.Server do
  @moduledoc false

  # The entrypoint for delegating `conn` handling to a runtime.
  #
  # The `Livebook.Proxy` modules are an implementation detail of the
  # runtime. `Livebook.Proxy.Server` lives on the Livebook-side and
  # it delegates request handling to `Livebook.Proxy.Handler`, which
  # lives in the runtime node. The handler uses a custom plug adapter
  # that dispatches `%Plug.Conn{}` operations as messages back to the
  # server.
  #
  # Note that the server is not itself a new process, it is whoever
  # calls `serve/2`.

  import Plug.Conn

  @doc """
  Handles a request by delegating to a new handler process in the
  runtime.

  This function blocks until the request handler is done and it returns
  the final `conn`.
  """
  @spec serve(pid(), Plug.Conn.t()) :: Plug.Conn.t()
  def serve(supervisor_pid, %Plug.Conn{} = conn) when is_pid(supervisor_pid) do
    args = [self(), build_handler_conn(conn)]

    {:ok, handler_pid} =
      Task.Supervisor.start_child(supervisor_pid, Livebook.Proxy.Handler, :serve, args)

    monitor_ref = Process.monitor(handler_pid)
    loop(monitor_ref, conn)
  end

  defp build_handler_conn(conn) do
    %{
      adapter: nil,
      host: conn.host,
      method: conn.method,
      owner: conn.owner,
      port: conn.port,
      remote_ip: conn.remote_ip,
      query_string: conn.query_string,
      path_info: conn.path_info,
      scheme: conn.scheme,
      script_name: conn.script_name,
      req_headers: conn.req_headers
    }
  end

  defp loop(monitor_ref, conn) do
    receive do
      {:send_resp, pid, ref, status, headers, body} ->
        conn = send_resp(%{conn | resp_headers: headers}, status, body)
        send(pid, {ref, :ok})
        loop(monitor_ref, conn)

      {:get_peer_data, pid, ref} ->
        send(pid, {ref, get_peer_data(conn)})
        loop(monitor_ref, conn)

      {:get_http_protocol, pid, ref} ->
        send(pid, {ref, get_http_protocol(conn)})
        loop(monitor_ref, conn)

      {:read_req_body, pid, ref, opts} ->
        {message, conn} =
          case read_body(conn, opts) do
            {:ok, data, conn} -> {{:ok, data}, conn}
            {:more, data, conn} -> {{:more, data}, conn}
            {:error, _} = error -> {error, conn}
          end

        send(pid, {ref, message})
        loop(monitor_ref, conn)

      {:send_chunked, pid, ref, status, headers} ->
        conn = send_chunked(%{conn | resp_headers: headers}, status)
        send(pid, {ref, :ok})
        loop(monitor_ref, conn)

      {:chunk, pid, ref, chunk} ->
        {message, conn} =
          case chunk(conn, chunk) do
            {:error, _} = error -> {error, conn}
            result -> result
          end

        send(pid, {ref, message})
        loop(monitor_ref, conn)

      {:inform, pid, ref, status, headers} ->
        conn = inform(conn, status, headers)
        send(pid, {ref, :ok})
        loop(monitor_ref, conn)

      {:DOWN, ^monitor_ref, :process, _pid, _reason} ->
        conn
    end
  end
end
