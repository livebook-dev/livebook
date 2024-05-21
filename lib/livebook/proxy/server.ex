defmodule Livebook.Proxy.Server do
  @moduledoc false
  import Plug.Conn

  def serve(pid, name, %Plug.Conn{} = conn) when is_pid(pid) and is_atom(name) do
    args = [self(), name, build_client_conn(conn)]
    {:ok, spawn_pid} = Task.Supervisor.start_child(pid, Livebook.Proxy.Handler, :serve, args)
    monitor_ref = Process.monitor(spawn_pid)
    loop(monitor_ref, conn)
  end

  defp build_client_conn(conn) do
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

      {:DOWN, ^monitor_ref, :process, _pid, reason} ->
        {conn, reason}
    end
  end
end
