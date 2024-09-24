defmodule Livebook.Proxy.Adapter do
  @moduledoc false

  # Implements a Plug adapter for handling `conn` within the runtime.
  #
  # All actions are forwarded to the parent process (`Livebook.Proxy.Server`),
  # which operates within Livebook itself.

  @behaviour Plug.Conn.Adapter

  @impl true
  def send_resp({pid, ref}, status, headers, body) do
    send(pid, {:send_resp, self(), ref, status, headers, body})

    receive do
      {^ref, :ok} -> {:ok, body, {pid, ref}}
      {:DOWN, ^ref, _, _, reason} -> exit_fun(:send_resp, 4, reason)
    end
  end

  @impl true
  def get_peer_data({pid, ref}) do
    send(pid, {:get_peer_data, self(), ref})

    receive do
      {^ref, peer_data} -> peer_data
      {:DOWN, ^ref, _, _, reason} -> exit_fun(:get_peer_data, 1, reason)
    end
  end

  @impl true
  def get_http_protocol({pid, ref}) do
    send(pid, {:get_http_protocol, self(), ref})

    receive do
      {^ref, http_protocol} -> http_protocol
      {:DOWN, ^ref, _, _, reason} -> exit_fun(:get_http_protocol, 1, reason)
    end
  end

  @impl true
  def read_req_body({pid, ref}, opts) do
    send(pid, {:read_req_body, self(), ref, opts})

    receive do
      {^ref, {:ok, data}} -> {:ok, data, {pid, ref}}
      {^ref, {:more, data}} -> {:more, data, {pid, ref}}
      {^ref, {:error, _} = error} -> error
      {:DOWN, ^ref, _, _, reason} -> exit_fun(:read_req_body, 2, reason)
    end
  end

  @impl true
  def send_chunked({pid, ref}, status, headers) do
    send(pid, {:send_chunked, self(), ref, status, headers})

    receive do
      {^ref, :ok} -> {:ok, nil, {pid, ref}}
      {:DOWN, ^ref, _, _, reason} -> exit_fun(:send_chunked, 3, reason)
    end
  end

  @impl true
  def chunk({pid, ref}, chunk) do
    send(pid, {:chunk, self(), ref, chunk})

    receive do
      {^ref, :ok} -> :ok
      {^ref, {:error, _} = error} -> error
      {:DOWN, ^ref, _, _, reason} -> exit_fun(:chunk, 2, reason)
    end
  end

  @impl true
  def inform({pid, ref}, status, headers) do
    send(pid, {:inform, self(), ref, status, headers})

    receive do
      {^ref, :ok} -> {:ok, {pid, ref}}
      {:DOWN, ^ref, _, _, reason} -> exit_fun(:inform, 3, reason)
    end
  end

  @impl true
  def send_file({pid, ref}, status, headers, path, offset, length) do
    %File.Stat{type: :regular, size: size} = File.stat!(path)

    length =
      cond do
        length == :all -> size
        is_integer(length) -> length
      end

    {:ok, body} =
      File.open!(path, [:read, :raw, :binary], fn device ->
        :file.pread(device, offset, length)
      end)

    send(pid, {:send_resp, self(), ref, status, headers, body})

    receive do
      {^ref, :ok} -> {:ok, body, {pid, ref}}
      {:DOWN, ^ref, _, _, reason} -> exit_fun(:send_file, 6, reason)
    end
  end

  @impl true
  def upgrade(_payload, _protocol, _opts), do: {:error, :not_supported}

  @impl true
  def push(_payload, _path, _headers), do: {:error, :not_supported}

  defp exit_fun(fun, arity, reason) do
    exit({{__MODULE__, fun, arity}, reason})
  end
end
