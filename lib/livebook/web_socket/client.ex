defmodule Livebook.WebSocket.Client do
  @moduledoc false

  alias Mint.WebSocket.UpgradeFailureError

  @ws_path "/livebook/websocket"

  @type conn :: Mint.HTTP.t()
  @type websocket :: Mint.WebSocket.t()
  @type frame :: Mint.WebSocket.frame() | Mint.WebSocket.shorthand_frame()
  @type ref :: Mint.Types.request_ref()
  @type ws_error :: Mint.WebSocket.error()
  @type mint_error :: Mint.Types.error()

  defmodule Response do
    defstruct [:body, :status, :headers]

    @type t :: %__MODULE__{
            body: Livebook.WebSocket.Response.t() | nil,
            status: Mint.Types.status() | nil,
            headers: Mint.Types.headers() | nil
          }
  end

  defguard is_frame(value) when value in [:close, :ping] or elem(value, 0) == :binary

  @doc """
  Connects to the WebSocket server with given url and headers.
  """
  @spec connect(String.t(), list({String.t(), String.t()})) ::
          {:ok, conn(), ref()}
          | {:error, mint_error()}
          | {:error, conn(), ws_error()}
  def connect(url, headers \\ []) do
    uri = URI.parse(url)
    http_scheme = parse_http_scheme(uri)
    ws_scheme = parse_ws_scheme(uri)

    with {:ok, conn} <- Mint.HTTP.connect(http_scheme, uri.host, uri.port),
         {:ok, conn, ref} <- Mint.WebSocket.upgrade(ws_scheme, conn, @ws_path, headers) do
      {:ok, conn, ref}
    end
  end

  defp parse_http_scheme(uri) when uri.scheme in ["http", "ws"], do: :http
  defp parse_http_scheme(uri) when uri.scheme in ["https", "wss"], do: :https

  defp parse_ws_scheme(uri) when uri.scheme in ["http", "ws"], do: :ws
  defp parse_ws_scheme(uri) when uri.scheme in ["https", "wss"], do: :wss

  @doc """
  Disconnects from the given connection, WebSocket and reference.

  If there's no WebSocket connection yet, it'll only close the HTTP connection.
  """
  @spec disconnect(conn(), websocket() | nil, ref()) ::
          {:ok, conn(), websocket() | nil}
          | {:error, conn() | websocket(), term()}
  def disconnect(conn, nil, _ref) do
    {:ok, conn} = Mint.HTTP.close(conn)
    {:ok, conn, nil}
  end

  def disconnect(conn, websocket, ref) do
    with {:ok, conn, websocket} <- send(conn, websocket, ref, :close),
         {:ok, conn} <- Mint.HTTP.close(conn) do
      {:ok, conn, websocket}
    end
  end

  @doc """
  Receive the message from the given HTTP connection.

  If the WebSocket isn't connected yet, it will try to get the connection
  response to start a new WebSocket connection.
  """
  @spec receive(conn() | nil, ref(), websocket() | nil, term()) ::
          {:ok, conn(), websocket(), Response.t() | :connected}
          | {:error, conn(), websocket(), Response.t()}
          | {:error, conn(), websocket(), ws_error() | mint_error()}
          | {:error, :not_connected | :unknown}
  def receive(conn, ref, websocket \\ nil, message \\ receive(do: (message -> message))) do
    do_receive(conn, ref, websocket, message)
  end

  defp do_receive(nil, _ref, _websocket, _message), do: {:error, :not_connected}

  defp do_receive(conn, ref, websocket, message) do
    case Mint.WebSocket.stream(conn, message) do
      {:ok, conn, responses} ->
        handle_responses(conn, ref, websocket, responses)

      {:error, conn, reason, []} ->
        {:error, conn, websocket, reason}

      {:error, conn, _reason, responses} ->
        handle_responses(conn, ref, websocket, responses)

      :unknown ->
        {:error, :unknown}
    end
  end

  @successful_status 100..299

  defp handle_responses(conn, ref, websocket, [{:data, ref, data}]) do
    with {:ok, websocket, frames} <- Mint.WebSocket.decode(websocket, data) do
      case handle_frames(%Response{}, frames) do
        {:ok, %Response{body: %{type: {:error, _}}} = response} ->
          {:error, conn, websocket, response}

        {:ok, response} ->
          {:ok, conn, websocket, response}

        {:close, result} ->
          handle_disconnect(conn, websocket, ref, result)

        {:error, response} ->
          {:error, conn, websocket, response}
      end
    end
  end

  defp handle_responses(conn, ref, websocket, [_ | _] = responses) do
    result =
      Enum.reduce(responses, %Response{}, fn
        {:status, ^ref, status}, acc -> %{acc | status: status}
        {:headers, ^ref, headers}, acc -> %{acc | headers: headers}
        {:data, ^ref, body}, acc -> %{acc | body: body}
        {:done, ^ref}, acc -> handle_done_response(conn, ref, websocket, acc)
      end)

    case result do
      %Response{} = response when response.status not in @successful_status ->
        {:error, conn, websocket, response}

      result ->
        result
    end
  end

  defp handle_done_response(conn, ref, websocket, response) do
    case Mint.WebSocket.new(conn, ref, response.status, response.headers) do
      {:ok, conn, websocket} ->
        case decode_response(websocket, response) do
          {websocket, {:ok, result}} ->
            {:ok, conn, websocket, result}

          {websocket, {:close, result}} ->
            handle_disconnect(conn, websocket, ref, result)

          {websocket, {:error, reason}} ->
            {:error, conn, websocket, reason}
        end

      {:error, conn, %UpgradeFailureError{status_code: status, headers: headers}} ->
        {:error, conn, websocket, %{response | status: status, headers: headers}}
    end
  end

  defp handle_disconnect(conn, websocket, ref, result) do
    with {:ok, conn, websocket} <- disconnect(conn, websocket, ref) do
      {:ok, conn, websocket, result}
    end
  end

  defp decode_response(websocket, %Response{status: 101, body: nil}) do
    {websocket, {:ok, :connected}}
  end

  defp decode_response(websocket, response) do
    case Mint.WebSocket.decode(websocket, response.body) do
      {:ok, websocket, frames} ->
        {websocket, handle_frames(response, frames)}

      {:error, websocket, reason} ->
        {websocket, {:error, reason}}
    end
  end

  defp handle_frames(response, frames) do
    Enum.reduce(frames, response, fn
      _, {_, _} = acc ->
        acc

      {:binary, binary}, acc ->
        {:ok, %{acc | body: binary}}

      {:close, _code, _data}, acc ->
        {:close, acc}

      _, acc ->
        {:ok, acc}
    end)
  end

  @doc """
  Sends a message to the given HTTP Connection and WebSocket connection.
  """
  @spec send(conn(), websocket(), ref(), frame()) ::
          {:ok, conn(), websocket()}
          | {:error, conn(), websocket(), term()}
  def send(conn, websocket, ref, frame) when is_frame(frame) do
    with {:ok, websocket, data} <- Mint.WebSocket.encode(websocket, frame),
         {:ok, conn} <- Mint.WebSocket.stream_request_body(conn, ref, data) do
      {:ok, conn, websocket}
    else
      {:error, %Mint.HTTP1{} = conn, reason} ->
        {:error, conn, websocket, reason}

      {:error, websocket, reason} ->
        {:error, conn, websocket, reason}
    end
  end
end
