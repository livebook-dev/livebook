defmodule Livebook.WebSocket.Client do
  @moduledoc false

  alias Mint.WebSocket.UpgradeFailureError

  @ws_path "/livebook/websocket"

  @type conn :: Mint.HTTP.t()
  @type websocket :: Mint.WebSocket.t()
  @type frame :: Mint.WebSocket.frame()
  @type ref :: Mint.Types.request_ref()
  @type ws_error :: Mint.WebSocket.error()
  @type mint_error :: Mint.Types.error()

  @type connect_fun ::
          {:ok, conn(), ref()}
          | {:error, mint_error()}
          | {:error, conn(), ws_error()}

  @type recv_fun ::
          {:ok, conn(), Response.t() | :connect}
          | {:error, conn(), Response.t()}
          | {:error, conn(), :unknown}

  @type send_fun ::
          {:ok, conn(), websocket()}
          | {:error, conn() | websocket(), term()}

  defmodule Response do
    defstruct [:body, :status, :headers]

    @type t :: %__MODULE__{
            body: Livebook.WebSocket.Response.t(),
            status: Mint.Types.status(),
            headers: Mint.Types.headers()
          }
  end

  @doc """
  Connects to the WebSocket server with given url and headers.
  """
  @spec connect(String.t(), list({String.t(), String.t()})) :: connect_fun()
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
  @spec disconnect(conn(), websocket(), ref()) :: :ok
  def disconnect(conn, websocket, ref) do
    if websocket do
      send(conn, websocket, ref, :close)
    end

    Mint.HTTP.close(conn)

    :ok
  end

  @doc """
  Receive the message from the given HTTP connection.

  If the WebSocket isn't connected yet, it will try to get the connection
  response to start a new WebSocket connection.
  """
  @spec recv(conn(), ref(), term()) :: recv_fun()
  def recv(conn, ref, websocket \\ nil, message \\ receive(do: (message -> message))) do
    case Mint.WebSocket.stream(conn, message) do
      {:ok, conn, responses} ->
        handle_responses(conn, ref, websocket, responses)

      {:error, conn, reason, []} ->
        {:error, conn, reason}

      {:error, conn, _reason, responses} ->
        handle_responses(conn, ref, websocket, responses)

      :unknown ->
        {:error, :unknown}
    end
  end

  @successful_status 100..299

  defp handle_responses(conn, ref, nil, responses) do
    result =
      Enum.reduce(responses, %Response{}, fn
        {:status, ^ref, status}, acc -> %{acc | status: status}
        {:headers, ^ref, headers}, acc -> %{acc | headers: headers}
        {:data, ^ref, body}, acc -> %{acc | body: body}
        {:done, ^ref}, acc -> handle_done_response(conn, ref, acc)
      end)

    case result do
      %Response{} = response when response.status not in @successful_status ->
        {:error, conn, response}

      result ->
        result
    end
  end

  defp handle_responses(conn, ref, websocket, [{:data, ref, data}]) do
    with {:ok, websocket, frames} <- Mint.WebSocket.decode(websocket, data) do
      case handle_frames(%Response{}, frames) do
        {:ok, %Response{body: %{type: {:error, _}}} = response} ->
          {:error, conn, websocket, response}

        {:ok, response} ->
          {:ok, conn, websocket, response}

        {:close, result} ->
          disconnect(conn, websocket, ref)
          {:ok, conn, websocket, result}

        {:error, response} ->
          {:error, conn, websocket, response}
      end
    end
  end

  defp handle_done_response(conn, ref, response) do
    case Mint.WebSocket.new(conn, ref, response.status, response.headers) do
      {:ok, conn, websocket} ->
        case decode_body(websocket, response) do
          {websocket, {:ok, result}} ->
            {:ok, conn, websocket, result}

          {websocket, {:close, result}} ->
            disconnect(conn, websocket, ref)
            {:ok, conn, websocket, result}

          {websocket, {:error, reason}} ->
            {:error, conn, websocket, reason}
        end

      {:error, conn, %UpgradeFailureError{status_code: status, headers: headers}} ->
        response = %{
          response
          | body: decode_text(response.body),
            status: status,
            headers: headers
        }

        {:error, conn, response}
    end
  end

  defp decode_body(websocket, %Response{status: 101, body: nil}) do
    {websocket, {:ok, :connected}}
  end

  defp decode_body(websocket, response) do
    case Mint.WebSocket.decode(websocket, response.body) do
      {:ok, websocket, frames} ->
        {websocket, handle_frames(response, frames)}

      {:error, websocket, reason} ->
        {websocket, {:error, reason}}
    end
  end

  defp decode_text(text) when is_binary(text) do
    Livebook.WebSocket.Response.decode(text)
  rescue
    Protobuf.DecodeError -> text
  end

  defp decode_text(any), do: any

  defp handle_frames(response, frames) do
    Enum.reduce(frames, response, fn
      {:text, text}, acc ->
        case decode_text(text) do
          %{type: {:error, _}} = body -> {:error, %{acc | body: body}}
          body -> {:ok, %{acc | body: body}}
        end

      {:close, _code, _data}, acc ->
        {:close, acc}
    end)
  end

  @dialyzer {:nowarn_function, send: 4}

  @doc """
  Sends a message to the given HTTP Connection and WebSocket connection.
  """
  @spec send(conn(), websocket(), ref(), frame()) :: send_fun()
  def send(conn, websocket, ref, frame) do
    with {:ok, websocket, data} <- Mint.WebSocket.encode(websocket, prepare_frame(frame)),
         {:ok, conn} <- Mint.WebSocket.stream_request_body(conn, ref, data) do
      {:ok, conn, websocket}
    end
  end

  defp prepare_frame(:close), do: :close

  defp prepare_frame(frame) when is_struct(frame) do
    frame
    |> Livebook.WebSocket.Response.encode()
    |> prepare_frame()
  end

  defp prepare_frame(frame) when is_binary(frame) do
    {:text, frame}
  end
end
