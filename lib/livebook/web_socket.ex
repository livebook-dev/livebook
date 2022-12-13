defmodule Livebook.WebSocket do
  @moduledoc false

  alias Livebook.WebSocket.Client
  alias LivebookProto.Request

  defmodule Connection do
    defstruct [:conn, :websocket, :ref]

    @type t :: %__MODULE__{
            conn: Client.conn(),
            websocket: Client.websocket(),
            ref: Client.ref()
          }
  end

  @type proto :: LivebookProto.SessionRequest.t()
  @typep headers :: Mint.Types.headers()

  @doc """
  Connects with the WebSocket server for given URL and headers.
  """
  @spec connect(String.t(), headers()) ::
          {:ok, Connection.t(), :connected | {atom(), proto()}}
          | {:error, Connection.t(), String.t() | LivebookProto.Error.t()}
  def connect(url, headers \\ []) do
    with {:ok, conn, ref} <- Client.connect(url, headers) do
      conn
      |> Client.receive(ref)
      |> handle_receive(ref)
    end
  end

  @dialyzer {:nowarn_function, disconnect: 1}

  @doc """
  Disconnects the given WebSocket client.
  """
  @spec disconnect(Connection.t()) ::
          {:ok, Connection.t()}
          | {:error, Connection.t(), Client.ws_error() | Client.mint_error()}
  def disconnect(%Connection{} = connection) do
    case Client.disconnect(connection.conn, connection.websocket, connection.ref) do
      {:ok, conn, websocket} ->
        {:ok, %{connection | conn: conn, websocket: websocket, ref: nil}}

      {:error, %Mint.WebSocket{} = websocket, reason} ->
        {:error, %{connection | websocket: websocket}, reason}

      {:error, conn, reason} ->
        {:error, %{connection | conn: conn}, reason}
    end
  end

  @dialyzer {:nowarn_function, send_request: 2}

  @doc """
  Sends a request to the given server.
  """
  @spec send_request(Connection.t(), proto()) ::
          {:ok, Connection.t()}
          | {:error, Connection.t(), Client.ws_error() | Client.mint_error()}
  def send_request(%Connection{} = connection, %struct{} = data) do
    type = LivebookProto.request_type(struct)
    message = Request.new!(type: {type, data})
    binary = {:binary, Request.encode(message)}

    case Client.send(connection.conn, connection.websocket, connection.ref, binary) do
      {:ok, conn, websocket} ->
        {:ok, %{connection | conn: conn, websocket: websocket}}

      {:error, %Mint.WebSocket{} = websocket, reason} ->
        {:error, %{connection | websocket: websocket}, reason}

      {:error, conn, reason} ->
        {:error, %{connection | conn: conn}, reason}
    end
  end

  @dialyzer {:nowarn_function, receive_response: 1}

  @doc """
  Receives a response from the given server.
  """
  @spec receive_response(Connection.t()) ::
          {:ok, Client.conn(), Client.websocket(), Client.Response.t() | :connect}
          | {:error, Client.conn(), Client.Response.t()}
  def receive_response(%Connection{conn: conn, websocket: websocket, ref: ref}) do
    conn
    |> Client.receive(ref, websocket)
    |> handle_receive(ref)
  end

  defp handle_receive({:ok, conn, websocket, :connected}, ref) do
    {:ok, %Connection{conn: conn, websocket: websocket, ref: ref}, :connected}
  end

  defp handle_receive({:ok, conn, websocket, %Client.Response{body: response}}, ref) do
    %{type: result} = LivebookProto.Response.decode(response)
    {:ok, %Connection{conn: conn, websocket: websocket, ref: ref}, result}
  end

  defp handle_receive({:error, conn, %Client.Response{body: nil, status: status}}, ref) do
    {:error, %Connection{conn: conn, ref: ref}, Plug.Conn.Status.reason_phrase(status)}
  end

  defp handle_receive({:error, conn, %Client.Response{body: response}}, ref) do
    %{type: {:error, error}} = LivebookProto.Response.decode(response)
    {:error, %Connection{conn: conn, ref: ref}, error}
  end
end
