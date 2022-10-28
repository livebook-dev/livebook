defmodule Livebook.WebSocket do
  @moduledoc false

  alias Livebook.WebSocket.{
    Client,
    Request,
    SessionRequest
  }

  defmodule Connection do
    defstruct [:conn, :websocket, :ref]

    @type t :: %__MODULE__{
            conn: Client.conn(),
            websocket: Client.websocket(),
            ref: Client.ref()
          }
  end

  @app_version Mix.Project.config()[:version]

  @typep header :: {String.t(), String.t()}
  @typep headers :: list(header())

  @doc """
  Connects with the WebSocket server for given URL and headers.
  """
  @spec connect(String.t(), headers()) :: Client.recv_fun()
  def connect(url, headers \\ []) do
    with {:ok, conn, ref} <- Client.connect(url, headers) do
      conn
      |> Client.recv(ref)
      |> handle_recv(ref)
    end
  end

  @doc """
  Disconnects the given WebSocket client.
  """
  @spec disconnect(Connection.t()) :: :ok
  def disconnect(%Connection{} = connection) do
    Client.disconnect(connection.conn, connection.websocket, connection.ref)
  end

  @dialyzer {:nowarn_function, send_session: 1}

  @doc """
  Sends a session request to the given server.
  """
  @spec send_session(Connection.t()) :: Client.send_fun()
  def send_session(%Connection{} = connection) do
    session_request = SessionRequest.new!(app_version: @app_version)
    message = Request.new!(type: {:session, session_request})

    case Client.send(connection.conn, connection.websocket, connection.ref, message) do
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
  @spec receive_response(Connection.t()) :: Client.recv_fun()
  def receive_response(%Connection{conn: conn, websocket: websocket, ref: ref}) do
    conn
    |> Client.recv(ref, websocket)
    |> handle_recv(ref)
  end

  defp handle_recv({:ok, conn, websocket, :connected}, ref) do
    {:ok, %Connection{conn: conn, websocket: websocket, ref: ref}, :connected}
  end

  defp handle_recv({:ok, conn, websocket, %Client.Response{body: response}}, ref) do
    {:ok, %Connection{conn: conn, websocket: websocket, ref: ref}, response.type}
  end

  defp handle_recv({:error, conn, %Client.Response{body: nil, status: status}}, ref) do
    {:error, %Connection{conn: conn, ref: ref}, Plug.Conn.Status.reason_phrase(status)}
  end

  defp handle_recv({:error, conn, %Client.Response{body: %{type: {:error, error}}}}, ref) do
    {:error, %Connection{conn: conn, ref: ref}, error}
  end
end
