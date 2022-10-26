defmodule Livebook.WebSocket do
  @moduledoc false

  import Plug.Conn.Status, only: [reason_atom: 1]

  alias Livebook.WebSocket.{
    Client,
    Request,
    Response,
    SessionRequest
  }

  @app_version Mix.Project.config()[:version]

  @typep header :: {String.t(), String.t()}
  @typep headers :: list(header())

  @doc """
  Starts the WebSocket client link for given ws URL.

  `Livebook.WebSocket.Response`s received from the server are forwarded to the sender pid.
  """
  @spec connect_link(String.t(), headers()) :: {:ok, pid()} | {:error, any()}
  def connect_link(url, headers \\ []) do
    with {:ok, socket} <- Client.start_link(),
         {:ok, :connected} <- handle_connect(socket, url, headers) do
      {:ok, socket}
    end
  end

  @doc """
  Starts the WebSocket client for given ws URL.

  `Livebook.WebSocket.Response`s received from the server are forwarded to the sender pid.
  """
  @spec connect(String.t(), headers()) :: {:ok, pid()} | {:error, any()}
  def connect(url, headers \\ []) do
    with {:ok, socket} <- Client.start(),
         {:ok, :connected} <- handle_connect(socket, url, headers) do
      {:ok, socket}
    end
  end

  defp handle_connect(socket, url, headers) do
    with {:error, reason} <- Client.connect(socket, url, headers) do
      case reason do
        reason when is_atom(reason) -> {:error, reason}
        %Mint.TransportError{reason: reason} -> {:error, reason}
        %Client.Error{body: nil, status: status} -> {:error, reason_atom(status)}
        %Client.Error{body: body} -> {:error, decode_error(body)}
      end
    end
  end

  defp decode_error(text) do
    try do
      response = Response.decode(text)
      {:error, error} = response.type

      error.details
    rescue
      _ -> text
    end
  end

  @doc """
  Disconnects the given WebSocket client.
  """
  @spec disconnect(pid()) :: :ok
  def disconnect(socket) do
    Client.close(socket)
  end

  @doc """
  Sends a session request to the given server.
  """
  @spec send_session(pid()) :: :ok
  def send_session(socket) do
    message = Request.new!(type: {:session, SessionRequest.new!(app_version: @app_version)})

    Client.send_message(socket, message)
  end
end
