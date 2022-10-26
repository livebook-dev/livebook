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
  Starts the WebSocket client link for given URL and headers.

  `Livebook.WebSocket.Response`s received from the server are forwarded to the sender pid.
  """
  @spec connect_link(String.t(), headers()) :: {:ok, pid()} | {:error, any()}
  def connect_link(url, headers \\ []) do
    with {:ok, pid} <- Client.start_link(),
         {:ok, :connected} <- handle_connect(pid, url, headers) do
      {:ok, pid}
    end
  end

  @doc """
  Starts the WebSocket client for given URL and headers.

  `Livebook.WebSocket.Response`s received from the server are forwarded to the sender pid.
  """
  @spec connect(String.t(), headers()) :: {:ok, pid()} | {:error, any()}
  def connect(url, headers \\ []) do
    with {:ok, pid} <- Client.start(),
         {:ok, :connected} <- handle_connect(pid, url, headers) do
      {:ok, pid}
    end
  end

  defp handle_connect(pid, url, headers) do
    with {:error, reason} <- Client.connect(pid, url, headers) do
      case reason do
        %Mint.TransportError{reason: reason} -> {:error, reason}
        %Client.Error{body: nil, status: status} -> {:error, reason_atom(status)}
        %Client.Error{body: body} -> {:error, decode_error(body)}
        _ -> {:error, reason}
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
  def disconnect(pid) do
    Client.close(pid)
  end

  @doc """
  Sends a session request to the given server.
  """
  @spec send_session(pid()) :: :ok
  def send_session(pid) do
    message = Request.new!(type: {:session, SessionRequest.new!(app_version: @app_version)})

    Client.send_message(pid, message)
  end
end
