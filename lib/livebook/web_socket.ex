defmodule Livebook.WebSocket do
  @moduledoc false

  @doc """
  Subscribe to WebSocket Server events.

  ## Messages

    * `{:ok, pid, random_id, :connected}`
    * `{:ok, pid, random_id, %Livebook.WebSocket.Response{}}`
    * `{:error, pid, random_id, %Livebook.WebSocket.Response{}}`
    * `{:error, pid, random_id, reason}`

  """
  @spec subscribe() :: :ok | {:error, {:already_registered, pid()}}
  def subscribe do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "websocket:clients")
  end

  @doc """
  Unsubscribes from `subscribe/0`.
  """
  @spec unsubscribe() :: :ok
  def unsubscribe do
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, "websocket:clients")
  end

  @doc """
  Notifies interested processes about WebSocket Server messages.

  Broadcasts the given message under the `"websocket:clients"` topic.
  """
  @spec broadcast_message(any()) :: :ok
  def broadcast_message(message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "websocket:clients", message)
  end
end
