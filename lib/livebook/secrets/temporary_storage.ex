defmodule Livebook.Secrets.TemporaryStorage do
  @moduledoc false
  use GenServer
  @name __MODULE__

  alias Livebook.Secrets.Secret

  defstruct [:secrets]

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: @name)
  end

  def fetch_secrets do
    GenServer.call(@name, :fetch_secrets)
  end

  def set_secret(%Secret{} = secret) do
    GenServer.cast(@name, {:set_secret, secret})
  end

  ## GenServer callbacks

  @impl true
  def init(_opts), do: {:ok, %__MODULE__{secrets: []}}

  @impl true
  def handle_call(:fetch_secrets, _from, state) do
    {:reply, state.secrets, state}
  end

  @impl true
  def handle_cast({:set_secret, secret}, state) do
    {:noreply, %{state | secrets: [secret | state.secrets]}}
  end
end
