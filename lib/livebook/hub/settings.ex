defmodule Livebook.Hub.Settings do
  @moduledoc false

  alias Livebook.Storage
  alias Livebook.Hub.Machine

  @namespace :hub

  @doc """
  Gets a list of hubs from storage.
  """
  @spec fetch_machines() :: list(Machine.t())
  def fetch_machines do
    Storage.current().all(@namespace)
  end

  @doc """
  Saves a new hub machine to the configured ones.
  """
  @spec save_machine(Machine.t()) :: Machine.t()
  def save_machine(machine) do
    attributes =
      machine
      |> Map.from_struct()
      |> Map.to_list()

    :ok = Storage.current().insert(@namespace, machine.id, attributes)

    machine
  end
end
