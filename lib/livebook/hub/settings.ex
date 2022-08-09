defmodule Livebook.Hub.Settings do
  @moduledoc false

  alias Livebook.Storage
  alias Livebook.Hub.Machine

  defmodule NotFoundError do
    @moduledoc false

    defexception [:id, plug_status: 404]

    def message(%{id: id}) do
      "could not find a machine matching #{inspect(id)}"
    end
  end

  @namespace :hub

  @doc """
  Gets a list of hubs from storage.
  """
  @spec fetch_machines() :: list(Machine.t())
  def fetch_machines do
    Storage.current().all(@namespace)
  end

  @doc """
  Gets one hub from storage.

  Raises `NotFoundError` if the machine does not exist.
  """
  @spec machine_by_id!(String.t()) :: Machine.t()
  def machine_by_id!(id) do
    case Storage.current().fetch(@namespace, id) do
      :error -> raise NotFoundError, id: id
      {:ok, fields} -> struct!(Machine, fields)
    end
  end

  @doc """
  Checks if hub already exists.
  """
  @spec machine_exists?(Machine.t()) :: boolean()
  def machine_exists?(%Machine{id: id}) do
    case Storage.current().fetch(@namespace, id) do
      :error -> false
      {:ok, _} -> true
    end
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
