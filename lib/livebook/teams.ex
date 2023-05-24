defmodule Livebook.Teams do
  @moduledoc false

  alias Livebook.Hubs
  alias Livebook.Hubs.Team
  alias Livebook.Teams.{Client, Org}

  import Ecto.Changeset, only: [add_error: 3, apply_action: 2, apply_action!: 2, get_field: 2]

  @doc """
  Creates an Org.

  With success, returns the response from Livebook Teams API to continue the org creation flow.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec create_org(Org.t(), map()) ::
          {:ok, map()}
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def create_org(%Org{} = org, attrs) do
    create_org_request(org, attrs, &Client.create_org/1)
  end

  @doc """
  Joins an Org.

  With success, returns the response from Livebook Teams API to continue the org joining flow.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec join_org(Org.t(), map()) ::
          {:ok, map()}
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def join_org(%Org{} = org, attrs) do
    create_org_request(org, attrs, &Client.join_org/1)
  end

  defp create_org_request(%Org{} = org, attrs, callback) when is_function(callback, 1) do
    changeset = Org.changeset(org, attrs)

    with {:ok, %Org{} = org} <- apply_action(changeset, :insert),
         {:ok, response} <- callback.(org) do
      {:ok, response}
    else
      {:error, %Ecto.Changeset{} = changeset} ->
        {:error, changeset}

      {:error, %{"errors" => errors_map}} ->
        errors_map =
          if errors = errors_map["key_hash"],
            do: Map.put_new(errors_map, "teams_key", errors),
            else: errors_map

        {:error, add_org_errors(changeset, errors_map)}

      any ->
        any
    end
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking org changes.
  """
  @spec change_org(Org.t(), map()) :: Ecto.Changeset.t()
  def change_org(%Org{} = org, attrs \\ %{}) do
    Org.changeset(org, attrs)
  end

  @doc """
  Send a request to Livebook Teams API to get an org request.
  """
  @spec get_org_request_completion_data(Org.t()) ::
          {:ok, map() | :awaiting_confirmation}
          | {:error, :expired}
          | {:transport_error, String.t()}
  def get_org_request_completion_data(%Org{id: id}) do
    case Client.get_org_request_completion_data(id) do
      {:ok, %{"status" => "awaiting_confirmation"}} -> {:ok, :awaiting_confirmation}
      {:ok, completion_data} -> {:ok, completion_data}
      {:error, %{"status" => "expired"}} -> {:error, :expired}
      any -> any
    end
  end

  @doc """
  Creates a Hub.

  It notifies interested processes about hub metadatas data change.
  """
  @spec create_hub!(map()) :: Team.t()
  def create_hub!(attrs) do
    changeset = Team.change_hub(%Team{}, attrs)
    team = apply_action!(changeset, :insert)

    Hubs.save_hub(team)
  end

  @doc """
  Updates a Hub.

  With success, notifies interested processes about hub metadatas data change.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec update_hub(Team.t(), map()) :: {:ok, Team.t()} | {:error, Ecto.Changeset.t()}
  def update_hub(%Team{} = team, attrs) do
    changeset = Team.change_hub(team, attrs)
    id = get_field(changeset, :id)

    if Hubs.hub_exists?(id) do
      with {:ok, struct} <- apply_action(changeset, :update) do
        {:ok, Hubs.save_hub(struct)}
      end
    else
      {:error, add_error(changeset, :hub_name, "does not exists")}
    end
  end

  defp add_org_errors(%Ecto.Changeset{} = changeset, errors_map) do
    for {key, errors} <- errors_map,
        field = String.to_atom(key),
        field in Org.__schema__(:fields),
        error <- errors,
        reduce: changeset,
        do: (acc -> add_error(acc, field, error))
  end
end
