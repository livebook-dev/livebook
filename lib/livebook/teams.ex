defmodule Livebook.Teams do
  @moduledoc false

  alias Livebook.Teams.{Client, Org}

  @timeout_error_message "Well, looks like we didn't get a response in time. Could you please try again?"
  @any_error_message "Well, this is embarrassing... An error has occurred and we're working to fix the problem!"

  @doc """
  Creates an Org.

  With success, returns the response from Livebook Teams API to continue the org creation flow.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec create_org(Org.t(), map()) ::
          {:ok, map()}
          | {:error, String.t() | Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def create_org(%Org{} = org, attrs) do
    changeset = Org.changeset(org, attrs)

    with {:ok, %Org{} = org} <- Ecto.Changeset.apply_action(changeset, :insert),
         {:ok, response} <- Client.create_org(org) do
      {:ok, response}
    else
      {:error, %Ecto.Changeset{} = changeset} ->
        {:error, changeset}

      {:error, %{"errors" => %{"detail" => reason}}} ->
        {:error, reason}

      {:error, %{"errors" => errors_map}} ->
        errors_map =
          if errors = errors_map["key_hash"],
            do: Map.put_new(errors_map, "teams_key", errors),
            else: errors_map

        {:error, add_org_errors(changeset, errors_map)}

      {:transport_error, :timeout} ->
        {:transport_error, @timeout_error_message}

      {:transport_error, _reason} ->
        {:transport_error, @any_error_message}
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
          | {:error, atom()}
          | {:transport_error, String.t()}
  def get_org_request_completion_data(%Org{id: id}) do
    case Client.get_org_request_completion_data(id) do
      {:ok, %{"status" => "awaiting_confirmation"}} -> {:ok, :awaiting_confirmation}
      {:ok, completion_data} -> {:ok, completion_data}
      {:error, %{"errors" => %{"detail" => "Not Found"}}} -> {:error, :not_found}
      {:error, %{"errors" => %{"detail" => "Gone"}}} -> {:error, :expired}
      {:transport_error, :timeout} -> {:transport_error, @timeout_error_message}
      {:transport_error, _} -> {:transport_error, @any_error_message}
    end
  end

  defp add_org_errors(%Ecto.Changeset{} = changeset, errors_map) do
    for {key, errors} <- errors_map,
        field <- String.to_atom(key),
        field in Org.__schema__(:fields),
        error <- errors,
        reduce: changeset,
        do: (acc -> Ecto.Changeset.add_error(acc, field, error))
  end
end
