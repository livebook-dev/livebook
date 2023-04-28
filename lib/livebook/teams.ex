defmodule Livebook.Teams do
  @moduledoc false

  alias Livebook.Teams.{Client, Org}

  @doc """
  Send a request to Livebook Team API to create a new org.
  """
  @spec create_org(Org.t()) :: {:ok, map()} | {:error, String.t() | Ecto.Changeset.t() | atom()}
  def create_org(%Org{user_code: "request"} = org) do
    with {:error, %{errors: errors}} <- Client.create_org(org) do
      {:error, Org.add_errors(org, errors)}
    end
  end

  @doc """
  Send a request to Livebook Team API to get an org request.
  """
  @spec get_org_request_completion_data(Org.t()) ::
          {:ok, map()} | {:error, String.t() | map() | atom()}
  def get_org_request_completion_data(%Org{id: id}) do
    Client.get_org_request_completion_data(id)
  end
end
