defmodule Livebook.Teams.Constants do
  @doc """
  Returns the public key prefix
  """
  @spec public_key_prefix() :: String.t()
  def public_key_prefix(), do: "lb_opk_"

  @doc """
  Returns the Agent Key prefix
  """
  @spec agent_key_prefix() :: String.t()
  def agent_key_prefix, do: "lb_ak_"

  @doc """
  Returns the Org Token prefix
  """
  @spec org_token_prefix() :: String.t()
  def org_token_prefix, do: "lb_ok_"

  @doc """
  Returns the Teams Key prefix
  """
  @spec teams_key_prefix() :: String.t()
  def teams_key_prefix(), do: "lb_tk_"
end
