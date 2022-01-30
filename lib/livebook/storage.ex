defmodule Livebook.Storage do
  @moduledoc """
  Behaviour defining an interface for storing arbitrary data in
  [Entity-Attribute-Value](https://en.wikipedia.org/wiki/Entity%E2%80%93attribute%E2%80%93value_model) fashion.
  """

  @type namespace :: atom()
  @type entity_id :: binary()
  @type attribute :: atom()
  @type value :: binary()
  @type timestamp :: non_neg_integer()

  @type entity :: %{required(:id) => entity_id(), optional(attribute()) => value()}

  @doc """
  Returns a map identified by `entity_id` in `namespace`.

      fetch(:filesystem, "rand-id")
      #=> {:ok, %{id: "rand-id", type: "s3", bucket_url: "/...", secret: "abc", access_key: "xyz"}}

  """
  @callback fetch(namespace(), entity_id()) :: {:ok, entity()} | :error

  @doc """
  Returns all values in namespace.

      all(:filesystem)
      [%{id: "rand-id", type: "s3", bucket_url: "/...", secret: "abc", access_key: "xyz"}]

  """
  @callback all(namespace()) :: [entity()]

  @doc """
  Inserts given list of attribute-value paris to a entity belonging to specified namespace.
  """
  @callback insert(namespace(), entity_id(), [{attribute(), value()}]) :: :ok

  @doc """
  Deletes an entity of given id from given namespace.
  """
  @callback delete(namespace(), entity_id()) :: :ok

  @spec current() :: module()
  def current(), do: Application.fetch_env!(:livebook, :storage)
end
