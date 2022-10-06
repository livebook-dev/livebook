defmodule Livebook.Storage do
  @moduledoc """
  Behaviour defining an interface for storing arbitrary data in
  [Entity-Attribute-Value](https://en.wikipedia.org/wiki/Entity%E2%80%93attribute%E2%80%93value_model) fashion.
  """

  @type namespace :: atom()
  @type entity_id :: binary()
  @type attribute :: atom()
  @type value :: binary() | nil
  @type timestamp :: non_neg_integer()
  @type entity :: %{required(:id) => entity_id(), optional(attribute()) => value()}

  defmodule NotFoundError do
    @moduledoc false

    defexception [:id, :namespace, plug_status: 404]

    def message(%{namespace: namespace, id: id}) do
      "could not find entry in \"#{namespace}\" with ID #{inspect(id)}"
    end
  end

  @doc """
  Returns all values in namespace.

      all(:filesystem)
      [%{id: "rand-id", type: "s3", bucket_url: "/...", secret: "abc", access_key: "xyz"}]

  """
  @callback all(namespace()) :: [entity()]

  @doc """
  Delegate for `c:all/1`.
  """
  def all(namespace), do: current().all(namespace)

  @doc """
  Returns a map identified by `entity_id` in `namespace`.

      fetch(:filesystem, "rand-id")
      #=> {:ok, %{id: "rand-id", type: "s3", bucket_url: "/...", secret: "abc", access_key: "xyz"}}

  """
  @callback fetch(namespace(), entity_id()) :: {:ok, entity()} | :error

  @doc """
  Delegate for `c:fetch/2`.
  """
  def fetch(namespace, id), do: current().fetch(namespace, id)

  @doc """
  Raising delegate for `c:fetch/2`.
  """
  def fetch!(namespace, id) do
    case current().fetch(namespace, id) do
      {:ok, entity} -> entity
      :error -> raise NotFoundError, namespace: namespace, id: id
    end
  end

  @doc """
  Returns the value for a given `namespace`-`entity_id`-`attribute`.

      fetch_key(:filesystem, "rand-id", :type)
      #=> {:ok, "s3"}

  """
  @callback fetch_key(namespace(), entity_id(), attribute()) :: {:ok, value()} | :error

  @doc """
  Delegate for `c:fetch_key/3`.
  """
  def fetch_key(namespace, id, attribute), do: current().fetch_key(namespace, id, attribute)

  @doc """
  Inserts given list of attribute-value paris to a entity belonging to specified namespace.
  """
  @callback insert(namespace(), entity_id(), [{attribute(), value()}]) :: :ok

  @doc """
  Delegate for `c:insert/3`.
  """
  def insert(namespace, id, attributes), do: current().insert(namespace, id, attributes)

  @doc """
  Deletes an entity of given id from given namespace.
  """
  @callback delete(namespace(), entity_id()) :: :ok

  @doc """
  Delegate for `c:delete/2`.
  """
  def delete(namespace, id), do: current().delete(namespace, id)

  @doc """
  Deletes an attribute from given entity.
  """
  @callback delete_key(namespace(), entity_id(), attribute()) :: :ok

  @doc """
  Delegate for `c:delete_key/3`.
  """
  def delete_key(namespace, id, attribute), do: current().delete_key(namespace, id, attribute)

  @spec current() :: module()
  def current(), do: Application.fetch_env!(:livebook, :storage)
end
