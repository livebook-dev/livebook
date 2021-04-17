defmodule Livebook.Runtime.Local do
  @moduledoc false

  # A runtime backed by the node running Livebook.

  defstruct [:node]

  @type t :: %__MODULE__{node: node()}

  @doc """
  Checks if the given node is available for use and initializes
  it with Livebook-specific modules and processes.
  """
  @spec init() :: {:ok, t()}
  def init() do
    _ = Livebook.Runtime.ErlDist.LocalManager.start()
    {:ok, %__MODULE__{node: node()}}
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.Local do
  alias Livebook.Runtime.ErlDist

  def connect(_runtime) do
    ErlDist.LocalManager.set_owner(self())
    :ok
  end

  def disconnect(_runtime) do
    :ok
  end

  def evaluate_code(
        _runtime,
        code,
        container_ref,
        evaluation_ref,
        prev_evaluation_ref \\ :initial,
        opts \\ []
      ) do
    ErlDist.LocalManager.evaluate_code(
      code,
      container_ref,
      evaluation_ref,
      prev_evaluation_ref,
      opts
    )
  end

  def forget_evaluation(_runtime, container_ref, evaluation_ref) do
    ErlDist.LocalManager.forget_evaluation(container_ref, evaluation_ref)
  end

  def drop_container(_runtime, container_ref) do
    ErlDist.LocalManager.drop_container(container_ref)
  end

  def request_completion_items(_runtime, send_to, ref, hint, container_ref, evaluation_ref) do
    ErlDist.LocalManager.request_completion_items(
      send_to,
      ref,
      hint,
      container_ref,
      evaluation_ref
    )
  end
end
