defmodule Livebook.Runtime.Attached do
  @moduledoc false

  # A runtime backed by an Elixir node managed externally.
  #
  # Such node must be already started and available,
  # Livebook doesn't manage its lifetime in any way
  # and only loads/unloads the necessary elements.
  # The node can be an ordinary Elixir runtime,
  # a Mix project shell, a running release or anything else.

  defstruct [:node]

  @type t :: %__MODULE__{
          node: node()
        }

  @doc """
  Checks if the given node is available for use and initializes
  it with Livebook-specific modules and processes.
  """
  @spec init(node()) :: {:ok, t()} | {:error, :unreachable | :already_in_use}
  def init(node) do
    case Node.ping(node) do
      :pong ->
        case Livebook.Runtime.ErlDist.initialize(node) do
          :ok ->
            {:ok, %__MODULE__{node: node}}

          {:error, :already_in_use} ->
            {:error, :already_in_use}
        end

      :pang ->
        {:error, :unreachable}
    end
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.Attached do
  alias Livebook.Runtime.ErlDist

  def connect(runtime) do
    ErlDist.Manager.set_owner(runtime.node, self())
    Process.monitor({ErlDist.Manager, runtime.node})
  end

  def disconnect(runtime) do
    ErlDist.Manager.stop(runtime.node)
  end

  def evaluate_code(
        runtime,
        code,
        container_ref,
        evaluation_ref,
        prev_evaluation_ref,
        opts \\ []
      ) do
    ErlDist.Manager.evaluate_code(
      runtime.node,
      code,
      container_ref,
      evaluation_ref,
      prev_evaluation_ref,
      opts
    )
  end

  def forget_evaluation(runtime, container_ref, evaluation_ref) do
    ErlDist.Manager.forget_evaluation(runtime.node, container_ref, evaluation_ref)
  end

  def drop_container(runtime, container_ref) do
    ErlDist.Manager.drop_container(runtime.node, container_ref)
  end

  def request_completion_items(runtime, send_to, ref, hint, container_ref, evaluation_ref) do
    ErlDist.Manager.request_completion_items(
      runtime.node,
      send_to,
      ref,
      hint,
      container_ref,
      evaluation_ref
    )
  end
end
