defprotocol Livebook.Runtime do
  @moduledoc false

  # This protocol defines an interface for evaluation backends.
  #
  # Usually a runtime involves a set of processes responsible
  # for evaluation, which could be running on a different node,
  # however the protocol does not require that.

  @typedoc """
  A term used to identify evaluation or container.
  """
  @type ref :: term()

  @typedoc """
  A single completion result.
  """
  @type completion_item :: %{
          label: String.t(),
          kind: completion_item_kind(),
          detail: String.t() | nil,
          documentation: String.t() | nil,
          insert_text: String.t()
        }

  @type completion_item_kind :: :function | :module | :type | :variable | :field

  @doc """
  Sets the caller as runtime owner.

  The runtime most likely has some kind of leading process,
  this method starts monitoring it and returns the monitor reference,
  so the caller knows if the runtime is down by listening to a :DOWN message.
  """
  @spec connect(t()) :: reference()
  def connect(runtime)

  @doc """
  Disconnects the current owner from runtime.

  This should cleanup the underlying node/processes.
  """
  @spec disconnect(t()) :: :ok
  def disconnect(runtime)

  @doc """
  Asynchronously parses and evaluates the given code.

  Container isolates a group of evaluations. Every evaluation can use previous
  evaluation's environment and bindings, as long as they belong to the same container.

  Evaluation outputs are send to the connected runtime owner.
  The messages should be of the form:

    * `{:evaluation_output, ref, output}` - output captured during evaluation
    * `{:evaluation_response, ref, output}` - final result of the evaluation

  The evaluation may request user input by sending `{:evaluation_input, ref, reply_to, prompt}`
  to the runtime owner, who is supposed to reply with `{:evaluation_input_reply, reply}`
  with `reply` being either `{:ok, input}` or `:error` if no matching input can be found.

  If the evaluation state within a container is lost (e.g. a process goes down),
  the runtime can send `{:container_down, container_ref, message}` to notify the owner.

  ## Options

    * `:file` - file to which the evaluated code belongs. Most importantly,
      this has an impact on the value of `__DIR__`.
  """
  @spec evaluate_code(t(), String.t(), ref(), ref(), ref() | nil, keyword()) :: :ok
  def evaluate_code(runtime, code, container_ref, evaluation_ref, prev_evaluation_ref, opts \\ [])

  @doc """
  Disposes of evaluation identified by the given ref.

  This should be used to cleanup resources related to old evaluation if no longer needed.
  """
  @spec forget_evaluation(t(), ref(), ref()) :: :ok
  def forget_evaluation(runtime, container_ref, evaluation_ref)

  @doc """
  Disposes of evaluation container identified by the given ref.

  This should be used to cleanup resources keeping track
  of the container and contained evaluations.
  """
  @spec drop_container(t(), ref()) :: :ok
  def drop_container(runtime, container_ref)

  @doc """
  Asynchronously finds completion items matching the given `hint` text.

  The given `{container_ref, evaluation_ref}` pair idenfities an evaluation,
  which bindings and environment are used to provide a more relevant completion results.
  If there's no appropriate evaluation, `nil` refs can be provided.

  Completion response is sent to the `send_to` process as `{:completion_response, ref, items}`,
  where `items` is a list of `Livebook.Runtime.completion_item()`.
  """
  @spec request_completion_items(t(), pid(), term(), String.t(), ref() | nil, ref() | nil) :: :ok
  def request_completion_items(
        runtime,
        send_to,
        ref,
        hint,
        container_ref,
        evaluation_ref
      )
end
