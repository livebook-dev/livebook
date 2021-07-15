defprotocol Livebook.Runtime do
  @moduledoc false

  # This protocol defines an interface for evaluation backends.
  #
  # Usually a runtime involves a set of processes responsible
  # for evaluation, which could be running on a different node,
  # however the protocol does not require that.

  @typedoc """
  An arbitrary term identifying an evaluation container.

  A container is an abstraction of an isolated group of evaluations.
  Containers are mostly independent and consequently can be evaluated
  concurrently if possible.

  Note that every evaluation can use the resulting environment
  and bindings of any previous evaluation, even from a different
  container.
  """
  @type container_ref :: term()

  @typedoc """
  An arbitrary term identifying an evaluation.
  """
  @type evaluation_ref :: term()

  @typedoc """
  A pair identifying evaluation together with its container.
  """
  @type locator :: {container_ref(), evaluation_ref() | nil}

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

  It's advised for each runtime to have a leading process
  that is coupled to the lifetime of the underlying runtime
  resources. In this case the `connect` function may start
  monitoring that process and return the monitor reference.
  This way the caller is notified when the runtime goes down
  by listening to the :DOWN message.
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

  The given `locator` identifies the container where
  the code should be evaluated as well as the evaluation
  reference to store the resulting contxt under.

  Additionally, `prev_locator` points to a previous
  evaluation to be used as the starting point of this
  evaluation. If not applicable, the previous evaluation
  reference may be specified as `nil`.

  ## Communication

  Evaluation outputs are sent to the connected runtime owner.
  The messages should be of the form:

    * `{:evaluation_output, ref, output}` - output captured
      during evaluation

    * `{:evaluation_response, ref, output, metadata}` - final
      result of the evaluation. Recognised metadata entries
      are: `evaluation_time_ms`

  The evaluation may request user input by sending
  `{:evaluation_input, ref, reply_to, prompt}` to the runtime owner,
  which is supposed to reply with `{:evaluation_input_reply, reply}`
  where `reply` is either `{:ok, input}` or `:error` if no matching
  input can be found.

  In all of the above `ref` is the evaluation reference.

  If the evaluation state within a container is lost (for example
  a process goes down), the runtime may send `{:container_down, container_ref, message}`
  to notify the owner.

  ## Options

    * `:file` - file to which the evaluated code belongs. Most importantly,
      this has an impact on the value of `__DIR__`.
  """
  @spec evaluate_code(t(), String.t(), locator(), locator(), keyword()) :: :ok
  def evaluate_code(runtime, code, locator, prev_locator, opts \\ [])

  @doc """
  Disposes of an evaluation identified by the given locator.

  This can be used to cleanup resources related to an old evaluation
  if no longer needed.
  """
  @spec forget_evaluation(t(), locator()) :: :ok
  def forget_evaluation(runtime, locator)

  @doc """
  Disposes of an evaluation container identified by the given ref.

  This should be used to cleanup resources keeping track of the
  container all of its evaluations.
  """
  @spec drop_container(t(), container_ref()) :: :ok
  def drop_container(runtime, container_ref)

  @doc """
  Asynchronously finds completion items matching the given `hint` text.

  The given `locator` idenfities an evaluation, which bindings
  and environment are used to provide a more relevant completion
  results. If there's no appropriate evaluation, `nil` refs can
  be provided.

  Completion response is sent to the `send_to` process as
  `{:completion_response, ref, items}`, where `items` is a
  list of `t:Livebook.Runtime.completion_item/0`.
  """
  @spec request_completion_items(t(), pid(), term(), String.t(), locator()) :: :ok
  def request_completion_items(runtime, send_to, completion_ref, hint, locator)

  @doc """
  Synchronously starts a runtime of the same type with the
  same parameters.
  """
  @spec duplicate(Runtime.t()) :: {:ok, Runtime.t()} | {:error, String.t()}
  def duplicate(runtime)
end
