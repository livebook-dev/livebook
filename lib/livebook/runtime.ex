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
  Recognised intellisense requests.
  """
  @type intellisense_request ::
          completion_request()
          | details_request()
          | signature_request()
          | format_request()

  @typedoc """
  Expected intellisense responses.

  Responding with `nil` indicates there is no relevant reply
  and effectively aborts the request, so it's suitable for
  error cases.
  """
  @type intellisense_response ::
          nil
          | completion_response()
          | details_response()
          | signature_response()
          | format_response()

  @typedoc """
  Looks up a list of identifiers that are suitable code
  completions for the given hint.
  """
  @type completion_request :: {:completion, hint :: String.t()}

  @type completion_response :: %{
          items: list(completion_item())
        }

  @type completion_item :: %{
          label: String.t(),
          kind: completion_item_kind(),
          detail: String.t() | nil,
          documentation: String.t() | nil,
          insert_text: String.t()
        }

  @type completion_item_kind ::
          :function | :module | :struct | :interface | :type | :variable | :field | :keyword

  @typedoc """
  Looks up more details about an identifier found in `column` in `line`.
  """
  @type details_request :: {:details, line :: String.t(), column :: pos_integer()}

  @type details_response :: %{
          range: %{
            from: non_neg_integer(),
            to: non_neg_integer()
          },
          contents: list(String.t())
        }

  @typedoc """
  Looks up a list of function signatures matching the given hint.

  The resulting information includes current position in the
  argument list.
  """
  @type signature_request :: {:signature, hint :: String.t()}

  @type signature_response :: %{
          active_argument: non_neg_integer(),
          signature_items: list(signature_item())
        }

  @type signature_item :: %{
          signature: String.t(),
          arguments: list(String.t()),
          documentation: String.t() | nil
        }

  @typedoc """
  Formats the given code snippet.
  """
  @type format_request :: {:format, code :: String.t()}

  @type format_response :: %{
          code: String.t() | nil,
          code_error: code_error() | nil
        }

  @type code_error :: %{line: pos_integer(), description: String.t()}

  @typedoc """
  The runtime memory usage for each type in bytes.

  The runtime may periodically send messages of type {:memory_usage, runtime_memory()}
  """
  @type runtime_memory :: %{
          atom: non_neg_integer(),
          binary: non_neg_integer(),
          code: non_neg_integer(),
          ets: non_neg_integer(),
          other: non_neg_integer(),
          processes: non_neg_integer(),
          total: non_neg_integer()
        }

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
  reference to store the resulting context under.

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
      are: `code_error`, `evaluation_time_ms` and `memory_usage`

  The output may include input fields. The evaluation may then
  request the current value of a previously rendered input by
  sending `{:evaluation_input, ref, reply_to, input_id}` to the
  runtime owner, which is supposed to reply with `{:evaluation_input_reply, reply}`
  where `reply` is either `{:ok, value}` or `:error` if no matching
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
  Asynchronously handles an intellisense request.

  This part of runtime functionality is used to provide
  language and context specific intellisense features in
  the text editor.

  The response is sent to the `send_to` process as
  `{:intellisense_response, ref, request, response}`.

  The given `locator` idenfities an evaluation that may be used
  as context when resolving the request (if relevant).
  """
  @spec handle_intellisense(t(), pid(), reference(), intellisense_request(), locator()) :: :ok
  def handle_intellisense(runtime, send_to, ref, request, locator)

  @doc """
  Synchronously starts a runtime of the same type with the
  same parameters.
  """
  @spec duplicate(Runtime.t()) :: {:ok, Runtime.t()} | {:error, String.t()}
  def duplicate(runtime)

  @doc """
  Returns true if the given runtime is self-contained.

  A standalone runtime always starts fresh and frees all
  resources when terminated. This may not be the case for
  for runtimes that connect to an external running system
  and use it for code evaluation.
  """
  @spec standalone?(Runtime.t()) :: boolean()
  def standalone?(runtime)

  @doc """
  Reads file at the given absolute path within the runtime
  file system.
  """
  @spec read_file(Runtime.t(), String.t()) :: {:ok, binary()} | {:error, String.t()}
  def read_file(runtime, path)
end
