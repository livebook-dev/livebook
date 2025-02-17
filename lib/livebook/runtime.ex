defprotocol Livebook.Runtime do
  # This protocol defines an interface for code evaluation backends.
  #
  # Usually a runtime involves a set of processes responsible for
  # evaluation, which could be running on a different node, however
  # the protocol does not require that.
  #
  # ## Files
  #
  # The runtime can request access to notebook files by sending a
  # request:
  #
  #   * `{:runtime_file_entry_path_request, reply_to, name}`
  #
  # to which the runtime owner is supposed to reply with
  # `{:runtime_file_entry_path_reply, reply}` where `reply` is either
  # `{:ok, path}` or `{:error, message | :forbidden}` if accessing the
  # file fails. Note that `path` should be accessible within the runtime
  # and can be obtained using `transfer_file/4`.
  #
  # Similarly the runtime can request details about the file source:
  #
  #   * `{:runtime_file_entry_spec_request, reply_to, name}`
  #
  # Instead of a path, the owner replies with a details map.
  #
  # ## Apps
  #
  # The runtime may be used to run Livebook apps and can request app
  # information by sending a request:
  #
  #   * `{:runtime_app_info_request, reply_to}`
  #
  # The owner replies with `{:runtime_app_info_reply, reply}`, where
  # reply is `{:ok, info}` and `info` is a details map.

  @typedoc """
  A language accepted for code evaluation.
  """
  @type language :: :elixir | :erlang | :python | :"pyproject.toml"

  @typedoc """
  An arbitrary term identifying an evaluation container.

  A container is an abstraction of an isolated group of evaluations.
  Containers are mostly independent and therefore can be evaluated
  concurrently (if possible).

  Note that every evaluation can use the resulting binding and env
  of any previous evaluation, even from a different container.
  """
  @type container_ref :: term()

  @typedoc """
  An arbitrary term identifying an evaluation.
  """
  @type evaluation_ref :: term()

  @typedoc """
  A pair identifying evaluation together with its container.
  """
  @type locator :: {container_ref(), evaluation_ref()}

  @typedoc """
  A sequence of locators representing a multi-stage evaluation.

  The evaluation locators should be ordered from most recent to oldest.
  """
  @type parent_locators :: list(locator())

  @typedoc """
  An output emitted during evaluation or as the final result.
  """
  @type output ::
          ignored_output()
          | terminal_text_output()
          | plain_text_output()
          | markdown_output()
          | image_output()
          | js_output()
          | frame_output()
          | frame_update_output()
          | tabs_output()
          | grid_output()
          | input_output()
          | control_output()
          | error_output()

  @typedoc """
  An empty output that should be ignored whenever encountered.
  """
  @type ignored_output :: %{type: :ignored}

  @typedoc ~S"""
  Terminal text content.

  Supports ANSI escape codes and overwriting lines with `\r`.

  Adjacent outputs with `:chunk` set to `true` are merged and rendered
  as a whole.
  """
  @type terminal_text_output :: %{type: :terminal_text, text: String.t(), chunk: boolean()}

  @typedoc """
  Plain text content.

  Adjacent outputs with `:chunk` set to `true` are merged and rendered
  as a whole.

  Similar to `t:markdown/0`, but with no special markup.
  """
  @type plain_text_output :: %{
          type: :plain_text,
          text: String.t(),
          chunk: boolean(),
          style: [{:color | :font_weight | :font_size, String.Chars.t()}]
        }

  @typedoc """
  Markdown content.

  Adjacent outputs with `:chunk` set to `true` are merged and rendered
  as a whole.
  """
  @type markdown_output :: %{type: :markdown, text: String.t(), chunk: boolean()}

  @typedoc """
  A raw image in the given format.

  ## Pixel data

  Note that a special `image/x-pixel` MIME type is supported. The
  binary consists of the following consecutive parts:

    * height - 32 bits (unsigned big-endian integer)
    * width - 32 bits (unsigned big-endian integer)
    * channels - 8 bits (unsigned integer)
    * data - pixel data in HWC order

  Pixel data consists of 8-bit unsigned integers. The number of channels
  can be either: 1 (grayscale), 2 (grayscale + alpha), 3 (RGB), or 4
  (RGB + alpha).
  """
  @type image_output :: %{type: :image, content: binary(), mime_type: String.t()}

  @typedoc """
  JavaScript powered output with dynamic data and events.

  See `Kino.JS` and `Kino.JS.Live` for more details.

  ## Export

  The `:export` specifies whether the given output supports persistence.
  When enabled, the JS view server should handle the following message:

      {:export, pid(), info :: %{ref: ref()}}

  And reply with:

      {:export_reply, export_result, info :: %{ref: ref()}}

  Where `export_result` is a tuple `{info_string, payload}`.
  `info_string` is used as the info string for the Markdown code block,
  while `payload` is its content. Payload can be either a string,
  otherwise it is serialized into JSON.
  """
  @type js_output() :: %{
          type: :js,
          js_view: js_view(),
          export: boolean()
        }

  @typedoc """
  A JavaScript view definition.

  JS view is a component rendered on the client side and possibly
  interacting with a server process within the runtime.

    * `:ref` - unique identifier

    * `:pid` - the server process holding the data and handling
      interactions

  ## Assets

  The `:assets` map includes information about the relevant files.

      * `:archive_path` - an absolute path to a `.tar.gz` archive with
        all the assets

      * `:hash` - a checksum of all assets in the archive

      * `:js_path` - a relative asset path pointing to the JavaScript
        entrypoint module

      * `:cdn_url` - an absolute URL to a CDN directory where the asset
        files can be accessed from. Note that this URL is not guaranteed
        to be accessible, since it may be pointing to a private package

  ## Communication protocol

  A client process should connect to the server process by sending:

      {:connect, pid(), info :: %{ref: ref(), origin: term()}}

  And expect the following reply:

      {:connect_reply, payload, info :: %{ref: ref()}}

  The server process may then keep sending one of the following events:

      {:event, event :: String.t(), payload, info :: %{ref: ref()}}

  The client process may keep sending one of the following events:

      {:event, event :: String.t(), payload, info :: %{ref: ref(), origin: term()}}

  The client can also send a ping message:

      {:ping, pid(), metadata :: term(), info :: %{ref: ref()}}

  And the server should respond with:

      {:pong, metadata :: term(), info :: %{ref: ref()}}

  """
  @type js_view :: %{
          ref: ref(),
          pid: Process.dest(),
          assets: %{
            archive_path: String.t(),
            hash: String.t(),
            js_path: String.t(),
            cdn_url: String.t() | nil
          }
        }

  @typedoc """
  An area to dynamically put outputs into.

  This output includes the initial outputs and can be later updated
  by sending `t:frame_update_output/0` outputs.

  The outputs order is always reversed, that is, most recent outputs
  are at the top of the stack.
  """
  @type frame_output :: %{
          type: :frame,
          ref: frame_ref(),
          outputs: list(output()),
          placeholder: boolean()
        }

  @typedoc """
  An output emitted to update and existing `t:frame_output/0`.
  """
  @type frame_update_output :: %{
          type: :frame_update,
          ref: frame_ref(),
          update: {:replace, list(output())} | {:append, list(output())}
        }

  @type frame_ref :: String.t()

  @typedoc """
  Multiple outputs arranged into tabs.
  """
  @type tabs_output :: %{type: :tabs, outputs: list(t()), labels: list(String.t())}

  @typedoc """
  Multiple outputs arranged in a grid.
  """
  @type grid_output :: %{
          type: :grid,
          outputs: list(t()),
          columns: pos_integer() | tuple(),
          gap: non_neg_integer(),
          max_height: pos_integer() | nil,
          boxed: boolean()
        }

  @typedoc """
  An input field.

    * `:ref` - a unique identifier

    * `:id` - a persistent input identifier, the same on every
      reevaluation

    * `:default` - the initial input value

    * `:destination` - the process to send event messages to

    * `:attrs` - input-specific attributes. The required fields are
      `:type` and `:default`

  """
  @type input_output :: %{
          type: :input,
          ref: ref(),
          id: input_id(),
          destination: Process.dest(),
          attrs: input_attrs()
        }

  @type input_id :: String.t()

  @type input_attrs ::
          %{
            type: :text,
            default: String.t(),
            label: String.t(),
            debounce: :blur | non_neg_integer()
          }
          | %{
              type: :textarea,
              default: String.t(),
              label: String.t(),
              debounce: :blur | non_neg_integer(),
              monospace: boolean()
            }
          | %{
              type: :password,
              default: String.t(),
              label: String.t(),
              debounce: :blur | non_neg_integer()
            }
          | %{
              type: :number,
              default: number() | nil,
              label: String.t(),
              debounce: :blur | non_neg_integer()
            }
          | %{
              type: :url,
              default: String.t() | nil,
              label: String.t(),
              debounce: :blur | non_neg_integer()
            }
          | %{
              type: :select,
              default: term(),
              label: String.t(),
              options: list({value :: term(), label :: String.t()})
            }
          | %{
              type: :checkbox,
              default: boolean(),
              label: String.t()
            }
          | %{
              type: :range,
              default: number(),
              label: String.t(),
              debounce: non_neg_integer(),
              min: number(),
              max: number(),
              step: number()
            }
          | %{
              type: :utc_datetime,
              default: NaiveDateTime.t() | nil,
              label: String.t(),
              min: NaiveDateTime.t() | nil,
              max: NaiveDateTime.t() | nil
            }
          | %{
              type: :utc_time,
              default: Time.t() | nil,
              label: String.t(),
              min: Time.t() | nil,
              max: Time.t() | nil
            }
          | %{
              type: :date,
              default: Date.t(),
              label: String.t(),
              min: Date.t(),
              max: Date.t()
            }
          | %{
              type: :color,
              default: String.t(),
              label: String.t(),
              debounce: :blur | non_neg_integer()
            }
          | %{
              type: :image,
              default: nil,
              label: String.t(),
              format: :rgb | :png | :jpeg,
              size: {pos_integer(), pos_integer()} | nil,
              fit: :match | :contain | :pad | :crop
            }
          | %{
              type: :audio,
              default: nil,
              label: String.t(),
              format: :pcm_f32 | :wav,
              sampling_rate: pos_integer()
            }
          | %{
              type: :file,
              default: nil,
              label: String.t(),
              accept: list(String.t()) | :any
            }

  @typedoc """
  A control widget.

    * `:ref` - a unique identifier

    * `:destination` - the process to send event messages to

    * `:attrs` - control-specific attributes. The only required field
      is `:type`

  ## Events

  All control events are sent to `:destination` as `{:event, id, info}`,
  where info is a map including additional details. In particular, it
  always includes `:origin`, which is an opaque identifier of the client
  that triggered the event.
  """
  @type control_output :: %{
          type: :control,
          ref: ref(),
          destination: Process.dest(),
          attrs: control_attrs()
        }

  @type control_attrs ::
          %{
            type: :keyboard,
            events: list(:keyup | :keydown | :status),
            default_handlers: :off | :on | :disable_only
          }
          | %{
              type: :button,
              label: String.t()
            }
          | %{
              type: :form,
              fields: list({field :: atom(), input_output() | nil}),
              submit: String.t() | nil,
              # Currently we always use true, but we can support
              # other tracking modes in the future
              report_changes: %{(field :: atom()) => true},
              reset_on_submit: list(field :: atom())
            }

  @type ref :: String.t()

  @typedoc """
  Error content, usually representing evaluation failure.

  The `:context` field is used by Livebook to suggest a way to fix
  the error.
  """
  @type error_output :: %{
          type: :error,
          message: String.t(),
          context:
            {:missing_secret, name :: String.t()}
            | {:interrupt, variant :: :normal | :error, message :: String.t()}
            | {:file_entry_forbidden, name :: String.t()}
            | :dependencies
            | nil
        }

  @typedoc """
  Additional information about a completed evaluation.

  ## Identifiers

  When possible, the metadata may include a list of identifiers (such
  as variables, modules, imports) used during evaluation, and a list
  of identifiers defined along with the version (such as a hash digest
  of the underlying value). With this information, Livebook can track
  dependencies between evaluations and avoids unnecessary reevaluations.
  """
  @type evaluation_response_metadata :: %{
          interrupted: boolean(),
          errored: boolean(),
          evaluation_time_ms: non_neg_integer(),
          code_markers: list(code_marker()),
          memory_usage: runtime_memory(),
          identifiers_used: list(identifier :: term()) | :unknown,
          identifiers_defined: %{(identifier :: term()) => version :: term()},
          identifier_definitions:
            list(%{label: String.t(), file: String.t(), line: pos_integer()})
        }

  @typedoc """
  Includes information about a running or finished doctest.

  Failed doctests have additional details formatted as a string.
  """
  @type doctest_report ::
          %{
            status: :running | :success,
            line: pos_integer(),
            column: pos_integer()
          }
          | %{
              status: :failed,
              line: pos_integer(),
              column: pos_integer(),
              end_line: pos_integer(),
              details: String.t()
            }

  @typedoc """
  Recognised intellisense request.
  """
  @type intellisense_request ::
          completion_request()
          | details_request()
          | signature_request()
          | format_request()

  @typedoc """
  Expected intellisense response.

  Responding with `nil` indicates there is no relevant reply and
  effectively aborts the request, so it's suitable for error cases.
  """
  @type intellisense_response ::
          nil
          | completion_response()
          | details_response()
          | signature_response()
          | format_response()

  @typedoc """
  Looks up a list of identifiers that are suitable code completions
  for the given hint.
  """
  @type completion_request :: {:completion, hint :: String.t()}

  @type completion_response :: %{
          items: list(completion_item())
        }

  @type completion_item :: %{
          label: String.t(),
          kind: completion_item_kind(),
          documentation: String.t() | nil,
          insert_text: String.t()
        }

  @type completion_item_kind ::
          :function | :module | :struct | :interface | :type | :variable | :field | :keyword

  @typedoc """
  Looks up more details about an identifier found in `column` in
  `line`.
  """
  @type details_request :: {:details, line :: String.t(), column :: pos_integer()}

  @type details_response :: %{
          range: %{
            from: non_neg_integer(),
            to: non_neg_integer()
          },
          contents: list(String.t()),
          definition: %{file: String.t(), line: pos_integer()} | nil
        }

  @typedoc """
  Looks up a list of function signatures matching the given hint.

  The resulting information includes current position in the argument
  list.
  """
  @type signature_request :: {:signature, hint :: String.t()}

  @type signature_response :: %{
          active_argument: non_neg_integer(),
          items: list(signature_item())
        }

  @type signature_item :: %{
          signature: String.t(),
          arguments: list(String.t())
        }

  @typedoc """
  Formats the given code snippet.
  """
  @type format_request :: {:format, code :: String.t()}

  @type format_response :: %{
          code: String.t() | nil,
          code_markers: list(code_marker())
        }

  @typedoc """
  A descriptive error or warning pointing to a specific line in the code.
  """
  @type code_marker :: %{
          line: pos_integer(),
          description: String.t(),
          severity: :error | :warning
        }

  @typedoc """
  A detailed runtime memory usage.

  The runtime may periodically send memory usage updates as

    * `{:runtime_memory_usage, runtime_memory()}`
  """
  @type runtime_memory :: %{
          atom: size_in_bytes(),
          binary: size_in_bytes(),
          code: size_in_bytes(),
          ets: size_in_bytes(),
          other: size_in_bytes(),
          processes: size_in_bytes(),
          total: size_in_bytes()
        }

  @type size_in_bytes :: non_neg_integer()

  @typedoc """
  An information about a smart cell kind.

  The `kind` attribute is an opaque identifier.

  Whenever new smart cells become available the runtime should send
  the updated list as

    * `{:runtime_smart_cell_definitions, list(smart_cell_definition())}`

  Additionally, the runtime may report extra definitions that require
  installing external packages, as described by `:requirement_presets`.
  Also see `add_dependencies/3`.
  """
  @type smart_cell_definition :: %{
          kind: String.t(),
          name: String.t(),
          requirement_presets:
            list(%{
              name: String.t(),
              packages: list(package())
            })
        }

  @type package :: %{name: String.t(), dependency: dependency()}

  @type dependency :: term()

  @type search_packages_response :: {:ok, list(package_details())} | {:error, String.t()}

  @type package_details :: %{
          name: String.t(),
          version: String.t(),
          description: String.t() | nil,
          url: String.t() | nil,
          dependency: dependency()
        }

  @typedoc """
  An information about a predefined code snippets.
  """
  @type snippet_definition :: example_snippet_definition() | file_action_snippet_definition()

  @typedoc """
  Code snippet with fixed source, serving as an example or boilerplate.
  """
  @type example_snippet_definition :: %{
          type: :example,
          name: String.t(),
          icon: String.t(),
          variants:
            list(%{
              name: String.t(),
              source: String.t(),
              packages: list(package())
            })
        }

  @typedoc """
  Code snippet for acting on files of the given type.

  The action is applicable to files matching any of the specified types,
  where a type can be either:

    * specific MIME type, like `text/csv`
    * MIME type family, like `image/*`
    * file extension, like `.csv`

  The source is expected to include `{{NAME}}`, which is replaced with
  the actual file name.
  """
  @type file_action_snippet_definition :: %{
          type: :file_action,
          file_types: :any | list(String.t()),
          description: String.t(),
          source: String.t(),
          packages: list(package())
        }

  @type smart_cell_ref :: String.t()

  @type smart_cell_attrs :: map()

  @typedoc """
  Marks a part of smart cell source.

  Both the offset ans size are expressed in bytes.
  """
  @type chunk :: {offset :: non_neg_integer(), size :: non_neg_integer()}

  @type chunks :: list(chunks())

  @typedoc """
  Smart cell editor configuration.
  """
  @type editor :: %{
          language: String.t() | nil,
          placement: :bottom | :top,
          source: String.t(),
          intellisense_node: {atom(), atom()} | nil,
          visible: boolean()
        }

  @typedoc """
  An opaque file reference.

  Such reference can be obtained from a file input, for example.

  The runtime may ask for the file by sending a request:

    * `{:runtime_file_path_request, reply_to, file_ref}`

  to which the runtime owner is supposed to reply with
  `{:runtime_file_path_reply, reply}` where `reply` is either
  `{:ok, path}` or `:error` if no matching file can be found. Note
  that `path` should be accessible within the runtime and can be
  obtained using `transfer_file/4`.
  """
  @type file_ref :: {:file, id :: String.t()}

  @typedoc """
  A state that can optionally be passed from one runtime to another.

  To report a new transition state, the runtime may send:

      {:runtime_transition_state, transition_state()}

  The runtime owner can then use `restore_transient_state/2` when
  starting another instance of this runtime.

  The state should be considered complementary, it is not guaranteed
  that any future runtime will receive it. Therefore, a transient state
  should never point to resources with the expectation that a future
  runtime will clean them. One valid use case is for the transient state
  to point to some global cache, that is not managed by the runtime
  itself.
  """
  @type transient_state :: %{atom() => term()}

  @typedoc """
  An identifier representing a client process.

  A client is a connected user that interacts with the runtime outputs.
  """
  @type client_id :: String.t()

  @typedoc """
  User information about a particular client.
  """
  @type user_info :: %{
          id: String.t(),
          name: String.t() | nil,
          email: String.t() | nil,
          source: atom()
        }

  @type proxy_handler_spec :: {module :: module(), function :: atom(), args :: list()}

  @typedoc """
  An information about Elixir nodes that the runtime is connected to.

  Whenever the node list change, the runtime should send an updated
  list as:

    * `{:runtime_connected_nodes, connected_nodes()}`

  """
  @type connected_nodes :: list(node())

  @doc """
  Returns relevant information about the runtime.

  Every runtime is expected to have an item with the `"Type"` label.
  """
  @spec describe(t()) :: list({label :: String.t(), String.t()})
  def describe(runtime)

  @doc """
  Asynchronously initializes the given runtime.

  The initialization should take care of starting any OS processes
  necessary, setting up resources and communication.

  Since the initialization may take time, it should always happen in
  a separate process. This function should return the `pid` of that
  process. Once the initialization is finished, the process should
  send the following message to the caller:

    * `{:runtime_connect_done, pid, {:ok, runtime} | {:error, message}}`

  The `runtime` should be the struct updated with all information
  necessary for further communication.

  In case the initialization is a particularly involved, the process
  may send updates to the caller:

      * `{:runtime_connect_info, pid, info}`

  Where `info` is a few word text describing the current initialization
  step.

  If the caller decides to abort the initialization, they can forecefully
  kill the process. The runtime resources should already be tolerant
  to abrupt Livebook termination and autodestroy through monitoring
  and timeouts. However, when the initialization process gets killed,
  it may be desirable to eagerly remove the resources it has already
  allocated, which can be achieved with an additional watcher process.
  """
  @spec connect(t()) :: pid()
  def connect(runtime)

  @doc """
  Sets the caller as the runtime owner.

  The runtime owner is the target for most of the runtime messages
  and the runtime lifetime is tied to the owner.

  It is advised for each runtime to have a leading process that is
  coupled to the lifetime of the underlying runtime resources. In
  such case the `take_ownership/2` function may start monitoring this
  process and return the monitor reference. This way the owner is
  notified when the runtime goes down by listening to the :DOWN
  message with that reference.

  ## Options

    * `:runtime_broadcast_to` - the process to send runtime broadcast
      events to. Defaults to the owner

  """
  @spec take_ownership(t(), keyword()) :: reference()
  def take_ownership(runtime, opts \\ [])

  @doc """
  Synchronously disconnects the runtime and cleans up the underlying
  resources.
  """
  @spec disconnect(t()) :: :ok
  def disconnect(runtime)

  @doc """
  Returns a fresh runtime of the same type with the same configuration.

  This function is expected to only modify the runtime struct, unsetting
  any information added by `connect/1`. It should not have any side
  effects.
  """
  @spec duplicate(t()) :: t()
  def duplicate(runtime)

  @doc """
  Asynchronously parses and evaluates the given code.

  The given `locator` identifies the container where the code should
  be evaluated as well as the evaluation reference to store the
  resulting context under.

  Additionally, `parent_locators` points to a sequence of previous
  evaluations to be used as the starting point of this evaluation.

  ## Communication

  During evaluation a number of messages may be sent to the runtime
  owner. All captured outputs have the form:

    * `{:runtime_evaluation_output, evaluation_ref, output}`

  When the evaluation completes, the resulting output and metadata
  is sent as:

    * `{:runtime_evaluation_response, evaluation_ref, output, metadata}`

  Outputs may include input fields. The evaluation may then request
  the current value of a previously rendered input by sending

    * `{:runtime_evaluation_input_request, evaluation_ref, reply_to, input_id}`

  to the  runtime owner who is supposed to reply with
  `{:runtime_evaluation_input_reply, reply}` where `reply` is either
  `{:ok, value}` or `:error` if no matching input can be found.

  If the evaluation state within a container is lost (for example when
  a process goes down), the runtime may send

    * `{:runtime_container_down, container_ref, message}`

  to notify the owner.

  ### Doctests

  If the cell includes doctests, the runtime can evaluate them and
  send reports as a message:

    * `{:runtime_doctest_report, evaluation_ref, doctest_report}`

  ## Options

    * `:file` - the file considered as the source during evaluation.
      This information is relevant for errors formatting and imparts
      the value of `__DIR__`

    * `:smart_cell_ref` - a reference of the smart cell which code is
      to be evaluated, if applicable

    * `:disable_dependencies_cache` - disables dependencies cache, so
      they are fetched and compiled from scratch

  """
  @spec evaluate_code(t(), language(), String.t(), locator(), parent_locators(), keyword()) :: :ok
  def evaluate_code(runtime, language, code, locator, parent_locators, opts \\ [])

  @doc """
  Disposes of an evaluation identified by the given locator.

  This can be used to cleanup resources related to an old evaluation
  if it is no longer needed.
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

  This part of runtime functionality is used to provide language-
  and context-specific intellisense features in the text editor.

  The response is sent to the `send_to` process as

    * `{:runtime_intellisense_response, ref, request, response}`.

  The given `parent_locators` identifies a sequence of evaluations
  that may be used as the context when resolving the request (if relevant).
  """
  @spec handle_intellisense(
          t(),
          pid(),
          intellisense_request(),
          parent_locators(),
          {atom(), atom()} | nil
        ) :: reference()
  def handle_intellisense(runtime, send_to, request, parent_locators, node)

  @doc """
  Reads file at the given absolute path within the runtime file system.
  """
  @spec read_file(t(), String.t()) :: {:ok, binary()} | {:error, String.t()}
  def read_file(runtime, path)

  @doc """
  Transfers file at `path` to the runtime.

  This operation is asynchronous and `callback` is called with the
  path of the transferred file (on the runtime host) once the transfer
  is complete.

  If the runtime is on the same host as the caller, the implementation
  may simply use `path`.
  """
  @spec transfer_file(t(), String.t(), String.t(), (path :: String.t() | nil -> any())) :: :ok
  def transfer_file(runtime, path, file_id, callback)

  @doc """
  Updates the id by which the file is referenced.
  """
  @spec relabel_file(t(), String.t(), String.t()) :: :ok
  def relabel_file(runtime, file_id, new_file_id)

  @doc """
  Cleans up resources allocated with `transfer_file/4`, if any.
  """
  @spec revoke_file(t(), String.t()) :: :ok
  def revoke_file(runtime, file_id)

  @doc """
  Starts a smart cell of the given kind.

  `kind` must point to an available `t:smart_cell_definition/0`, which
  was reported by the runtime. The cell gets initialized with `attrs`,
  which represent the persisted cell state and determine the current
  version of the generated source code. The given `ref` is used to
  identify the cell.

  The cell may depend on evaluation context to provide a better user
  experience, for instance it may suggest relevant variable names.
  Similarly to `evaluate_code/5`, `parent_locators` must be specified
  pointing to the sequence of evaluations to use as the context. When
  the sequence changes, it can be updated with `set_smart_cell_parent_locators/3`.

  Once the cell starts, the runtime sends the following message

    * `{:runtime_smart_cell_started, ref, %{js_view: js_view(), source: String.t(), chunks: chunks() | nil, editor: editor() | nil}}`

  In case of an unexpected failure it should also send

    * `{:runtime_smart_cell_down, ref}`

  ## Communication

  Apart from the regular JS view communication, the cell sends updates
  to the runtime owner whenever attrs and the generated source code
  change.

    * `{:runtime_smart_cell_update, ref, attrs, source, %{reevaluate: boolean(), chunks: chunks() | nil}}`

  The attrs are persisted and may be used to restore the smart cell
  state later. Note that for persistence they get serialized and
  deserialized as JSON.

  When the smart cell editor is enabled, the runtime owner sends the
  new editor source whenever it changes as:

    * `{:editor_source, source :: String.t()}`

  The cell can also update some of the editor configuration or source
  by sending:

    * `{:runtime_smart_cell_editor_update, ref, %{optional(:source) => String.t(), optional(:intellisense_node) => {atom(), atom()} | nil}, optional(:visible) => boolean()}`

  """
  @spec start_smart_cell(
          t(),
          String.t(),
          smart_cell_ref(),
          smart_cell_attrs(),
          parent_locators()
        ) :: :ok
  def start_smart_cell(runtime, kind, ref, attrs, parent_locators)

  @doc """
  Updates the parent locator used by a smart cell as its context.

  See `start_smart_cell/5` for more details.
  """
  @spec set_smart_cell_parent_locators(t(), smart_cell_ref(), parent_locators()) :: :ok
  def set_smart_cell_parent_locators(runtime, ref, parent_locators)

  @doc """
  Stops smart cell identified by the given reference.
  """
  @spec stop_smart_cell(t(), smart_cell_ref()) :: :ok
  def stop_smart_cell(runtime, ref)

  @doc """
  Returns true if the given runtime by definition has only a specific
  set of dependencies.

  Note that if restarting the runtime allows for installing different
  dependencies, the dependencies are not considered fixed.

  When dependencies are fixed, the following functions are allowed to
  raise an implementation error: `add_dependencies/3`, `search_packages/3`.
  """
  @spec fixed_dependencies?(t()) :: boolean()
  def fixed_dependencies?(runtime)

  @doc """
  Updates the given source code to install the given dependencies.
  """
  @spec add_dependencies(t(), String.t(), list(dependency())) ::
          {:ok, String.t()} | {:error, String.t()}
  def add_dependencies(runtime, code, dependencies)

  @doc """
  Checks if the given dependencies are installed within the runtime.
  """
  @spec has_dependencies?(t(), list(dependency())) :: boolean()
  def has_dependencies?(runtime, dependencies)

  @doc """
  Returns a list of predefined code snippets.
  """
  @spec snippet_definitions(t()) :: list(snippet_definition())
  def snippet_definitions(runtime)

  @doc """
  Looks up packages matching the given search.

  The response is sent to the `send_to` process as

    * `{:runtime_search_packages_response, ref, response}`.

  """
  @spec search_packages(t(), pid(), String.t()) :: reference()
  def search_packages(runtime, send_to, search)

  @doc """
  Sets the given environment variables.
  """
  @spec put_system_envs(t(), list({String.t(), String.t()})) :: :ok
  def put_system_envs(runtime, envs)

  @doc """
  Unsets the given environment variables.
  """
  @spec delete_system_envs(t(), list(String.t())) :: :ok
  def delete_system_envs(runtime, names)

  @doc """
  Restores information from a past runtime.

  See `t:transient_state/0` for details.
  """
  @spec restore_transient_state(t(), transient_state()) :: :ok
  def restore_transient_state(runtime, transient_state)

  @doc """
  Notifies the runtime about connected clients.
  """
  @spec register_clients(t(), list(client_id())) :: :ok
  def register_clients(runtime, clients)

  @doc """
  Notifies the runtime about clients leaving.
  """
  @spec unregister_clients(t(), list(client_id())) :: :ok
  def unregister_clients(runtime, client_ids)

  @doc """
  Fetches information about a proxy request handler, if available.

  When the handler is available, this function returns MFA. In order
  to handle a connection, the caller should invoke the MFA, appending
  `conn` to the argument list, where `conn` is a `%Plug.Conn{}` struct
  for the specific request.

  Once done, the handler MFA should return the final `conn`.
  """
  @spec fetch_proxy_handler_spec(t()) :: {:ok, proxy_handler_spec()} | {:error, :not_found}
  def fetch_proxy_handler_spec(runtime)

  @doc """
  Asks the runtime to disconnect from the given connected node.

  The node should be one of `connected_nodes()` reported by the runtime
  earlier.
  """
  @spec disconnect_node(t(), node()) :: :ok
  def disconnect_node(runtime, node)
end
