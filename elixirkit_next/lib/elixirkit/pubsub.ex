defmodule ElixirKit.PubSub do
  @moduledoc """
  Publish/Subscribe server to exchange messages between Elixir and native code.

  PubSub is designed to be started alongside [`elixirkit::PubSub`] from a native app.

  Subscribes and broadcasts are always unidirectional, that is, an Elixir process will not receive
  their own broadcasts.

  See `start_link/1` for an example.

  [`elixirkit::PubSub`]: rs/elixirkit/struct.PubSub.html
  """

  use GenServer, restart: :temporary
  require Logger

  @type topic() :: String.t()

  @type message() :: binary()

  @doc """
  Starts PubSub and links it to the current process.

  ## Options

    * `:connect` - how to connect to the native side. Can be one of:

      * `:ignore` - don't start the server.

      * a URL - e.g. `"tcp://127.0.0.1:12345"`.

      [`elixirkit::elixir`] and friends set the `ELIXIRKIT_PUBSUB` environment variable,
      so a common pattern is `connect: System.get_env("ELIXIRKIT_PUBSUB") || :ignore`.

      [`elixirkit::elixir`]: rs/elixirkit/fn.elixir.html

    * `:on_exit` - a zero-arity function called when PubSub exits (e.g. when the native side
      disconnects).

      > #### Handling exit {: .warning}
      >
      > It is recommended to set `on_exit: fn -> System.stop() end` to cleanly shutdown the VM
      > when the native side exits abruptly.
      >
      > Alternatively, consider monitoring the `ElixirKit.PubSub` process and respond accordingly.
      >
      > Finally, you may instead set `{ElixirKit.PubSub, ..., significant: true}` in your
      > supervision tree and set your supervisor to
      > `auto_shutdown: :any_significant | :all_significant`.

  ## Examples

      defmodule Server do
        use GenServer

        @impl true
        def init(_) do
          ElixirKit.PubSub.subscribe("messages")
          ElixirKit.PubSub.broadcast("messages", "ready")
          {:ok, %{}}
        end

        @impl true
        def handle_info(message, state) do
          dbg(message)
          {:noreply, state}
        end
      end

      children = [
        {ElixirKit.PubSub,
         connect: System.fetch_env!("ELIXIRKIT_PUBSUB"),
         on_exit: fn -> System.stop() end},
        Server
      ]

      {:ok, _} = Supervisor.start_link(children, strategy: :one_for_one)

  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(options) do
    options =
      Keyword.validate!(options, [
        :connect,
        :listen,
        :on_exit,

        # TODO: :name is not documented, using multiple pubsubs just for testing for now.
        name: __MODULE__
      ])

    options =
      cond do
        options[:connect] && options[:listen] ->
          raise ArgumentError, "cannot set both :connect and :listen"

        options[:connect] ->
          options

        # TODO: :listen not documented, used just for testing for now.
        options[:listen] ->
          options

        true ->
          raise ArgumentError, "expected :connect option"
      end

    GenServer.start_link(__MODULE__, options, name: options[:name])
  end

  @doc """
  Returns a specification to start PubSub under a supervisor.

  ## Options

    * `:significant` - a boolean indicating if the child process should be
      considered significant with regard to automatic shutdown. Defaults to `false`.

  Remaining options are the same as in `start_link/1`.
  """
  @spec child_spec(keyword()) :: Supervisor.child_spec()
  def child_spec(options) do
    {significant, options} = Keyword.pop(options, :significant, false)
    options = Keyword.put_new(options, :name, __MODULE__)

    %{
      id: options[:name],
      start: {__MODULE__, :start_link, [options]},
      restart: :temporary,
      significant: significant
    }
  end

  @doc """
  Subscribes the caller to messages on the given topic from the Rust side.
  """
  @spec subscribe(topic()) :: :ok
  def subscribe(topic) do
    subscribe(__MODULE__, topic)
  end

  # TODO: Passing server is not documented, used just for testing for now.
  @doc false
  def subscribe(server, topic)
      when is_atom(server) and is_binary(topic) and byte_size(topic) < 256 do
    {:ok, _} = Registry.register(registry_name(server), topic, [])
    :ok
  end

  @doc false
  def url(server) when is_atom(server) do
    {:ok, url} = Registry.meta(registry_name(server), :url)
    url
  end

  @doc """
  Broadcasts a message on the given topic to the Rust side.
  """
  @spec broadcast(topic(), message()) :: :ok
  def broadcast(topic, message) do
    broadcast(__MODULE__, topic, message)
  end

  @doc false
  def broadcast(server, topic, message)
      when is_atom(server) and is_binary(topic) and byte_size(topic) < 256 and is_binary(message) do
    GenServer.cast(server, {:broadcast, topic, message})
  end

  @impl true
  def init(options) do
    registry = registry_name(options[:name])

    state = %{
      registry: registry,
      socket: nil
    }

    cond do
      connect = options[:connect] ->
        case connect do
          :ignore ->
            :ignore

          url ->
            with "tcp://127.0.0.1:" <> rest <- url,
                 {port, ""} = Integer.parse(rest) do
              case :gen_tcp.connect(~c"127.0.0.1", port,
                     mode: :binary,
                     packet: 4,
                     active: true
                   ) do
                {:ok, socket} ->
                  maybe_start_on_exit_handler(options)
                  {:ok, _} = start_registry(registry, url: url)
                  {:ok, %{state | socket: socket}}

                {:error, reason} ->
                  {:stop, reason}
              end
            else
              _ ->
                raise ArgumentError, "expected tcp://127.0.0.1:{port} URL, got: #{inspect(url)}"
            end
        end

      url = options[:listen] ->
        with "tcp://127.0.0.1:" <> rest <- url,
             {port, ""} = Integer.parse(rest) do
          case :gen_tcp.listen(port, [:binary, packet: 4, ip: {127, 0, 0, 1}, reuseaddr: true]) do
            {:ok, socket} ->
              {:ok, {_ip, actual_port}} = :inet.sockname(socket)
              url = "tcp://127.0.0.1:#{actual_port}"

              maybe_start_on_exit_handler(options)
              {:ok, _} = start_registry(registry, url: url)

              {:ok, %{state | socket: socket}, {:continue, :accept}}

            {:error, reason} ->
              {:stop, reason}
          end
        else
          _ -> raise ArgumentError, "expected tcp://127.0.0.1:{port} URL, got: #{inspect(url)}"
        end
    end
  end

  defp maybe_start_on_exit_handler(options) do
    if on_exit = options[:on_exit] do
      pid = self()

      spawn(fn ->
        ref = Process.monitor(pid)

        receive do
          {:DOWN, ^ref, _, _, _reason} ->
            on_exit.()
        end
      end)
    end
  end

  defp start_registry(registry, meta) do
    Registry.start_link(keys: :duplicate, name: registry, meta: meta)
  end

  @impl true
  def handle_continue(:accept, state) do
    listen_socket = state.socket
    {:ok, socket} = :gen_tcp.accept(listen_socket)
    :ok = :gen_tcp.close(listen_socket)
    {:noreply, %{state | socket: socket}}
  end

  @impl true
  def handle_info({:tcp, _socket, data}, state) do
    <<topic_len::8, topic::binary-size(topic_len), payload::binary>> = data

    Registry.dispatch(state.registry, topic, fn entries ->
      for {pid, _value} <- entries do
        send(pid, payload)
      end
    end)

    {:noreply, state}
  end

  def handle_info({:tcp_closed, _socket}, state) do
    {:stop, {:shutdown, :tcp_closed}, state}
  end

  def handle_info({:tcp_error, _socket, reason}, state) do
    {:stop, {:shutdown, {:tcp_error, reason}}, state}
  end

  @impl true
  def handle_cast({:broadcast, topic, message}, state) do
    data = [
      <<byte_size(topic)::8>>,
      topic,
      message
    ]

    case :gen_tcp.send(state.socket, data) do
      :ok ->
        {:noreply, state}

      {:error, reason} ->
        {:stop, {:shutdown, {:send_error, reason}}, state}
    end
  end

  defp registry_name(server) when is_atom(server) do
    :"#{server}.Registry"
  end
end
