defmodule Livebook.EnterpriseServer do
  @moduledoc false
  use GenServer

  defstruct [:token, :user, :node, :port, :app_port, :url, :env]

  @name __MODULE__
  @timeout 10_000
  @default_enterprise_dir "../enterprise"

  def available?() do
    System.get_env("ENTERPRISE_PATH") != nil or File.exists?(@default_enterprise_dir)
  end

  def start(name \\ @name, opts \\ []) do
    GenServer.start(__MODULE__, opts, name: name)
  end

  def url(name \\ @name) do
    GenServer.call(name, :fetch_url, @timeout)
  end

  def token(name \\ @name) do
    GenServer.call(name, :fetch_token, @timeout)
  end

  def user(name \\ @name) do
    GenServer.call(name, :fetch_user, @timeout)
  end

  def get_node(name \\ @name) do
    GenServer.call(name, :fetch_node, @timeout)
  end

  def drop_database(name \\ @name) do
    app_port = GenServer.call(name, :fetch_port)
    state_env = GenServer.call(name, :fetch_env)

    app_port |> env(state_env) |> mix(["ecto.drop", "--quiet"])
  end

  def reconnect(name \\ @name) do
    GenServer.cast(name, :reconnect)
  end

  def disconnect(name \\ @name) do
    GenServer.cast(name, :disconnect)
  end

  # GenServer Callbacks

  @impl true
  def init(opts) do
    state = struct!(__MODULE__, opts)

    {:ok, %{state | node: enterprise_node()}, {:continue, :start_enterprise}}
  end

  @impl true
  def handle_continue(:start_enterprise, state) do
    ensure_app_dir!()
    prepare_database(state)

    {:noreply, %{state | port: start_enterprise(state)}}
  end

  @impl true
  def handle_call(:fetch_token, _from, state) do
    state = if state.token, do: state, else: create_enterprise_token(state)

    {:reply, state.token, state}
  end

  @impl true
  def handle_call(:fetch_user, _from, state) do
    state = if state.user, do: state, else: create_enterprise_user(state)

    {:reply, state.user, state}
  end

  @impl true
  def handle_call(:fetch_url, _from, state) do
    state = if state.app_port, do: state, else: %{state | app_port: app_port()}
    url = state.url || fetch_url(state)

    {:reply, url, %{state | url: url}}
  end

  def handle_call(:fetch_node, _from, state) do
    {:reply, state.node, state}
  end

  def handle_call(:fetch_port, _from, state) do
    port = state.app_port || app_port()
    {:reply, port, state}
  end

  def handle_call(:fetch_env, _from, state) do
    {:reply, state.env, state}
  end

  @impl true
  def handle_cast(:reconnect, state) do
    if state.port do
      {:noreply, state}
    else
      {:noreply, %{state | port: start_enterprise(state)}}
    end
  end

  def handle_cast(:disconnect, state) do
    if state.port do
      Port.close(state.port)
    end

    {:noreply, %{state | port: nil}}
  end

  # Port Callbacks

  @impl true
  def handle_info({_port, {:data, message}}, state) do
    info(message)
    {:noreply, state}
  end

  def handle_info({_port, {:exit_status, status}}, _state) do
    error("enterprise quit with status #{status}")
    System.halt(status)
  end

  # Private

  defp create_enterprise_token(state) do
    if user = state.user do
      token = call_erpc_function(state.node, :generate_user_session_token!, [user])
      %{state | token: token}
    else
      user = call_erpc_function(state.node, :create_user)
      token = call_erpc_function(state.node, :generate_user_session_token!, [user])

      %{state | user: user, token: token}
    end
  end

  defp create_enterprise_user(state) do
    %{state | user: call_erpc_function(state.node, :create_user)}
  end

  defp call_erpc_function(node, function, args \\ []) do
    :erpc.call(node, Enterprise.Integration, function, args)
  end

  defp start_enterprise(state) do
    env =
      for {key, value} <- env(state), into: [] do
        {String.to_charlist(key), String.to_charlist(value)}
      end

    args = [
      "-e",
      "spawn(fn -> IO.gets([]) && System.halt(0) end)",
      "--sname",
      to_string(state.node),
      "--cookie",
      to_string(Node.get_cookie()),
      "-S",
      "mix",
      "phx.server"
    ]

    port =
      Port.open({:spawn_executable, elixir_executable()}, [
        :exit_status,
        :use_stdio,
        :stderr_to_stdout,
        :binary,
        :hide,
        env: env,
        cd: app_dir(),
        args: args
      ])

    wait_on_start(state, port)
  end

  defp fetch_url(state) do
    port = state.app_port || app_port()
    "http://localhost:#{port}"
  end

  defp prepare_database(state) do
    :ok = mix(state, ["ecto.drop", "--quiet"])
    :ok = mix(state, ["ecto.create", "--quiet"])
    :ok = mix(state, ["ecto.migrate", "--quiet"])
  end

  defp ensure_app_dir! do
    dir = app_dir()

    unless File.exists?(dir) do
      IO.puts(
        "Unable to find #{dir}, make sure to clone the enterprise repository " <>
          "into it to run integration tests or set ENTERPRISE_PATH to its location"
      )

      System.halt(1)
    end
  end

  defp app_dir do
    System.get_env("ENTERPRISE_PATH", @default_enterprise_dir)
  end

  defp app_port do
    System.get_env("ENTERPRISE_PORT", "4043")
  end

  defp debug do
    System.get_env("ENTERPRISE_DEBUG", "false")
  end

  defp proto do
    System.get_env("ENTERPRISE_LIVEBOOK_PROTO_PATH")
  end

  defp wait_on_start(state, port) do
    url = state.url || fetch_url(state)

    case :httpc.request(:get, {~c"#{url}/public/health", []}, [], []) do
      {:ok, _} ->
        port

      {:error, _} ->
        Process.sleep(10)
        wait_on_start(state, port)
    end
  end

  defp mix(state, args) when is_struct(state) do
    state |> env() |> mix(args)
  end

  defp mix(env, args) do
    cmd_opts = [
      stderr_to_stdout: true,
      env: env,
      cd: app_dir(),
      into: IO.stream(:stdio, :line)
    ]

    args = ["--erl", "-elixir ansi_enabled true", "-S", "mix" | args]

    case System.cmd(elixir_executable(), args, cmd_opts) do
      {_, 0} -> :ok
      _ -> :error
    end
  end

  defp env(state) do
    app_port = state.app_port || app_port()
    env(app_port, state.env)
  end

  defp env(app_port, state_env) do
    env = %{
      "MIX_ENV" => "livebook",
      "PORT" => to_string(app_port),
      "DEBUG" => debug()
    }

    env = if proto(), do: Map.merge(env, %{"LIVEBOOK_PROTO_PATH" => proto()}), else: env

    if state_env do
      Map.merge(env, state_env)
    else
      env
    end
  end

  defp elixir_executable do
    System.find_executable("elixir")
  end

  defp enterprise_node do
    :"enterprise_#{Livebook.Utils.random_short_id()}@#{hostname()}"
  end

  defp hostname do
    [nodename, hostname] =
      node()
      |> Atom.to_charlist()
      |> :string.split(~c"@")

    with {:ok, nodenames} <- :erl_epmd.names(hostname),
         true <- List.keymember?(nodenames, nodename, 0) do
      hostname
    else
      _ ->
        raise "Error"
    end
  end

  defp info(message), do: log([:blue, message <> "\n"])
  defp error(message), do: log([:red, message <> "\n"])
  defp log(data), do: data |> IO.ANSI.format() |> IO.write()
end
