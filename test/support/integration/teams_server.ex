defmodule Livebook.TeamsServer do
  use GenServer

  defstruct [:node, :port, :app_port, :url, :env]

  @name __MODULE__
  @timeout 10_000
  @default_teams_dir "../teams"

  def available?() do
    System.get_env("TEAMS_PATH") != nil or File.exists?(@default_teams_dir)
  end

  def setup do
    if available?() do
      :ok =
        mix(%__MODULE__{}, [
          "do",
          "compile",
          "+",
          "ecto.drop",
          "--quiet",
          "+",
          "ecto.create",
          "--quiet",
          "+",
          "ecto.migrate",
          "--quiet"
        ])
    end
  end

  def start(opts \\ []) do
    GenServer.start(__MODULE__, opts, name: @name)
  end

  def url() do
    GenServer.call(@name, :fetch_url, @timeout)
  end

  def get_node() do
    GenServer.call(@name, :fetch_node, @timeout)
  end

  # GenServer Callbacks

  @impl true
  def init(opts) do
    state = struct!(__MODULE__, opts)
    {:ok, %{state | node: app_node()}, {:continue, :start_app}}
  end

  @impl true
  def handle_continue(:start_app, state) do
    ensure_app_dir!()
    {:noreply, %{state | port: start_app(state)}}
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

  # Port Callbacks

  @impl true
  def handle_info({_port, {:data, message}}, state) do
    if Livebook.Config.boolean!("TEAMS_DEBUG", false) do
      info(message)
    end

    {:noreply, state}
  end

  def handle_info({_port, {:exit_status, status}}, _state) do
    error("team quit with status #{status}")
    System.halt(status)
  end

  # Private

  defp start_app(state) do
    env =
      for {key, value} <- env(state), into: [] do
        {String.to_charlist(key), String.to_charlist(value)}
      end

    args = [
      "-e",
      "spawn(fn -> IO.gets([]) && System.halt(0) end)",
      "--name",
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

  defp ensure_app_dir! do
    dir = app_dir()

    unless File.exists?(dir) do
      IO.puts(
        "Unable to find #{dir}, make sure to clone the hub repository " <>
          "into it to run integration tests or set TEAMS_PATH to its location"
      )

      System.halt(1)
    end
  end

  defp app_dir do
    System.get_env("TEAMS_PATH", @default_teams_dir)
  end

  defp app_port do
    System.get_env("TEAMS_PORT", "4123")
  end

  defp wait_on_start(state, port) do
    url = state.url || fetch_url(state)

    case :httpc.request(:get, {~c"#{url}/healthz", []}, [], []) do
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
    env =
      Map.filter(
        %{
          "MIX_ENV" => "livebook",
          "PORT" => to_string(app_port),
          "DEBUG" => System.get_env("TEAMS_DEBUG", "false"),
          "LIVEBOOK_PROTO_PATH" => System.get_env("TEAMS_LIVEBOOK_PROTO_PATH")
        },
        fn {_key, value} -> value not in ["", nil] end
      )

    if state_env do
      Map.merge(env, state_env)
    else
      env
    end
  end

  defp elixir_executable do
    System.find_executable("elixir")
  end

  defp app_node do
    :"teams_#{Livebook.Utils.random_short_id()}@#{hostname()}"
  end

  defp hostname do
    [_nodename, hostname] =
      node()
      |> Atom.to_charlist()
      |> :string.split(~c"@")

    hostname
  end

  defp info(message), do: log([:blue, message <> "\n"])
  defp error(message), do: log([:red, message <> "\n"])
  defp log(data), do: data |> IO.ANSI.format() |> IO.write()
end
