defmodule LivebookTest.Integration.EnterpriseServer do
  @moduledoc false
  use GenServer

  @name __MODULE__

  defmodule CollectableMapper do
    defstruct [:collectable, :fun]

    defimpl Collectable do
      def into(%{collectable: collectable, fun: fun}) do
        {term, collectable_fun} = Collectable.into(collectable)
        {term, wrap(collectable_fun, fun)}
      end

      defp wrap(collectable_fun, fun) do
        fn
          term, {:cont, x} ->
            collectable_fun.(term, {:cont, fun.(x)})

          term, command ->
            collectable_fun.(term, command)
        end
      end
    end
  end

  def start do
    GenServer.start(__MODULE__, [], name: @name)
  end

  def start_link do
    GenServer.start_link(__MODULE__, [], name: @name)
  end

  def url do
    "http://localhost:#{app_port()}"
  end

  def token do
    GenServer.call(@name, :fetch_token)
  end

  # GenServer Callbacks

  @impl true
  def init(_opts) do
    {:ok, %{token: nil, port: nil}, {:continue, :start_enterprise}}
  end

  @impl true
  def handle_continue(:start_enterprise, state) do
    {:noreply, %{state | port: start_enterprise()}}
  end

  @impl true
  def handle_call(:fetch_token, _from, state) do
    token = state.token || fetch_token_from_enterprise()

    {:reply, token, %{state | token: token}}
  end

  # Port Callbacks

  @impl true
  def handle_info({_port, {:data, msg}}, state) do
    [:blue, msg]
    |> IO.ANSI.format()
    |> IO.write()

    {:noreply, state}
  end

  def handle_info({_port, {:exit_status, status}}, _state) do
    [:red, "enterprise quit with status #{status}"]
    |> IO.ANSI.format()
    |> IO.write()

    System.halt(status)
  end

  # Private

  defp fetch_token_from_enterprise do
    token = cmd(["enterprise.gen.token"], with_return: true)
    String.trim(token)
  end

  defp start_enterprise do
    ensure_app_dir!()
    prepare_database()

    env = [
      {~c"MIX_ENV", ~c"livebook"},
      {~c"LIVEBOOK_ENTERPRISE_PORT", String.to_charlist(app_port())}
    ]

    args = [
      "-e",
      "spawn(fn -> IO.gets([]) && System.halt(0) end)",
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

    wait_on_start(port)
  end

  defp prepare_database do
    cmd(["ecto.drop", "--quiet"])
    cmd(["ecto.create", "--quiet"])
    cmd(["ecto.migrate", "--quiet"])
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
    System.get_env("ENTERPRISE_PATH", "../enterprise")
  end

  defp app_port do
    System.get_env("ENTERPRISE_PORT", "4043")
  end

  defp wait_on_start(port) do
    case :httpc.request(:get, {~c"#{url()}/public/health", []}, [], []) do
      {:ok, _} ->
        port

      {:error, _} ->
        Process.sleep(10)
        wait_on_start(port)
    end
  end

  defp cmd(args, opts \\ []) do
    env = [
      {"MIX_ENV", "livebook"},
      {"LIVEBOOK_ENTERPRISE_PORT", app_port()}
    ]

    cmd_opts = [stderr_to_stdout: true, env: env, cd: app_dir()]

    cmd_opts =
      if opts[:with_return],
        do: cmd_opts,
        else: Keyword.put(cmd_opts, :into, stream_stdout())

    if opts[:with_return] do
      case System.cmd(mix_executable(), args, cmd_opts) do
        {result, 0} ->
          result

        {error, status} ->
          [:red, error]
          |> IO.ANSI.format()
          |> IO.write()

          System.halt(status)
      end
    else
      0 = System.cmd(mix_executable(), args, cmd_opts) |> elem(1)
    end
  end

  defp elixir_executable do
    System.find_executable("elixir")
  end

  defp mix_executable do
    System.find_executable("mix")
  end

  defp stream_stdout do
    %CollectableMapper{
      collectable: IO.stream(:stdio, :line),
      fun: &IO.ANSI.format([:blue, &1])
    }
  end
end
