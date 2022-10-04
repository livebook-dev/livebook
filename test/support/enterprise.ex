defmodule LivebookTest.Enterprise do
  @moduledoc false

  defmodule WrappedCollectable do
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

  defp stream_enterprise do
    %WrappedCollectable{
      collectable: IO.stream(:stdio, :line),
      fun: &IO.ANSI.format([:blue, &1])
    }
  end

  def init() do
    check_enterprise()
    mix = :os.find_executable(~c"mix") |> List.to_string()

    cmd(mix, ["ecto.drop", "--quiet"])
    cmd(mix, ["ecto.create", "--quiet"])
    cmd(mix, ["ecto.migrate"])
  end

  def start() do
    spawn(fn ->
      port =
        Port.open({:spawn_executable, :os.find_executable(~c"elixir")}, [
          :exit_status,
          :use_stdio,
          :stderr_to_stdout,
          :binary,
          :hide,
          env: [{~c"MIX_ENV", ~c"livebook"}],
          cd: enterprise_dir(),
          args: [
            "-e",
            "spawn(fn -> IO.gets([]) && System.halt(0) end)",
            "-S",
            "mix",
            "phx.server"
          ]
        ])

      fun = fn fun ->
        receive do
          {^port, {:data, data}} ->
            IO.ANSI.format([:blue, data]) |> IO.write()
            fun.(fun)

          {^port, {:exit_status, status}} ->
            IO.puts("enterprise quit with status #{status}")
            System.halt(status)
        end
      end

      fun.(fun)
    end)

    wait_on_start()
  end

  defp check_enterprise do
    dir = enterprise_dir()

    unless File.exists?(dir) do
      IO.puts(
        "Unable to find #{dir}, make sure to clone the enterprise repository " <>
          "into it to run integration tests or set ENTERPRISE_PATH to its location"
      )

      System.halt(1)
    end
  end

  defp enterprise_dir do
    System.get_env("ENTERPRISE_PATH") || "../enterprise"
  end

  defp cmd(command, args, opts \\ []) do
    cmd_opts = [stderr_to_stdout: true, env: [{"MIX_ENV", "livebook"}], cd: enterprise_dir()]

    cmd_opts =
      if opts[:with_return],
        do: cmd_opts,
        else: Keyword.put(cmd_opts, :into, stream_enterprise())

    if opts[:with_return] do
      case System.cmd(command, args, cmd_opts) do
        {result, 0} ->
          result

        {error, status} ->
          raise RuntimeError, IO.ANSI.format([:red, error])
          System.halt(status)
      end
    else
      0 = System.cmd(command, args, cmd_opts) |> elem(1)
    end
  end

  defp wait_on_start do
    case :httpc.request(:get, {~c"http://localhost:4043/public/health", []}, [], []) do
      {:ok, _} ->
        :ok

      {:error, _} ->
        :timer.sleep(10)
        wait_on_start()
    end
  end

  def persist_token! do
    check_enterprise()
    mix = :os.find_executable(~c"mix") |> List.to_string()
    token = cmd(mix, ["enterprise.gen.token"], with_return: true)

    :persistent_term.put(:enterprise_token, String.trim(token))
    :ok
  end

  def fetch_token do
    :persistent_term.get(:enterprise_token)
  end

  def url, do: "http://localhost:4043"
end
