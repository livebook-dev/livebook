defmodule Livebook.Application do
  use Application

  require Logger

  def start(_type, _args) do
    Livebook.ZTA.init()
    create_teams_hub = parse_teams_hub()
    setup_optional_dependencies()
    ensure_directories!()
    set_local_file_system!()

    set_epmd_module!()
    start_distribution!()
    set_cookie()

    children =
      if serverless?() do
        []
      else
        [{DNSCluster, query: Application.get_env(:livebook, :dns_cluster_query) || :ignore}]
      end ++
        [
          # Start the Telemetry supervisor
          LivebookWeb.Telemetry,
          # Start the PubSub system
          {Phoenix.PubSub, name: Livebook.PubSub},
          # Start a supervisor for Livebook tasks
          {Task.Supervisor, name: Livebook.TaskSupervisor},
          # Start the unique task dependencies
          Livebook.Utils.UniqueTask,
          # Start the storage module
          Livebook.Storage,
          # Run migrations as soon as the storage is running
          {Livebook.Utils.SupervisionStep, {:migration, &Livebook.Migration.run/0}},
          # Start the periodic version check
          Livebook.UpdateCheck,
          # Periodic measurement of system resources
          Livebook.SystemResources,
          # Start the notebook manager server
          Livebook.NotebookManager,
          # Start the tracker server for sessions and apps on this node
          {Livebook.Tracker, pubsub_server: Livebook.PubSub},
          # Start the node pool for managing node names
          Livebook.EPMD.NodePool,
          # Start the server responsible for associating files with sessions
          Livebook.Session.FileGuard,
          # Start the supervisor dynamically managing runtimes
          {DynamicSupervisor, name: Livebook.RuntimeSupervisor, strategy: :one_for_one},
          # Start the supervisor dynamically managing sessions
          {DynamicSupervisor, name: Livebook.SessionSupervisor, strategy: :one_for_one},
          # Start the registry for managing unique connections
          {Registry, keys: :unique, name: Livebook.HubsRegistry},
          # Start the supervisor dynamically managing connections
          {DynamicSupervisor, name: Livebook.HubsSupervisor, strategy: :one_for_one},
          # Run startup logic relying on the supervision tree
          {Livebook.Utils.SupervisionStep, {:boot, boot(create_teams_hub)}},
          # App manager supervision tree. We do it after boot, because
          # permanent apps are going to be started right away and this
          # depends on hubs being started
          Livebook.Apps.DeploymentSupervisor
        ] ++
        if serverless?() do
          []
        else
          {_type, module, key} = Livebook.Config.identity_provider()

          iframe_server_specs() ++
            [
              {module, name: LivebookWeb.ZTA, identity_key: key},
              # We skip the access url as we do our own logging below
              endpoint_childspec(log_access_url: false)
            ] ++ app_specs()
        end

    opts = [strategy: :one_for_one, name: Livebook.Supervisor]

    case Supervisor.start_link(children, opts) do
      {:ok, _} = result ->
        display_startup_info()
        result

      {:error, error} ->
        Livebook.Config.abort!(Application.format_error(error))
    end
  end

  def boot(create_teams_hub) do
    fn ->
      load_lb_env_vars()
      create_teams_hub.()
      clear_env_vars()
      Livebook.Hubs.connect_hubs()

      unless serverless?() do
        load_apps_dir()
      end
    end
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    LivebookWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  defp setup_optional_dependencies() do
    if Livebook.Config.aws_credentials?() do
      {:ok, _} = Application.ensure_all_started(:aws_credentials)
    end
  end

  defp ensure_directories!() do
    File.mkdir_p!(Livebook.Config.home())
    File.mkdir_p!(Livebook.Config.data_path())
  end

  defp set_local_file_system!() do
    home =
      Livebook.Config.home()
      |> Livebook.FileSystem.Utils.ensure_dir_path()

    local_file_system = Livebook.FileSystem.Local.new(default_path: home)
    :persistent_term.put(:livebook_local_file_system, local_file_system)
  end

  defp set_epmd_module!() do
    # We use a custom EPMD module. In releases and Escript, we make
    # sure the necessary erl flags are set. When running from source,
    # we try to use the new :kernel configuration available in OTP 27.2,
    # otherwise it needs to be set explicitly.

    # TODO: always rely on :kernel configuration once we require OTP 27.2

    case :init.get_argument(:epmd_module) do
      {:ok, [[~c"Elixir.Livebook.EPMD"]]} ->
        :ok

      _ ->
        Application.put_env(:kernel, :epmd_module, Livebook.EPMD, persistent: true)

        # Note: this is a private API
        if :net_kernel.epmd_module() != Livebook.EPMD do
          Livebook.Config.abort!("""
          You must set the environment variable ELIXIR_ERL_OPTIONS="-epmd_module Elixir.Livebook.EPMD" \
          before the command (and exclusively before the command)
          """)
        end
    end
  end

  defp start_distribution!() do
    node = get_node_name()

    case Node.start(node, :longnames) do
      {:ok, _} ->
        :ok

      {:error, reason} ->
        Livebook.Config.abort!("Could not start distributed node: #{inspect(reason)}")
    end
  end

  defp set_cookie() do
    cookie = Application.fetch_env!(:livebook, :cookie)
    Node.set_cookie(cookie)
  end

  defp get_node_name() do
    Application.get_env(:livebook, :node) || random_long_name()
  end

  defp random_long_name() do
    host =
      if Livebook.Utils.proto_dist() == :inet6_tcp do
        "::1"
      else
        "127.0.0.1"
      end

    :"livebook_#{Livebook.Utils.random_short_id()}@#{host}"
  end

  defp display_startup_info() do
    if Process.whereis(LivebookWeb.Endpoint) &&
         Phoenix.Endpoint.server?(:livebook, LivebookWeb.Endpoint) do
      IO.puts("[Livebook] Application running at #{LivebookWeb.Endpoint.access_url()}")
    end
  end

  @app? Mix.target() == :app

  if @app? do
    defp app_specs, do: [LivebookApp]
  else
    defp app_specs, do: []
  end

  # In order to provide good first experience with the desktop app,
  # in case the endpoint or iframe port is taken, we automatically
  # fallback to a random port.

  if @app? do
    defp endpoint_childspec(opts) do
      %{start: start} = childspec = LivebookWeb.Endpoint.child_spec(opts)
      %{childspec | start: {__MODULE__, :endpoint_start, [start]}}
    end
  else
    defp endpoint_childspec(opts), do: LivebookWeb.Endpoint.child_spec(opts)
  end

  @doc false
  def endpoint_start({mod, fun, args}) do
    with {:error,
          {:shutdown,
           {:failed_to_start_child, {LivebookWeb.Endpoint, :http},
            {:shutdown, {:failed_to_start_child, :listener, :eaddrinuse}}}}} <-
           apply(mod, fun, args) do
      config = Application.get_env(:livebook, LivebookWeb.Endpoint)
      config = put_in(config[:http][:port], 0)
      Application.put_env(:livebook, LivebookWeb.Endpoint, config, persistent: true)
      Logger.warning("Starting server using a random port")
      endpoint_start({mod, fun, args})
    end
  end

  defp iframe_server_specs() do
    server? = Phoenix.Endpoint.server?(:livebook, LivebookWeb.Endpoint)
    port = Livebook.Config.iframe_port()

    if server? do
      http = Application.fetch_env!(:livebook, LivebookWeb.Endpoint)[:http]

      opts =
        [
          scheme: :http,
          plug: LivebookWeb.IframeEndpoint,
          port: port,
          thousand_island_options: [supervisor_options: [name: LivebookWeb.IframeEndpoint]]
        ] ++ Keyword.take(http, [:ip])

      [iframe_endpoint_childspec(opts)]
    else
      []
    end
  end

  if @app? do
    defp iframe_endpoint_childspec(opts) do
      %{start: start} = childspec = Bandit.child_spec(opts)
      %{childspec | start: {__MODULE__, :iframe_endpoint_start, [start]}}
    end
  else
    defp iframe_endpoint_childspec(opts), do: Bandit.child_spec(opts)
  end

  @doc false
  def iframe_endpoint_start({mod, fun, [opts]}) do
    with {:error, {:shutdown, {:failed_to_start_child, :listener, :eaddrinuse}}} <-
           apply(mod, fun, [opts]) do
      Application.put_env(:livebook, :iframe_port, 0, persistent: true)
      opts = Keyword.replace!(opts, :port, 0)
      Logger.warning("Starting iframe server using a random port")
      iframe_endpoint_start({mod, fun, [opts]})
    end
  end

  defp load_lb_env_vars() do
    secrets =
      for {"LB_" <> name = var, value} <- System.get_env() do
        System.delete_env(var)

        %Livebook.Secrets.Secret{
          name: name,
          value: value,
          hub_id: nil
        }
      end

    Livebook.Secrets.set_startup_secrets(secrets)
  end

  defp clear_env_vars() do
    for {var, _} <- System.get_env(), config_env_var?(var) do
      System.delete_env(var)
    end
  end

  defp parse_teams_hub() do
    teams_key = System.get_env("LIVEBOOK_TEAMS_KEY")
    auth = System.get_env("LIVEBOOK_TEAMS_AUTH")

    cond do
      teams_key && auth ->
        Application.put_env(:livebook, :teams_auth?, true)

        {hub_id, fun} =
          case String.split(auth, ":") do
            ["offline", name, public_key] ->
              hub_id = "team-#{name}"
              {hub_id, fn -> create_offline_hub(teams_key, hub_id, name, public_key) end}

            ["online", name, org_id, org_key_id, agent_key] ->
              hub_id = "team-" <> name

              with :error <- Application.fetch_env(:livebook, :identity_provider) do
                Application.put_env(
                  :livebook,
                  :identity_provider,
                  {:zta, Livebook.ZTA.LivebookTeams, hub_id}
                )
              end

              {hub_id,
               fn -> create_online_hub(teams_key, hub_id, name, org_id, org_key_id, agent_key) end}

            _ ->
              Livebook.Config.abort!("Invalid LIVEBOOK_TEAMS_AUTH configuration.")
          end

        Application.put_env(:livebook, :apps_path_hub_id, hub_id)
        fun

      teams_key || auth ->
        Livebook.Config.abort!(
          "You must specify both LIVEBOOK_TEAMS_KEY and LIVEBOOK_TEAMS_AUTH."
        )

      true ->
        fn -> :ok end
    end
  end

  defp create_offline_hub(teams_key, id, name, public_key) do
    encrypted_secrets = System.get_env("LIVEBOOK_TEAMS_SECRETS")
    encrypted_file_systems = System.get_env("LIVEBOOK_TEAMS_FS")
    secret_key = Livebook.Teams.derive_key(teams_key)

    secrets =
      if encrypted_secrets do
        case Livebook.Teams.decrypt(encrypted_secrets, secret_key) do
          {:ok, json} ->
            for {name, value} <- JSON.decode!(json) do
              %Livebook.Secrets.Secret{name: name, value: value, hub_id: id}
            end

          :error ->
            Livebook.Config.abort!(
              "You specified LIVEBOOK_TEAMS_SECRETS, but we couldn't decrypt with the given LIVEBOOK_TEAMS_KEY."
            )
        end
      else
        []
      end

    file_systems =
      if encrypted_file_systems do
        case Livebook.Teams.decrypt(encrypted_file_systems, secret_key) do
          {:ok, json} ->
            for %{"type" => type} = dumped_data <- JSON.decode!(json),
                do: Livebook.FileSystems.load(type, dumped_data)

          :error ->
            Livebook.Config.abort!(
              "You specified LIVEBOOK_TEAMS_FS, but we couldn't decrypt with the given LIVEBOOK_TEAMS_KEY."
            )
        end
      else
        []
      end

    Livebook.Hubs.save_hub(%Livebook.Hubs.Team{
      id: id,
      hub_name: name,
      hub_emoji: "⭐️",
      user_id: nil,
      org_id: nil,
      org_key_id: nil,
      session_token: "",
      teams_key: teams_key,
      org_public_key: public_key,
      offline: %Livebook.Hubs.Team.Offline{
        secrets: secrets,
        file_systems: file_systems
      }
    })
  end

  defp create_online_hub(teams_key, id, name, org_id, org_key_id, agent_key) do
    Livebook.Hubs.save_hub(%Livebook.Hubs.Team{
      id: id,
      hub_name: name,
      hub_emoji: "💡",
      user_id: nil,
      org_id: org_id,
      org_key_id: org_key_id,
      session_token: agent_key,
      teams_key: teams_key,
      org_public_key: nil,
      offline: nil
    })
  end

  # We set ELIXIR_ERL_OPTIONS to set our custom EPMD module when
  # running from source. By design, we don't allow ELIXIR_ERL_OPTIONS
  # to pass through. Use ERL_AFLAGS and ERL_ZFLAGS if you want to
  # configure both Livebook and spawned runtimes.
  defp config_env_var?("ELIXIR_ERL_OPTIONS"), do: true
  defp config_env_var?("LIVEBOOK_" <> _), do: true
  defp config_env_var?("RELEASE_" <> _), do: true
  defp config_env_var?("MIX_ENV"), do: true
  defp config_env_var?(_), do: false

  defp load_apps_dir() do
    if apps_path = Livebook.Config.apps_path() do
      should_warmup = Livebook.Config.apps_path_warmup() == :auto

      specs =
        Livebook.Apps.build_app_specs_in_dir(apps_path,
          password: Livebook.Config.apps_path_password(),
          hub_id: Livebook.Config.apps_path_hub_id(),
          should_warmup: should_warmup
        )

      Livebook.Apps.set_startup_app_specs(specs)
    end
  end

  defp serverless?() do
    Application.get_env(:livebook, :serverless, false)
  end
end
