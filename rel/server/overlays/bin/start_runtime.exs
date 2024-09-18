File.cd!(System.user_home!())

%{
  node_base: node_base,
  cookie: cookie,
  dist_port: dist_port
} = System.fetch_env!("LIVEBOOK_RUNTIME") |> Base.decode64!() |> :erlang.binary_to_term()

node =
  cond do
    System.get_env("FLY_APP_NAME") ->
      # This is the only Fly-specific part of starting Livebook as runtime
      app = System.fetch_env!("FLY_APP_NAME")
      machine_id = System.fetch_env!("FLY_MACHINE_ID")
      :"#{node_base}@#{machine_id}.vm.#{app}.internal"

    System.get_env("POD_IP") ->
      # This is the only K8s-specific part of starting Livebook as runtime
      hostname = System.fetch_env!("POD_IP")
      :"#{node_base}@#{hostname}"

    true ->
      raise "expected either POD_IP (for k8s) or FLY_APP_NAME (for Fly.io) to be set"
  end

# We persist the information before the node is reachable
:persistent_term.put(:livebook_runtime_info, %{
  pid: self()
})

Application.put_env(:kernel, :inet_dist_listen_min, dist_port)
Application.put_env(:kernel, :inet_dist_listen_max, dist_port)

{:ok, _} = :net_kernel.start(node, %{name_domain: :longnames, hidden: true})
Node.set_cookie(cookie)

IO.puts("Runtime node started, waiting for the parent finish initialization")

receive do
  :node_initialized ->
    manager_ref = Process.monitor(Livebook.Runtime.ErlDist.NodeManager)

    receive do
      {:DOWN, ^manager_ref, :process, _object, _reason} -> :ok
    end

    IO.puts("The owner disconnected from the runtime, shutting down")
after
  20_000 ->
    IO.puts(:stderr, "No node initialization within 20s, shutting down")
end

System.halt()
