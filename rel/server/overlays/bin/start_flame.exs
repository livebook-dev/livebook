flame_parent = System.fetch_env!("FLAME_PARENT") |> Base.decode64!() |> :erlang.binary_to_term()

%{
  pid: parent_pid,
  flame_vsn: flame_parent_vsn,
  backend: _backend,
  backend_app: backend_app,
  backend_vsn: backend_vsn,
  node_base: node_base,
  host_env: host_env
} = flame_parent

flame_node_name = :"#{node_base}@#{System.fetch_env!(host_env)}"
flame_node_cookie = String.to_atom(System.fetch_env!("LIVEBOOK_COOKIE"))

flame_dep =
  if git_ref = System.get_env("FLAME_GIT_REF") do
    {:flame, github: "phoenixframework/flame", ref: git_ref}
  else
    {:flame, flame_parent_vsn}
  end

flame_backend_deps =
  case backend_app do
    :flame -> []
    _ -> [{backend_app, backend_vsn}]
  end

{:ok, _} = :net_kernel.start(flame_node_name, %{name_domain: :longnames})
Node.set_cookie(flame_node_cookie)

Mix.install([flame_dep | flame_backend_deps], consolidate_protocols: false)

IO.puts(
  "[Livebook] starting #{inspect(flame_node_name)} in FLAME mode with parent: #{inspect(parent_pid)}, backend: #{inspect(backend_app)}"
)

System.no_halt(true)
