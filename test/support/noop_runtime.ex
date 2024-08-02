defmodule Livebook.Runtime.NoopRuntime do
  # A runtime that doesn't do any actual evaluation,
  # thus not requiring any underlying resources.

  defstruct [:trace_to]

  def new(trace_to \\ nil) do
    %__MODULE__{trace_to: trace_to}
  end

  defimpl Livebook.Runtime do
    def describe(_runtime) do
      [{"Type", "Noop"}]
    end

    def connect(runtime) do
      caller = self()

      spawn(fn ->
        send(caller, {:runtime_connect_done, self(), {:ok, runtime}})
      end)
    end

    def take_ownership(_, _), do: make_ref()
    def disconnect(_), do: :ok
    def duplicate(runtime), do: Livebook.Runtime.NoopRuntime.new(runtime.trace_to)

    def evaluate_code(_, _, _, _, _, _ \\ []), do: :ok
    def forget_evaluation(_, _), do: :ok
    def drop_container(_, _), do: :ok
    def handle_intellisense(_, _, _, _, _), do: make_ref()

    def read_file(_, path) do
      case File.read(path) do
        {:ok, binary} -> {:ok, binary}
        {:error, reason} -> {:error, "failed to read the file, got: #{inspect(reason)}"}
      end
    end

    def transfer_file(_runtime, path, _file_id, callback) do
      callback.(path)
      :ok
    end

    def relabel_file(_runtime, _file_id, _new_file_id), do: :ok

    def revoke_file(runtime, file_id) do
      trace(runtime, :revoke_file, [file_id])
      :ok
    end

    def start_smart_cell(_, _, _, _, _), do: :ok
    def set_smart_cell_parent_locators(_, _, _), do: :ok
    def stop_smart_cell(_, _), do: :ok

    def fixed_dependencies?(_), do: false

    def add_dependencies(_runtime, code, dependencies) do
      Livebook.Runtime.Dependencies.add_dependencies(code, dependencies)
    end

    def has_dependencies?(_runtime, _dependencies), do: true

    def snippet_definitions(_runtime) do
      Livebook.Runtime.Definitions.snippet_definitions()
    end

    def search_packages(_, _, _), do: make_ref()

    def put_system_envs(_, _), do: :ok
    def delete_system_envs(_, _), do: :ok

    def restore_transient_state(runtime, transient_state) do
      trace(runtime, :restore_transient_state, [transient_state])
      :ok
    end

    def register_clients(_, _), do: :ok
    def unregister_clients(_, _), do: :ok
    def fetch_proxy_handler_spec(_), do: {:error, :not_found}

    def disconnect_node(runtime, node) do
      trace(runtime, :disconnect_node, [node])
      :ok
    end

    defp trace(runtime, fun, args) do
      if runtime.trace_to do
        send(runtime.trace_to, {:runtime_trace, fun, args})
      end
    end
  end
end
