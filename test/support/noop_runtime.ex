defmodule Livebook.Runtime.NoopRuntime do
  @moduledoc false

  # A runtime that doesn't do any actual evaluation,
  # thus not requiring any underlying resources.

  defstruct [:started, :trace_to]

  def new(trace_to \\ nil) do
    %__MODULE__{started: false, trace_to: trace_to}
  end

  defimpl Livebook.Runtime do
    def describe(_runtime) do
      [{"Type", "Noop"}]
    end

    def connect(runtime), do: {:ok, %{runtime | started: true}}
    def connected?(runtime), do: runtime.started
    def take_ownership(_, _), do: make_ref()
    def disconnect(runtime), do: {:ok, %{runtime | started: false}}
    def duplicate(_), do: Livebook.Runtime.NoopRuntime.new()

    def evaluate_code(_, _, _, _, _, _ \\ []), do: :ok
    def forget_evaluation(_, _), do: :ok
    def drop_container(_, _), do: :ok
    def handle_intellisense(_, _, _, _), do: make_ref()

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

    def disable_dependencies_cache(_), do: :ok

    def put_system_envs(_, _), do: :ok
    def delete_system_envs(_, _), do: :ok

    defp trace(runtime, fun, args) do
      if runtime.trace_to do
        send(runtime.trace_to, {:runtime_trace, fun, args})
      end
    end
  end
end
