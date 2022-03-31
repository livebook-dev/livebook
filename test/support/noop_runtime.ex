defmodule Livebook.Runtime.NoopRuntime do
  @moduledoc false

  # A runtime that doesn't do any actual evaluation,
  # thus not requiring any underlying resources.

  defstruct [:search_dependencies_response]

  def new(opts \\ []) do
    %__MODULE__{search_dependencies_response: opts[:search_dependencies_response]}
  end

  defimpl Livebook.Runtime do
    def describe(_runtime) do
      [{"Type", "Noop"}]
    end

    def connect(_, _), do: make_ref()
    def disconnect(_), do: :ok
    def evaluate_code(_, _, _, _, _ \\ []), do: :ok
    def forget_evaluation(_, _), do: :ok
    def drop_container(_, _), do: :ok
    def handle_intellisense(_, _, _, _, _), do: :ok
    def duplicate(_), do: {:ok, Livebook.Runtime.NoopRuntime.new()}
    def standalone?(_), do: false
    def read_file(_, _), do: raise("not implemented")
    def start_smart_cell(_, _, _, _, _), do: :ok
    def set_smart_cell_base_locator(_, _, _), do: :ok
    def stop_smart_cell(_, _), do: :ok

    def fixed_dependencies?(_), do: false

    def add_dependencies(_runtime, code, dependencies) do
      Livebook.Runtime.Dependencies.add_mix_deps(code, dependencies)
    end

    def search_dependencies(runtime, send_to, _search) do
      ref = make_ref()

      if response = runtime.search_dependencies_response do
        send(send_to, {:runtime_search_dependencies_response, ref, response})
      end

      ref
    end
  end
end
