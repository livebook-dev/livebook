defmodule Livebook.Runtime.Evaluator.Tracer do
  @moduledoc false

  # Compilation tracer used by the evaluator.
  #
  # Events are pre-processed and sent to the group leader, where the
  # tracer state is accumulated. After evaluation the evaluator reads
  # the accumulated state.

  alias Livebook.Runtime.Evaluator

  defstruct modules_used: MapSet.new(),
            modules_defined: %{},
            aliases_used: MapSet.new(),
            aliases_defined: %{},
            requires_used: MapSet.new(),
            requires_defined: MapSet.new(),
            imports_used?: false,
            imports_defined?: false,
            undefined_vars: MapSet.new()

  @type t :: %__MODULE__{
          modules_used: MapSet.t(),
          modules_defined: map(),
          aliases_used: MapSet.t(),
          aliases_defined: map(),
          requires_used: MapSet.t(),
          requires_defined: MapSet.t(),
          imports_used?: boolean(),
          imports_defined?: boolean(),
          undefined_vars: MapSet.t()
        }

  @doc false
  def trace(event, env) do
    case event_to_updates(event, env) do
      [] ->
        :ok

      updates ->
        io_proxy = Process.group_leader()
        Evaluator.IOProxy.tracer_updates(io_proxy, updates)
    end

    :ok
  end

  defp event_to_updates(event, env) do
    # Note that import/require/alias/defmodule don't trigger `:alias_reference`
    # for the used alias, so we add it explicitly

    case event do
      :start ->
        if Code.ensure_loaded?(env.module) do
          raise CompileError,
            line: env.line,
            file: env.file,
            description: "module #{inspect(env.module)} is already defined"
        end

        []

      {:import, _meta, module, _opts} ->
        if(env.module, do: [], else: [:import_defined]) ++
          [{:module_used, module}, {:alias_used, module}]

      {:imported_function, meta, module, name, _arity} ->
        var? = Keyword.has_key?(meta, :if_undefined)
        [{:module_used, module}, {:import_used, name, var?}]

      {:imported_macro, meta, module, name, _arity} ->
        var? = Keyword.has_key?(meta, :if_undefined)
        [{:module_used, module}, {:import_used, name, var?}]

      {:alias, _meta, alias, as, _opts} ->
        if(env.module, do: [], else: [{:alias_defined, as, alias}]) ++
          [{:alias_used, alias}]

      {:alias_expansion, _meta, as, _alias} ->
        [{:alias_used, as}]

      {:alias_reference, _meta, alias} ->
        [{:alias_used, alias}]

      {:require, _meta, module, _opts} ->
        if(env.module, do: [], else: [{:require_defined, module}]) ++
          [{:module_used, module}, {:alias_used, module}]

      {:struct_expansion, _meta, module, _keys} ->
        [{:module_used, module}]

      {:remote_function, _meta, module, _name, _arity} ->
        [{:module_used, module}]

      {:remote_macro, _meta, module, _name, _arity} ->
        [{:module_used, module}, {:require_used, module}]

      {:on_module, bytecode, _ignore} ->
        module = env.module
        vars = Map.keys(env.versioned_vars)
        Evaluator.write_module!(module, bytecode)
        [{:module_defined, module, vars}, {:alias_used, module}]

      _ ->
        []
    end
  end

  @doc """
  Applies updates to the tracer state.
  """
  @spec apply_updates(%__MODULE__{}, list()) :: %__MODULE__{}
  def apply_updates(info, updates) do
    Enum.reduce(updates, info, &apply_update(&2, &1))
  end

  defp apply_update(info, {:module_used, module}) do
    update_in(info.modules_used, &MapSet.put(&1, module))
  end

  defp apply_update(info, {:module_defined, module, vars}) do
    put_in(info.modules_defined[module], vars)
  end

  defp apply_update(info, {:alias_used, alias}) do
    update_in(info.aliases_used, &MapSet.put(&1, alias))
  end

  defp apply_update(info, {:alias_defined, as, alias}) do
    put_in(info.aliases_defined[as], alias)
  end

  defp apply_update(info, {:require_used, module}) do
    update_in(info.requires_used, &MapSet.put(&1, module))
  end

  defp apply_update(info, {:require_defined, module}) do
    update_in(info.requires_defined, &MapSet.put(&1, module))
  end

  defp apply_update(info, {:import_used, name, var?}) do
    info = put_in(info.imports_used?, true)

    if var? do
      update_in(info.undefined_vars, &MapSet.put(&1, {name, nil}))
    else
      info
    end
  end

  defp apply_update(info, :import_defined) do
    put_in(info.imports_defined?, true)
  end
end
