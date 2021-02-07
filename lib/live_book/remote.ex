defmodule LiveBook.Remote do
  @required_modules [LiveBook.Evaluator, LiveBook.Evaluator.IOProxy, LiveBook.EvaluatorSupervisor]

  def initialize(node) do
    load_required_modules(node)
    start_supervisor(node)
    :ok
  end

  def deinitialize(node) do
    unload_required_modules(node)
    stop_supervisor(node)
    :ok
  end

  defp load_required_modules(node) do
    for module <- @required_modules do
      {_module, binary, filename} = :code.get_object_code(module)
      {:module, _} = :rpc.call(node, :code, :load_binary, [module, filename, binary])
    end
  end

  defp unload_required_modules(node) do
    for module <- @required_modules do
      :rpc.call(node, :code, :delete, [module])
      :rpc.call(node, :code, :purge, [module])
    end
  end

  defp start_supervisor(node) do
    children = [LiveBook.EvaluatorSupervisor]
    opts = [strategy: :one_for_one, name: LiveBook.Remote.Supervisor]
    {:ok, _pid} = :rpc.block_call(node, Supervisor, :start_link, [children, opts])
  end

  defp stop_supervisor(node) do
    :rpc.call(node, Supervisor, :stop, [LiveBook.Remote.Supervisor, :normal])
  end
end
