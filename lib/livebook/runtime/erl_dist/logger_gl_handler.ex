defmodule Livebook.Runtime.ErlDist.LoggerGLHandler do
  @moduledoc false

  @doc false
  def log(%{meta: meta} = event, %{formatter: {formatter_module, formatter_config}}) do
    message = apply(formatter_module, :format, [event, formatter_config])

    if io_proxy?(meta.gl) do
      async_io(meta.gl, message)
    else
      send(Livebook.Runtime.ErlDist.NodeManager, {:orphan_log, message})
    end
  end

  defp io_proxy?(pid) do
    try do
      info = Process.info(pid, [:dictionary])
      info[:dictionary][:"$initial_call"] == {Livebook.Runtime.Evaluator.IOProxy, :init, 1}
    rescue
      _ -> false
    end
  end

  def async_io(device, output) when is_pid(device) do
    send(device, {:io_request, self(), make_ref(), {:put_chars, :unicode, output}})
  end
end
