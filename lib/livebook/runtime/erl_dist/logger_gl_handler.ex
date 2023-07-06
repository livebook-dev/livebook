defmodule Livebook.Runtime.ErlDist.LoggerGLHandler do
  @moduledoc false

  @doc false
  def log(%{meta: meta} = event, %{formatter: {formatter_module, formatter_config}}) do
    message = apply(formatter_module, :format, [event, formatter_config])

    if Livebook.Runtime.ErlDist.NodeManager.known_io_proxy?(meta.gl) do
      async_io(meta.gl, message)
    else
      send(Livebook.Runtime.ErlDist.NodeManager, {:orphan_log, message})
    end
  end

  def async_io(device, output) when is_pid(device) do
    reply_to = Livebook.Runtime.ErlDist.Sink.pid()
    send(device, {:io_request, reply_to, make_ref(), {:put_chars, :unicode, output}})
  end

  @doc false
  def filter_code_server_logs(%{msg: msg} = event, _) do
    # During intellisense we check if certain modules are loaded. If
    # the module name is miscapitalized, such as "Io", and we are on
    # a case insensitive file system, this results in a log error
    # message: "Error loading module 'Elixir.Io'". We want to ignore
    # such logs

    with {~c"~s~n", [content]} when is_list(content) <- msg,
         true <- :string.str(content, ~c"Error loading module") > 0,
         true <- :string.str(content, ~c"module name in object code is") > 0 do
      :stop
    else
      _ -> event
    end
  end
end
