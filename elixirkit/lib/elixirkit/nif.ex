defmodule ElixirKit.NIF do
  @moduledoc false
  @on_load {:__init__, 0}

  def __init__ do
    cc_target =
      case :erlang.system_info(:system_architecture) do
        ~c"aarch64-apple-darwin" ++ _ -> "arm64-apple-macos11.0"
        ~c"x86_64-apple-darwin" ++ _ -> "x86_64-apple-macos11.0"
      end

    :erlang.load_nif(Application.app_dir(:elixirkit, "priv/#{cc_target}/elixirkit_nif"), 0)
  end

  def publish(_name, _data) do
    :erlang.nif_error("NIF library not loaded")
  end
end
