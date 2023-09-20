defmodule Livebook.RemoteIntellisenseTest do
  use ExUnit.Case, async: false

  alias Livebook.Intellisense

  @tmp_dir "tmp/test/remote_intellisense"

  setup_all do
    {:ok, _pid, node} =
      :peer.start(%{
        name: :remote_runtime,
        args: [~c"-setcookie", Atom.to_charlist(Node.get_cookie())]
      })

    {:module, Elixir.RemoteModule, bytecode, _} =
      defmodule Elixir.RemoteModule do
        @compile {:autoload, false}
        @moduledoc """
        Remote module docs
        """

        @doc """
        Hello
        """
        def hello(message) do
          message
        end
      end

    false = Code.ensure_loaded?(RemoteModule)

    File.rm_rf!(@tmp_dir)
    File.mkdir_p!(@tmp_dir)
    File.write!("#{@tmp_dir}/Elixir.RemoteModule.beam", bytecode)

    true = :erpc.call(node, :code, :set_path, [:code.get_path() ++ [~c"#{@tmp_dir}"]])
    {:ok, _} = :erpc.call(node, :application, :ensure_all_started, [:elixir])
    {:module, RemoteModule} = :erpc.call(node, :code, :load_file, [:"Elixir.RemoteModule"])
    :loaded = :erpc.call(node, :code, :module_status, [:"Elixir.RemoteModule"])

    [node: node]
  end

  defmacrop eval(node \\ nil, do: block) do
    quote do
      block = unquote(Macro.escape(block))
      binding = []
      env = Code.env_for_eval([])
      {value, binding, env} = Code.eval_quoted_with_env(block, binding, env)
      node = unquote(node) || node()

      %{
        env: env,
        map_binding: fn fun -> fun.(binding) end,
        node: node
      }
    end
  end

  test "do not find the RemoteModule inside the Livebook node" do
    context = eval(do: nil)

    assert [] == Intellisense.get_completion_items("RemoteModule", context)
  end

  test "find the RemoteModule inside the remote node and its docs", %{node: node} do
    context = eval(node, do: nil)

    assert %{
             label: "RemoteModule",
             kind: :module,
             detail: "module",
             documentation: "Remote module docs",
             insert_text: "RemoteModule"
           } in Intellisense.get_completion_items("RemoteModule", context)
  end
end
