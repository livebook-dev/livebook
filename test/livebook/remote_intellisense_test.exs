defmodule Livebook.RemoteIntellisenseTest do
  use ExUnit.Case, async: true
  alias Livebook.Intellisense

  @moduletag :with_epmd

  # Returns intellisense context resulting from evaluating
  # the given block of code in a fresh context.
  defmacrop eval(do: block) do
    quote do
      block = unquote(Macro.escape(block))
      binding = []
      env = Code.env_for_eval([])
      {value, binding, env} = Code.eval_quoted_with_env(block, binding, env)

      %{
        env: env,
        map_binding: fn fun -> fun.(binding) end
      }
    end
  end

  setup_all do
    # We use the standalone runtime to mimic a remote node. Note that
    # in the past we used :peer.start, but it was often failing on CI
    # (the start was timing out)

    {:ok, runtime} = Livebook.Runtime.ElixirStandalone.new() |> Livebook.Runtime.connect()

    parent = self()

    runtime_owner_pid =
      start_supervised!({
        Task,
        fn ->
          Livebook.Runtime.take_ownership(runtime)

          code =
            ~S'''
            defmodule RemoteModule do
              @moduledoc """
              RemoteModule module docs
              """

              @doc """
              Hello doc
              """
              def hello(message) do
                message
              end
            end
            '''

          Livebook.Runtime.evaluate_code(runtime, :elixir, code, {:c1, :e1}, [])

          receive do: ({:runtime_evaluation_response, :e1, _, _} -> :ok)
          send(parent, :continue)

          Process.sleep(:infinity)
        end
      })

    receive do: (:continue -> :ok)

    on_exit(fn ->
      Process.exit(runtime_owner_pid, :kill)
    end)

    [node: runtime.node]
  end

  describe "intellisense completion for remote nodes" do
    test "do not find the RemoteModule inside the Livebook node" do
      context = eval(do: nil)
      assert [] == Intellisense.get_completion_items("RemoteModule", context, node())
    end

    test "find the RemoteModule and its docs", %{node: node} do
      context = eval(do: nil)

      assert %{
               label: "RemoteModule",
               kind: :module,
               documentation: """
               No documentation available

               (module)\
               """,
               insert_text: "RemoteModule"
             } in Intellisense.get_completion_items("RemoteModule", context, node)
    end

    test "find RemoteModule exported functions and its docs", %{node: node} do
      context = eval(do: nil)

      assert %{
               label: "hello/1",
               kind: :function,
               documentation: "No documentation available",
               insert_text: "hello(${})"
             } in Intellisense.get_completion_items("RemoteModule.hel", context, node)
    end

    @tag :erl_docs
    test "find modules from apps", %{node: node} do
      context = eval(do: nil)

      assert [
               %{
                 label: "unsubscribe/1",
                 kind: :function,
                 documentation: "No documentation available",
                 insert_text: "unsubscribe(${})"
               }
             ] = Intellisense.get_completion_items(":mnesia.unsub", context, node)
    end

    test "get details", %{node: node} do
      context = eval(do: nil)

      assert %{contents: [content]} = Intellisense.get_details("RemoteModule", 6, context, node)
      assert content =~ "No documentation available"
    end
  end
end
