defmodule Livebook.Runtime.CodeTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime

  @dep {:kino, "~> 0.5.0"}

  describe "add_mix_dependency/2" do
    test "prepends Mix.install/2 call if there is none" do
      assert Runtime.Code.add_mix_dependency("", @dep) ==
               {:ok,
                """
                Mix.install([
                  {:kino, "~> 0.5.0"}
                ])\
                """}

      assert Runtime.Code.add_mix_dependency("# Comment", @dep) ==
               {:ok,
                """
                Mix.install([
                  {:kino, "~> 0.5.0"}
                ])

                # Comment\
                """}

      assert Runtime.Code.add_mix_dependency(
               """
               # Outer comment
               for key <- [:key1, :key2] do
                 # Inner comment
                 Application.put_env(:app, key, :value)
               end

               # Final comment\
               """,
               @dep
             ) ==
               {:ok,
                """
                Mix.install([
                  {:kino, "~> 0.5.0"}
                ])

                # Outer comment
                for key <- [:key1, :key2] do
                  # Inner comment
                  Application.put_env(:app, key, :value)
                end

                # Final comment\
                """}
    end

    test "appends dependency to an existing Mix.install/2 call" do
      assert Runtime.Code.add_mix_dependency(
               """
               Mix.install([
                 {:req, "~> 0.2.0"}
               ])\
               """,
               @dep
             ) ==
               {:ok,
                """
                Mix.install([
                  {:req, "~> 0.2.0"},
                  {:kino, "~> 0.5.0"}
                ])\
                """}

      assert Runtime.Code.add_mix_dependency(
               """
               # Outer comment
               Mix.install([
                 # Inner comment leading
                 {:req, "~> 0.2.0"}
                 # Inner comment trailing
               ])

               # Result
               :ok\
               """,
               @dep
             ) ==
               {:ok,
                """
                # Outer comment
                Mix.install([
                  # Inner comment leading
                  {:req, "~> 0.2.0"},
                  {:kino, "~> 0.5.0"}
                  # Inner comment trailing
                ])

                # Result
                :ok\
                """}
    end

    test "does not add the dependency if it already exists" do
      code = """
      Mix.install([
        {:kino, "~> 0.5.2"}
      ])\
      """

      assert Runtime.Code.add_mix_dependency(code, @dep) == {:ok, code}

      code = """
      Mix.install([
        {:kino, "~> 0.5.2", runtime: false}
      ])\
      """

      assert Runtime.Code.add_mix_dependency(code, @dep) == {:ok, code}
    end

    test "returns an error if the code has a syntax error" do
      assert Runtime.Code.add_mix_dependency(
               """
               # Comment
               [,1]
               """,
               @dep
             ) ==
               {:error,
                """
                ** (SyntaxError) nofile:2:2: syntax error before: ','
                    |
                  2 | [,1]
                    |  ^\
                """}
    end
  end
end
