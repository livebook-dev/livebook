defmodule Livebook.Runtime.CodeTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime

  @kino {:kino, "~> 0.5.0"}

  describe "add_mix_deps/2" do
    test "prepends Mix.install/2 call if there is none" do
      assert Runtime.Code.add_mix_deps("", [@kino]) ==
               {:ok,
                """
                Mix.install([
                  {:kino, "~> 0.5.0"}
                ])\
                """}

      assert Runtime.Code.add_mix_deps("# Comment", [@kino]) ==
               {:ok,
                """
                Mix.install([
                  {:kino, "~> 0.5.0"}
                ])

                # Comment\
                """}

      assert Runtime.Code.add_mix_deps(
               """
               # Outer comment
               for key <- [:key1, :key2] do
                 # Inner comment
                 Application.put_env(:app, key, :value)
               end

               # Final comment\
               """,
               [@kino]
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
      assert Runtime.Code.add_mix_deps(
               """
               Mix.install([
                 {:req, "~> 0.2.0"}
               ])\
               """,
               [@kino]
             ) ==
               {:ok,
                """
                Mix.install([
                  {:req, "~> 0.2.0"},
                  {:kino, "~> 0.5.0"}
                ])\
                """}

      assert Runtime.Code.add_mix_deps(
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
               [@kino]
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

      assert Runtime.Code.add_mix_deps(code, [@kino]) == {:ok, code}

      code = """
      Mix.install([
        {:kino, "~> 0.5.2", runtime: false}
      ])\
      """

      assert Runtime.Code.add_mix_deps(code, [@kino]) == {:ok, code}
    end

    test "given multiple dependencies adds the missing ones" do
      assert Runtime.Code.add_mix_deps(
               """
               Mix.install([
                 {:kino, "~> 0.5.2"}
               ])\
               """,
               [{:vega_lite, "~> 0.1.3"}, {:kino, "~> 0.5.0"}, {:req, "~> 0.2.0"}]
             ) ==
               {:ok,
                """
                Mix.install([
                  {:kino, "~> 0.5.2"},
                  {:vega_lite, "~> 0.1.3"},
                  {:req, "~> 0.2.0"}
                ])\
                """}

      code = """
      Mix.install([
        {:kino, "~> 0.5.2", runtime: false}
      ])\
      """

      assert Runtime.Code.add_mix_deps(code, [@kino]) == {:ok, code}
    end

    test "returns an error if the code has a syntax error" do
      assert Runtime.Code.add_mix_deps(
               """
               # Comment
               [,1]
               """,
               [@kino]
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
