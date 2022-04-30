defmodule Livebook.Runtime.DependenciesTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime.Dependencies

  doctest Dependencies

  @jason {:jason, "~> 1.3.0"}

  describe "add_mix_deps/2" do
    test "prepends Mix.install/2 call if there is none" do
      assert Dependencies.add_mix_deps("", [@jason]) ==
               {:ok,
                """
                Mix.install([
                  {:jason, "~> 1.3.0"}
                ])\
                """}

      assert Dependencies.add_mix_deps("# Comment", [@jason]) ==
               {:ok,
                """
                Mix.install([
                  {:jason, "~> 1.3.0"}
                ])

                # Comment\
                """}

      assert Dependencies.add_mix_deps(
               """
               # Outer comment
               for key <- [:key1, :key2] do
                 # Inner comment
                 Application.put_env(:app, key, :value)
               end

               # Final comment\
               """,
               [@jason]
             ) ==
               {:ok,
                """
                Mix.install([
                  {:jason, "~> 1.3.0"}
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
      assert Dependencies.add_mix_deps(
               """
               Mix.install([
                 {:req, "~> 0.2.0"}
               ])\
               """,
               [@jason]
             ) ==
               {:ok,
                """
                Mix.install([
                  {:req, "~> 0.2.0"},
                  {:jason, "~> 1.3.0"}
                ])\
                """}

      assert Dependencies.add_mix_deps(
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
               [@jason]
             ) ==
               {:ok,
                """
                # Outer comment
                Mix.install([
                  # Inner comment leading
                  {:req, "~> 0.2.0"},
                  {:jason, "~> 1.3.0"}
                  # Inner comment trailing
                ])

                # Result
                :ok\
                """}

      assert Dependencies.add_mix_deps(
               """
               Mix.install(
                 [
                   {:req, "~> 0.2.0"}
                 ],
                 system_env: [
                   # {"XLA_TARGET", "cuda111"}
                 ]
               )\
               """,
               [@jason]
             ) ==
               {:ok,
                """
                Mix.install(
                  [
                    {:req, "~> 0.2.0"},
                    {:jason, "~> 1.3.0"}
                  ],
                  system_env: [
                    # {"XLA_TARGET", "cuda111"}
                  ]
                )\
                """}
    end

    test "does not add the dependency if it already exists" do
      code = """
      Mix.install([
        {:jason, "~> 1.3.0"}
      ])\
      """

      assert Dependencies.add_mix_deps(code, [@jason]) == {:ok, code}

      code = """
      Mix.install([
        {:jason, "~> 1.3.0", runtime: false}
      ])\
      """

      assert Dependencies.add_mix_deps(code, [@jason]) == {:ok, code}
    end

    test "given multiple dependencies adds the missing ones" do
      assert Dependencies.add_mix_deps(
               """
               Mix.install([
                 {:jason, "~> 1.3.0"}
               ])\
               """,
               [{:vega_lite, "~> 0.1.3"}, {:jason, "~> 1.3.0"}, {:req, "~> 0.2.0"}]
             ) ==
               {:ok,
                """
                Mix.install([
                  {:jason, "~> 1.3.0"},
                  {:vega_lite, "~> 0.1.3"},
                  {:req, "~> 0.2.0"}
                ])\
                """}

      code = """
      Mix.install([
        {:jason, "~> 1.3.0", runtime: false}
      ])\
      """

      assert Dependencies.add_mix_deps(code, [@jason]) == {:ok, code}
    end

    test "returns an error if the code has a syntax error" do
      assert Dependencies.add_mix_deps(
               """
               # Comment
               [,1]
               """,
               [@jason]
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

  describe "search_hex/2" do
    setup do
      bypass = Bypass.open()
      {:ok, bypass: bypass}
    end

    test "parses the response into a list of packages", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/api/packages", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, ~S"""
        [
          {
            "configs": {
              "erlang.mk": "dep_ecto = hex 3.7.2",
              "mix.exs": "{:ecto, \"~> 3.7\"}",
              "rebar.config": "{ecto, \"3.7.2\"}"
            },
            "docs_html_url": "https://hexdocs.pm/ecto/",
            "html_url": "https://hex.pm/packages/ecto",
            "latest_stable_version": "3.7.2",
            "latest_version": "3.7.2",
            "meta": {
              "description": "A toolkit for data mapping and language integrated query for Elixir",
              "licenses": ["Apache-2.0"],
              "links": { "GitHub": "https://github.com/elixir-ecto/ecto" }
            },
            "name": "ecto",
            "url": "https://hex.pm/api/packages/ecto"
          },
          {
            "configs": {
              "erlang.mk": "dep_ecto_sql = hex 3.7.2",
              "mix.exs": "{:ecto_sql, \"~> 3.7\"}",
              "rebar.config": "{ecto_sql, \"3.7.2\"}"
            },
            "docs_html_url": "https://hexdocs.pm/ecto_sql/",
            "html_url": "https://hex.pm/packages/ecto_sql",
            "latest_stable_version": "3.7.2",
            "latest_version": "3.7.2",
            "meta": {
              "description": "SQL-based adapters for Ecto and database migrations",
              "licenses": ["Apache-2.0"],
              "links": { "GitHub": "https://github.com/elixir-ecto/ecto_sql" }
            },
            "name": "ecto_sql",
            "url": "https://hex.pm/api/packages/ecto_sql"
          }
        ]
        """)
      end)

      api_url = api_url(bypass.port)

      assert Dependencies.search_hex("ecto", api_url: api_url) ==
               {:ok,
                [
                  %{
                    dependency: {:ecto, "~> 3.7"},
                    description:
                      "A toolkit for data mapping and language integrated query for Elixir",
                    name: "ecto",
                    url: "https://hex.pm/packages/ecto",
                    version: "3.7.2"
                  },
                  %{
                    dependency: {:ecto_sql, "~> 3.7"},
                    description: "SQL-based adapters for Ecto and database migrations",
                    name: "ecto_sql",
                    url: "https://hex.pm/packages/ecto_sql",
                    version: "3.7.2"
                  }
                ]}
    end

    test "returns an error on unsuccessful API response", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/api/packages", fn conn ->
        Plug.Conn.resp(conn, 500, "Error")
      end)

      api_url = api_url(bypass.port)

      assert Dependencies.search_hex("ecto", api_url: api_url) == {:error, "unexpected response"}
    end

    test "returns an empty list for an empty search", %{bypass: bypass} do
      api_url = api_url(bypass.port)

      assert Dependencies.search_hex("", api_url: api_url) == {:ok, []}
    end
  end

  defp api_url(port), do: "http://localhost:#{port}/api"
end
