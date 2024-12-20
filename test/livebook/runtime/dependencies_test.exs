defmodule Livebook.Runtime.DependenciesTest do
  use ExUnit.Case, async: true

  import Livebook.TestHelpers

  alias Livebook.Runtime.Dependencies

  doctest Dependencies

  @req {:req, "~> 0.5.0"}

  describe "add_dependencies/3" do
    test "adds dependencies and config" do
      assert Dependencies.add_dependencies("", [
               %{dep: {:nx, "~> 0.4.0"}, config: []},
               %{
                 dep: {:exla, "~> 0.4.0"},
                 config: [nx: [default_defn_options: [compiler: EXLA]]]
               },
               %{dep: {:torchx, "~> 0.4.0"}, config: [nx: [default_backend: Torchx.Backend]]}
             ]) ==
               {:ok,
                """
                Mix.install(
                  [
                    {:nx, "~> 0.4.0"},
                    {:exla, "~> 0.4.0"},
                    {:torchx, "~> 0.4.0"}
                  ],
                  config: [nx: [default_defn_options: [compiler: EXLA], default_backend: Torchx.Backend]]
                )\
                """}
    end
  end

  describe "add_mix_deps/3" do
    test "prepends Mix.install/2 call if there is none" do
      assert Dependencies.add_mix_deps("", [@req]) ==
               {:ok,
                """
                Mix.install([
                  {:req, "~> 0.5.0"}
                ])\
                """}

      assert Dependencies.add_mix_deps("# Comment", [@req]) ==
               {:ok,
                """
                Mix.install([
                  {:req, "~> 0.5.0"}
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
               [@req]
             ) ==
               {:ok,
                """
                Mix.install([
                  {:req, "~> 0.5.0"}
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
                 {:kino, "~> 0.14.0"}
               ])\
               """,
               [@req]
             ) ==
               {:ok,
                """
                Mix.install([
                  {:kino, "~> 0.14.0"},
                  {:req, "~> 0.5.0"}
                ])\
                """}

      assert Dependencies.add_mix_deps(
               """
               # Outer comment
               Mix.install([
                 # Inner comment leading
                 {:kino, "~> 0.14.0"}
                 # Inner comment trailing
               ])

               # Result
               :ok\
               """,
               [@req]
             ) ==
               {:ok,
                """
                # Outer comment
                Mix.install([
                  # Inner comment leading
                  {:kino, "~> 0.14.0"},
                  {:req, "~> 0.5.0"}
                  # Inner comment trailing
                ])

                # Result
                :ok\
                """}

      assert Dependencies.add_mix_deps(
               """
               Mix.install(
                 [
                  {:kino, "~> 0.14.0"}
                 ],
                 system_env: [
                   # {"XLA_TARGET", "cuda111"}
                 ]
               )\
               """,
               [@req]
             ) ==
               {:ok,
                """
                Mix.install(
                  [
                    {:kino, "~> 0.14.0"},
                    {:req, "~> 0.5.0"}
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
        {:req, "~> 0.5.0"}
      ])\
      """

      assert Dependencies.add_mix_deps(code, [@req]) == {:ok, code}

      code = """
      Mix.install([
        {:req, "~> 0.5.0", runtime: false}
      ])\
      """

      assert Dependencies.add_mix_deps(code, [@req]) == {:ok, code}
    end

    test "given multiple dependencies adds the missing ones" do
      assert Dependencies.add_mix_deps(
               """
               Mix.install([
                 {:req, "~> 0.5.0"}
               ])\
               """,
               [{:vega_lite, "~> 0.1.3"}, {:req, "~> 0.5.0"}, {:kino, "~> 0.14.0"}]
             ) ==
               {:ok,
                """
                Mix.install([
                  {:req, "~> 0.5.0"},
                  {:vega_lite, "~> 0.1.3"},
                  {:kino, "~> 0.14.0"}
                ])\
                """}

      code = """
      Mix.install([
        {:req, "~> 0.5.0", runtime: false}
      ])\
      """

      assert Dependencies.add_mix_deps(code, [@req]) == {:ok, code}
    end

    test "returns an error if the code has a syntax error" do
      {:error, message} =
        Dependencies.add_mix_deps(
          """
          # Comment
          [,1]
          """,
          [@req]
        )

      assert clean_message(message) ==
               """
               ** (SyntaxError) invalid syntax found on nofile:2:2:
                   error: syntax error before: ','
                   │
                 2 │ [,1]
                   │  ^
                   │
                   └─ nofile:2:2\
               """
    end

    test "adds config if specified" do
      config = [nx: [default_backend: EXLA.Backend]]

      assert Dependencies.add_mix_deps("", [@req], config) ==
               {:ok,
                """
                Mix.install(
                  [
                    {:req, "~> 0.5.0"}
                  ],
                  config: [nx: [default_backend: EXLA.Backend]]
                )\
                """}

      assert Dependencies.add_mix_deps(
               """
               Mix.install([
                 {:req, "~> 0.5.0"}
               ])\
               """,
               [],
               config
             ) ==
               {:ok,
                """
                Mix.install(
                  [
                    {:req, "~> 0.5.0"}
                  ],
                  config: [nx: [default_backend: EXLA.Backend]]
                )\
                """}
    end

    test "merges config in flat manner" do
      assert Dependencies.add_mix_deps(
               """
               Mix.install(
                 [
                   {:req, "~> 0.5.0"}
                 ],
                 config: [
                   # Comment 1
                   nx: [
                     # Comment 2
                     default_backend: Torchx.Backend
                     # Comment 3
                   ],
                   test: [x: :y]
                   # Comment 4
                 ]
               )\
               """,
               [],
               nx: [
                 default_defn_options: [compiler: EXLA]
               ],
               other: [
                 default_defn_options: [compiler: EXLA]
               ]
             ) ==
               {:ok,
                """
                Mix.install(
                  [
                    {:req, "~> 0.5.0"}
                  ],
                  config: [
                    # Comment 1
                    nx: [
                      # Comment 2
                      default_backend: Torchx.Backend
                      # Comment 3
                    ],
                    test: [x: :y],
                    other: [default_defn_options: [compiler: EXLA]]
                    # Comment 4
                  ]
                )\
                """}

      assert Dependencies.add_mix_deps(
               """
               Mix.install(
                 [
                   {:req, "~> 0.5.0"}
                 ],
                 [config: []]
               )\
               """,
               [],
               nx: [default_backend: EXLA.Backend]
             ) ==
               {:ok,
                """
                Mix.install(
                  [
                    {:req, "~> 0.5.0"}
                  ],
                  config: [nx: [default_backend: EXLA.Backend]]
                )\
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
                    dependency: %{dep: {:ecto, "~> 3.7"}, config: []},
                    description:
                      "A toolkit for data mapping and language integrated query for Elixir",
                    name: "ecto",
                    url: "https://hex.pm/packages/ecto",
                    version: "3.7.2"
                  },
                  %{
                    dependency: %{dep: {:ecto_sql, "~> 3.7"}, config: []},
                    description: "SQL-based adapters for Ecto and database migrations",
                    name: "ecto_sql",
                    url: "https://hex.pm/packages/ecto_sql",
                    version: "3.7.2"
                  }
                ]}
    end

    test "lists a package matching exactly first", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/api/packages", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/json")
        |> Plug.Conn.resp(200, ~S"""
        [
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
          },
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
          }
        ]
        """)
      end)

      api_url = api_url(bypass.port)

      assert {:ok, [%{name: "ecto"}, %{name: "ecto_sql"}]} =
               Dependencies.search_hex("ecto", api_url: api_url)
    end

    test "returns an error on unsuccessful API response", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/api/packages", fn conn ->
        Plug.Conn.resp(conn, 404, "Error")
      end)

      api_url = api_url(bypass.port)

      assert Dependencies.search_hex("ecto", api_url: api_url) ==
               {:error, "unexpected response, HTTP status 404"}
    end

    test "returns an empty list for an empty search", %{bypass: bypass} do
      api_url = api_url(bypass.port)

      assert Dependencies.search_hex("", api_url: api_url) == {:ok, []}
    end
  end

  defp api_url(port), do: "http://localhost:#{port}/api"
end
