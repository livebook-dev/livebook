defmodule LivebookWeb.ExploreLiveTest do
  use LivebookWeb.ConnCase

  import Phoenix.LiveViewTest

  alias Livebook.{SessionSupervisor, Session}

  describe "explore.json" do
    test "explore file exists" do
      assert File.exists?(rewrite_path("explore.json"))
    end

    test "explore file is valid json" do
      assert {:ok, _} =
               rewrite_path("explore.json")
               |> File.read!()
               |> Jason.decode()
    end

    test "all example records are valid" do
      notebooks =
        rewrite_path("explore.json")
        |> File.read!()
        |> Jason.decode!(keys: :atoms)

      assert Enum.all?(notebooks, fn notebook ->
               Enum.all?([:title, :snippet, :path, :type], fn atom ->
                 Map.has_key?(notebook, atom)
               end)
             end)
    end

    test "welcome is present in explore file" do
      assert "explore.json"
             |> rewrite_path()
             |> File.read!()
             |> Jason.decode!(keys: :atoms)
             |> Enum.find(fn notebook -> notebook.title == "Welcome to Livebook" end)
    end
  end

  describe "opening an example" do
    test "correctly creates a session", %{conn: conn} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "session")

      {:ok, view, _} = live(conn, "/explore")

      assert {:error, {:live_redirect, %{to: to}}} =
               view
               |> element(~s{[phx-click="open"]}, "Welcome")
               |> render_click()

      assert to =~ "/sessions/"

      {:ok, view, _} = live(conn, to)
      assert render(view) =~ "Welcome"
    end

    #    test "from local file correctly creates a session", %{conn: conn} do
    #      notebook = %{
    #        title: "Foobar",
    #        snippet: "Janfu",
    #        path: rewrite_path("example_notebooks/welcome.livemd"),
    #        type: "path"
    #      }
    #
    #      assert {:error, {:live_redirect, %{to: to}}} =
    #               render_component(LivebookWeb.ExploreLive.NotebookComponent, notebook: notebook)
    #               |> element(~s{[phx-value-click="open"]})
    #               |> render_click()
    #
    #      assert to =~ "/sessions/"
    #
    #      {:ok, view, _} = live(conn, to)
    #      assert render(view) =~ "Welcome to Livebook"
    #    end
    #
    #    test "from online file correctly creates a session", %{conn: conn} do
    #      notebook = %{
    #        title: "Foobar",
    #        snippet: "Janfu",
    #        path: "https://github.com/elixir-nx/axon/blob/main/notebooks/mnist.livemd",
    #        type: "url"
    #      }
    #
    #      assert {:error, {:live_redirect, %{to: to}}} =
    #               render_component(LivebookWeb.ExploreLive.NotebookComponent, notebook: notebook)
    #               |> element(~s{[phx-click="open"]})
    #               |> render_click()
    #
    #      assert to =~ "/sessions/"
    #
    #      {:ok, view, _} = live(conn, to)
    #      assert render(view) =~ "MNIST"
    #    end
  end

  defp rewrite_path(path) do
    Path.join(Livebook.Config.root_path(), path)
  end
end
