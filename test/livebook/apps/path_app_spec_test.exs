defmodule Livebook.Apps.PathAppSpecTest do
  use ExUnit.Case, async: true

  import Livebook.TestHelpers

  describe "load" do
    @tag :tmp_dir
    test "returns import warnings", %{tmp_dir: tmp_dir} do
      [app_dir, file_tmp_path] = create_subdirs!(tmp_dir, 2)

      app_path = Path.join(app_dir, "app.livemd")

      slug = "app"

      File.write!(app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"}} -->

      # App

      ```elixir
      """)

      app_spec = %Livebook.Apps.PathAppSpec{slug: slug, path: app_path}

      assert {:ok, %{warnings: [warning]}} = Livebook.Apps.AppSpec.load(app_spec, file_tmp_path)
      assert warning =~ "line 5 - fenced Code Block opened with"
    end

    @tag :tmp_dir
    test "copies files use by the notebook", %{tmp_dir: tmp_dir} do
      [app_dir, file_tmp_path] = create_subdirs!(tmp_dir, 2)

      app_path = Path.join(app_dir, "app.livemd")

      files_path = Path.join(app_dir, "files")
      File.mkdir_p!(files_path)
      files_path |> Path.join("image1.jpg") |> File.write!("content")
      files_path |> Path.join("image2.jpg") |> File.write!("content")

      slug = "app"

      File.write!(app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"},"file_entries":[{"name":"image1.jpg","type":"attachment"}]} -->

      # App
      """)

      app_spec = %Livebook.Apps.PathAppSpec{slug: slug, path: app_path}

      assert {:ok, %{notebook: _}} = Livebook.Apps.AppSpec.load(app_spec, file_tmp_path)

      assert File.ls!(file_tmp_path) == ["image1.jpg"]
    end

    @tag :tmp_dir
    test "returns error when file does not exist", %{tmp_dir: tmp_dir} do
      [app_dir, file_tmp_path] = create_subdirs!(tmp_dir, 2)

      app_path = Path.join(app_dir, "app.livemd")

      slug = "app"

      app_spec = %Livebook.Apps.PathAppSpec{slug: slug, path: app_path}

      assert {:error, message} = Livebook.Apps.AppSpec.load(app_spec, file_tmp_path)
      assert message =~ "no such file or directory"
    end

    @tag :tmp_dir
    test "returns error when one of the notebook files is missing", %{tmp_dir: tmp_dir} do
      [app_dir, file_tmp_path] = create_subdirs!(tmp_dir, 2)

      app_path = Path.join(app_dir, "app.livemd")

      slug = "app"

      File.write!(app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"},"file_entries":[{"name":"image.jpg","type":"attachment"}]} -->

      # App
      """)

      app_spec = %Livebook.Apps.PathAppSpec{slug: slug, path: app_path}

      assert {:error, message} = Livebook.Apps.AppSpec.load(app_spec, file_tmp_path)
      assert message == "failed to copy notebook file image.jpg, no such file or directory"
    end
  end
end
