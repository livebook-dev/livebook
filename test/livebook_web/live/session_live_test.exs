defmodule LivebookWeb.SessionLiveTest do
  use LivebookWeb.ConnCase, async: true

  import Livebook.SessionHelpers
  import Livebook.TestHelpers
  import Phoenix.LiveViewTest

  alias Livebook.{Sessions, Session, Settings, Runtime, FileSystem}
  alias Livebook.Notebook.Cell

  setup do
    {:ok, session} = Sessions.create_session(notebook: Livebook.Notebook.new())

    on_exit(fn ->
      Session.close(session.pid)
    end)

    %{session: session}
  end

  test "disconnected and connected render", %{conn: conn, session: session} do
    {:ok, view, disconnected_html} = live(conn, ~p"/sessions/#{session.id}")
    assert disconnected_html =~ "Untitled notebook"
    assert render(view) =~ "Untitled notebook"
  end

  describe "asynchronous updates" do
    test "renders an updated notebook name", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      Session.set_notebook_name(session.pid, "My notebook")
      wait_for_session_update(session.pid)

      assert render(view) =~ "My notebook"
    end

    test "renders an updated notebook name in title", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert page_title(view) =~ "Untitled notebook"

      Session.set_notebook_name(session.pid, "My notebook")
      wait_for_session_update(session.pid)

      # Wait for LV to update
      _ = render(view)

      assert page_title(view) =~ "My notebook"
    end

    test "renders a newly inserted section", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      section_id = insert_section(session.pid)

      assert render(view) =~ section_id
    end

    test "renders an updated section name", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      Session.set_section_name(session.pid, section_id, "My section")
      wait_for_session_update(session.pid)

      assert render(view) =~ "My section"
    end

    test "renders a newly inserted cell", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      cell_id = insert_text_cell(session.pid, section_id, :markdown)

      assert render(view) =~ "cell-" <> cell_id
    end

    test "un-renders a deleted cell", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :markdown)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      Session.delete_cell(session.pid, cell_id)
      wait_for_session_update(session.pid)

      refute render(view) =~ "cell-" <> cell_id
    end
  end

  describe "UI events update the shared state" do
    test "adding a new section", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element("button", "New section")
      |> render_click()

      assert %{notebook: %{sections: [_section]}} = Session.get_data(session.pid)
    end

    test "adding a new cell", %{conn: conn, session: session} do
      Session.insert_section(session.pid, 0)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element("button", "Markdown")
      |> render_click()

      assert %{notebook: %{sections: [%{cells: [%Cell.Markdown{}]}]}} =
               Session.get_data(session.pid)
    end

    test "queueing cell evaluation", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      section_id = insert_section(session.pid)
      {source, continue_fun} = source_for_blocking()
      cell_id = insert_text_cell(session.pid, section_id, :code, source)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert %{cell_infos: %{^cell_id => %{eval: %{status: :evaluating}}}} =
               Session.get_data(session.pid)

      continue_fun.()
    end

    test "reevaluting the setup cell with dependencies cache disabled",
         %{conn: conn, session: session} do
      Session.subscribe(session.id)

      # Use the standalone runtime, to encapsulate env var changes
      Session.set_runtime(session.pid, Runtime.Standalone.new())

      evaluate_setup(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{
        "cell_id" => "setup",
        "disable_dependencies_cache" => true
      })

      section_id = insert_section(session.pid)

      cell_id =
        insert_text_cell(session.pid, section_id, :code, ~s/System.get_env("MIX_INSTALL_FORCE")/)

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id,
                       terminal_text("\e[32m\"true\"\e[0m"), _}}
    end

    test "cancelling cell evaluation", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code, "Process.sleep(2000)")

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      view
      |> element(~s{[data-el-session]})
      |> render_hook("cancel_cell_evaluation", %{"cell_id" => cell_id})

      assert %{cell_infos: %{^cell_id => %{eval: %{status: :ready}}}} =
               Session.get_data(session.pid)
    end

    test "setting cell to always reevaluating", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element("#cell-#{cell_id}-evaluation-menu button", "Reevaluate automatically")
      |> render_click()

      assert %{notebook: %{sections: [%{cells: [%Cell.Code{reevaluate_automatically: true}]}]}} =
               Session.get_data(session.pid)
    end

    test "inserting a cell below the given cell", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("insert_cell_below", %{"cell_id" => cell_id, "type" => "markdown"})

      assert %{notebook: %{sections: [%{cells: [_first_cell, %Cell.Markdown{}]}]}} =
               Session.get_data(session.pid)
    end

    test "inserting a cell at section start", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      _cell_id = insert_text_cell(session.pid, section_id, :code)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("insert_cell_below", %{"section_id" => section_id, "type" => "markdown"})

      assert %{notebook: %{sections: [%{cells: [%Cell.Markdown{}, _first_cell]}]}} =
               Session.get_data(session.pid)
    end

    test "inserting an image", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(
        ~s/[phx-click="insert_image"][phx-value-section_id="#{section_id}"][phx-value-cell_id="#{cell_id}"]/
      )
      |> render_click()

      view
      |> element(~s/#insert-image-modal form/)
      |> render_change(%{"data" => %{"name" => "image.jpg"}})

      view
      |> file_input(~s/#insert-image-modal form/, :image, [
        %{
          last_modified: 1_594_171_879_000,
          name: "image.jpg",
          content: "content",
          size: 7,
          type: "text/plain"
        }
      ])
      |> render_upload("image.jpg")

      view
      |> element(~s/#insert-image-modal form/)
      |> render_submit(%{"data" => %{"name" => "image.jpg"}})

      assert %{
               notebook: %{
                 sections: [
                   %{cells: [_first_cell, %Cell.Markdown{source: "![](files/image.jpg)"}]}
                 ]
               }
             } =
               Session.get_data(session.pid)

      assert FileSystem.File.resolve(session.files_dir, "image.jpg") |> FileSystem.File.read() ==
               {:ok, "content"}
    end

    test "inserting a file", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      %{files_dir: files_dir} = session
      image_file = FileSystem.File.resolve(files_dir, "file.bin")
      :ok = FileSystem.File.write(image_file, "content")
      Session.add_file_entries(session.pid, [%{type: :attachment, name: "file.bin"}])

      Session.subscribe(session.id)
      Session.set_runtime(session.pid, Livebook.Runtime.NoopRuntime.new(self()))
      connect_and_await_runtime(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("insert_file", %{
        "file_entry_name" => "file.bin",
        "section_id" => section_id,
        "cell_id" => cell_id
      })

      view
      |> element(~s/#insert-file-modal [phx-click]/, "Read file content")
      |> render_click()

      assert %{
               notebook: %{
                 sections: [
                   %{
                     cells: [
                       _first_cell,
                       %Cell.Code{
                         source: """
                         content =
                           Kino.FS.file_path("file.bin")
                           |> File.read!()\
                         """
                       }
                     ]
                   }
                 ]
               }
             } = Session.get_data(session.pid)
    end

    test "inserting a file as markdown image", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      %{files_dir: files_dir} = session
      image_file = FileSystem.File.resolve(files_dir, "image.jpg")
      :ok = FileSystem.File.write(image_file, "content")
      Session.add_file_entries(session.pid, [%{type: :attachment, name: "image.jpg"}])

      Session.subscribe(session.id)
      Session.set_runtime(session.pid, Livebook.Runtime.NoopRuntime.new(self()))
      connect_and_await_runtime(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("insert_file", %{
        "file_entry_name" => "image.jpg",
        "section_id" => section_id,
        "cell_id" => cell_id
      })

      view
      |> element(~s/#insert-file-modal [phx-click]/, "Insert as Markdown image")
      |> render_click()

      assert %{
               notebook: %{
                 sections: [
                   %{cells: [_first_cell, %Cell.Markdown{source: "![](files/image.jpg)"}]}
                 ]
               }
             } = Session.get_data(session.pid)
    end

    test "inserting file after file drop upload", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.subscribe(session.id)
      Session.set_runtime(session.pid, Livebook.Runtime.NoopRuntime.new(self()))
      connect_and_await_runtime(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> render_hook("handle_file_drop", %{"section_id" => section_id, "cell_id" => cell_id})

      view
      |> element(~s{#add-file-entry-form})
      |> render_change(%{"data" => %{"name" => "image.jpg"}})

      view
      |> file_input(~s{#add-file-entry-form}, :file, [
        %{
          last_modified: 1_594_171_879_000,
          name: "image.jpg",
          content: "content",
          size: 7,
          type: "text/plain"
        }
      ])
      |> render_upload("image.jpg")

      view
      |> element(~s{#add-file-entry-form})
      |> render_submit(%{"data" => %{"name" => "image.jpg"}})

      view
      |> element(~s/#insert-file-modal [phx-click]/, "Insert as Markdown image")
      |> render_click()

      assert %{
               notebook: %{
                 sections: [
                   %{cells: [_first_cell, %Cell.Markdown{source: "![](files/image.jpg)"}]}
                 ]
               }
             } = Session.get_data(session.pid)
    end

    test "deleting section with no cells requires no confirmation",
         %{conn: conn, session: session} do
      section_id = insert_section(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(
        ~s{[data-section-id="#{section_id}"] [data-el-section-headline] [aria-label="delete section"]}
      )
      |> render_click()

      assert %{notebook: %{sections: []}} = Session.get_data(session.pid)
    end

    test "deleting section with cells requires confirmation", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      insert_text_cell(session.pid, section_id, :code)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(
        ~s{[data-section-id="#{section_id}"] [data-el-section-headline] [aria-label="delete section"]}
      )
      |> render_click()

      render_confirm(view)

      assert %{notebook: %{sections: []}} = Session.get_data(session.pid)
    end

    test "deleting the given cell", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("delete_cell", %{"cell_id" => cell_id})

      render_confirm(view)

      assert %{notebook: %{sections: [%{cells: []}]}} = Session.get_data(session.pid)
    end

    test "restoring a deleted cell", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.delete_cell(session.pid, cell_id)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      refute render(view) =~ "cell-" <> cell_id

      view
      |> element(~s{a[aria-label="Bin (sb)"]})
      |> render_click()

      view
      |> element("button", "Restore")
      |> render_click()

      assert %{notebook: %{sections: [%{cells: [%{id: ^cell_id}]}]}} =
               Session.get_data(session.pid)
    end

    test "editing input field in cell output", %{conn: conn, session: session, test: test} do
      section_id = insert_section(session.pid)

      Process.register(self(), test)

      input = %{
        type: :input,
        ref: "ref1",
        id: "input1",
        destination: test,
        attrs: %{type: :number, default: 1, label: "Name", debounce: :blur}
      }

      Session.subscribe(session.id)

      insert_cell_with_output(session.pid, section_id, input)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s/[data-el-outputs-container] form/)
      |> render_change(%{"html_value" => "10"})

      assert %{input_infos: %{"input1" => %{value: 10}}} = Session.get_data(session.pid)

      assert_receive {:event, "ref1", %{value: 10, type: :change}}
    end

    test "newlines in text input are normalized", %{conn: conn, session: session, test: test} do
      section_id = insert_section(session.pid)

      Process.register(self(), test)

      input = %{
        type: :input,
        ref: "ref1",
        id: "input1",
        destination: test,
        attrs: %{
          type: :textarea,
          default: "hey",
          label: "Name",
          debounce: :blur,
          monospace: false
        }
      }

      Session.subscribe(session.id)

      insert_cell_with_output(session.pid, section_id, input)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s/[data-el-outputs-container] form/)
      |> render_change(%{"html_value" => "line\r\nline"})

      assert %{input_infos: %{"input1" => %{value: "line\nline"}}} = Session.get_data(session.pid)
    end

    test "form input changes are reflected only in local LV data",
         %{conn: conn, session: session, test: test} do
      section_id = insert_section(session.pid)

      Process.register(self(), test)

      form_control = %{
        type: :control,
        ref: "control_ref1",
        destination: test,
        attrs: %{
          type: :form,
          fields: [
            name: %{
              type: :input,
              ref: "input_ref1",
              id: "input1",
              destination: test,
              attrs: %{type: :text, default: "initial", label: "Name", debounce: :blur}
            },
            value: nil
          ],
          submit: "Send",
          report_changes: %{},
          reset_on_submit: []
        }
      }

      Session.subscribe(session.id)

      insert_cell_with_output(session.pid, section_id, form_control)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s/[data-el-outputs-container] form/)
      |> render_change(%{"html_value" => "sherlock"})

      # The new value is on the page
      assert render(view) =~ "sherlock"
      # but it's not reflected in the synchronized session data
      assert %{input_infos: %{"input1" => %{value: "initial"}}} = Session.get_data(session.pid)

      view
      |> element(~s/[data-el-outputs-container] button/, "Send")
      |> render_click()

      assert_receive {:event, "control_ref1",
                      %{data: %{name: "sherlock", value: nil}, type: :submit}}
    end

    test "file input", %{conn: conn, session: session, test: test} do
      section_id = insert_section(session.pid)

      Process.register(self(), test)

      input = %{
        type: :input,
        ref: "ref1",
        id: "input1",
        destination: test,
        attrs: %{type: :file, default: nil, label: "File", accept: :any}
      }

      Session.subscribe(session.id)

      insert_cell_with_output(session.pid, section_id, input)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> file_input(~s/[data-el-outputs-container] form/, :file, [
        %{
          last_modified: 1_594_171_879_000,
          name: "data.txt",
          content: "content",
          size: 7,
          type: "text/plain"
        }
      ])
      |> render_upload("data.txt")

      assert %{input_infos: %{"input1" => %{value: value}}} = Session.get_data(session.pid)

      assert %{file_ref: file_ref, client_name: "data.txt"} = value

      send(session.pid, {:runtime_file_path_request, self(), file_ref})
      assert_receive {:runtime_file_path_reply, {:ok, path}}
      assert File.read!(path) == "content"
    end

    test "enabling a language", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element("[data-el-language-buttons] button", "Python")
      |> render_click()

      assert %{
               notebook: %{
                 setup_section: %{cells: [%Cell.Code{}, %Cell.Code{language: :"pyproject.toml"}]},
                 default_language: :python
               }
             } = Session.get_data(session.pid)

      refute view
             |> element("[data-el-language-buttons] button", "Python")
             |> has_element?()
    end

    test "disabling a language", %{conn: conn, session: session} do
      Session.enable_language(session.pid, :python)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      refute view
             |> element("[data-el-language-buttons] button", "Python")
             |> has_element?()

      view
      |> element(~s/button[phx-click="disable_language"]/)
      |> render_click()

      assert %{notebook: %{setup_section: %{cells: [%Cell.Code{}]}}} =
               Session.get_data(session.pid)

      assert view
             |> element("[data-el-language-buttons] button", "Python")
             |> has_element?()
    end

    test "changing cell language", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      view
      |> element(~s/#cell-#{cell_id} button/, "Erlang")
      |> render_click()

      assert %{notebook: %{sections: [%{cells: [%Cell.Code{language: :erlang}]}]}} =
               Session.get_data(session.pid)
    end

    test "shows an error when a cell langauge is not enabled", %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.set_cell_attributes(session.pid, cell_id, %{language: :python})

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert render(view) =~ "Python is not enabled for the current notebook."
      assert render(view) =~ "Enable Python"
    end
  end

  describe "outputs" do
    test "chunked text output update", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.queue_cell_evaluation(session.pid, cell_id)

      send(session.pid, {:runtime_evaluation_output, cell_id, terminal_text("line 1\n", true)})

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      assert render(view) =~ "line 1"

      send(session.pid, {:runtime_evaluation_output, cell_id, terminal_text("line 2\n", true)})

      wait_for_session_update(session.pid)
      # Render once, so that the send_update is processed
      _ = render(view)
      assert render(view) =~ "line 2"
    end

    test "frame output update", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.queue_cell_evaluation(session.pid, cell_id)

      frame = %{type: :frame, ref: "1", outputs: [terminal_text("In frame")], placeholder: true}
      send(session.pid, {:runtime_evaluation_output, cell_id, frame})

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      assert render(view) =~ "In frame"

      frame_update = %{
        type: :frame_update,
        ref: "1",
        update: {:replace, [terminal_text("Updated frame")]}
      }

      send(session.pid, {:runtime_evaluation_output, cell_id, frame_update})

      wait_for_session_update(session.pid)

      # Render once, so that frame send_update is processed
      _ = render(view)

      content = render(view)
      assert content =~ "Updated frame"
      refute content =~ "In frame"
    end

    test "chunked text within frame output update", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.queue_cell_evaluation(session.pid, cell_id)

      frame = %{
        type: :frame,
        ref: "1",
        outputs: [terminal_text("line 1\n", true)],
        placeholder: true
      }

      send(session.pid, {:runtime_evaluation_output, cell_id, frame})

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      assert render(view) =~ "line 1"

      frame_update = %{
        type: :frame_update,
        ref: "1",
        update: {:append, [terminal_text("line 2\n", true)]}
      }

      send(session.pid, {:runtime_evaluation_output, cell_id, frame_update})

      wait_for_session_update(session.pid)

      # Render once, so that frame send_update is processed
      _ = render(view)

      content = render(view)
      assert content =~ "line 1"
      assert content =~ "line 2"
    end

    test "frame output update when within grid", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.queue_cell_evaluation(session.pid, cell_id)

      frame = %{type: :frame, ref: "1", outputs: [terminal_text("In frame")], placeholder: true}
      grid = %{type: :grid, outputs: [frame], columns: ["Frame"], gap: 8, boxed: false}
      send(session.pid, {:runtime_evaluation_output, cell_id, grid})

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      assert render(view) =~ "In frame"

      frame_update = %{
        type: :frame_update,
        ref: "1",
        update: {:replace, [terminal_text("Updated frame")]}
      }

      send(session.pid, {:runtime_evaluation_output, cell_id, frame_update})

      wait_for_session_update(session.pid)

      # Render once, so that frame send_update is processed
      _ = render(view)

      content = render(view)
      assert content =~ "Updated frame"
      refute content =~ "In frame"
    end

    test "client-specific output is sent only to one target", %{conn: conn, session: session} do
      user1 = build(:user, name: "Jake Peralta")
      {_, client_id} = Session.register_client(session.pid, self(), user1)

      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.queue_cell_evaluation(session.pid, cell_id)

      send(
        session.pid,
        {:runtime_evaluation_output_to, client_id, cell_id, terminal_text("line 1\n", true)}
      )

      assert_receive {:operation,
                      {:add_cell_evaluation_output, _, ^cell_id, terminal_text("line 1\n", true)}}

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      refute render(view) =~ "line 1"
    end

    test "clients-only output is sent to all targets, but not reflected in session",
         %{conn: conn, session: session} do
      user1 = build(:user, name: "Jake Peralta")
      Session.register_client(session.pid, self(), user1)

      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.queue_cell_evaluation(session.pid, cell_id)

      send(
        session.pid,
        {:runtime_evaluation_output_to_clients, cell_id, terminal_text("line 1\n")}
      )

      assert_receive {:operation,
                      {:add_cell_evaluation_output, _, ^cell_id, terminal_text("line 1\n")}}

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      refute render(view) =~ "line 1"
    end

    test "shows change indicator on bound inputs",
         %{conn: conn, session: session, test: test} do
      section_id = insert_section(session.pid)

      Process.register(self(), test)

      input = %{
        type: :input,
        ref: "ref1",
        id: "input1",
        destination: test,
        attrs: %{type: :number, default: 1, label: "Name", debounce: :blur}
      }

      Session.subscribe(session.id)

      insert_cell_with_output(session.pid, section_id, input)

      code = source_for_input_read(input.id)
      cell_id = insert_text_cell(session.pid, section_id, :code, code)
      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      refute render(view) =~ "This input has changed."

      Session.set_input_value(session.pid, input.id, 10)
      wait_for_session_update(session.pid)

      assert render(view) =~ "This input has changed."
    end

    test "frame output update with input", %{conn: conn, session: session, test: test} do
      Session.subscribe(session.id)
      evaluate_setup(session.pid)

      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code)

      Session.queue_cell_evaluation(session.pid, cell_id)

      frame = %{type: :frame, ref: "1", outputs: [], placeholder: true}
      send(session.pid, {:runtime_evaluation_output, cell_id, frame})

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      input = %{
        type: :input,
        ref: "ref1",
        id: "input1",
        destination: test,
        attrs: %{type: :number, default: 1, label: "Input inside frame", debounce: :blur}
      }

      frame_update = %{
        type: :frame_update,
        ref: "1",
        update: {:replace, [input]}
      }

      send(session.pid, {:runtime_evaluation_output, cell_id, frame_update})

      wait_for_session_update(session.pid)

      # Render once, so that frame send_update is processed
      _ = render(view)

      content = render(view)
      assert content =~ "Input inside frame"
      assert has_element?(view, ~s/input[value="1"]/)
    end
  end

  describe "smart cells" do
    test "shows a new cell insert button when a new smart cell kind becomes available",
         %{conn: conn, session: session} do
      insert_section(session.pid)

      Session.subscribe(session.id)
      connect_and_await_runtime(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      refute render(view) =~ "Database connection"

      send(
        session.pid,
        {:runtime_smart_cell_definitions,
         [%{kind: "dbconn", name: "Database connection", requirement_presets: []}]}
      )

      wait_for_session_update(session.pid)

      assert render(view) =~ "Database connection"
    end
  end

  describe "runtime settings" do
    test "connecting to standalone updates connect button to reconnect",
         %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/runtime")

      Session.subscribe(session.id)

      view
      |> element("#runtime-settings-modal button", "Standalone")
      |> render_click()

      view
      |> element("#runtime-settings-modal form")
      |> render_submit(%{data: %{}})

      assert_receive {:operation, {:set_runtime, _pid, %Runtime.Standalone{}}}
      assert_receive {:operation, {:runtime_connected, _pid, %Runtime.Standalone{} = runtime}}

      assert_patch(view, "/sessions/#{session.id}")
      assert render(view) =~ Atom.to_string(runtime.node)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/runtime")

      page = render(view)
      assert page =~ "Reconnect"
      assert page =~ "Disconnect"
    end

    test "disconnecting a connected node", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      Session.set_runtime(session.pid, Livebook.Runtime.NoopRuntime.new(self()))
      connect_and_await_runtime(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert render(view) =~ "No connected nodes"

      # Mimic the runtime reporting a connected node
      node = :node@host
      send(session.pid, {:runtime_connected_nodes, [node]})

      assert_receive {:operation, {:set_runtime_connected_nodes, _pid, _nodes}}

      refute render(view) =~ "No connected nodes"
      assert render(view) =~ "#{node}"

      view
      |> element(~s{button[phx-click="runtime_disconnect_node"]})
      |> render_click()

      assert_receive {:runtime_trace, :disconnect_node, [^node]}
    end

    test "configuring fly runtime", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/runtime")

      Session.subscribe(session.id)

      view
      |> element("#runtime-settings-modal button", "Fly.io machine")
      |> render_click()

      Livebook.FlyAPI.stub(fn conn when conn.method == "POST" ->
        Req.Test.json(conn, %{
          "data" => nil,
          "errors" => [
            %{
              "extensions" => %{"code" => "UNAUTHORIZED"},
              "locations" => [%{"column" => 3, "line" => 2}],
              "message" => "You must be authenticated to view this.",
              "path" => ["organizations"]
            }
          ]
        })
      end)

      view
      |> element(~s{form[phx-change="set_token"]})
      |> render_change(%{token: "invalid"})

      assert render_async(view) =~ "Error: could not authorize with the given token"

      Livebook.FlyAPI.stub(fn conn when conn.method == "POST" ->
        Req.Test.json(conn, %{
          "data" => %{
            "organizations" => %{
              "nodes" => [
                %{
                  "id" => "1",
                  "name" => "Grumpy Cat",
                  "rawSlug" => "grumpy-cat",
                  "slug" => "personal"
                }
              ]
            },
            "platform" => %{
              "regions" => [
                %{"code" => "ams", "name" => "Amsterdam, Netherlands"},
                %{"code" => "fra", "name" => "Frankfurt, Germany"}
              ],
              "requestRegion" => "fra"
            }
          }
        })
      end)

      view
      |> element(~s{form[phx-change="set_token"]})
      |> render_change(%{token: "valid"})

      assert render_async(view) =~ "Grumpy Cat"

      # Selects the closest region by default
      assert view
             |> element(~s/select[name="region"] option[value="fra"][selected]/)
             |> has_element?()

      Livebook.FlyAPI.stub(fn conn
                              when conn.method == "GET" and
                                     conn.path_info == ["v1", "apps", "new-app", "volumes"] ->
        conn
        |> Plug.Conn.put_status(404)
        |> Req.Test.json(%{"error" => "App not found"})
      end)

      # Create a new app
      view
      |> element(~s{form[phx-change="set_app_name"]})
      |> render_change(%{app_name: "new-app"})

      assert render_async(view) =~ ~r/App .*new-app.* does not exist yet/

      Livebook.FlyAPI.stub(fn conn
                              when conn.method == "POST" and conn.path_info == ["v1", "apps"] ->
        Plug.Conn.send_resp(conn, 201, "")
      end)

      view
      |> element(~s/button[phx-click="create_app"]/)
      |> render_click()

      assert render_async(view) =~ "CPU kind"

      # Create a new volume

      Livebook.FlyAPI.stub(fn conn
                              when conn.method == "POST" and
                                     conn.path_info == ["v1", "apps", "new-app", "volumes"] ->
        Req.Test.json(conn, %{
          "id" => "vol_1",
          "name" => "new_volume",
          "region" => "ams",
          "size_gb" => 1,
          "state" => "created"
        })
      end)

      view
      |> element(~s/button[phx-click="new_volume"]/)
      |> render_click()

      view
      |> element(~s/form[phx-submit="create_volume"]/)
      |> render_submit(%{volume: %{name: "new_volume", size_gb: "1"}})

      assert render_async(view) =~ "name: new_volume"

      # The volume is automatically selected
      assert view
             |> element(~s/select[name="volume_id"] option[value="vol_1"][selected]/)
             |> has_element?()

      # Delete the volume

      Livebook.FlyAPI.stub(fn conn
                              when conn.method == "DELETE" and
                                     conn.path_info == [
                                       "v1",
                                       "apps",
                                       "new-app",
                                       "volumes",
                                       "vol_1"
                                     ] ->
        Req.Test.json(conn, %{})
      end)

      view
      |> element(~s/button[phx-click="delete_volume"]/)
      |> render_click()

      view
      |> element(~s/button[phx-click="confirm_delete_volume"]/)
      |> render_click()

      refute render_async(view) =~ "name: new_volume"

      assert view
             |> element(~s/select[name="volume_id"] option[value=""][selected]/)
             |> has_element?()

      # We do not actually connect the runtime. We test connecting
      # againast the real API separately
    end

    test "populates fly runtime config form existing runtime", %{conn: conn, session: session} do
      runtime =
        Runtime.Fly.new(%{
          token: "my-token",
          app_name: "my-app",
          region: "ams",
          cpu_kind: "performance",
          cpus: 1,
          memory_gb: 1,
          gpu_kind: nil,
          gpus: nil,
          volume_id: "vol_1",
          docker_tag: "nightly"
        })

      Session.set_runtime(session.pid, runtime)

      Livebook.FlyAPI.stub(fn
        conn when conn.method == "POST" ->
          Req.Test.json(conn, %{
            "data" => %{
              "organizations" => %{
                "nodes" => [
                  %{
                    "id" => "1",
                    "name" => "Grumpy Cat",
                    "rawSlug" => "grumpy-cat",
                    "slug" => "personal"
                  }
                ]
              },
              "platform" => %{
                "regions" => [
                  %{"code" => "ams", "name" => "Amsterdam, Netherlands"},
                  %{"code" => "fra", "name" => "Frankfurt, Germany"}
                ],
                "requestRegion" => "fra"
              }
            }
          })

        conn
        when conn.method == "GET" and
               conn.path_info == ["v1", "apps", "my-app", "volumes"] ->
          Req.Test.json(conn, [
            %{
              "id" => "vol_1",
              "name" => "new_volume",
              "region" => "ams",
              "size_gb" => 1,
              "state" => "created"
            }
          ])
      end)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/runtime")

      assert render_async(view) =~ "Grumpy Cat"

      assert view
             |> element(~s/select[name="region"] option[value="ams"][selected]/)
             |> has_element?()

      assert view
             |> element(~s/select[name="volume_id"] option[value="vol_1"][selected]/)
             |> has_element?()

      assert view
             |> element(~s/select[name="specs[cpu_kind]"] option[value="performance"][selected]/)
             |> has_element?()
    end

    test "configuring k8s runtime", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/runtime")

      Session.subscribe(session.id)

      Req.Test.stub(:k8s_cluster, Livebook.K8sClusterStub)

      view
      |> element("#runtime-settings-modal button", "Kubernetes Pod")
      |> render_click()

      # Check context switcher and switch to context with no permission

      view
      |> element(~s{form[phx-change="set_context"]})
      |> render_change(%{context: "no-permission"})

      rendered = render_async(view)

      assert rendered =~ "Authenticated user has no permission to"
      refute rendered =~ "You can fully customize"

      # Test cluster with full access

      view
      |> element(~s{form[phx-change="set_context"]})
      |> render_change(%{context: "default"})

      render_async(view)

      view
      |> element(~s{form[phx-change="set_namespace"]})
      |> render_change(%{namespace: "default"})

      render_async(view)

      assert view
             |> element(~s{select[name="pvc_name"] option[value="foo-pvc"]})
             |> has_element?()

      assert view
             |> element(~s{select[name="pvc_name"] option[value="new-pvc"]})
             |> has_element?()

      assert render_async(view) =~ "You can fully customize"

      # Create new PVC

      view
      |> element(~s{button[phx-click="new_pvc"]})
      |> render_click()

      assert view
             |> element(~s{form[phx-submit="create_pvc"]})
             |> has_element?()

      # Cancel button intermezzo

      view
      |> element(~s{button[phx-click="cancel_new_pvc"]})
      |> render_click()

      refute view
             |> element(~s{form[phx-submit="create_pvc"]})
             |> has_element?()

      # Create new PVC again

      view
      |> element(~s{button[phx-click="new_pvc"]})
      |> render_click()

      assert view
             |> element(
               ~s{form[phx-submit="create_pvc"] select[name="pvc[storage_class]"] option[value="first-storage-class"]}
             )
             |> has_element?()

      assert view
             |> element(
               ~s{form[phx-submit="create_pvc"] select[name="pvc[storage_class]"] option[value="second-storage-class"]}
             )
             |> has_element?()

      assert view
             |> element(~s{form[phx-submit="create_pvc"] button[type="submit"][disabled]})
             |> has_element?()

      view
      |> element(~s{form[phx-submit="create_pvc"]})
      |> render_change(%{pvc: %{name: "new-pvc", size_gb: 1}})

      assert view
             |> element(~s{form[phx-submit="create_pvc"] button[type="submit"]:not([disabled])})
             |> has_element?()

      Req.Test.expect(:k8s_cluster, Livebook.K8sClusterStub)

      view
      |> element(~s{form[phx-submit="create_pvc"]})
      |> render_submit(%{pvc: %{name: "new-pvc", size_gb: 1}})

      Req.Test.verify!()

      # Delete a PVC

      view
      |> element(~s{button[phx-click="delete_pvc"]})
      |> render_click()

      assert render_async(view) =~
               "Are you sure you want to irreversibly delete Persistent Volume Claim"

      Req.Test.expect(:k8s_cluster, Livebook.K8sClusterStub)

      view
      |> element(~s{button[phx-click="confirm_delete_pvc"]})
      |> render_click()

      Req.Test.verify!()

      # Pod Template Validation

      refute render_async(view) =~ ~s/Make sure to define a valid resource of apiVersion /

      view
      |> element(~s{form[phx-change="set_pod_template"]})
      |> render_change(%{pod_template: ""})

      assert render_async(view) =~ ~s/Make sure to define a valid resource of apiVersion /

      view
      |> element(~s{form[phx-change="set_pod_template"]})
      |> render_change(%{
        pod_template: """
        apiVersion: v1
        kind: Pod
        metadata:
          generateName: livebook-runtime-
        spec:
          containers:
            - name: other-name
        """
      })

      assert render_async(view) =~ ~s/Main container is missing./

      # We do not actually connect the runtime. We test connecting againast the
      # real API separately
    end

    test "populates k8s runtime config form existing runtime", %{conn: conn, session: session} do
      pod_template = """
      apiVersion: v1
      kind: Pod
      metadata:
        generateName: livebook-runtime-
        labels:
          livebook.dev/component: test
      spec:
        containers:
          - name: livebook-runtime\
      """

      runtime =
        Runtime.K8s.new(%{
          context: "default",
          namespace: "default",
          pvc_name: "foo-pvc",
          docker_tag: "nightly",
          pod_template: pod_template
        })

      Req.Test.stub(:k8s_cluster, Livebook.K8sClusterStub)

      Session.set_runtime(session.pid, runtime)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/runtime")

      # We load cluster information async, and then run namespace
      # checks async, so we need to await async twice
      _ = render_async(view)
      assert render_async(view) =~ "You can fully customize"

      assert view
             |> element(~s{select[name="pvc_name"] option[value="foo-pvc"][selected]})
             |> has_element?()

      assert view
             |> element(~s{select[name="pvc_name"] option[value="new-pvc"]})
             |> has_element?()

      assert view
             |> element(~s{button[phx-click="init"]:not([disabled])})
             |> has_element?()
    end

    test "saving and loading config from secret", %{conn: conn, session: session} do
      runtime =
        Runtime.Fly.new(%{
          token: "my-token",
          app_name: "my-app",
          region: "ams",
          cpu_kind: "performance",
          cpus: 1,
          memory_gb: 1,
          gpu_kind: nil,
          gpus: nil,
          volume_id: "vol_1",
          docker_tag: "nightly"
        })

      Session.set_runtime(session.pid, runtime)

      Livebook.FlyAPI.stub(fn
        conn when conn.method == "POST" ->
          Req.Test.json(conn, %{
            "data" => %{
              "organizations" => %{
                "nodes" => [
                  %{
                    "id" => "1",
                    "name" => "Grumpy Cat",
                    "rawSlug" => "grumpy-cat",
                    "slug" => "personal"
                  }
                ]
              },
              "platform" => %{
                "regions" => [
                  %{"code" => "ams", "name" => "Amsterdam, Netherlands"},
                  %{"code" => "fra", "name" => "Frankfurt, Germany"}
                ],
                "requestRegion" => "fra"
              }
            }
          })

        conn
        when conn.method == "GET" and
               conn.path_info == ["v1", "apps", "my-app", "volumes"] ->
          Req.Test.json(conn, [
            %{
              "id" => "vol_1",
              "name" => "new_volume",
              "region" => "ams",
              "size_gb" => 1,
              "state" => "created"
            }
          ])
      end)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/runtime")

      # The form is already filled with the runtime configuration, we
      # just save it in a secret
      view
      |> element("button", "Save config")
      |> render_click()

      secret_name = "FLY_RUNTIME_#{System.unique_integer([:positive])}"

      view
      |> element(~s/form[phx-submit="save_config"]/)
      |> render_submit(%{secret: %{name: secret_name}})

      assert render_async(view) =~ "Load config"

      # Set a different runtime, so there are no defaults
      Session.set_runtime(session.pid, Runtime.Standalone.new())

      # Open new runtime configuration
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/runtime")

      view
      |> element("#runtime-settings-modal button", "Fly.io machine")
      |> render_click()

      refute render(view) =~ "CPU kind"

      # Load the configuration from secret
      view
      |> element("#config-secret-menu-content button", secret_name)
      |> render_click()

      assert render_async(view) =~ "Grumpy Cat"

      assert view
             |> element(~s/select[name="region"] option[value="ams"][selected]/)
             |> has_element?()

      assert view
             |> element(~s/select[name="volume_id"] option[value="vol_1"][selected]/)
             |> has_element?()

      assert view
             |> element(~s/select[name="specs[cpu_kind]"] option[value="performance"][selected]/)
             |> has_element?()
    end
  end

  describe "persistence settings" do
    @tag :tmp_dir
    test "saving to file shows the newly created file in file selector",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/file")

      path = Path.join(tmp_dir, "notebook.livemd")

      view
      |> element(~s{form[id*="path-form"]})
      |> render_change(%{path: path})

      view
      |> element(~s{#persistence-modal button}, "Save")
      |> render_click()

      assert Session.get_data(session.pid).file

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/file")

      assert view
             |> element("button", "notebook.livemd")
             |> has_element?()

      view
      |> element(~s{#persistence-modal button}, "Stop saving")
      |> render_click()

      refute Session.get_data(session.pid).file
    end

    @tag :tmp_dir
    test "changing output persistence updates data",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/file")

      path = Path.join(tmp_dir, "notebook.livemd")

      view
      |> element(~s{form[id*="path-form"]})
      |> render_change(%{path: path})

      view
      |> element(~s{form[phx-change="set_options"]})
      |> render_change(%{persist_outputs: "true"})

      view
      |> element(~s{#persistence-modal button}, "Save")
      |> render_click()

      assert %{notebook: %{persist_outputs: true}} = Session.get_data(session.pid)
    end

    @tag :tmp_dir
    test "showing all files from default directory",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      old_dir = Livebook.Settings.default_dir()
      new_dir = Livebook.FileSystem.File.local(tmp_dir)

      Livebook.Settings.set_default_dir(new_dir)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/file")

      assert render(view) =~ new_dir.path

      Livebook.Settings.set_default_dir(old_dir)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/settings/file")

      refute render(view) =~ new_dir.path
      assert render(view) =~ old_dir.path
    end
  end

  describe "completion" do
    test "replies with nil completion reference when no runtime is started",
         %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code, "Process.sleep(10)")

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("intellisense_request", %{
        "cell_id" => cell_id,
        "type" => "completion",
        "hint" => "System.ver",
        "editor_auto_completion" => false
      })

      assert_reply(view, %{"ref" => nil})
    end

    test "replies with completion reference and then sends asynchronous response",
         %{conn: conn, session: session} do
      section_id = insert_section(session.pid)
      cell_id = insert_text_cell(session.pid, section_id, :code, "Process.sleep(10)")

      Session.subscribe(session.id)
      connect_and_await_runtime(session.pid)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("intellisense_request", %{
        "cell_id" => cell_id,
        "type" => "completion",
        "hint" => "System.ver",
        "editor_auto_completion" => false
      })

      assert_reply(view, %{"ref" => ref})
      assert ref != nil

      assert_push_event(
        view,
        "intellisense_response",
        %{
          "ref" => ^ref,
          "response" => %{items: [%{label: "version/0"}]}
        },
        1000
      )
    end
  end

  test "forking the session", %{conn: conn, session: session} do
    Session.set_notebook_name(session.pid, "My notebook")
    wait_for_session_update(session.pid)

    {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

    assert {:error, {:live_redirect, %{to: "/sessions/" <> session_id}}} =
             result =
             view
             |> element("button", "Fork")
             |> render_click()

    {:ok, view, _} = follow_redirect(result, conn)
    assert render(view) =~ "My notebook - fork"

    close_session_by_id(session_id)
  end

  @tag :tmp_dir
  test "starring and unstarring the notebook", %{conn: conn, session: session, tmp_dir: tmp_dir} do
    notebook_path = Path.join(tmp_dir, "notebook.livemd")
    file = Livebook.FileSystem.File.local(notebook_path)
    Session.set_file(session.pid, file)

    Livebook.NotebookManager.subscribe_starred_notebooks()

    {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

    view
    |> element(~s/button[phx-click="star_notebook"]/)
    |> render_click()

    assert_receive {:starred_notebooks_updated, starred_notebooks}

    assert Enum.any?(starred_notebooks, &(&1.file == file))

    view
    |> element(~s/button[phx-click="unstar_notebook"]/)
    |> render_click()

    assert_receive {:starred_notebooks_updated, starred_notebooks}

    refute Enum.any?(starred_notebooks, &(&1.file == file))
  end

  describe "connected users" do
    test "lists connected users", %{conn: conn, session: session} do
      user1 = build(:user, name: "Jake Peralta")

      client_pid =
        spawn_link(fn ->
          receive do
            :stop -> :ok
          end
        end)

      Session.register_client(session.pid, client_pid, user1)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert render(view) =~ "Jake Peralta"

      send(client_pid, :stop)
    end

    test "updates users list whenever a user joins or leaves",
         %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      Session.subscribe(session.id)

      user1 = build(:user, name: "Jake Peralta")

      client_pid =
        spawn_link(fn ->
          receive do
            :stop -> :ok
          end
        end)

      {_, client_id} = Session.register_client(session.pid, client_pid, user1)

      assert_receive {:operation, {:client_join, ^client_id, _user}}
      assert render(view) =~ "Jake Peralta"

      send(client_pid, :stop)
      assert_receive {:operation, {:client_leave, ^client_id}}
      refute render(view) =~ "Jake Peralta"
    end

    test "updates users list whenever a user changes his data",
         %{conn: conn, session: session} do
      user1 = build(:user, name: "Jake Peralta")

      client_pid =
        spawn_link(fn ->
          receive do
            :stop -> :ok
          end
        end)

      Session.register_client(session.pid, client_pid, user1)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      Session.subscribe(session.id)

      assert render(view) =~ "Jake Peralta"

      Livebook.Users.broadcast_change(%{user1 | name: "Raymond Holt"})
      assert_receive {:operation, {:update_user, _client_id, _user}}

      refute render(view) =~ "Jake Peralta"
      assert render(view) =~ "Raymond Holt"

      send(client_pid, :stop)
    end
  end

  describe "relative paths" do
    test "renders an info message when the path doesn't have notebook extension",
         %{conn: conn, session: session} do
      session_path = "/sessions/#{session.id}"

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               result = live(conn, ~p"/sessions/#{session.id}/document.pdf")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Got unrecognised session path: document.pdf"
    end

    test "renders an info message when the session has neither original url nor path",
         %{conn: conn, session: session} do
      session_path = "/sessions/#{session.id}"

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               result = live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)

      assert render(view) =~
               "Cannot resolve notebook path notebook.livemd, because the current notebook has no location"
    end

    @tag :tmp_dir
    test "renders an error message when the relative notebook does not exist",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      index_file = FileSystem.File.resolve(tmp_dir, "index.livemd")
      notebook_file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")

      Session.set_file(session.pid, index_file)
      wait_for_session_update(session.pid)

      session_path = "/sessions/#{session.id}"

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               result = live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)

      assert render(view) =~
               "Cannot navigate, failed to read #{notebook_file.path}, reason: no such file or directory"
    end

    @tag :tmp_dir
    test "opens a relative notebook if it exists",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      index_file = FileSystem.File.resolve(tmp_dir, "index.livemd")
      notebook_file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")

      Session.set_file(session.pid, index_file)
      wait_for_session_update(session.pid)

      :ok = FileSystem.File.write(notebook_file, "# Sibling notebook")

      assert {:error, {:live_redirect, %{to: new_session_path}}} =
               result = live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Sibling notebook"

      "/sessions/" <> session_id = new_session_path
      {:ok, session} = Sessions.fetch_session(session_id)
      data = Session.get_data(session.pid)
      assert data.file == notebook_file

      Session.close(session.pid)
    end

    @tag :tmp_dir
    test "if the current session has no path, forks the relative notebook",
         %{conn: conn, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      index_file = FileSystem.File.resolve(tmp_dir, "index.livemd")
      notebook_file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")

      {:ok, session} = Sessions.create_session(origin: {:file, index_file})

      :ok = FileSystem.File.write(notebook_file, "# Sibling notebook")

      assert {:error, {:live_redirect, %{to: new_session_path}}} =
               result = live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Sibling notebook - fork"

      "/sessions/" <> session_id = new_session_path
      {:ok, new_session} = Sessions.fetch_session(session_id)
      data = Session.get_data(new_session.pid)
      assert data.file == nil
      assert data.origin == {:file, notebook_file}

      Session.close([session.pid, new_session.pid])
    end

    @tag :tmp_dir
    test "if the notebook is already open, redirects to the session",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      index_file = FileSystem.File.resolve(tmp_dir, "index.livemd")
      notebook_file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")

      Session.set_file(session.pid, index_file)
      wait_for_session_update(session.pid)

      :ok = FileSystem.File.write(notebook_file, "# Sibling notebook")

      assert {:error, {:live_redirect, %{to: session_path}}} =
               live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      "/sessions/" <> session_id = session_path
      close_session_by_id(session_id)
    end

    @tag :tmp_dir
    test "handles nested paths", %{conn: conn, session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      parent_file = FileSystem.File.resolve(tmp_dir, "parent.livemd")
      child_dir = FileSystem.File.resolve(tmp_dir, "dir/")
      child_file = FileSystem.File.resolve(child_dir, "child.livemd")

      Session.set_file(session.pid, parent_file)
      wait_for_session_update(session.pid)

      :ok = FileSystem.File.write(child_file, "# Child notebook")

      assert {:error, {:live_redirect, %{to: "/sessions/" <> session_id}}} =
               result = live(conn, ~p"/sessions/#{session.id}/dir/child.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Child notebook"

      close_session_by_id(session_id)
    end

    @tag :tmp_dir
    test "handles parent paths", %{conn: conn, session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      parent_file = FileSystem.File.resolve(tmp_dir, "parent.livemd")
      child_dir = FileSystem.File.resolve(tmp_dir, "dir/")
      child_file = FileSystem.File.resolve(child_dir, "child.livemd")

      Session.set_file(session.pid, child_file)
      wait_for_session_update(session.pid)

      :ok = FileSystem.File.write(parent_file, "# Parent notebook")

      assert {:error, {:live_redirect, %{to: "/sessions/" <> session_id}}} =
               result = live(conn, ~p"/sessions/#{session.id}/__parent__/parent.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Parent notebook"

      close_session_by_id(session_id)
    end

    test "resolves remote URLs", %{conn: conn} do
      bypass = Bypass.open()

      Bypass.expect_once(bypass, "GET", "/notebook.livemd", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("text/plain")
        |> Plug.Conn.resp(200, "# My notebook")
      end)

      index_url = bypass_url(bypass.port) <> "/index.livemd"
      {:ok, session} = Sessions.create_session(origin: {:url, index_url})

      assert {:error, {:live_redirect, %{to: "/sessions/" <> session_id}}} =
               result = live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "My notebook"

      Session.close(session.pid)
      close_session_by_id(session_id)
    end

    test "when a remote URL cannot be loaded, attempts to resolve a flat URL", %{conn: conn} do
      bypass = Bypass.open()

      # Multi-level path is not available
      Bypass.expect_once(bypass, "GET", "/nested/path/to/notebook.livemd", fn conn ->
        Plug.Conn.resp(conn, 404, "Error")
      end)

      # A flat path is available
      Bypass.expect_once(bypass, "GET", "/notebook.livemd", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("text/plain")
        |> Plug.Conn.resp(200, "# My notebook")
      end)

      index_url = bypass_url(bypass.port) <> "/index.livemd"
      {:ok, session} = Sessions.create_session(origin: {:url, index_url})

      assert {:error, {:live_redirect, %{to: "/sessions/" <> session_id}}} =
               result = live(conn, ~p"/sessions/#{session.id}/nested/path/to/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "My notebook"

      Session.close(session.pid)
      close_session_by_id(session_id)
    end

    test "renders an error message if relative remote notebook cannot be loaded", %{conn: conn} do
      bypass = Bypass.open()

      Bypass.expect_once(bypass, "GET", "/notebook.livemd", fn conn ->
        Plug.Conn.resp(conn, 404, "Error")
      end)

      index_url = bypass_url(bypass.port) <> "/index.livemd"

      {:ok, session} = Sessions.create_session(origin: {:url, index_url})

      session_path = "/sessions/#{session.id}"

      assert {:error, {:live_redirect, %{to: ^session_path}}} =
               result = live(conn, ~p"/sessions/#{session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)
      assert render(view) =~ "Cannot navigate, failed to download notebook from the given URL"

      Session.close(session.pid)
    end

    test "if the remote notebook is already imported, redirects to the session",
         %{conn: conn, test: test} do
      test_path = test |> to_string() |> URI.encode_www_form()
      index_url = "http://example.com/#{test_path}/index.livemd"
      notebook_url = "http://example.com/#{test_path}/notebook.livemd"

      {:ok, index_session} = Sessions.create_session(origin: {:url, index_url})
      {:ok, notebook_session} = Sessions.create_session(origin: {:url, notebook_url})

      notebook_session_path = "/sessions/#{notebook_session.id}"

      assert {:error, {:live_redirect, %{to: ^notebook_session_path}}} =
               live(conn, ~p"/sessions/#{index_session.id}/notebook.livemd")

      Session.close([index_session.pid, notebook_session.pid])
    end

    test "renders an error message if there are already multiple session imported from the relative URL",
         %{conn: conn, test: test} do
      test_path = test |> to_string() |> URI.encode_www_form()
      index_url = "http://example.com/#{test_path}/index.livemd"
      notebook_url = "http://example.com/#{test_path}/notebook.livemd"

      {:ok, index_session} = Sessions.create_session(origin: {:url, index_url})
      {:ok, notebook_session1} = Sessions.create_session(origin: {:url, notebook_url})
      {:ok, notebook_session2} = Sessions.create_session(origin: {:url, notebook_url})

      index_session_path = "/sessions/#{index_session.id}"

      assert {:error, {:live_redirect, %{to: ^index_session_path}}} =
               result = live(conn, ~p"/sessions/#{index_session.id}/notebook.livemd")

      {:ok, view, _} = follow_redirect(result, conn)

      assert render(view) =~
               "Cannot navigate, because multiple sessions were found for #{notebook_url}"

      Session.close([index_session.pid, notebook_session1.pid, notebook_session2.pid])
    end
  end

  describe "package search" do
    test "lists search entries", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/package-search")

      [search_view] = live_children(view)

      # Search the predefined dependencies in the embedded runtime
      search_view
      |> element(~s{form[phx-change="search"]})
      |> render_change(%{"search" => "re"})

      page = render(view)
      assert page =~ "req"
      assert page =~ "Req is a batteries-included HTTP client for Elixir."
      assert page =~ "0.5.0"
    end
  end

  describe "secrets" do
    setup do
      {:ok, hub: Livebook.Hubs.fetch_hub!(Livebook.Hubs.Personal.id())}
    end

    test "adds a secret from form", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/secrets")
      secret = build(:secret, hub_id: nil)

      view
      |> element(~s{form[phx-submit="save"]})
      |> render_submit(%{
        secret: %{name: secret.name, value: secret.value, hub_id: secret.hub_id}
      })

      assert_session_secret(view, session.pid, secret)
    end

    test "adds a livebook secret from form", %{conn: conn, session: session, hub: hub} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/secrets")
      secret = build(:secret)

      view
      |> element(~s{form[phx-submit="save"]})
      |> render_submit(%{
        secret: %{name: secret.name, value: secret.value, hub_id: secret.hub_id}
      })

      assert secret in Livebook.Hubs.get_secrets(hub)
    end

    test "syncs secrets", %{conn: conn, session: session, hub: hub} do
      session_secret = insert_secret(value: "123")
      secret = build(:secret, name: session_secret.name, value: "456")

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/secrets")

      view
      |> element(~s{form[phx-submit="save"]})
      |> render_submit(%{
        secret: %{name: secret.name, value: secret.value, hub_id: secret.hub_id}
      })

      assert_session_secret(view, session.pid, secret)
      assert secret in Livebook.Hubs.get_secrets(hub)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/secrets")
      Session.set_secret(session.pid, session_secret)

      secret = build(:secret, name: session_secret.name, value: "789")

      view
      |> element(~s{form[phx-submit="save"]})
      |> render_submit(%{
        secret: %{name: secret.name, value: secret.value, hub_id: secret.hub_id}
      })

      assert_session_secret(view, session.pid, secret)
      assert secret in Livebook.Hubs.get_secrets(hub)
    end

    test "never syncs secrets when updating from session",
         %{conn: conn, session: session, hub: hub} do
      hub_secret = insert_secret(value: "123")
      secret = build(:secret, name: hub_secret.name, value: "456", hub_id: nil)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/secrets")
      Session.set_secret(session.pid, hub_secret)

      view
      |> element(~s{form[phx-submit="save"]})
      |> render_submit(%{
        secret: %{name: secret.name, value: secret.value, hub_id: secret.hub_id}
      })

      assert_session_secret(view, session.pid, secret)
      refute secret in Livebook.Hubs.get_secrets(hub)
      assert hub_secret in Livebook.Hubs.get_secrets(hub)
    end

    test "shows the 'Add secret' button for missing secrets", %{conn: conn, session: session} do
      secret = build(:secret, hub_id: nil)
      Session.subscribe(session.id)
      section_id = insert_section(session.pid)
      code = ~s{System.fetch_env!("LB_#{secret.name}")}
      cell_id = insert_text_cell(session.pid, section_id, :code, code)

      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert view
             |> element("a", "Add secret")
             |> has_element?()
    end

    test "adding a missing secret using 'Add secret' button",
         %{conn: conn, session: session, hub: hub} do
      secret = build(:secret, hub_id: nil)

      # Subscribe and executes the code to trigger
      # the `System.EnvError` exception and outputs the 'Add secret' button
      Session.subscribe(session.id)
      section_id = insert_section(session.pid)
      code = ~s{System.fetch_env!("LB_#{secret.name}")}
      cell_id = insert_text_cell(session.pid, section_id, :code, code)

      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

      # Enters the session to check if the button exists
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      expected_url = ~p"/sessions/#{session.id}/secrets?secret_name=#{secret.name}"
      add_secret_button = element(view, "a[href='#{expected_url}']")
      assert has_element?(add_secret_button)

      # Clicks the button and fills the form to create a new secret
      # that prefilled the name with the received from exception.
      render_click(add_secret_button)
      form_element = element(view, "#secrets-modal form[phx-submit='save']")
      assert has_element?(form_element)
      render_submit(form_element, %{secret: %{value: secret.value, hub_id: secret.hub_id}})

      # Checks if the secret isn't an app secret
      refute secret in Livebook.Hubs.get_secrets(hub)

      # Checks if the secret exists and is inside the session,
      # then executes the code cell again and checks if the
      # secret value is what we expected.
      assert_session_secret(view, session.pid, secret)
      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, terminal_text(output), _}}

      assert output == "\e[32m\"#{secret.value}\"\e[0m"
    end

    test "granting access for unavailable secret using 'Add secret' button",
         %{conn: conn, session: session, hub: hub} do
      secret = insert_secret()

      # Subscribe and executes the code to trigger
      # the `System.EnvError` exception and outputs the 'Add secret' button
      Session.subscribe(session.id)
      section_id = insert_section(session.pid)
      code = ~s{System.fetch_env!("LB_#{secret.name}")}
      cell_id = insert_text_cell(session.pid, section_id, :code, code)

      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

      # Enters the session to check if the button exists
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      expected_url = ~p"/sessions/#{session.id}/secrets?secret_name=#{secret.name}"
      add_secret_button = element(view, "a[href='#{expected_url}']")
      assert has_element?(add_secret_button)

      # Checks if the secret is persisted
      assert secret in Livebook.Hubs.get_secrets(hub)

      # Clicks the button and checks if the 'Grant access' banner
      # is being shown, so clicks it's button to set the app secret
      # to the session, allowing the user to fetches the secret.
      render_click(add_secret_button)

      assert render(view) =~
               "in the #{hub_label(secret)} workspace. Allow this notebook to access it?"

      grant_access_button = element(view, "#secrets-modal button", "Grant access")
      render_click(grant_access_button)

      # Checks if the secret exists and is inside the session,
      # then executes the code cell again and checks if the
      # secret value is what we expected.
      assert_session_secret(view, session.pid, secret)
      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, terminal_text(output), _}}

      assert output == "\e[32m\"#{secret.value}\"\e[0m"
    end

    test "reloading outdated secret value", %{conn: conn, session: session} do
      hub_secret = insert_secret(value: "123")
      Session.set_secret(session.pid, hub_secret)

      {:ok, updated_hub_secret} = Livebook.Secrets.update_secret(hub_secret, %{value: "456"})
      hub = Livebook.Hubs.fetch_hub!(hub_secret.hub_id)
      :ok = Livebook.Hubs.update_secret(hub, updated_hub_secret)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/secrets")

      assert_session_secret(view, session.pid, hub_secret)

      view
      |> element(~s{button[aria-label="load latest value"]})
      |> render_click()

      assert_session_secret(view, session.pid, updated_hub_secret)
    end

    test "redirects the user to update or delete a secret",
         %{conn: conn, session: session, hub: hub} do
      Session.subscribe(session.id)

      # creates a secret
      %{name: secret_name, value: secret_value} = insert_secret()

      # receives the operation event
      assert_receive {:operation, {:sync_hub_secrets, "__server__"}}

      # selects the notebook's hub with team hub id
      Session.set_notebook_hub(session.pid, hub.id)

      # loads the session page
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      # clicks the button to edit a secret
      view
      |> element("#hub-#{hub.id}-secret-#{secret_name}-edit-button")
      |> render_click()

      # redirects to hub page and loads the modal with
      # the secret name and value filled
      assert_redirect(view, ~p"/hub/#{hub.id}/secrets/edit/#{secret_name}")
      {:ok, view, _} = live(conn, ~p"/hub/#{hub.id}/secrets/edit/#{secret_name}")

      assert render(view) =~ "Edit secret"

      # fills and submits the secrets modal form
      # to update the secret on team hub page
      secret_new_value = "123456"
      attrs = %{secret: %{name: secret_name, value: secret_new_value}}
      form = element(view, "#secrets-form")

      render_change(form, attrs)
      render_submit(form, attrs)

      # receives the operation event
      assert_receive {:operation, {:sync_hub_secrets, "__server__"}}

      # validates the secret
      secrets = Livebook.Hubs.get_secrets(hub)
      hub_secret = Enum.find(secrets, &(&1.name == secret_name))

      assert hub_secret.value == secret_new_value
      refute hub_secret.value == secret_value
    end
  end

  describe "environment variables" do
    test "outputs persisted env var from settings", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      name = "MY_ENV_#{System.unique_integer([:positive])}"

      section_id = insert_section(session.pid)

      cell_id =
        insert_text_cell(session.pid, section_id, :code, ~s{System.get_env("#{name}")})

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id,
                       terminal_text("\e[35mnil\e[0m"), _}}

      attrs = params_for(:env_var, name: name, value: "MyEnvVarValue")
      Settings.set_env_var(attrs)

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id,
                       terminal_text("\e[32m\"MyEnvVarValue\"\e[0m"), _}}

      Settings.set_env_var(%{attrs | value: "OTHER_VALUE"})

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id,
                       terminal_text("\e[32m\"OTHER_VALUE\"\e[0m"), _}}

      Settings.unset_env_var(name)

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id,
                       terminal_text("\e[35mnil\e[0m"), _}}
    end

    @tag :tmp_dir
    test "outputs persisted PATH delimited with os PATH env var",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      # Use the standalone runtime, to encapsulate env var changes
      Session.set_runtime(session.pid, Runtime.Standalone.new())

      # We start the runtime before adding the env var setting,
      # otherwise a concurrent embedded runtime server could set PATH
      # in this node and the standalone runtime would inherit it
      Session.subscribe(session.id)
      connect_and_await_runtime(session.pid)

      separator =
        case :os.type() do
          {:win32, _} -> ";"
          _ -> ":"
        end

      initial_os_path = System.get_env("PATH", "")
      expected_path = initial_os_path <> separator <> tmp_dir

      attrs = params_for(:env_var, name: "PATH", value: tmp_dir)
      Settings.set_env_var(attrs)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      section_id = insert_section(session.pid)

      # Note that we use IO.write, to make sure the value is not truncated
      # (which inspect does)
      cell_id =
        insert_text_cell(session.pid, section_id, :code, ~s{IO.write(System.get_env("PATH"))})

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_output, _, ^cell_id, terminal_text(output, true)}}

      assert output == expected_path
      # assert output == "\e[32m\"#{String.replace(expected_path, "\\", "\\\\")}\"\e[0m"

      Settings.unset_env_var("PATH")

      view
      |> element(~s{[data-el-session]})
      |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

      assert_receive {:operation,
                      {:add_cell_evaluation_output, _, ^cell_id, terminal_text(output, true)}}

      assert output == initial_os_path
    end
  end

  describe "file management" do
    @tag :tmp_dir
    test "adding :attachment file entry from storage",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      Session.subscribe(session.id)

      path = Path.join(tmp_dir, "image.jpg")
      File.write!(path, "content")

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/add-file/storage")

      view
      |> element(~s{form[id*="path-form"]})
      |> render_change(%{path: path})

      # Validations
      assert view
             |> element(~s{#add-file-entry-form})
             |> render_change(%{"data" => %{"name" => "na me", "copy" => "true"}}) =~
               "should contain only alphanumeric characters, dash, underscore and dot"

      view
      |> element(~s{#add-file-entry-form})
      |> render_submit(%{"data" => %{"name" => "image.jpg", "copy" => "true"}})

      assert_receive {:operation, {:add_file_entries, _client_id, [%{name: "image.jpg"}]}}

      assert %{notebook: %{file_entries: [%{type: :attachment, name: "image.jpg"}]}} =
               Session.get_data(session.pid)

      assert FileSystem.File.resolve(session.files_dir, "image.jpg") |> FileSystem.File.read() ==
               {:ok, "content"}
    end

    @tag :tmp_dir
    test "adding :file file entry from storage", %{conn: conn, session: session, tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "image.jpg")
      File.write!(path, "content")

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/add-file/storage")

      view
      |> element(~s{form[id*="path-form"]})
      |> render_change(%{path: path})

      view
      |> element(~s{#add-file-entry-form})
      |> render_submit(%{"data" => %{"name" => "image.jpg", "copy" => "false"}})

      assert %{
               notebook: %{
                 file_entries: [
                   %{
                     type: :file,
                     name: "image.jpg",
                     file: %FileSystem.File{
                       file_system_module: Livebook.FileSystem.Local,
                       path: ^path
                     }
                   }
                 ]
               }
             } = Session.get_data(session.pid)
    end

    test "adding :attachment file entry from url", %{conn: conn, session: session} do
      Session.subscribe(session.id)

      bypass = Bypass.open()
      file_url = "http://localhost:#{bypass.port}/image.jpg"

      Bypass.expect_once(bypass, "GET", "/image.jpg", fn conn ->
        Plug.Conn.resp(conn, 200, "content")
      end)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/add-file/url")

      # Validations
      page =
        view
        |> element(~s{#add-file-entry-form})
        |> render_change(%{"data" => %{"name" => "na me", "copy" => "true", "url" => "invalid"}})

      page =~ "should contain only alphanumeric characters, dash, underscore and dot"
      page =~ "must be a valid url"

      view
      |> element(~s{#add-file-entry-form})
      |> render_submit(%{
        "data" => %{"name" => "image.jpg", "copy" => "true", "url" => file_url}
      })

      assert_receive {:operation, {:add_file_entries, _client_id, [%{name: "image.jpg"}]}}

      assert %{notebook: %{file_entries: [%{type: :attachment, name: "image.jpg"}]}} =
               Session.get_data(session.pid)

      assert FileSystem.File.resolve(session.files_dir, "image.jpg") |> FileSystem.File.read() ==
               {:ok, "content"}
    end

    test "adding :url file entry from url", %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/add-file/url")

      url = "https://example.com/image.jpg"

      view
      |> element(~s{#add-file-entry-form})
      |> render_submit(%{
        "data" => %{"name" => "image.jpg", "copy" => "false", "url" => url}
      })

      assert %{notebook: %{file_entries: [%{type: :url, name: "image.jpg", url: ^url}]}} =
               Session.get_data(session.pid)
    end

    test "adding :attachment file entry from upload", %{conn: conn, session: session} do
      Session.subscribe(session.id)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/add-file/upload")

      # Validations
      assert view
             |> element(~s{#add-file-entry-form})
             |> render_change(%{"data" => %{"name" => "na me"}}) =~
               "should contain only alphanumeric characters, dash, underscore and dot"

      assert view
             |> element(~s{#add-file-entry-form})
             |> render_change(%{"data" => %{"name" => "image.jpg"}})

      view
      |> file_input(~s{#add-file-entry-form}, :file, [
        %{
          last_modified: 1_594_171_879_000,
          name: "image.jpg",
          content: "content",
          size: 7,
          type: "text/plain"
        }
      ])
      |> render_upload("image.jpg")

      view
      |> element(~s{#add-file-entry-form})
      |> render_submit(%{"data" => %{"name" => "image.jpg"}})

      assert_receive {:operation, {:add_file_entries, _client_id, [%{name: "image.jpg"}]}}

      assert %{notebook: %{file_entries: [%{type: :attachment, name: "image.jpg"}]}} =
               Session.get_data(session.pid)

      assert FileSystem.File.resolve(session.files_dir, "image.jpg") |> FileSystem.File.read() ==
               {:ok, "content"}
    end

    test "adding :attachment file entry from unlisted files", %{conn: conn, session: session} do
      for name <- ["file1.txt", "file2.txt", "file3.txt"] do
        file = FileSystem.File.resolve(session.files_dir, name)
        :ok = FileSystem.File.write(file, "content")
      end

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/add-file/unlisted")

      page = render(view)

      assert page =~ "file1.txt"
      assert page =~ "file2.txt"
      assert page =~ "file3.txt"

      view
      |> element(~s{#add-file-entry-form})
      |> render_submit(%{"selected_indices" => ["0", "2"]})

      assert %{
               notebook: %{
                 file_entries: [
                   %{type: :attachment, name: "file1.txt"},
                   %{type: :attachment, name: "file3.txt"}
                 ]
               }
             } = Session.get_data(session.pid)
    end

    test "renaming :attachment file entry", %{conn: conn, session: session} do
      %{files_dir: files_dir} = session
      image_file = FileSystem.File.resolve(files_dir, "image.jpg")
      :ok = FileSystem.File.write(image_file, "")
      Session.add_file_entries(session.pid, [%{type: :attachment, name: "image.jpg"}])

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert view |> element(~s/[data-el-files-list]/) |> render() =~ "image.jpg"

      view
      |> element(~s/[data-el-files-list] menu a/, "Rename")
      |> render_click()

      view
      |> element(~s/#rename-file-entry-form/)
      |> render_submit(%{"data" => %{"name" => "image2.jpg"}})

      assert %{notebook: %{file_entries: [%{type: :attachment, name: "image2.jpg"}]}} =
               Session.get_data(session.pid)

      page = view |> element(~s/[data-el-files-list]/) |> render()
      assert page =~ "image2.jpg"
      refute page =~ "image.jpg"
    end

    test "deleting :attachment file entry and removing the file from the file system",
         %{conn: conn, session: session} do
      %{files_dir: files_dir} = session
      image_file = FileSystem.File.resolve(files_dir, "image.jpg")
      :ok = FileSystem.File.write(image_file, "")
      Session.add_file_entries(session.pid, [%{type: :attachment, name: "image.jpg"}])

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert view |> element(~s/[data-el-files-list]/) |> render() =~ "image.jpg"

      view
      |> element(~s/[data-el-files-list] menu button/, "Delete")
      |> render_click()

      render_confirm(view, delete_from_file_system: true)

      assert {:ok, false} = FileSystem.File.exists?(image_file)

      assert %{notebook: %{file_entries: []}} = Session.get_data(session.pid)

      refute view |> element(~s/[data-el-files-list]/) |> render() =~ "image.jpg"
    end

    test "deleting :attachment file entry and keeping the file in the file system",
         %{conn: conn, session: session} do
      %{files_dir: files_dir} = session
      image_file = FileSystem.File.resolve(files_dir, "image.jpg")
      :ok = FileSystem.File.write(image_file, "")
      Session.add_file_entries(session.pid, [%{type: :attachment, name: "image.jpg"}])

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert view |> element(~s/[data-el-files-list]/) |> render() =~ "image.jpg"

      view
      |> element(~s/[data-el-files-list] menu button/, "Delete")
      |> render_click()

      render_confirm(view, delete_from_file_system: false)

      assert {:ok, true} = FileSystem.File.exists?(image_file)

      assert %{notebook: %{file_entries: []}} = Session.get_data(session.pid)

      refute view |> element(~s/[data-el-files-list]/) |> render() =~ "image.jpg"
    end

    @tag :tmp_dir
    test "transferring file entry", %{conn: conn, session: session, tmp_dir: tmp_dir} do
      Session.subscribe(session.id)

      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      image_file = FileSystem.File.resolve(tmp_dir, "image.jpg")
      :ok = FileSystem.File.write(image_file, "content")
      Session.add_file_entries(session.pid, [%{type: :file, name: "image.jpg", file: image_file}])

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert view |> element(~s/[data-el-files-list]/) |> render() =~ "image.jpg"

      view
      |> element(~s/[data-el-files-list] menu button/, "Move to attachments")
      |> render_click()

      assert_receive {:operation,
                      {:add_file_entries, _client_id, [%{type: :attachment, name: "image.jpg"}]}}

      assert %{notebook: %{file_entries: [%{type: :attachment, name: "image.jpg"}]}} =
               Session.get_data(session.pid)

      assert {:ok, true} = FileSystem.File.exists?(image_file)

      assert FileSystem.File.resolve(session.files_dir, "image.jpg") |> FileSystem.File.read() ==
               {:ok, "content"}
    end

    test "allowing access to file entry in quarantine", %{conn: conn} do
      file = Livebook.FileSystem.File.new(Livebook.FileSystem.Local.new(), p("/document.pdf"))

      notebook = %{
        Livebook.Notebook.new()
        | file_entries: [
            %{type: :file, name: "document.pdf", file: file}
          ],
          quarantine_file_entry_names: MapSet.new(["document.pdf"])
      }

      {:ok, session} = Sessions.create_session(notebook: notebook)

      Session.subscribe(session.id)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert view
             |> element(~s/[data-el-files-list]/)
             |> render() =~ "Click to review access"

      view
      |> element(~s/[data-el-files-list] button/, "document.pdf")
      |> render_click()

      render_confirm(view)

      assert_receive {:operation, {:allow_file_entry, _client_id, "document.pdf"}}

      refute view
             |> element(~s/[data-el-files-list]/)
             |> render() =~ "Click to review access"

      Session.close(session.pid)
    end
  end

  describe "apps" do
    test "deploying an app", %{conn: conn, session: session} do
      Session.subscribe(session.id)
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      section_id = insert_section(session.pid)

      insert_cell_with_output(
        session.pid,
        section_id,
        terminal_text("Hello from the app!")
      )

      slug = Livebook.Utils.random_short_id()

      Livebook.Apps.subscribe()

      view
      |> element(~s/[data-el-app-info] a/, "Configure")
      |> render_click()

      view
      |> element(~s/#app-settings-modal form/)
      |> render_change(%{"app_settings" => %{"slug" => slug}})

      view
      |> element(~s/#app-settings-modal form/)
      |> render_submit(%{"app_settings" => %{"slug" => slug}})

      view
      |> element(~s/[data-el-app-info] button/, "Launch preview")
      |> render_click()

      assert_receive {:app_created, %{slug: ^slug} = app}

      assert_receive {:operation, {:set_deployed_app_slug, _client_id, ^slug}}

      assert render(view) =~ "/apps/#{slug}"

      {:ok, view, _} =
        conn
        |> live(~p"/apps/#{slug}")
        |> follow_redirect(conn)

      assert_receive {:app_updated,
                      %{slug: ^slug, sessions: [%{app_status: %{execution: :executed}}]}}

      assert render(view) =~ "Hello from the app!"

      Livebook.App.close(app.pid)
    end

    test "stopping and terminating app session", %{conn: conn, session: session} do
      Session.subscribe(session.id)

      slug = Livebook.Utils.random_short_id()
      app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}
      Session.set_app_settings(session.pid, app_settings)

      Livebook.Apps.subscribe()
      Session.deploy_app(session.pid)

      assert_receive {:app_created, %{slug: ^slug} = app}

      assert_receive {:app_updated,
                      %{
                        slug: ^slug,
                        sessions: [%{app_status: %{execution: :executed}} = app_session]
                      }}

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element(~s/[data-el-app-info] button[aria-label="deactivate app session"]/)
      |> render_click()

      assert_receive {:app_updated,
                      %{slug: ^slug, sessions: [%{app_status: %{lifecycle: :deactivated}}]}}

      assert render(view) =~ "/apps/#{slug}/sessions/#{app_session.id}"

      view
      |> element(~s/[data-el-app-info] button[aria-label="terminate app session"]/)
      |> render_click()

      assert_receive {:app_updated, %{slug: ^slug, sessions: []}}

      refute render(view) =~ "/apps/#{slug}/sessions/#{app_session.id}"

      Livebook.App.close(app.pid)
    end

    test "shows a warning when any session secrets are defined", %{conn: conn, session: session} do
      secret = build(:secret, hub_id: nil)
      Session.set_secret(session.pid, secret)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert render(view) =~
               "The notebook uses session secrets, but those are not available to deployed apps. Convert them to Workspace secrets instead."
    end
  end

  describe "docker deployment" do
    test "instructs to choose a file when the notebook is not persisted",
         %{conn: conn, session: session} do
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/app-docker")

      assert render(view) =~ "To deploy this app, make sure to save the notebook first."
      assert render(view) =~ ~p"/sessions/#{session.id}/settings/file"
    end

    @tag :tmp_dir
    test "instructs to change app settings when invalid",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      notebook_path = Path.join(tmp_dir, "notebook.livemd")
      file = Livebook.FileSystem.File.local(notebook_path)
      Session.set_file(session.pid, file)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/app-docker")

      assert render(view) =~ "To deploy this app, make sure to specify valid settings."
      assert render(view) =~ ~p"/sessions/#{session.id}/settings/app"
    end

    @tag :tmp_dir
    test "shows dockerfile and allows saving it",
         %{conn: conn, session: session, tmp_dir: tmp_dir} do
      notebook_path = Path.join(tmp_dir, "notebook.livemd")
      file = Livebook.FileSystem.File.local(notebook_path)
      Session.set_file(session.pid, file)

      slug = Livebook.Utils.random_short_id()
      app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}
      Session.set_app_settings(session.pid, app_settings)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/app-docker")

      assert render(view) =~ "FROM ghcr.io/livebook-dev/livebook:"

      view
      |> element("button", "Save alongside notebook")
      |> render_click()

      dockerfile_path = Path.join(tmp_dir, "Dockerfile")

      assert File.read!(dockerfile_path) =~ "COPY notebook.livemd /apps"
    end
  end

  test "defined modules under sections", %{conn: conn, session: session} do
    Code.put_compiler_option(:debug_info, true)

    Session.subscribe(session.id)

    {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
    refute render(view) =~ "LivebookWeb.SessionLiveTest.Module1"
    refute render(view) =~ "LivebookWeb.SessionLiveTest.Module2"

    cell_id =
      insert_text_cell(session.pid, insert_section(session.pid), :code, ~S'''
      defmodule LivebookWeb.SessionLiveTest.Module1 do
        def bar, do: :baz
      end

      defmodule LivebookWeb.SessionLiveTest.Module2 do
        def bar, do: :baz
      end
      ''')

    Session.queue_cell_evaluation(session.pid, cell_id)
    assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

    assert has_element?(
             view,
             ~s/[data-el-outline-definition-item][data-file="#cell:#{cell_id}"][data-line="1"]/,
             "LivebookWeb.SessionLiveTest.Module1"
           )

    assert has_element?(
             view,
             ~s/[data-el-outline-definition-item][data-file="#cell:#{cell_id}"][data-line="5"]/,
             "LivebookWeb.SessionLiveTest.Module2"
           )
  after
    Code.put_compiler_option(:debug_info, false)
  end

  test "python code evaluation end-to-end", %{conn: conn, session: session} do
    # Use the standalone runtime, to install Pythonx and setup the interpreter
    Session.set_runtime(session.pid, Runtime.Standalone.new())

    {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

    Session.subscribe(session.id)

    view
    |> element("[data-el-language-buttons] button", "Python")
    |> render_click()

    section_id = insert_section(session.pid)

    cell_id =
      insert_text_cell(session.pid, section_id, :code, "len([1, 2])", %{language: :python})

    view
    |> element(~s{[data-el-session]})
    |> render_hook("queue_cell_evaluation", %{"cell_id" => cell_id})

    assert_receive {:operation,
                    {:add_cell_evaluation_response, _, ^cell_id, terminal_text(output), _}},
                   40_000

    assert output == "2"
  end
end
