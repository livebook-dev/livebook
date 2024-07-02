defmodule LivebookWeb.AppSessionLive.SourceComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      assign_new(socket, :source, fn ->
        # Note: we need to load the notebook, so that we don't track
        # the whole notebook in assigns
        notebook = Session.get_notebook(socket.assigns.session.pid)

        # We ignore the stamp, since it's not relevant for end-users,
        # and this way we don't generate the stamp every time they
        # look at the source
        {source, _warnings} =
          Livebook.LiveMarkdown.notebook_to_livemd(notebook,
            include_outputs: false,
            include_stamp: false
          )

        source
      end)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-3">
      <h3 class="text-2xl font-semibold text-gray-800">
        App source
      </h3>
      <p class="text-gray-700">
        This app is built from the following notebook source:
      </p>

      <.code_preview_with_title_and_copy
        title={Session.file_name_for_download(@session) <> ".livemd"}
        source_id="export-notebook-source"
        language="markdown"
        source={@source}
      />
    </div>
    """
  end
end
