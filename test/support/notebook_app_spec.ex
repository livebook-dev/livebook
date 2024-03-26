defmodule Livebook.Apps.NotebookAppSpec do
  # App spec carrying an in-memory notebook.

  defstruct [:slug, :notebook, :load_failures, :should_warmup, version: "1"]

  def new(notebook, opts \\ []) do
    opts = Keyword.validate!(opts, load_failures: 0, should_warmup: false)

    slug = notebook.app_settings.slug

    :persistent_term.put({slug, :failures}, 0)

    %__MODULE__{
      slug: slug,
      notebook: notebook,
      load_failures: opts[:load_failures],
      should_warmup: opts[:should_warmup]
    }
  end
end

defimpl Livebook.Apps.AppSpec, for: Livebook.Apps.NotebookAppSpec do
  def load(app_spec, _files_tmp_path) do
    key = {app_spec.slug, :failures}
    num_failures = :persistent_term.get(key)

    if num_failures < app_spec.load_failures do
      :persistent_term.put(key, num_failures + 1)
      {:error, "failed to load"}
    else
      {:ok, %{notebook: app_spec.notebook, warnings: []}}
    end
  end

  def should_warmup?(app_spec) do
    app_spec.should_warmup
  end
end
