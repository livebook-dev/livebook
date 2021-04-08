defmodule Livebook.Notebook.Welcome do
  livemd = ~s'''
  # Welcome to Livebook

  ## Introduction

  We are happy you decided to give Livebook a try, hopefully it empowers
  you to build great stuff! 🚀

  Livebook is a tool for crafting **interactive** and **collaborative** code notebooks.
  It is primarily meant as a tool rapid prototyping - think of it as an IEx session
  combined with your editor.
  You can also use it for authoring shareable articles that people can easily
  run and play around with.
  Package authors can write notebooks as interactive tutorials
  and include them in their repository, so that users can easily download
  and run them locally.

  ## Basic usage

  Each notebook consists of a number of cells, which serve as primary building blocks.
  There are **Markdown** cells (such as this one) that allow you to describe your work
  and **Elixir** cells where the magic takes place!

  To insert a new cell move your cursor between cells and click one of the revealed buttons. 👇

  ```elixir
  # This is an Elixir cell - as the name suggests that's where the code goes.
  # To evaluate this cell, you can either press the "Evaluate" button above
  # or use `Ctrl + Enter`!

  message = "hey, grab yourself a cup of 🍵"
  ```

  Subsequent cells have access to the bindings you defined:

  ```elixir
  String.replace(message, "🍵", "☕")
  ```

  Note however that bindings are not global, so each cell *sees* only stuff that goes
  above itself. This approach helps to keep the notebook clean and predictable
  as you keep working on it!

  ## Sections

  You can leverage so called **sections** to nicely group related cells together.
  Click on the "Book" icon in the sidebar to reveal a list of all sections.
  As you can see, this approach helps to easily jump around the notebook,
  especially once it grows.

  Let's make use of this section to see how output is captured!

  ```elixir
  cats = ~w(😼 😹 😻 😺 😸 😽)

  for _ <- 1..3 do
    cats
    |> Enum.take_random(3)
    |> Enum.join(" ")
    |> IO.puts()
  end
  ```

  ## Notebook files

  By default notebooks are kept in memory, which is fine for interactive hacking,
  but oftentimes you will want to save your work for later. Fortunately notebooks
  can be persisted by clicking on the "Settings" icon in the sidebar
  and selecting the file location.

  Notebooks are stored in **live markdown** format, which is essentially the markdown you know,
  with just a few assumptions on how particular elements are represented. Thanks to this
  approach you can easily keep notebooks under version control and get readable diffs.
  You can also easily preview those files, reuse for blog posts, and even edit in a text editor.

  ## Modules

  As we already saw, Elixir cells can be used for working on tiny snippets,
  but you may as well define a module!

  ```elixir
  defmodule Utils do
    @doc """
    Generates a random binary id.
    """
    @spec random_id() :: binary()
    def random_id() do
      :crypto.strong_rand_bytes(20) |> Base.encode32(case: :lower)
    end
  end
  ```

  If you're surprised by the above output, keep in mind that
  every Elixir expression evaluates to some value and as so does module compilation!

  Having the module defined, let's take it for a spin.

  ```elixir
  Utils.random_id()
  ```

  ## Imports

  You can import modules as normally to make the imported functions visible
  to all subsequent cells. Usually you want to keep `import`, `alias` and `require`
  in the first section, as part of the notebook setup.

  ```elixir
  import IEx.Helpers
  ```

  ```elixir
  h(Enum.map())
  ```

  ```elixir
  # Sidenote: http://www.numbat.org.au/thenumbat
  i("I ❤️ Numbats")
  ```

  ## Runtimes

  Livebook has a concept of **runtime**, which in practice is an Elixir node responsible
  for evaluating your code.

  By default a new Elixir node is started (similarly to starting `iex`),
  but you can also choose to run inside a Mix project (as you would with `iex -S mix`)
  or even manually attach to an existing distributed node!
  You can configure the runtime by clicking the "Settings" icon on the sidebar.

  ## Using packages

  Sometimes you need a dependency or two and notebooks are no exception to this.

  One way to work with packages is to create a Mix project and configure the notebook
  to run in its context (as pointed out above). This approach makes sense if you already have
  a Mix project that you are working on, especially because this makes all project's
  modules available as well.

  But there are cases when you just want to play around with a new package
  or quickly prototype some code that relies on such. Fortunately, starting
  version `v1.12` Elixir ships with `Mix.install/2` that allows for installing
  dependencies into Elixir runtime! This approach is especially useful when sharing notebooks,
  because everyone will be able to get the same dependencies. Let's try this out:

  ```elixir
  # Note: this requires Elixir version >= 1.12
  Mix.install([
    {:jason, "~> 1.2"}
  ])
  ```

  ```elixir
  %{elixir: "is awesome"}
  |> Jason.encode!()
  |> IO.puts()
  ```

  It is a good idea to specify versions of the installed packages,
  so that the notebook is easily reproducible later on.

  Also keep in mind that `Mix.install/2` can be called only once
  per runtime, so if you need to modify the dependencies, you should
  go to the notebook runtime configuration and **reconnect** the current runtime.

  ## Math

  Livebook supports both inline formulas like $e^{\\pi i} + 1 = 0$, as well as block formulas:

  $$
  S(x) = \\frac{1}{1 + e^{-x}} = \\frac{e^{x}}{e^{x} + 1}
  $$

  You can explore all supported expressions [here](https://katex.org/docs/supported.html).

  ## Stepping up your workflow

  Once you start using notebooks more, it's gonna be beneficial
  to optimise how you move around. Livebook leverages the concept of
  **navigation**/**insert** modes and offers many shortcuts for common operations.
  Make sure to check out the shortcuts by clicking the "Keyboard" icon in
  the sidebar or by typing `?`.

  ## Final notes

  Livebook is an open source project, so feel free to look into
  [the repository](https://github.com/elixir-nx/livebook)
  to contribute, report bugs, suggest features or just skim over the codebase.

  Now go ahead and build something cool! 🚢
  '''

  {notebook, []} = Livebook.LiveMarkdown.Import.notebook_from_markdown(livemd)

  @notebook notebook

  def new() do
    @notebook
  end
end
