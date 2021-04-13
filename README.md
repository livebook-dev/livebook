<h1><img src="https://github.com/elixir-nx/livebook/raw/main/priv/static/logo-with-text.png" alt="Livebook" width="400"></h1>

Livebook is a web application for writing interactive and collaborative code notebooks. It features:

  * A deployable web app built with [Phoenix LiveView](https://github.com/phoenixframework/phoenix_live_view)
    where users can create, fork, and run multiple notebooks.
  
  * Each notebook is made of multiple sections: each section is made of Markdown and Elixir
    cells. Code in Elixir cells can be evaluated on demand. Mathematical formulas are also
    supported via [KaTeX](https://katex.org/).

  * Persistence: notebooks can be persisted to disk through the `.livemd` format, which is a
    subset of Markdown. This means your notebooks can be saved for later, easily shared, and
    they also play well with version control.

  * Sequential evaluation: code cells run in a specific order, guaranteeing future users of
    the same Livebook see the same output. If you re-execute a previous cell, following cells
    are marked as stale to make it clear they depend on outdated notebook state.

  * Custom runtimes: when executing Elixir code, you can either start a fresh Elixir process,
    connect to an existing node, or run it inside an existing Elixir project, with access to
    all of its modules and dependencies. This means Livebook can be a great tool to provide
    live documentation for existing projects.

  * Explicit dependencies: if your notebook has dependencies, they are explicitly listed and
    installed with the help of the `Mix.install/2` command in Elixir v1.12+.

  * Collaborative features allow multiple users to work on the same notebook at once.
    Collaboration works either in single-node or multi-node deployments - without a
    need for additional tooling.

There is a [screencast by José Valim showing some of Livebook features](https://www.youtube.com/watch?v=RKvqc-UEe34).
Otherwise, here is a peek at the "Welcome to Livebook" introductory notebook:

![Screenshot](https://user-images.githubusercontent.com/9582/113567534-166f4980-960f-11eb-98df-c0b8b81f8a27.png)

The current version provides only the initial step of our Livebook vision. Our plan is to
continue focusing on visual, collaborative, and interactive features in the upcoming releases.

## Usage

For now, the best way to run Livebook is by cloning it and running it locally:

    $ git clone https://github.com/elixir-nx/livebook.git
    $ cd livebook
    $ mix deps.get --only prod
    $ MIX_ENV=prod mix phx.server

You will need [Elixir v1.11](https://elixir-lang.org/install.html) or later.

Keep in mind that Livebook is built to document and execute code. Anyone with
access to a Livebook instance will be able to access any file and execute any
code in the machine Livebook is running.

We will work on other distribution modes (escripts, Docker images, etc) once
we start distributing official releases.

## License

Copyright (C) 2021 Dashbit

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
