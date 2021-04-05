<h1><img src="https://github.com/elixir-nx/livebook/raw/main/priv/static/logo-with-text.png" alt="Livebook" width="400"></h1>

Livebook is a tool for crafting interactive and collaborative code notebooks. It features:

  * An interactive web application built with [Phoenix LiveView](https://github.com/phoenixframework/phoenix_live_view)
    where users can create, fork, and run multiple notebooks.
  
  * Each notebook is made of multiple sections: each section is made of Markdown and Elixir
    cells. Code in Elixir cells can be evaluated on demand.

  * Persistence: notebooks can be persisted to disk using the `.livemd` format, which is a
    subset of Markdown. This means your notebooks can be saved for later, easily shared, and
    also play well with version control.

  * Sequential evaluation: code cells run in a specific order, guaranteeing future users of
    the same Livebook see the same output.

  * Custom runtimes: when executing Elixir code, you can either start a fresh Elixir process,
    connect to an existing node, or run it inside an existing Elixir project, with access to
    all of its modules and dependencies. This means Livebook can be a great tool to provide
    live documentation for existing projects.

  * Collaborative features allow multiple users to work on the same notebook at once.
    Collaboration works either in single-node or multi-node deployments - without a
    need for additional tooling.

Here is a peek at the "Welcome to Livebook" introductory notebook:

![Screenshot](https://user-images.githubusercontent.com/9582/113567534-166f4980-960f-11eb-98df-c0b8b81f8a27.png)

Note the current release provides only the initial step of our Livebook vision. Our plan
is to continue focusing on visual, collaborative, and interactive features in the upcoming
releases.

## Usage

For now, the best way to run Livebook is by cloning it and running it locally:

    $ git clone https://github.com/elixir-nx/livebook.git
    $ cd livebook
    $ mix deps.get --only prod
    $ MIX_ENV=prod mix phx.server

You will need [Elixir v1.11](https://elixir-lang.org/install.html) or later.

We will work on other distribution modes (escripts, Docker images, etc) in future
releases.

## License

Copyright (C) 2021 Dashbit

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

#### Rationale

*Note: this section is not legal advice and does not give you any guarantees
beyond those listed in the LICENSE file.*

Dashbit has released many open source projects throughout the years under
the Apache 2 License. However, for Livebook, we choose to use AGPL v3.
The reason for this choice is because Livebook is an *application* and not
a library. Other projects that use AGPL are [RStudio](https://www.rstudio.com/)
and [ScyllaDB](https://www.scylladb.com/).

Livebook's LICENSE does not impact your own notebooks. All of the notebooks
you write belong to you and under the license of your choice (if any).
However, if you make changes to Livebook source code, your modifications
have to be available for those using your version of Livebook. For example,
if you change Livebook to run it exclusively inside your own company,
your changes only have to be available within the company itself. However,
if you modify Livebook in order to build your own software package or your
own service, then you must link back to Livebook and distribute the modified
source code under the same license as Livebook.

At the end of the day, it is not different from using a database that is AGPL
licensed: your data, the queries you write, and the applications that talk to
the DB are all distinct and separate from the database itself. However, if you
want to add new functionality to the database and charge a fee for others to
use it, then you need to make your derived source code public too.

Here is further reading for those interested:

  * [GNU Affero General Public License](http://www.gnu.org/licenses/agpl-3.0.html)
  * [Understanding the AGPL: The Most Misunderstood License](https://medium.com/swlh/understanding-the-agpl-the-most-misunderstood-license-86fd1fe91275)
