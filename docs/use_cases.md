# Use cases

There are many ways Elixir developers use and leverage Livebook today.

## Documentation with `Mix.install`

Livebook is an excellent tool for documentation. Many Elixir packages use
Livebook as tutorials but you can also directly run Livebook within the context
of your existing application with the help of `Mix.install/2`.

As an example, imagine you have created a notebook inside your current project,
at `notebooks/example.livemd`. In order to run within the root Mix project, using
the same configuration and dependencies versions, you can change your notebook
setup cell to invoke `Mix.install/2` with the following arguments:

```elixir
Mix.install(
  [
    {:my_app, path: Path.join(__DIR__, ".."), env: :dev}
  ],
  config_path: :my_app,
  lockfile: :my_app
)
```

## Deploying custom apps and internal tooling

Your Livebook notebooks can be deployed as applications which
you may then share within your team and company. Docker deployment
is provided out of the box and you can automate your deployment,
share secrets, configure authentication and more with
[Livebook Teams](https://livebook.dev/teams).

## Communication and automation of Elixir systems

You may also deploy notebooks as applications that automate and monitor
live Elixir systems. By clicking on "+ Smart cell", you will find Livebook
provides a "Remote execution" widget, that allows you to configure the
node name, cookie, and the code you want to execute on the remote node.

The node and cookie information are configured directly in the production
system you want to connect to. For example, to connect to a
[Phoenix application](https://phoenixframework.org/) running on your machine,
you may start it as follows:

```shell
$ iex --name phoenix-app@127.0.0.1 --cookie secret -S mix phx.server
```

With this information in hand, you can query and automate tasks within
existing Elixir systems. You may also mix remote execution with Livebook's
rich features to deploy applications that interact with those systems.

## Debugging live systems (with attached mode)

Livebook uses the concept of a **runtime**, which in practice is an Elixir node
responsible for evaluating your code. You can choose the runtime by clicking
the "Runtime" icon on the sidebar (or by using the `s` `r` keyboard shortcut).

By default, a new Elixir runtime is started (similarly to starting `iex`)
for each notebook. You can click reconnect whenever you want to discard the
current runtime and start a new one.

You can also manually *attach* to an existing node by picking the "Attached Node"
runtime. While in the previous section we used the "Remote execution" smart cell
to connect the default Livebook runtime to an existing node, the "Attached Node"
will make it so the Livebook runtime itself runs within the external node.

To do so, open up a new notebook and click the "Runtime" icon on the sidebar.
Click to "Configure" the runtime and choose "Attached node". Input the
name and cookie from the remote node and you should be ready to connect
to it. Once connected, be careful: any code that you execute in the notebook
now runs within the connected application. You are also limited on actions
you may perform. For example, you can't install dependencies (nor would that
be a good idea on a running system).

You may also [connect your local Livebook instance to a node running in
production depending on your platform](https://fly.io/docs/elixir/advanced-guides/connect-livebook-to-your-app/).

## Scaffolding embedded systems with Nerves

If you want to run Livebook on embedded devices, such as Raspberry Pi,
BeagleBone, etc., check out [the Livebook
firmware](https://github.com/nerves-livebook/nerves_livebook) built
with [Nerves](https://www.nerves-project.org/). In such cases, Livebook
uses a special runtime, called the Embedded Runtime, where all of your
code runs within Livebook itself, without starting additional runtimes
(which may be too expensive on limited devices).

## Ready to get started?

Head out to [Livebook homepage](https://livebook.dev/) to install it.
Once you have it up and running, head to the "Learn" section on the
sidebar to learn more!
