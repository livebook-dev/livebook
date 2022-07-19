<h1>
  <a href="https://livebook.dev/" target="_blank">
   <img src="https://github.com/livebook-dev/livebook/raw/main/static/images/logo-with-text.png" alt="Livebook" width="400">
  </a>
</h1>

 [![Website](https://img.shields.io/badge/-Website-%23ff87a7)](https://livebook.dev/) [![Hex.pm](https://img.shields.io/hexpm/v/livebook?color=b5a3be)](https://hex.pm/packages/livebook)

Livebook is a web application for writing interactive and collaborative code notebooks for Elixir, built with [Phoenix LiveView](https://github.com/phoenixframework/phoenix_live_view). It features:

  * Code notebooks with Markdown support and Code cells where Elixir code is evaluated on demand.

  * Rich code editor through [Monaco](https://microsoft.github.io/monaco-editor/): with support for autocompletion, inline documentation, code formatting, etc.

  * Interactive results via [Kino](https://github.com/elixir-nx/kino): display [Vega-Lite charts](https://vega.github.io/vega-lite/), tables, maps, and more.

  * Automation: use Smart cells to perform high-level tasks and write notebooks faster than ever. Query databases, plot charts, build maps, and more directly from Livebook's UI.

  * Reproducible: Livebook ensures your code runs in a predictable order, all the way down to package management. It also tracks your notebook state, annotating which parts are stale.

  * Collaboration: multiple users can work on the same notebook at once, no additional setup required.

  * Decentralized: Livebook is open-source and you can run it anywhere. The ["Run in Livebook" badge](https://livebook.dev/badge) makes it easy to import any Livebook into your own Livebook.

  * Versionable: notebooks are stored in the `.livemd` format, which is a subset of Markdown with support for diagrams via [Mermaid](https://mermaid-js.github.io/mermaid) and for mathematical formulas via [KaTex](https://katex.org/). `.livemd` files can be shared and play well with version control.

  * Custom runtimes: when executing Elixir code, you can either start a fresh Elixir instance, connect to an existing node, or run it inside an existing Elixir project, with access to all of its modules and dependencies. This means Livebook can be a great tool to introspect and document existing projects too.

We are actively working on Livebook and you can consult the issues tracker to see some of the features we are exploring!

## Getting started

Livebook comes with a series of introductory notebooks to get you up and running. Just head down to the "Installation" section below to install it. Here is a peek at the "Welcome to Livebook" introductory notebook:

![Screenshot](https://github.com/livebook-dev/livebook/raw/main/.github/imgs/welcome.png)

For screencasts and news, check out [news.livebook.dev](https://news.livebook.dev/).

## Installation

We provide several methods for running Livebook,
pick the one that best fits your use case.

### On the cloud

  * [Launch a Livebook instance close to you on Fly.io](https://fly.io/launch/livebook)

### Desktop app

  * [Download the installer for Windows and Mac from our homepage](https://livebook.dev/)

### Docker

Running Livebook using Docker is a great option for cloud deployments
and also for local usage in case you don't have Elixir installed.

```shell
# Running with the default configuration
docker run -p 8080:8080 -p 8081:8081 --pull always livebook/livebook

# In order to access and save notebooks directly to your machine
# you can mount a local directory into the container.
# Make sure to specify the user with "-u $(id -u):$(id -g)"
# so that the created files have proper permissions
docker run -p 8080:8080 -p 8081:8081 --pull always -u $(id -u):$(id -g) -v $(pwd):/data livebook/livebook

# You can configure Livebook using environment variables,
# for all options see the dedicated "Environment variables" section below
docker run -p 8080:8080 -p 8081:8081 --pull always -e LIVEBOOK_PASSWORD="securesecret" livebook/livebook
```

To try out features from the main branch you can alternatively
use the `livebook/livebook:edge` image.
See [Livebook on Docker Hub](https://hub.docker.com/r/livebook/livebook/tags?page=1&ordering=last_updated).

### Embedded devices

If you want to run Livebook on embedded devices, such as Raspberry Pi, BeagleBone, etc.,
check out [our Livebook firmware](https://github.com/livebook-dev/nerves_livebook) built
with [Nerves](https://www.nerves-project.org/).

### Direct installation with Elixir

You can run Livebook on your own machine using just Elixir. You will need
[Elixir v1.13](https://elixir-lang.org/install.html) or later.
Livebook also requires the following Erlang applications: `inets`,
`os_mon`, `runtime_tools`, `ssl` and `xmerl`. Those applications come
with most Erlang distributions but certain package managers may split
them apart. For example, on Ubuntu, these Erlang applications can
be installed as follows:

```shell
sudo apt install erlang-inets erlang-os-mon erlang-runtime-tools erlang-ssl erlang-xmerl erlang-dev erlang-parsetools
```

#### Escript

Running Livebook using Escript makes for a very convenient option
for local usage and provides easy configuration via CLI options.

```shell
mix do local.rebar --force, local.hex --force
mix escript.install hex livebook

# Start the Livebook server
livebook server

# See all the configuration options
livebook server --help
```

After you install the escript, make sure you add the directory where
Elixir keeps escripts to your [$PATH](https://en.wikipedia.org/wiki/PATH_(variable)).
If you installed Elixir with `asdf`, you'll need to run `asdf reshim elixir`
once the escript is built.

To try out features from the main branch you can alternatively
install the escript directly from GitHub like this:

```shell
mix escript.install github livebook-dev/livebook
```

#### Mix

You can run latest Livebook directly with Mix.

```shell
git clone https://github.com/livebook-dev/livebook.git
cd livebook
mix deps.get --only prod

# Run the Livebook server
MIX_ENV=prod mix phx.server
```

## Security considerations

Livebook is built to document and execute code. Anyone with access to a Livebook instance
will be able to access any file and execute any code in the machine Livebook is running.

For this reason, Livebook only binds to the 127.0.0.1, allowing access to happen only within
the current machine. When running Livebook in the production environment - the recommended
environment - we also generate a token on initialization and we only allow access to the
Livebook if said token is supplied as part of the URL.

## Environment variables
<!-- Environment variables -->

The following environment variables configure Livebook:

  * LIVEBOOK_APP_SERVICE_NAME - sets the application name used by the cloud
    provider to aid debugging.

  * LIVEBOOK_APP_SERVICE_URL - sets the application url to manage this
    Livebook instance within the cloud provider platform.

  * LIVEBOOK_COOKIE - sets the cookie for running Livebook in a cluster.
    Defaults to a random string that is generated on boot.

  * LIVEBOOK_DATA_PATH - the directory to store Livebook configuration.
    Defaults to "livebook" under the default user data directory.

  * LIVEBOOK_DEFAULT_RUNTIME - sets the runtime type that is used by default
    when none is started explicitly for the given notebook. Must be either
    "standalone" (Elixir standalone), "mix[:PATH][:FLAGS]" (Mix standalone),
    "attached:NODE:COOKIE" (Attached node) or "embedded" (Embedded).
    Defaults to "standalone".

  * LIVEBOOK_FORCE_SSL_HOST - sets a host to redirect to if the request is not over HTTP.
    Note it does not apply when accessing Livebook via localhost. Defaults to nil.

  * LIVEBOOK_HOME - sets the home path for the Livebook instance. This is the
    default path used on file selection screens and others. Defaults to the
    user's operating system home.

  * LIVEBOOK_IFRAME_PORT - sets the port that Livebook serves iframes at.
    This is relevant only when running Livebook without TLS. Defaults to 8081.

  * LIVEBOOK_IFRAME_URL - sets the URL that Livebook loads iframes from.
    By default iframes are loaded from local LIVEBOOK_IFRAME_PORT when accessing
    Livebook over http:// and from https://livebook.space when accessing over `https://`.

  * LIVEBOOK_IP - sets the ip address to start the web application on.
    Must be a valid IPv4 or IPv6 address.

  * LIVEBOOK_PASSWORD - sets a password that must be used to access Livebook.
    Must be at least 12 characters. Defaults to token authentication.

  * LIVEBOOK_PORT - sets the port Livebook runs on. If you want to run multiple
    instances on the same domain with the same credentials but on different ports,
    you also need to set LIVEBOOK_SECRET_KEY_BASE. Defaults to 8080. If set to 0,
    a random port will be picked.

  * LIVEBOOK_SECRET_KEY_BASE - sets a secret key that is used to sign and encrypt
    the session and other payloads used by Livebook. Must be at least 64 characters
    long and it can be generated by commands such as: 'openssl rand -base64 48'.
    Defaults to a random secret on every boot.

  * LIVEBOOK_SHUTDOWN_ENABLED - controls if a shutdown button should be shown
    in the homepage. Set it to "true" to enable it.

  * LIVEBOOK_TOKEN_ENABLED - controls whether token authentication is enabled.
    Enabled by default unless `LIVEBOOK_PASSWORD` is set. Set it to "false" to
    disable it.

  * LIVEBOOK_UPDATE_INSTRUCTIONS_URL - sets the URL to direct the user to for
    updating Livebook when a new version becomes available.

<!-- Environment variables -->

If running Livebook as a Docker image or an Elixir release, [the environment
variables used by Elixir releases are also available](
https://hexdocs.pm/mix/Mix.Tasks.Release.html#module-environment-variables).
The notables ones are `RELEASE_NODE` and `RELEASE_DISTRIBUTION`.

## Rendering notebooks as Markdown on GitHub

By default GitHub renders the `.livemd` notebooks as regular text files. Depending
on your use case and the target audience, you may find it useful to render notebooks
content as Markdown files instead. There is an option to override how a particular
file gets rendered on GitHub, so all you need to do is add a magic comment in every
such notebook:

```
<!-- vim: set syntax=markdown: -->

# My notebook

...
```

For more details see [the documentation](https://github.com/github/linguist/blob/master/docs/overrides.md#using-emacs-or-vim-modelines).

## Development

Livebook is primarily a Phoenix web application and can be setup as such:

```shell
git clone https://github.com/livebook-dev/livebook.git
cd livebook
mix dev.setup

# Run the Livebook server
mix phx.server

# To test escript
MIX_ENV=prod mix escript.build
./livebook server
```

## Sponsors

Livebook development is sponsored by:

<a href="https://fly.io"><img src="https://fly.io/public/images/brand/logo.svg" width="320" /></a>

## Continuous Integration

Our CI server and desktop app for macOS are powered by:

<a href="https://www.macstadium.com"><img src="https://user-images.githubusercontent.com/9582/151619816-b7794798-8261-46c8-bb88-d12108e3ff12.png" width="320" /></a>

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
