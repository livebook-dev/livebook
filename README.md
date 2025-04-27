# Livebook

[![Website](https://img.shields.io/badge/-Website-%23ff87a7)](https://livebook.dev/)
[![Latest Version](https://img.shields.io/hexpm/v/livebook?color=b5a3be&label=Latest+version)](https://hexdocs.pm/livebook)

Livebook is a web application for writing interactive and collaborative code notebooks. It features:

  * Code notebooks with Markdown support and Code cells where Elixir code is evaluated on demand.

  * Rich code editor through [CodeMirror](https://codemirror.net/): with support for autocompletion, inline documentation, code formatting, etc.

  * Interactive results via [Kino](https://github.com/elixir-nx/kino): display [Vega-Lite charts](https://vega.github.io/vega-lite/), tables, maps, and more.

  * Automation: use Smart cells to perform high-level tasks and write notebooks faster than ever. Query databases, plot charts, build maps, and more directly from Livebook's UI.

  * Reproducible: Livebook ensures your code runs in a predictable order, all the way down to package management. It also tracks your notebook state, annotating which parts are stale.

  * Collaboration: multiple users can work on the same notebook at once, no additional setup required.

  * Decentralized: Livebook is open-source and you can run it anywhere. The ["Run in Livebook" badge](https://livebook.dev/badge) makes it easy to import any Livebook into your own Livebook.

  * Versionable: notebooks are stored in the `.livemd` format, which is a subset of Markdown with support for diagrams via [Mermaid](https://mermaid-js.github.io/mermaid) and for mathematical formulas via [KaTex](https://katex.org/). `.livemd` files can be shared and play well with version control.

  * Custom runtimes: when executing Elixir code, you can either start a fresh Elixir instance, connect to an existing node, or run it inside an existing Elixir project, with access to all of its modules and dependencies. This means Livebook can be a great tool to introspect and document existing projects too.

## Getting started

Head out to [the Install section](https://livebook.dev/#install) of Livebook's website to get started. Once Livebook is up and running on your machine, **visit the "Learn" section** with introductory guides and documentation on several Livebook features. Here is a sneak peak of the "Welcome to Livebook" guide:

![Screenshot](https://github.com/livebook-dev/livebook/raw/main/.github/imgs/welcome.png)

There are more resources for those who are ready to dig deeper:

  * [Our official documentation](https://hexdocs.pm/livebook/) with in-depth guides

  * [Our Integrations page](https://livebook.dev/integrations/) covering the languages, data sources, and libraries that Livebook works with out-of-the-box

  * [Our blog](https://news.livebook.dev/) with news, posts, and screencasts

## Installation

We provide several methods for running Livebook,
pick the one that best fits your use case.

### Desktop app

  * [Download the installer for Mac and Windows from our homepage](https://livebook.dev/#install)

  * Latest stable builds: [Mac (Universal)](https://github.com/livebook-dev/livebook/releases/latest/download/LivebookInstall-macos-universal.dmg),
    [Windows](https://github.com/livebook-dev/livebook/releases/latest/download/LivebookInstall-windows-x86_64.exe)

  * Nightly builds: [Mac (Universal)](https://github.com/livebook-dev/livebook/releases/download/nightly/LivebookInstall-macos-universal.dmg),
    [Windows](https://github.com/livebook-dev/livebook/releases/download/nightly/LivebookInstall-windows-x86_64.exe)

  * Builds for particular Livebook version are available on our
    [GitHub releases](https://github.com/livebook-dev/livebook/releases).

### Docker

Running Livebook using Docker is another great option to run Livebook
in case you don't have Elixir installed.

```shell
# Running with the default configuration
docker run -p 8080:8080 -p 8081:8081 --pull always ghcr.io/livebook-dev/livebook

# In order to access and save notebooks directly to your machine
# you can mount a local directory into the container.
# Make sure to specify the user with "-u $(id -u):$(id -g)"
# so that the created files have proper permissions
docker run -p 8080:8080 -p 8081:8081 --pull always -u $(id -u):$(id -g) -v $(pwd):/data ghcr.io/livebook-dev/livebook

# You can configure Livebook using environment variables,
# for all options see the dedicated "Environment variables" section below
docker run -p 8080:8080 -p 8081:8081 --pull always -e LIVEBOOK_PASSWORD="securesecret" ghcr.io/livebook-dev/livebook

# Or if you need to run on different ports:
docker run -p 8090:8090 -p 8091:8091 --pull always -e LIVEBOOK_PORT=8090 -e LIVEBOOK_IFRAME_PORT=8091 ghcr.io/livebook-dev/livebook
```

To deploy Livebook on your cloud platform, see our [Docker Deployment](docs/deployment/docker.md) guide.

For CUDA support, [see images with the "cuda" tag](https://github.com/livebook-dev/livebook/pkgs/container/livebook).

To try out features from the main branch you can alternatively
use the `ghcr.io/livebook-dev/livebook:nightly` image.
See [Livebook images](https://github.com/livebook-dev/livebook/pkgs/container/livebook).

### Embedded devices

If you want to run Livebook on embedded devices, such as Raspberry Pi, BeagleBone, etc.,
check out [the Livebook firmware](https://github.com/nerves-livebook/nerves_livebook) built
with [Nerves](https://www.nerves-project.org/).

### Direct installation with Elixir

You can run Livebook on your own machine using just Elixir. You will need
[Elixir v1.18](https://elixir-lang.org/install.html) or later.
Livebook also requires the following Erlang applications: `inets`,
`os_mon`, `runtime_tools`, `ssl` and `xmerl`. Those applications come
with most Erlang distributions but certain package managers may split
them apart. For example, on Ubuntu, these Erlang applications can
be installed as follows:

```shell
sudo apt install erlang-inets erlang-os-mon erlang-runtime-tools erlang-ssl erlang-xmerl erlang-dev erlang-parsetools
```

**Note:** The [`livebook` package](https://hex.pm/packages/livebook)
is meant to be used as a CLI tool. Livebook is not officially
supported as a Mix/Hex dependency.

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

#### From source

You can run latest Livebook directly from source.

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

The following environment variables can be used to configure Livebook on boot:

  * `LIVEBOOK_ALLOW_URI_SCHEMES` - sets additional allowed hyperlink schemes to the
    Markdown content. Livebook sanitizes links in Markdown, allowing only a few
    standard schemes by default (such as http and https). Set it to a comma-separated
    list of schemes.

  * `LIVEBOOK_APP_SERVICE_NAME` - sets the application name used by the cloud
    provider to aid debugging.

  * `LIVEBOOK_APP_SERVICE_URL` - sets the application url to manage this
    Livebook instance within the cloud provider platform.

  * `LIVEBOOK_APPS_PATH` - the directory with app notebooks. When set, the apps
    are deployed on Livebook startup with the persisted settings. Password-protected
    notebooks will receive a random password, unless `LIVEBOOK_APPS_PATH_PASSWORD`
    is set. When deploying using Livebook's Docker image, consider using
    `LIVEBOOK_APPS_PATH_WARMUP`.

  * `LIVEBOOK_APPS_PATH_PASSWORD` - the password to use for all protected apps
    deployed from `LIVEBOOK_APPS_PATH`.

  * `LIVEBOOK_APPS_PATH_WARMUP` - sets the warmup mode for apps deployed from
    `LIVEBOOK_APPS_PATH`. Must be either "auto" (apps are warmed up on Livebook
    startup, right before app deployment) or "manual" (apps are warmed up when
    building the Docker image; to do so add "RUN /app/bin/warmup_apps" to
    your image). Defaults to "auto".

  * `LIVEBOOK_AWS_CREDENTIALS` - enable Livebook to read AWS Credentials from
    environment variables, AWS Credentials, EC2/ECS metadata when configuring
    S3 buckets.

  * `LIVEBOOK_BASE_URL_PATH` - sets the base url path the web application is
    served on. Useful when deploying behind a reverse proxy.

  * `LIVEBOOK_PUBLIC_BASE_URL_PATH` - sets the base url path the `/public/*` routes
    are served on. Note that this takes precedence over `LIVEBOOK_BASE_URL_PATH`,
    if both are set. Setting this may be useful to create exceptions when deploying
    behind a reverse proxy that requires au1thentication.

  * `LIVEBOOK_CACERTFILE` - path to a local file containing CA certificates.
    Those certificates are used during for server authentication when Livebook
    accesses files from external sources.

  * `LIVEBOOK_CLUSTER` - configures clustering strategy when running multiple
    instances of Livebook using either the Docker image or an Elixir release.
    See the "Clustering" docs for more information:
    https://hexdocs.pm/livebook/clustering.html

  * `LIVEBOOK_COOKIE` - sets the cookie for running Livebook in a cluster.
    Defaults to a random string that is generated on boot.

  * `LIVEBOOK_DATA_PATH` - the directory to store Livebook's internal
    configuration. Defaults to "livebook" under the default user data
    directory.

  * `LIVEBOOK_DEFAULT_RUNTIME` - sets the runtime type that is used by default
    when none is started explicitly for the given notebook. Must be either
    "standalone" (Standalone), "attached:NODE:COOKIE" (Attached node)
    or "embedded" (Embedded). Defaults to "standalone".

  * `LIVEBOOK_FIPS` - if set to "true", it enables the FIPS mode on startup.
    See more details in [the documentation](https://hexdocs.pm/livebook/fips.html).

  * `LIVEBOOK_FORCE_SSL_HOST` - sets a host to redirect to if the request is not over HTTPS.
    Note it does not apply when accessing Livebook via localhost. Defaults to nil.

  * `LIVEBOOK_HOME` - sets the home path for the Livebook instance. This is the
    default path used on file selection screens and others. Defaults to the
    user's operating system home.

  * `LIVEBOOK_IDENTITY_PROVIDER` - controls whether Zero Trust Authentication
    must be used for this Livebook instance. This is useful when deploying
    Livebook airgapped inside a cloud platform, such as Cloudflare and Google.
    Supported values are:

      * `basic_auth:<username>:<password>`
      * `cloudflare:<your-team-name (domain)>`
      * `google_iap:<your-audience (aud)>`
      * `tailscale:<tailscale-cli-socket-path>`
      * `custom:YourElixirModule`

    See our authentication docs for more information: https://hexdocs.pm/livebook/authentication.html

  * `LIVEBOOK_IFRAME_PORT` - sets the port that Livebook serves iframes at.
    This is relevant only when running Livebook without TLS. Defaults to 8081.

  * `LIVEBOOK_IFRAME_URL` - sets the URL that Livebook loads iframes from.
    By default iframes are loaded from local `LIVEBOOK_IFRAME_PORT` when accessing
    Livebook over http:// and from https://livebookusercontent.com when accessing over https://.

  * `LIVEBOOK_IP` - sets the ip address to start the web application on.
    Must be a valid IPv4 or IPv6 address.

  * `LIVEBOOK_LOG_LEVEL` - sets the logger level, allowing for more verbose
    logging, either of: error, warning, notice, info, debug. Defaults to warning.

  * `LIVEBOOK_LOG_METADATA` - a comma-separated list of metadata keys that should
    be included in the log messages. Currently the only Livebook-spcecific key is
    users (attached to evaluation and request logs). By default includes only
    request_id.

  * `LIVEBOOK_NODE` - sets the node name for running Livebook in a cluster.
    Note that Livebook always runs using long names distribution, so the
    node host name must use a fully qualified domain name (FQDN) or an IP
    address.

  * `LIVEBOOK_PASSWORD` - sets a password that must be used to access Livebook.
    Must be at least 12 characters. Defaults to token authentication.

  * `LIVEBOOK_PROXY_HEADERS` - a comma-separated list of headers that are set by
    proxies. For example, `x-forwarded-for,x-forwarded-proto`. Configuring those
    may be required when running Livebook behind reverse proxies.

  * `LIVEBOOK_PORT` - sets the port Livebook runs on. If you want to run multiple
    instances on the same domain with the same credentials but on different ports,
    you also need to set `LIVEBOOK_SECRET_KEY_BASE`. Defaults to 8080. If set to 0,
    a random port will be picked.

  * `LIVEBOOK_SECRET_KEY_BASE` - sets a secret key that is used to sign and encrypt
    the session and other payloads used by Livebook. Must be at least 64 characters
    long and it can be generated by commands such as: `openssl rand -base64 48`.
    Defaults to a random secret on every boot.

  * `LIVEBOOK_SHUTDOWN_ENABLED` - controls if a shutdown button should be shown
    in the homepage. Set it to "true" to enable it.

  * `LIVEBOOK_TOKEN_ENABLED` - controls whether token authentication is enabled.
    Enabled by default unless `LIVEBOOK_PASSWORD` is set. Set it to "false" to
    disable it.

  * `LIVEBOOK_UPDATE_INSTRUCTIONS_URL` - sets the URL to direct the user to for
    updating Livebook when a new version becomes available.

  * `LIVEBOOK_WITHIN_IFRAME` - controls if the application is running inside an
    iframe. Set it to "true" to enable it. If you do enable it, then the application
    must run with HTTPS.

The environment variables `ERL_AFLAGS` and `ERL_ZFLAGS` can also be set to configure
Livebook and the notebook runtimes. `ELIXIR_ERL_OPTIONS` are also available to customize
Livebook, but it is not forwarded to runtimes.

<!-- Environment variables -->

If running Livebook via the command line, run `livebook server --help` to see
all CLI-specific options.

### Livebook Desktop

When running Livebook Desktop, Livebook will invoke on boot a file named
`~/.livebookdesktop.sh` on macOS or `%USERPROFILE%\.livebookdesktop.bat`
on Windows. This file can set environment variables used by Livebook,
such as:

  * [the `PATH` environment variable](https://en.wikipedia.org/wiki/PATH_(variable))

  * or to configure the Erlang VM, for instance, by setting
    `ERL_AFLAGS="-proto_dist inet6_tcp"` if you need Livebook to run over IPv6

Be careful when modifying boot files, Livebook may be unable to start if
configured incorrectly.

## Development

Livebook is primarily a Phoenix web application and can be setup as such:

```shell
git clone https://github.com/livebook-dev/livebook.git
cd livebook
mix setup

# Run the Livebook server
mix phx.server

# Run tests
mix test
```

### Acknowledgements

Thank you to [Uffizzi](https://www.uffizzi.com) for providing ephemeral environments to preview pull requests.

### Desktop app builds

For macOS, run:

```shell
# Test macOS app locally
(cd rel/app/macos && ./run.sh)

# Build macOS installer
.github/scripts/app/build_macos.sh
```

For Windows, run:

```shell
# Test Windows app locally
(cd rel/app/windows && ./run.sh)

# Build Windows installer
.github/scripts/app/build_windows.sh
```

## Platinum sponsors

<a href="https://fly.io">
 <picture>
    <source media="(prefers-color-scheme: dark)" srcset="https://fly.io/public/images/brand/logo-inverted.svg">
    <source media="(prefers-color-scheme: light)" srcset="https://fly.io/public/images/brand/logo.svg">
    <img height="130" src="https://fly.io/public/images/brand/logo.svg" alt="Fly.io">
  </picture>
</a>

Fly is a platform for running full stack apps and databases close to your users.

## Sponsors

<a href="https://huggingface.co/">
  <img height="70" src="https://huggingface.co/datasets/huggingface/brand-assets/resolve/main/hf-logo-with-title.png" alt="Hugging Face">
</a>

The platform where the machine learning community<br />
collaborates on models, datasets, and applications.

<br />

<a href="https://www.tigrisdata.com/">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="https://www.tigrisdata.com/docs/logo/dark.png">
    <source media="(prefers-color-scheme: light)" srcset="https://www.tigrisdata.com/docs/logo/light.png">
    <img height="50" src="https://www.tigrisdata.com/docs/logo/light.png" alt="Tigris">
  </picture>
</a>

Tigris is a globally distributed S3-compatible object storage<br />
service that provides low latency anywhere in the world.

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
