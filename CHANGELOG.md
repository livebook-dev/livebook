# Changelog for Livebook v0.15

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [v0.15.0](https://github.com/livebook-dev/livebook/tree/v0.15.0) (2025-02-21)

### Added

* Support for defining modules in the Erlang cell ([#2806](https://github.com/livebook-dev/livebook/pull/2806))
* Option to specifying erl flags for Standalone runtime node ([#2843](https://github.com/livebook-dev/livebook/pull/2843))
* Support for specifying max height on grid output ([#2846](https://github.com/livebook-dev/livebook/pull/2846))
* Introduced authentication via Livebook Teams for Livebook app servers, including third-party SSO integrations ([#2837](https://github.com/livebook-dev/livebook/pull/2837), [#2919](https://github.com/livebook-dev/livebook/pull/2919))
* HTTP proxy configuration via env variables (`HTTP_PROXY`, `HTTPS_PROXY`) ([#2850](https://github.com/livebook-dev/livebook/pull/2850))
* Deployment group environment variables included in generated Livebook app server instructions ([#2858](https://github.com/livebook-dev/livebook/pull/2858))
* Added Clickhouse to the database connection cell options ([#2840](https://github.com/livebook-dev/livebook/pull/2840))
* Logs whenever code is evaluated evaluation (level info) ([#2880](https://github.com/livebook-dev/livebook/pull/2880))
* Added `LIVEBOOK_LOG_LEVEL` and `LIVEBOOK_LOG_METADATA` for customizing logged information ([#2880](https://github.com/livebook-dev/livebook/pull/2880))
* Shell code block highlighting in Markdown cells
* Added a custom view option that hides code ([#2889](https://github.com/livebook-dev/livebook/pull/2889))
* Information accessed via `Kino.Workspace.info()` to include payload returned from identity provider ([#2890](https://github.com/livebook-dev/livebook/pull/2890))
* Support for text styles in plain text output ([#2928](https://github.com/livebook-dev/livebook/pull/2928))
* Support for `nil` fields in forms ([#2931](https://github.com/livebook-dev/livebook/pull/2931))

### Changed

* (Desktop) The server to start on a fixed port by default (`32123`) and fall back to random if taken ([#2867](https://github.com/livebook-dev/livebook/pull/2867))
* Bumped versions to Elixir 1.18.2 and Erlang 27.2.2 in the Docker image and the desktop app
* Bumped the required Elixir version to 1.18
* In case you set `RELEASE_DISTRIBUTION`, it has been ignored since v0.13, but now it must not be set
* Changed the Docker image to use Ubuntu 24.04
* Updateed Docker base image to Ubuntu 24.04 ([#2933](https://github.com/livebook-dev/livebook/pull/2933))
* Improveed file select to list files matching search anywhere in the name ([#2932](https://github.com/livebook-dev/livebook/pull/2932))

### Removed

* Removed `LIVEBOOK_APPS_PATH_HUB_ID` in favour of `LIVEBOOK_TEAMS_AUTH` ([#2829](https://github.com/livebook-dev/livebook/pull/2829))

### Fixed

* Proper namespace to be selected when reopening K8s runtime settings
* Importing notebook via livebook.dev/run to download attachments
* Kino forms to not emit change event on the first render ([#2852](https://github.com/livebook-dev/livebook/pull/2852))
* (Desktop) "Copy URL" not pasting properly into Terminal
* (Desktop) Subsequent installations on Windows leading to conflicting Hex archives ([#2859](https://github.com/livebook-dev/livebook/pull/2859))
* Erlang snippets in Markdown cells being exported as Erlang code cells
* (Desktop) Fixed opening notebook files with space in the path ([#2926](https://github.com/livebook-dev/livebook/pull/2926))
* Tab content disappearning in nested tab outputs

## v0.14

The CHANGELOG for v0.14 releases can be found in the [v0.14](https://github.com/livebook-dev/livebook/tree/v0.14/CHANGELOG.md) branch.
