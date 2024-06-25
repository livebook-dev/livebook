# Changelog for Livebook v0.13

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [v0.13.0](https://github.com/livebook-dev/livebook/tree/v0.13.0) (2024-06-25)

### Added

* Warning on the export page if there are stale output ([#2420](https://github.com/livebook-dev/livebook/pull/2420))
* Copy button to error outputs ([#2435](https://github.com/livebook-dev/livebook/pull/2435))
* Improved latency for showing completions in Remote execution cell ([#2447](https://github.com/livebook-dev/livebook/pull/2447))
* Reduced the time of `Mix.install/2` when adding a new dependency ([#2499](https://github.com/livebook-dev/livebook/pull/2499))
* Support for Web Bluetooth and Web Serial APIs within iframes ([#2514](https://github.com/livebook-dev/livebook/pull/2514))
* Added `LIVEBOOK_CLUSTER=auto` to automatically configure the cluster on Fly.io ([#2519](https://github.com/livebook-dev/livebook/pull/2519))
* Healthcheck definition to the Livebook Docker image ([#2517](https://github.com/livebook-dev/livebook/pull/2517))
* Persistence of app password in the notebook, encrypted in the stamp metadata ([#2550](https://github.com/livebook-dev/livebook/pull/2550))
* Support for Basic Authentication ZTA ([#2564](https://github.com/livebook-dev/livebook/pull/2564))
* Added `LIVEBOOK_FIPS` for enabling FIPS mode ([#2526](https://github.com/livebook-dev/livebook/pull/2526))
* Actions when dropping .xslx/.xslm files into the notebook ([#2577](https://github.com/livebook-dev/livebook/pull/2577))
* Deploying Livebook Apps to a private app server via Livebook Teams ([#2602](https://github.com/livebook-dev/livebook/pull/2602) and countless more)
* Added `LIVEBOOK_PROXY_HEADERS` useful when running Livebook behind a reverse proxy ([#2604](https://github.com/livebook-dev/livebook/pull/2604))
* Settings option to render ligatures in code editor ([#2609](https://github.com/livebook-dev/livebook/pull/2609))
* Functionality for Livebook to proxy requests to a user-defined handler. See `Kino.Proxy` for more details ([#2608](https://github.com/livebook-dev/livebook/pull/2608), [#2615](https://github.com/livebook-dev/livebook/pull/2615), [#2617](https://github.com/livebook-dev/livebook/pull/2617), [#2618](https://github.com/livebook-dev/livebook/pull/2618))
* List of connected distribution nodes to the runtime panel ([#2636](https://github.com/livebook-dev/livebook/pull/2636))
* FLAME support ([#2629](https://github.com/livebook-dev/livebook/pull/2629))

### Changed

* The code editor now uses CodeMirror. This change features a polished design, more precise code highlighting, minimised delay when opening large notebooks, more accurate collaborative cursors and other improvements ([#2444](https://github.com/livebook-dev/livebook/pull/2444))
* Cell evaluation time to also show when the cell is stale or aborted
* Changed the behaviour of `__DIR__` when the notebook has no explicit file, so that it points to the autosave location (and not `"."`) ([#2529](https://github.com/livebook-dev/livebook/pull/2529))
* Removed the invalid ":" character from Livebook cookie names. This change invalidates existing session data, such as user name and cursor color ([#2539](https://github.com/livebook-dev/livebook/pull/2539))
* Apps to respect automatically reevaluating cells ([#2569](https://github.com/livebook-dev/livebook/pull/2569))
* Renamed "hubs" to "workspaces" ([#2574](https://github.com/livebook-dev/livebook/pull/2574))
* (Desktop) Running without requiring EPMD ([#2591](https://github.com/livebook-dev/livebook/pull/2591))
* Code cells are no longer formatted on save ([#2605](https://github.com/livebook-dev/livebook/pull/2605))
* Livebook within Docker to bind to IPv6 by default
* Bumped versions to Elixir 1.17.1 and OTP 27 in the Docker image and the desktop app
* Livebook and runtimes to always run distribution in long names mode ([#2646](https://github.com/livebook-dev/livebook/pull/2646), [#2648](https://github.com/livebook-dev/livebook/pull/2648))

### Removed

* Support for rendering images from the deprecated `images/` directory
* Zero Trust authentication strategy for Teleport ([#2589](https://github.com/livebook-dev/livebook/pull/2589))

### Fixed

* File download in the .exs notebook export
* (Desktop) `~/.livebookdesktop.sh` to allow setting `LIVEBOOK_NODE` and `LIVEBOOK_SHUTDOWN_ENABLED` ([#2464](https://github.com/livebook-dev/livebook/pull/2464))
* (Desktop) Don't create duplicate log lines on Windows ([#2668](https://github.com/livebook-dev/livebook/pull/2668))
* Using environment variables for S3 file system credentials ([#2472](https://github.com/livebook-dev/livebook/pull/2472))
* Redesigned flash messages to allow copying the message without closing it ([#2484](https://github.com/livebook-dev/livebook/pull/2484))
* Completion relevance within multiline maps and bitstrings ([#2488](https://github.com/livebook-dev/livebook/pull/2488))
* File selector crashing when navigating to a directory with a large number of files ([#2491](https://github.com/livebook-dev/livebook/pull/2491))
* Crashes when opening a notebook with ":" in a section title ([#2495](https://github.com/livebook-dev/livebook/pull/2495))
* Listing S3 directories with space ([#2497](https://github.com/livebook-dev/livebook/pull/2497))
* Invalid redirect on successful authentication when using `LIVEBOOK_BASE_URL_PATH` ([#2516](https://github.com/livebook-dev/livebook/pull/2516))
* Stamp verification when content has been inserted before the stamp ([#2527](https://github.com/livebook-dev/livebook/pull/2527))
* Boot error when parsing `LIVEBOOK_IDENTITY_PROVIDER` ([#2533](https://github.com/livebook-dev/livebook/pull/2533))
* Switching between output tabs having the same height ([#2536](https://github.com/livebook-dev/livebook/pull/2536))
* Erlang variables, such as `JSON`, changing capitalization across cells ([#2556](https://github.com/livebook-dev/livebook/pull/2556))
* Saving and listing files when sibling files contain emoji names on Windows ([#2558](https://github.com/livebook-dev/livebook/pull/2558))
* File drag and drop on Safari ([#2582](https://github.com/livebook-dev/livebook/pull/2582))
* Elixir runtime to stop even if `System.no_halt(true)` is set (as done by Phoenix Playground) ([#2587](https://github.com/livebook-dev/livebook/pull/2587))
* Session assets resolution (for smart cells and outputs) in distributed deployments ([#2611](https://github.com/livebook-dev/livebook/pull/2611))
* Notebook export crashing when output data includes structs (such as dates in VegaLite data)
* Audio and image input preview not working in apps
* `<br>` getting duplicated in Mermaid graphs
* Some Erlang documentation links not working ([[#2653](https://github.com/livebook-dev/livebook/pull/2653)](https://github.com/livebook-dev/livebook/pull/2653))

## v0.12

The CHANGELOG for v0.12 releases can be found in the [v0.12](https://github.com/livebook-dev/livebook/tree/v0.12/CHANGELOG.md) branch.
