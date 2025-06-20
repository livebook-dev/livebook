# Changelog for Livebook v0.16

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [v0.16.4](https://github.com/livebook-dev/livebook/tree/v0.16.4) (2025-06-20)

### Fixed

* Livebook automatic clustering within AWS ECS ([#3022](https://github.com/livebook-dev/livebook/pull/3022))
* Embedding livebook inside iframe with LIVEBOOK_WITHIN_IFRAME ([#3024](https://github.com/livebook-dev/livebook/pull/3024))

## [v0.16.3](https://github.com/livebook-dev/livebook/tree/v0.16.3) (2025-06-17)

### Added

* Support for logging in JSON format via `LIVEBOOK_LOG_FORMAT=json` ([#3017](https://github.com/livebook-dev/livebook/pull/3017))

### Changed

* Updated Docker images to use CUDA 12.8 and include NCCL ([#3018](https://github.com/livebook-dev/livebook/pull/3018))

## [v0.16.2](https://github.com/livebook-dev/livebook/tree/v0.16.2) (2025-06-04)

### Added

* Notebook export to include errors when exporting with outputs ([#3009](https://github.com/livebook-dev/livebook/pull/3009))
* Support for enabling reevaluate automatically for Smart cells

### Fixed

* GitHub stars notebook to execute without errors when GitHubs API rate limit ([#3006](https://github.com/livebook-dev/livebook/pull/3006))
* Startup errors in env.sh when using the Docker deployment ([#3013](https://github.com/livebook-dev/livebook/pull/3013))

## [v0.16.1](https://github.com/livebook-dev/livebook/tree/v0.16.1) (2025-05-21)

### Fixed

* Fly runtime and FLAME workers failing to start due to missing epmd

## [v0.16.0](https://github.com/livebook-dev/livebook/tree/v0.16.0) (2025-05-20)

This release marks Livebook Teams coming out of beta. It also brings experimental support for Python code evaluation. The Python integration is not feature complete and we plan to continue work on it. If you give it a try and you want to provide feedback, feel free to drop a comment on [#2937](https://github.com/livebook-dev/livebook/issues/2937).

### Added

* (Experimental) Support for Python cells ([#2936](https://github.com/livebook-dev/livebook/pull/2936))
* Shortcut to toggle line wrapping in code cells ([#2974](https://github.com/livebook-dev/livebook/pull/2974))
* Support for automatic clustering in AWS execution environments ([#2797](https://github.com/livebook-dev/livebook/pull/2797))
* Example GitHub app notebook to the learn section ([#2990](https://github.com/livebook-dev/livebook/pull/2990))
* Support for OIDC group-based authorization in Teams apps ([#2984](https://github.com/livebook-dev/livebook/pull/2984))

### Changed

* Bumped versions to Elixir 1.18.3 and Erlang 27.3.3 in the Docker image and the desktop app ([#2987](https://github.com/livebook-dev/livebook/pull/2987))

### Fixed

* Improved UX when loading JS output fails or takes long time ([#2975](https://github.com/livebook-dev/livebook/pull/2975))
* Crashes when evaluating code that does a `:setopts` IO request ([#2983](https://github.com/livebook-dev/livebook/pull/2983))
* Missing box borders in app outputs
* Handling of `livebook://` URLs on macOS and Windows ([#3002](https://github.com/livebook-dev/livebook/pull/3002))

## v0.15

The CHANGELOG for v0.15 releases can be found in the [v0.15](https://github.com/livebook-dev/livebook/tree/v0.15/CHANGELOG.md) branch.
