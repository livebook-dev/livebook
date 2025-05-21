# Changelog for Livebook v0.16

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

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
