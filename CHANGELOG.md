# Changelog for Livebook v0.14

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [v0.14.0](https://github.com/livebook-dev/livebook/tree/v0.14.0) (2024-08-26)

### Added

* Introduced Fly.io runtime ([#2708](https://github.com/livebook-dev/livebook/pull/2708), [#2714](https://github.com/livebook-dev/livebook/pull/2714))
* Added `LIVEBOOK_PUBLIC_BASE_URL_PATH` to serve public routes from a different base url path ([#2704](https://github.com/livebook-dev/livebook/pull/2704))
* Suggestion to setup without cache on dependencies error ([#2716](https://github.com/livebook-dev/livebook/pull/2716))
* Added FLAME runner cell
* Support for specifying column ratios in grid output ([#2718](https://github.com/livebook-dev/livebook/pull/2718))
* Added go-to-definition feature for modules and functions defined in the notebook ([#2730](https://github.com/livebook-dev/livebook/pull/2730), [#2741](https://github.com/livebook-dev/livebook/pull/2741))
* Documented how to use Nginx as a HTTPS proxy in front of Livebook ([#2735](https://github.com/livebook-dev/livebook/pull/2735))
* Introduced nightly Docker image builds, tagged as `nightly` and `nightly-cuda12` ([#2739](https://github.com/livebook-dev/livebook/pull/2739))
* Highlight for the relevant symbol when showing hover details in the code editor ([#2747](https://github.com/livebook-dev/livebook/pull/2747))
* Link to live dashboard for the runtime node in the runtime panel ([#2754](https://github.com/livebook-dev/livebook/pull/2754))
* Showing module definitions under the corresponding sections in the side panel ([#2760](https://github.com/livebook-dev/livebook/pull/2760))

### Changed

* Bumped versions to Elixir 1.17.2 in the Docker image and the desktop app
* Change the scroll behavior to immediate jump when clicking a notebook sections ([#2705](https://github.com/livebook-dev/livebook/pull/2705))
* Reduced the size of CUDA-enabled images by including only a subset of the CUDA toolkit ([#2724](https://github.com/livebook-dev/livebook/pull/2724))
* (Desktop) The Windows installer to execute as normal user ([#2628](https://github.com/livebook-dev/livebook/pull/2628))
* New Docker images with CUDA 12 are now tagged `*-cuda12` ([#2739](https://github.com/livebook-dev/livebook/pull/2739))
* Restored browser-default outlines on tab-navigation ([#2749](https://github.com/livebook-dev/livebook/pull/2749))
* Docker images with CUDA 12 now include cuDNN 9 for compatibility with nx/exla v0.8+
* Renamed the "Sections" side panel to "Outline" (now under the `so` shortcut)
* Changed shortcut for showing secrets panel from `se` to `ss`

### Removed

* Docker `edge` images are no longer built, use `nightly` instead ([#2739](https://github.com/livebook-dev/livebook/pull/2739))
* Docker images with CUDA 11 (`*-cuda118`) are no longer built ([#2739](https://github.com/livebook-dev/livebook/pull/2739))
* `XLA_TARGET` env var is no longer included in Dockerfiles generated for apps. When using nx/exla < v0.8 you may need to set it explicitly ([#2755](https://github.com/livebook-dev/livebook/pull/2755))

### Fixed

* (Desktop) Fixed opening an already open notebook ([#2709](https://github.com/livebook-dev/livebook/pull/2709))
* Fixed horizontal editor scrollbar to not overlap content in Firefox

## v0.13

The CHANGELOG for v0.13 releases can be found in the [v0.13](https://github.com/livebook-dev/livebook/tree/v0.13/CHANGELOG.md) branch.
