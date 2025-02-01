# Changelog for Livebook v0.14

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [v0.14.7](https://github.com/livebook-dev/livebook/tree/v0.14.7) (2025-02-01)

### Fixed

* Teams websocket not reconnecting after a graceful close ([#2924](https://github.com/livebook-dev/livebook/pull/2924))

## [v0.14.6](https://github.com/livebook-dev/livebook/tree/v0.14.6) (2025-01-23)

### Added

* Extra logging around Livebook connection to Teams ([#2914](https://github.com/livebook-dev/livebook/pull/2914))

## [v0.14.5](https://github.com/livebook-dev/livebook/tree/v0.14.5) (2024-10-25)

### Fixed

* Imports and process dictionary being erased after errored evaluation ([#2822](https://github.com/livebook-dev/livebook/pull/2822))
* PVC deletion removing all PVCs in Kubernetes runtime settings

## [v0.14.4](https://github.com/livebook-dev/livebook/tree/v0.14.4) (2024-10-04)

### Added

* Documented stamping ([#2815](https://github.com/livebook-dev/livebook/pull/2815))

### Fixed

* Crashes under certain `exec` configuration when starting K8s runtime ([#2817](https://github.com/livebook-dev/livebook/pull/2817))
* Fixed Teams app deployments with attachment files
* App deployment continuing retries even once the app is deactivated

## [v0.14.3](https://github.com/livebook-dev/livebook/tree/v0.14.3) (2024-10-03)

### Added

* Support for go back and go forward in cell editors using keyboard shortcuts ([#2789](https://github.com/livebook-dev/livebook/pull/2789))
* Setting to disable autoclosing brackets in the editor ([#2805](https://github.com/livebook-dev/livebook/pull/2805))

### Changed

* Module names in the outline to always be shown in full ([#2810](https://github.com/livebook-dev/livebook/pull/2810))

### Fixed

* Crashes under certain `exec` configuration when starting K8s runtime ([#2793](https://github.com/livebook-dev/livebook/pull/2793))
* Handling of Erlang syntax errors ([#2800](https://github.com/livebook-dev/livebook/pull/2800))
* Fixed missing code highlighting on certain function calls

## [v0.14.2](https://github.com/livebook-dev/livebook/tree/v0.14.2) (2024-09-20)

### Fixed

* Fixed warning about missing kubectl to show only when applicable

## [v0.14.1](https://github.com/livebook-dev/livebook/tree/v0.14.1) (2024-09-20)

### Added

* Liveobok metadata to Fly runtime machines ([#2763](https://github.com/livebook-dev/livebook/pull/2763))
* Added code highlighting for Python snippets ([#2772](https://github.com/livebook-dev/livebook/pull/2772))
* Added menu option for FLAME runner cell to use Kubernetes backend ([#2762](https://github.com/livebook-dev/livebook/pull/2762))
* Added menu option for Database connection cell to use DuckDB ([#2773](https://github.com/livebook-dev/livebook/pull/2773))
* Ability to disconnect Fly runtime during initialization ([#2776](https://github.com/livebook-dev/livebook/pull/2776))
* Progress indicator to the file input upload ([#2785](https://github.com/livebook-dev/livebook/pull/2785))
* Introduced K8s runtime ([#2756](https://github.com/livebook-dev/livebook/pull/2756))

### Changed

* Updated CUDA version in the Docker image to 12.6
* Improved go-to-definition scroll to position the cursor near the viewport top

### Removed

### Fixed

* Editor rendering on socket reconnection ([#2765](https://github.com/livebook-dev/livebook/pull/2765))
* Fixed URL validation when opening notebook from URL to allow file:// ([#2783](https://github.com/livebook-dev/livebook/pull/2783))
* Fixed app recovery when the whole runtime goes down
* Crash caused by clock drifts when showing timestamps from Livebook Teams server ([#2787](https://github.com/livebook-dev/livebook/pull/2787))
* Crash when disconnecting remote runtime on unreachable node

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
