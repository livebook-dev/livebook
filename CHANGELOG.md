# Changelog for Livebook v0.14

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [v0.14.0-rc.0](https://github.com/livebook-dev/livebook/tree/v0.14.0-rc.0) (2024-07-26)

### Added

* Introduced Fly.io runtime ([#2708](https://github.com/livebook-dev/livebook/pull/2708), [#2714](https://github.com/livebook-dev/livebook/pull/2714))
* Added `LIVEBOOK_PUBLIC_BASE_URL_PATH` to serve public routes from a different base url path ([#2704](https://github.com/livebook-dev/livebook/pull/2704))
* Suggestion to setup without cache on dependencies error ([#2716](https://github.com/livebook-dev/livebook/pull/2716))
* Added FLAME runner cell

### Changed

* Bumped versions to Elixir 1.17.2 in the Docker image and the desktop app
* Change the scroll behavior to immediate jump when clicking a notebook sections ([#2705](https://github.com/livebook-dev/livebook/pull/2705))
* Reduced the size of CUDA-enabled images by including only a subset of the CUDA toolkit ([#2724](https://github.com/livebook-dev/livebook/pull/2724))

### Fixed

* (Desktop) Fixed opening an already open notebook ([#2709](https://github.com/livebook-dev/livebook/pull/2709))
* Fixed horizontal editor scrollbar to not overlap content in Firefox

## v0.13

The CHANGELOG for v0.13 releases can be found in the [v0.13](https://github.com/livebook-dev/livebook/tree/v0.13/CHANGELOG.md) branch.
