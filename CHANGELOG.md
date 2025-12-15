# Changelog for Livebook v0.18

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [v0.18.2](https://github.com/livebook-dev/livebook/tree/v0.18.2) (2025-12-15)

### Added

* Local network access within iframes ([#3107](https://github.com/livebook-dev/livebook/pull/3107))
* `--dry-run` flag to `livebook deploy` ([#3109](https://github.com/livebook-dev/livebook/pull/3109))

### Fixed

* CLI Teams deployment failing for notebooks with app folders ([#3108](https://github.com/livebook-dev/livebook/pull/3108))

## [v0.18.1](https://github.com/livebook-dev/livebook/tree/v0.18.1) (2025-12-07)

### Fixed

* Erlang packages failing to install due to old rebar3 version

## [v0.18.0](https://github.com/livebook-dev/livebook/tree/v0.18.0) (2025-12-01)

### Added

* `LIVEBOOK_IMAGE_REGISTRY_URL` env var to configure a custom Docker image registry ([#3066](https://github.com/livebook-dev/livebook/pull/3066))
* Git file storage for Teams app servers ([#3056](https://github.com/livebook-dev/livebook/pull/3056), [#3075](https://github.com/livebook-dev/livebook/pull/3075))
* New fields to `LIVEBOOK_LOG_METADATA` to support structured data in code evaluation logs ([#3077](https://github.com/livebook-dev/livebook/pull/3077), [#3078](https://github.com/livebook-dev/livebook/pull/3078))
* Support for grouping Teams apps into folders and updated apps page ([#3088](https://github.com/livebook-dev/livebook/pull/3088), [#3091](https://github.com/livebook-dev/livebook/pull/3091), [#3099](https://github.com/livebook-dev/livebook/pull/3099), [#3101](https://github.com/livebook-dev/livebook/pull/3101))
* Support for Google Workspace OIDC in Teams apps ([#3102](https://github.com/livebook-dev/livebook/pull/3102))

### Changed

* Drag-and-drop of `.csv` and `.parquet` files to generate ADBC code ([#3085](https://github.com/livebook-dev/livebook/pull/3085))

### Removed

* `LIVEBOOK_FIPS` env var using deprecated OTP API in favour of `ERL_AFLAGS="-crypto fips_mode true"` ([#3097](https://github.com/livebook-dev/livebook/pull/3097))

## v0.17

The CHANGELOG for v0.17 releases can be found in the [v0.17](https://github.com/livebook-dev/livebook/tree/v0.17/CHANGELOG.md) branch.
